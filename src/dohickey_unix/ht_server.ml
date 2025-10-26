(*
   Items are handled over the websocket.
*)

let handle_client username table websocket =
  match username with
  | Some username ->
    let client = Client.create username table websocket in
    Client_runner.start client
  | None -> Lwt.return_unit

let json_string_list xs =
  let jstr s = `String s in
  let jlst xs = `List (List.map jstr xs) in
  xs
  |> jlst
  |> Yojson.Basic.to_string

let create_table user name =
  let open Dohickey in
  let id = Friendly.dohickey_name name in
  let ts = World.send() |> Hulc.sprint in
  ignore @@ World.puts id [Item.of_title name user ts];
  id

(*
   Authentication.
*)

let username_cookie = Public.username_cookie
let username req = Dream.cookie req username_cookie

let dream_authentication inner_handler request =
  match username request with
  | Some _username -> inner_handler request
  | None -> Dream.respond ~code:302 ~headers:[("location", "/sign-in")] ""

(*
   Routing.
*)

let start_server listen_ip listen_port =
  Dream.serve ~interface:listen_ip ~port:listen_port
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.set_secret "super duper secret"
  @@ Dream.router [
    Dream.get "/sign-in" (Public.html "sign-in.html");
    Dream.post "/sign-in"
      (fun request ->
         match%lwt Dream.form ~csrf:false request with
         | `Ok ["username", username_cookie] ->
           let response = Dream.response ~code:302 "" in
           Dream.set_cookie response request "username" username_cookie;
           Dream.set_header response "location" "/a1/";
           Lwt.return response

         | _ -> Dream.empty `Bad_Request
      );

    Dream.scope "/a1" [dream_authentication] [
      Dream.get "/" (Public.html "index.html");
      Dream.get "/table" (Public.html "table.html");

      Dream.get "tables"
        (fun _request ->
           Dream.respond
             ~headers:["content-type", "application/json"]
             (World.tables() |> json_string_list));

      Dream.post "/tables"
        (fun request ->
           match%lwt Dream.form ~csrf:false request with
           | `Ok ["name", name] ->
             (match Dream.cookie request username_cookie with
              | Some user ->
                let id = create_table user name in
                Dream.respond
                  ~headers:["content-type", "application/json"]
                  (json_string_list [id])
              | None -> Dream.empty `Bad_Request)
           | _ -> Dream.empty `Bad_Request);

      Dream.get "/main.css" (Public.css "main.css");
      Dream.get "/js_client.js" (Public.static_js "js_client.js");
      Dream.get "/js_service_worker.js" (Public.static_js "js_service_worker.js");
      Dream.get "/socket/:table"
        (fun req ->
           let table = Dream.param req "table" in
           Dream.websocket (handle_client (username req) table);
        );
    ];
  ]
