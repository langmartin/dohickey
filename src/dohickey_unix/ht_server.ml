let public = [
  ("table", [%blob "../public/table.html"]);
  ("index", [%blob "../public/index.html"]);
  ("sign_in", [%blob "../public/sign-in.html"])
]

let html content =
  fun _request ->
  Dream.respond
    ~headers:["content-type", "text/html"]
    content

let js content =
  fun _request ->
  Dream.respond
    ~headers:["content-type", "application/json"]
    content

let handle_items table items =
  items
  |> Yojson.Safe.from_string
  |> Json.of_json
  |> World.puts table

let handle_client table websocket =
  let _client_id = World.add_client websocket in
  let rec loop () =
    match%lwt Dream.receive websocket with
    | Some items ->
      let%lwt () = handle_items table items in
      loop ()
    | None ->
      (* FIXME this seems to happen early. World.stop_client client_id; *)
      Dream.close_websocket websocket
  in
  loop ()

let start_server listen_ip listen_port =
  Dream.serve ~interface:listen_ip ~port:listen_port
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [

    Dream.get "/index.html" (html (List.assoc "index" public));
    Dream.get "/table.html" (html (List.assoc "table" public));
    Dream.get "/sign-in.html" (html (List.assoc "sign_in" public));

    (* FIXME for development only *)
    Dream.get "/public/**" (Dream.static "src/public");
    Dream.get "/static/**" (Dream.static "static");

    Dream.post "/sign-in"
      (fun request ->
         match%lwt Dream.form ~csrf:false request with
         | `Ok ["username", username] ->
           let response = Dream.response ~code:302 "" in
           Dream.set_cookie response request "username" username;
           Dream.set_header response "location" "index.html";
           Lwt.return response

         | _ -> Dream.empty `Bad_Request
      );

    Dream.get "/a1/socket"
      (fun request ->
         match Dream.headers request "host" with
         | host :: _ -> Dream.websocket (fun websocket -> handle_client host websocket);
         | _ -> Dream.respond ~code:404 ""
      );

    Dream.get "/a1/socket/:table"
      (fun req ->
         let table = Dream.param req "table" in
         Dream.websocket (fun websocket -> handle_client table websocket);
      );

    Dream.post "/a1/send/:table"
      (fun req ->
         let table = Dream.param req "table" in
         let%lwt items = Dream.body req in
         let%lwt () = handle_items table items in
         Dream.empty `OK);

  ]
