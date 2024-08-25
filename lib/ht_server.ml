let index = [%blob "../public/index.html"]
let table = [%blob "../public/table.html"]
let sign_in = [%blob "../public/sign-in.html"]

let html content =
  fun _request ->
  Dream.respond
    ~headers:["content-type", "text/html"]
    content

let handle_items table items =
  items
  |> Yojson.Safe.from_string
  |> Items.of_json
  |> World.puts table

let handle_client table websocket =
  let client_id = World.add_client websocket in
  let rec loop () =
    match%lwt Dream.receive websocket with
    | Some items ->
      handle_items table items;
      loop ()
    | None ->
      World.stop_client client_id;
      Dream.close_websocket websocket
  in
  loop ()

let start_server listen_ip listen_port =
  Dream.run ~interface:listen_ip ~port:listen_port
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [

    Dream.get "/index.html" (html index);
    Dream.get "/table.html" (html table);
    Dream.get "/sign-in.html" (html sign_in);

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

    Dream.get "/a1/socket/:table"
      (fun _request ->
         Dream.websocket (fun websocket -> handle_client table websocket);
      );

    Dream.post "/a1/send/:table"
      (fun request ->
         let%lwt items = Dream.body request in
         handle_items table items;
         (* store & broadcast items *)
         Dream.empty `OK);

  ]
