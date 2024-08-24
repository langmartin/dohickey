let index = [%blob "../public/index.html"]
let table = [%blob "../public/table.html"]
let sign_in = [%blob "../public/sign-in.html"]

let html content =
  fun _request ->
  Dream.respond
    ~headers:["content-type", "text/html"]
    content

let handle_items _table items = 
  let _xs = items
           |> Yojson.Safe.from_string
           |> Items.items_of_json
  in
  ()

let () =
  Dream.run
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
         Dream.websocket (fun websocket ->
             match%lwt Dream.receive websocket with
             | Some items ->
               handle_items table items;
               Dream.send websocket "ok"
             | _ -> Dream.close_websocket websocket
           );
      );

    Dream.post "/a1/send"
      (fun request ->
         let%lwt body = Dream.body request in
         let _items =
           body
           |> Yojson.Safe.from_string
           |> Items.items_of_json
         in
         (* store & broadcast items *)
         Dream.empty `OK);

  ]
