let sign_in = [%blob "../public/sign-in.html"]
let index = [%blob "../public/index.html"]

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [

    Dream.post "/sign-in"
      (fun request ->
         match%lwt Dream.form ~csrf:false request with
         | `Ok ["username", username] ->
           let response = Dream.response ~code:302 "" in
           Dream.set_cookie response request "username" username;
           Dream.set_header response "location" "index";
           Lwt.return response

         | _ -> Dream.empty `Bad_Request
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
