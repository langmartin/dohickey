(*
   Read the file contents from public. I'm sure there's a better way
   to do this, but the def mode switch allows editing without
   recompiling for local development. The switch can only be flipped
   from the repl, so in releases we'll use the blobs.
*)

let username_cookie = "username"

let dev_mode = ref false

let blobs = [
  ("table.html", [%blob "../public/table.html"]);
  ("index.html", [%blob "../public/index.html"]);
  ("sign-in.html", [%blob "../public/sign-in.html"]);
  ("main.css", [%blob "../public/main.css"]);

  (* See https://github.com/ocaml/dune/issues/3499 *)
  ("js_client.js", [%blob "../../static/js_client.js"]);
  ("js_service_worker.js", [%blob "../../static/js_service_worker.js"])
]

let slurp dir file =
  print_endline ("FRESH " ^ file);
  let file = Filename.concat dir file in
  let ch = open_in_bin file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let contents dir file =
  if !dev_mode then
    slurp dir file
  else
    List.assoc file blobs

let render content username =
  Mustache.render
    (Mustache.of_string content)
    (`O ["username", `String username])

let html file request =
  let content = (contents "src/public" file) in
  let content =
    match Dream.cookie request username_cookie with
    | Some u -> render content u
    | None -> content
  in
  Dream.respond
    ~headers:["content-type", "text/html"]
    content

let css file _request =
  Dream.respond
    ~headers:["content-type", "text/css"]
    (contents "src/public" file)

let static_js file _request =
  Dream.respond
    ~headers:["content-type", "application/javascript"]
    (contents "static" file)
