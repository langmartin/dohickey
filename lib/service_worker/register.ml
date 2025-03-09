open Js_of_ocaml.Js

class type setting = object
  method scope : js_string t prop
end

let navigator = Unsafe.pure_js_expr "navigator"

let register service setting =
  let console = Console.console in
  let m = Unsafe.get navigator "serviceWorker" |> def in
  Optdef.iter m (fun _f ->
      let promise : Promise.promise t = Unsafe.meth_call m "register" [| Unsafe.inject service; setting; |] in
      (promise##_then (fun _ -> console##log (string "registered")))##_catch (fun e -> console##log (string "can not registered"); console##log (e)) |> ignore
    )

let () = ignore begin
    register "/service_worker.js" (Unsafe.obj [| ("scope", Unsafe.inject (string "/")) |])
  end
