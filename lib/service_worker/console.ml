open Js_of_ocaml.Js

class type console = object
  method log : 'a t -> unit meth
end

let console : console t = Unsafe.pure_js_expr "console"
