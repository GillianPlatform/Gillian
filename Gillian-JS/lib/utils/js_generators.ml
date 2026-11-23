let fresh_sth (name : string) : (unit -> string) * (unit -> unit) =
  let counter = ref 0 in
  let f () =
    let v = name ^ string_of_int !counter in
    counter := !counter + 1;
    v
  in
  let r () = counter := 0 in
  (f, r)

let fresh_lvar, reset_lvar = fresh_sth "#lvar_js_"

(* The underscore of the surface syntax stands for a value of no interest. It
   is a logical variable like any other, but one allocated in a namespace of
   its own so that an [unfold] can tell that an out-parameter was deliberately
   left out (see [is_anon_lvar]). *)
let anon_lvar_prefix = "#lvar_js_anon_"
let fresh_anon_lvar, reset_anon_lvar = fresh_sth anon_lvar_prefix

let is_anon_lvar (x : string) : bool =
  String.starts_with ~prefix:anon_lvar_prefix x

let fresh_pvar, reset_pvar = fresh_sth "pvar_js_"

let reset () =
  reset_lvar ();
  reset_anon_lvar ();
  reset_pvar ()
