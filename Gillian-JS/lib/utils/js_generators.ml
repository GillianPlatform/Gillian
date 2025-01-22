let fresh_sth (name : string) : (unit -> string) * (unit -> unit) =
  let counter = ref 0 in
  let f () =
    let v = name ^ string_of_int !counter in
    counter := !counter + 1;
    v
  in
  let r () = counter := 0 in
  (f, r)

let fresh_lvar, reset_lvar = fresh_sth "_lvar_js_"
let fresh_pvar, reset_pvar = fresh_sth "pvar_js_"
let fresh_lvar () = Gil_syntax.LVar.of_string (fresh_lvar ())
let fresh_pvar () = Gil_syntax.Var.of_string (fresh_pvar ())

let reset () =
  reset_lvar ();
  reset_pvar ()
