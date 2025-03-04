(* id generator for AST *)
let curr_id = ref 0

let gen_id () =
  let nid = !curr_id in
  curr_id := !curr_id + 1;
  nid

(* label/var id generator *)
(* Maps label prefix and function name/gvar to current label/var id *)
let idHash = Hashtbl.create 1

let gen_str ~(fname : string) pre =
  let curr_id =
    Option.value ~default:0 (Hashtbl.find_opt idHash (fname, pre))
  in
  let () = Hashtbl.replace idHash (fname, pre) (curr_id + 1) in
  pre ^ string_of_int curr_id

let reset () =
  Hashtbl.reset idHash;
  curr_id := 0
