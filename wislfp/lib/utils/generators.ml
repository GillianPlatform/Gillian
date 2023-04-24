(* id generator for AST *)
let curr_id = ref 0

let gen_id () =
  let nid = !curr_id in
  curr_id := !curr_id + 1;
  nid

(* label/var id generator *)
(* Maps label prefix and function name/gvar to current label/var id *)
let idHash = Hashtbl.create 1

let gen_str fname pre =
  let curr_id =
    match Hashtbl.find_opt idHash (fname, pre) with
    | Some x -> x
    | None -> 0
  in
  let () = Hashtbl.replace idHash (fname, pre) (curr_id + 1) in
  pre ^ string_of_int curr_id

let reset () =
  curr_id := 0;
  Hashtbl.reset idHash
