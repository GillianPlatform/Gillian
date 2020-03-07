(***** This module defines a Wisl Symbolic Heap *******)
open Gillian.Symbolic
open Gillian.Gil_syntax

type t = (string, SFVL.t) Hashtbl.t

(* A symbolic heap is a map from location and offset to symbolic values *)

let init () = Hashtbl.create 1

(* Simply initializes an empty heap *)

(****** Standard stuff about hashtbls ********)

let copy heap = Hashtbl.copy heap

let to_list heap = Hashtbl.fold (fun loc fvl ac -> (loc, fvl) :: ac) heap []

let get_fvl = Hashtbl.find_opt

let set_fvl = Hashtbl.replace

let remove = Hashtbl.remove

(***** Implementation of local actions *****)

let alloc heap size =
  let loc = ALoc.alloc () in
  let expr_of_int i = Expr.Lit (Literal.Int i) in
  let rec get_list current_offset =
    if current_offset < 0 then []
    else
      (expr_of_int current_offset, Expr.Lit Literal.Null)
      :: get_list (current_offset - 1)
  in
  let l = get_list (size - 1) in
  let sfvl = SFVL.of_list l in
  let () = set_fvl heap loc sfvl in
  loc

(***** Some things specific to symbolic heaps ********)

let merge_loc heap new_loc old_loc =
  let osfvl = Option.value ~default:SFVL.empty (get_fvl heap old_loc) in
  let nsfvl = Option.value ~default:SFVL.empty (get_fvl heap new_loc) in
  let sfvl = SFVL.union nsfvl osfvl in
  let () = set_fvl heap new_loc sfvl in
  remove heap old_loc

let substitution_in_place subst heap =
  (* First we replace in the offset and values using fvl *)
  let () =
    Hashtbl.iter
      (fun loc fvl -> set_fvl heap loc (SFVL.substitution subst true fvl))
      heap
  in
  (* Then we replace within the locations themselves *)
  let aloc_subst =
    Subst.filter subst (fun var _ -> Gillian.Utils.Names.is_aloc_name var)
  in
  Subst.iter aloc_subst (fun aloc new_loc ->
      let new_loc_str =
        match new_loc with
        | Expr.Lit (Literal.Loc loc) -> loc
        | Expr.ALoc loc              -> loc
        | _                          ->
            raise
              (Failure
                 (Printf.sprintf "Heap substitution fail for loc: %s"
                    ((WPrettyUtils.to_str Expr.pp) new_loc)))
      in
      merge_loc heap new_loc_str aloc)

let assertions heap =
  let constr loc offset value =
    let cell = WislLActions.(str_ga Cell) in
    Asrt.GA (cell, [ loc; offset ], [ value ])
  in
  let make_loc_lexpr loc =
    if Gillian.Utils.Names.is_aloc_name loc then Expr.ALoc loc
    else Expr.Lit (Literal.Loc loc)
  in
  let assertions_of_loc (loc, fv_list) =
    let le_loc = make_loc_lexpr loc in
    SFVL.assertions_with_constructor ~constr le_loc fv_list
  in
  List.sort Asrt.compare
    (List.concat (List.map assertions_of_loc (to_list heap)))

(***** small things useful for printing ******)

(* let str_cell loc offset value =
  Printf.sprintf "(%s + %i) -> %s" loc offset (Values.str value)

let rec insert (a, b) l =
  match l with
  | [] -> [(a, b)]
  | (x, y)::r when x < a -> (x, y)::(insert (a, b) r)
  | l -> (a, b)::l *)

let str heap =
  let vstr = WPrettyUtils.to_str Values.pp in
  let bindings =
    Hashtbl.fold
      (fun loc sfvl ac ->
        match SFVL.to_list sfvl with
        | [] -> ac
        | l  -> (loc, l) :: ac)
      heap []
  in
  let one_loc_str loc l =
    String.concat "\n"
      (List.map
         (fun (offset, value) ->
           Printf.sprintf "(%s, %s) -> %s" loc (vstr offset) (vstr value))
         l)
  in
  let all_string =
    String.concat "\n\n"
      (List.map (fun (loc, assocs) -> one_loc_str loc assocs) bindings)
  in
  Printf.sprintf "{\n%s\n}" all_string
