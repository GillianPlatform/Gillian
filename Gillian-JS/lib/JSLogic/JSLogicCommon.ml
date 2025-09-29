module L = Logging
module Expr = Gillian.Gil_syntax.Expr
open Jsil_syntax

(** Tables *)
module SS = Containers.SS

let small_tbl_size = 1
let medium_tbl_size = 1

type var_to_fid_tbl_type = (string, string) Hashtbl.t
type cc_tbl_type = (string, var_to_fid_tbl_type) Hashtbl.t

type fun_tbl_type =
  ( string,
    string * string list * JS_Parser.Syntax.exp option * bool * Spec.t option
  )
  Hashtbl.t

type pre_fun_tbl_type =
  ( string,
    string
    * string list
    * JS_Parser.Syntax.exp option
    * bool
    * (JS_Parser.Syntax.annotation list
      * string list
      * (string, string) Hashtbl.t) )
  Hashtbl.t

type vis_tbl_type = (string, string list) Hashtbl.t

let get_vis_list (vis_tbl : vis_tbl_type) (fid : string) =
  try Hashtbl.find vis_tbl fid
  with _ ->
    let msg =
      Printf.sprintf "vis-list of function %s is not in vis-table" fid
    in
    L.fail msg

let get_scope_table (cc_tbl : cc_tbl_type) (fid : string) =
  try Hashtbl.find cc_tbl fid
  with _ ->
    let msg = Printf.sprintf "var tbl of function %s is not in cc-table" fid in
    L.fail msg

let main_fid = !Config.entry_point
let pi_pred_name = "Pi"
let object_class = "Object"
let syntax_error_pred_name = "SyntaxError"
let type_error_pred_name = "TypeError"
let initial_heap_pre_pred_name = "initialHeapPre"
let initial_heap_post_pred_name = "initialHeapPost"
let function_object_pred_name = "function_object"
let standard_object_pred_name = "standardObject"
let this_logic_var_name = "#this"
let locGlobName = "$lg"
let var_te = "x__te"
let var_se = "x__se"
let var_er = "x__er"
let var_this = "x__this"
let var_scope = "x__scope"
let logic_var_scope = "#x__scope"
let var_scope_final = "x__scope_f"
let funobj_pred_name = "JSFunctionObject"
let js_obj_internal_fields = [ "@proto"; "@class"; "@extensible" ]

(** Fresh Names *)

let fid_to_lvar fid = "#fid_" ^ fid

let fid_to_lvar_fresh =
  let fids_tbl = Hashtbl.create 1 in
  fun fid ->
    let fid_count = try Hashtbl.find fids_tbl fid with Not_found -> 0 in
    Hashtbl.replace fids_tbl fid (fid_count + 1);
    "#fid_" ^ string_of_int fid_count ^ "_" ^ fid

let vislist_2_les (_ : string list) (i : int) : Expr.t list =
  Array.to_list
    (Array.init i (fun j ->
         if j = 0 then Expr.Lit (Loc locGlobName)
         else Expr.LVar (Javert_utils.Js_generators.fresh_lvar ())))

(** Lists *)

let find_in_list (lst : string list) (x : string) =
  let rec loop lst i =
    match lst with
    | [] -> raise (Failure "DEATH: string not found in list")
    | y :: rest -> if x = y then i else loop rest (i + 1)
  in
  loop lst 0

let list_overlap (lst_1 : string list) (lst_2 : string list) =
  (* Format.printf "List overlap:\n\t%s\n\t%s" (String.concat ", " lst_1)
     (String.concat ", " lst_2); *)
  let rec loop lst_1 lst_2 i =
    match (lst_1, lst_2) with
    | [], _ | _, [] -> i
    | x1 :: rest_1, x2 :: rest_2 ->
        if x1 = x2 then loop rest_1 rest_2 (i + 1) else i
  in
  loop lst_1 lst_2 0

(** Scope Clarification *)

let psi
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fid : string)
    (x : string) =
  let var_to_fid_tbl = get_scope_table cc_tbl fid in
  try
    let fid' = Hashtbl.find var_to_fid_tbl x in
    let vis_list = get_vis_list vis_tbl fid in
    let i =
      try find_in_list vis_list fid'
      with Not_found -> raise (Failure "DEATH. psi: find_in_list")
    in
    Some i
  with Not_found -> None

let o_psi (vis_tbl : vis_tbl_type) (fid1 : string) (fid2 : string) =
  let vis_list_1 =
    try get_vis_list vis_tbl fid1
    with Not_found -> raise (Failure "DEATH. o_psi: get_vis_list")
  in
  let vis_list_2 =
    try get_vis_list vis_tbl fid2
    with Not_found -> raise (Failure "DEATH. o_psi: get_vis_list")
  in
  let i_overlap = list_overlap vis_list_1 vis_list_2 in
  i_overlap
