(** GIL Typing Environment *)

open Names
open SVal
module L = Logging

type k = Id.any_var Id.t [@@deriving yojson]
type t = (k, Type.t) Hashtbl.t [@@deriving yojson]

let as_hashtbl x = x

(*************************************)
(** Typing Environment Functions    **)

(*************************************)

(* Initialisation *)
let init () : t = Hashtbl.create Config.medium_tbl_size

(* Copy *)
let copy : t -> t = Hashtbl.copy

(* Type of a variable *)
let get (x : t) (k : [< Id.any_var ] Id.t) : Type.t option =
  Hashtbl.find_opt x (k :> Id.any_var Id.t)

(* Membership *)
let mem (x : t) (k : [< Id.any_var ] Id.t) : bool =
  Hashtbl.mem x (k :> Id.any_var Id.t)

(* Empty *)
let empty (x : t) : bool = Hashtbl.length x == 0

(* Type of a variable *)
let get_unsafe (x : t) (k : [< Id.any_var ] Id.t) : Type.t =
  try Hashtbl.find x (k :> Id.any_var Id.t)
  with _ -> Fmt.failwith "Type_env.get_unsafe: variable %a not found." Id.pp k

(* Get all logical variables *)
let lvars (x : t) : LVar.Set.t =
  Hashtbl.fold
    (fun var _ ac ->
      let var = Id.str var in
      if is_lvar_name var then LVar.Set.add (LVar.of_string var) ac else ac)
    x LVar.Set.empty

(* Get all variables of specific type *)
let get_vars_of_type (x : t) (tt : Type.t) : k list =
  Hashtbl.fold
    (fun var t ac_vars -> if t = tt then var :: ac_vars else ac_vars)
    x []

(* Get all var-type pairs as a list *)
let get_var_type_pairs : t -> (k * Type.t) Seq.t = Hashtbl.to_seq

(* Iteration *)
let iter (x : t) (f : k -> Type.t -> unit) : unit = Hashtbl.iter f x

let fold (x : t) (f : k -> Type.t -> 'a -> 'a) (init : 'a) : 'a =
  Hashtbl.fold f x init

let pp fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%a: %s)" Id.pp v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

let pp_by_need vars fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%a: %s)" Id.pp v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  let bindings =
    List.filter (fun (v, _) -> Id.Sets.VarSet.mem v vars) bindings
  in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

(* Update with removal *)

let update (te : t) (k : [< Id.any_var ] Id.t) (t : Type.t) : unit =
  match get te (k :> Id.any_var Id.t) with
  | None -> Hashtbl.replace te (k :> Id.any_var Id.t) t
  | Some t' when t' = t -> ()
  | Some t' ->
      Fmt.failwith
        "Type_env update: Conflict: %a has type %s but required extension is %s"
        Id.pp k (Type.str t') (Type.str t)

let remove (te : t) (k : [< Id.any_var ] Id.t) : unit =
  Hashtbl.remove te (k :> Id.any_var Id.t)

(* Extend gamma with more_gamma *)
let extend (x : t) (y : t) : unit =
  iter y (fun v t ->
      match Hashtbl.find_opt x v with
      | None -> Hashtbl.replace x v t
      | Some t' ->
          if t <> t' then
            raise (Failure "Typing environment cannot be extended."))

(* Filter using function on variables *)
let filter (x : t) (f : k -> bool) : t =
  let new_gamma = init () in
  iter x (fun v v_type -> if f v then update new_gamma v v_type);
  new_gamma

(* Filter using function on variables *)
let filter_in_place (x : t) (f : k -> bool) : unit =
  iter x (fun v _ -> if not (f v) then remove x v)

(* Filter for specific variables *)
let filter_vars (gamma : t) (vars : Id.Sets.VarSet.t) : t =
  filter gamma (fun v -> Id.Sets.VarSet.mem v vars)

(* Filter for specific variables *)
let filter_vars_in_place (gamma : t) (vars : Id.Sets.VarSet.t) : unit =
  filter_in_place gamma (fun v -> Id.Sets.VarSet.mem v vars)

(* Perform substitution, return new typing environment *)
let substitution (x : t) (subst : SESubst.t) (partial : bool) : t =
  let new_gamma = init () in
  iter x (fun var v_type ->
      let evar = Expr.var_to_expr var in
      let new_var = SESubst.get subst evar in
      match new_var with
      | Some (LVar new_var) -> update new_gamma (new_var :> k) v_type
      | Some _ -> if partial then update new_gamma var v_type
      | None ->
          if partial then update new_gamma var v_type
          else if Names.is_lvar_name (Id.str var) then (
            let new_lvar = LVar.alloc () in
            SESubst.put subst evar (LVar new_lvar);
            update new_gamma (new_lvar :> k) v_type));
  new_gamma

let to_list_expr (x : t) : (Expr.t * Type.t) list =
  Hashtbl.fold (fun x t pairs -> (Expr.var_to_expr x, t) :: pairs) x []

let to_list (x : t) : (k * Type.t) list =
  Hashtbl.fold (fun x t pairs -> (x, t) :: pairs) x []

let reset (x : t) (reset : (k * Type.t) list) =
  Hashtbl.clear x;
  List.iter (fun (y, t) -> Hashtbl.replace x y t) reset

let is_well_formed (_ : t) : bool = true

let filter_with_info (pvars, lvars, _) (x : t) =
  let open Id.Sets in
  let relevant = VarSet.union (pvar_to_varset pvars) (lvar_to_varset lvars) in
  filter x (fun k -> VarSet.mem k relevant)
