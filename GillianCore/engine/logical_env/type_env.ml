(** GIL Typing Environment *)

open SVal
module L = Logging

type constructors_tbl_t = (string, Constructor.t) Hashtbl.t [@@deriving yojson]
type datatypes_tbl_t = (string, Datatype.t) Hashtbl.t [@@deriving yojson]
type k = Id.any_var Id.t
type t = (k, Type.t) Hashtbl.t

let to_yojson : t -> Yojson.Safe.t =
  Hashtbl.to_yojson Id.Any_var.to_yojson Type.to_yojson

let of_yojson : Yojson.Safe.t -> (t, string) result =
  Hashtbl.of_yojson Id.Any_var.of_yojson Type.of_yojson

let key (x : [< Id.any_var ] Id.t) : k = (x :> k)
let as_hashtbl x = x

(*************************************)
(** Typing Environment Functions **)

(*************************************)

(* Initialisation *)
let init () : t = Hashtbl.create Config.medium_tbl_size

(* Copy *)
let copy x : t = Hashtbl.copy x

(* Type of a variable *)
let get (x : t) var : Type.t option = Hashtbl.find_opt x (key var)

(* Membership *)
let mem (x : t) v : bool = Hashtbl.mem x (key v)

(* Empty *)
let empty (x : t) : bool = Hashtbl.length x == 0

(* Type of a variable *)
let get_exn (x : t) var : Type.t =
  match get x var with
  | Some t -> t
  | None -> Fmt.failwith "Type_env.get_exn: variable %a not found." Id.pp var

(* Get all matchable elements *)
let matchables (x : t) : Id.Any_var.Set.t =
  Hashtbl.fold
    (fun var _ ac -> Id.Any_var.Set.add var ac)
    x Id.Any_var.Set.empty

(* Get all variables *)
let vars = matchables

(* Get all logical variables *)
let lvars (x : t) : LVar.Set.t =
  Hashtbl.fold
    (fun var _ ac ->
      match Id.as_lvar var with
      | Some var -> LVar.Set.add var ac
      | None -> ac)
    x LVar.Set.empty

(* Get all variables of specific type *)
let get_vars_of_type (x : t) (tt : Type.t) : k list =
  Hashtbl.fold
    (fun var t ac_vars -> if t = tt then var :: ac_vars else ac_vars)
    x []

(* Get all var-type pairs as a list *)
let get_var_type_pairs (x : t) : (k * Type.t) Seq.t = Hashtbl.to_seq x

(* Iteration *)
let iter (x : t) (f : k -> Type.t -> unit) : unit = Hashtbl.iter f x

let fold (x : t) (f : k -> Type.t -> 'a -> 'a) (init : 'a) : 'a =
  Hashtbl.fold f x init

let pp_bindings fmt bindings =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%a: %s)" Id.pp v (Type.str vt) in
  let bindings = List.sort (fun (v, _) (w, _) -> Id.compare v w) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

let pp fmt tenv = pp_bindings fmt (fold tenv (fun x t ac -> (x, t) :: ac) [])

let pp_by_need (pvars, lvars) fmt tenv =
  let relevant x =
    match Id.classify_var x with
    | `Var x -> Var.Set.mem x pvars
    | `LVar x -> LVar.Set.mem x lvars
  in
  pp_bindings fmt
    (fold tenv (fun x t ac -> if relevant x then (x, t) :: ac else ac) [])

(* Update with removal *)

let update (te : t) x (t : Type.t) : unit =
  match get te x with
  | None -> Hashtbl.replace te (key x) t
  | Some t' when t' = t -> ()
  | Some t' ->
      Fmt.failwith
        "Type_env update: Conflict: %a has type %s but required extension is %s"
        Id.pp x (Type.str t') (Type.str t)

let remove (te : t) x : unit = Hashtbl.remove te (key x)

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

let mem_lvars vars v =
  match Id.as_lvar v with
  | Some v -> LVar.Set.mem v vars
  | None -> false

(* Filter for specific variables *)
let filter_vars (gamma : t) (vars : LVar.Set.t) : t =
  filter gamma (mem_lvars vars)

(* Filter for specific variables *)
let filter_vars_in_place (gamma : t) (vars : LVar.Set.t) : unit =
  filter_in_place gamma (mem_lvars vars)

(* Perform substitution, return new typing environment *)
let substitution (x : t) (subst : SESubst.t) (partial : bool) : t =
  let new_gamma = init () in
  iter x (fun var v_type ->
      let evar = Expr.of_any_var var in
      let new_var = SESubst.get subst evar in
      match new_var with
      | Some (LVar new_var) -> update new_gamma new_var v_type
      | Some _ -> if partial then update new_gamma var v_type
      | None ->
          if partial then update new_gamma var v_type
          else if Option.is_some (Id.as_lvar var) then (
            let new_lvar = LVar.alloc () in
            SESubst.put subst evar (LVar new_lvar);
            update new_gamma new_lvar v_type));
  new_gamma

let to_list_expr (x : t) : (Expr.t * Type.t) list =
  Hashtbl.fold (fun x t pairs -> (Expr.of_any_var x, t) :: pairs) x []

let to_list (x : t) : (k * Type.t) list =
  Hashtbl.fold (fun x t pairs -> (x, t) :: pairs) x []

let reset (x : t) (reset : (k * Type.t) list) =
  Hashtbl.clear x;
  List.iter (fun (y, t) -> Hashtbl.replace x y t) reset

let is_well_formed (_ : t) : bool = true

let filter_with_info (pvars, lvars, _) (x : t) =
  filter x (fun x ->
      match Id.classify_var x with
      | `Var x -> Var.Set.mem x pvars
      | `LVar x -> LVar.Set.mem x lvars)
