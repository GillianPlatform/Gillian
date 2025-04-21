(** GIL Typing Environment *)

open Names
open SVal
module L = Logging

type constructors_tbl_t = (string, Constructor.t) Hashtbl.t [@@deriving yojson]
type datatypes_tbl_t = (string, Datatype.t) Hashtbl.t [@@deriving yojson]

type t = {
  var_types : (string, Type.t) Hashtbl.t;
  constructor_defs : constructors_tbl_t;
  datatype_defs : datatypes_tbl_t;
}
[@@deriving yojson]

let as_hashtbl x = x.var_types

(*************************************)
(** Typing Environment Functions    **)

(*************************************)

(* Initialisation *)
let init ?(datatype_defs = Hashtbl.create Config.medium_tbl_size) () : t =
  let constructor_defs = Hashtbl.create Config.medium_tbl_size in
  let add_constructor_to_tbl (constructor : Constructor.t) =
    Hashtbl.add constructor_defs constructor.constructor_name constructor
  in
  let add_constructors_to_tbl _ (datatype : Datatype.t) =
    List.iter add_constructor_to_tbl datatype.datatype_constructors
  in
  let () = Hashtbl.iter add_constructors_to_tbl datatype_defs in
  {
    var_types = Hashtbl.create Config.medium_tbl_size;
    datatype_defs;
    constructor_defs;
  }

(* Copy *)
let copy { var_types; datatype_defs; constructor_defs } : t =
  {
    var_types = Hashtbl.copy var_types;
    datatype_defs = Hashtbl.copy datatype_defs;
    constructor_defs = Hashtbl.copy constructor_defs;
  }

(* Type of a variable *)
let get (x : t) (var : string) : Type.t option =
  Hashtbl.find_opt x.var_types var

(* Membership *)
let mem (x : t) (v : string) : bool = Hashtbl.mem x.var_types v

(* Empty *)
let empty (x : t) : bool = Hashtbl.length x.var_types == 0

(* Type of a variable *)
let get_unsafe (x : t) (var : string) : Type.t =
  match Hashtbl.find_opt x.var_types var with
  | Some t -> t
  | None ->
      raise (Failure ("Type_env.get_unsafe: variable " ^ var ^ " not found."))

(* Get all matchable elements *)
let matchables (x : t) : SS.t =
  Hashtbl.fold (fun var _ ac -> SS.add var ac) x.var_types SS.empty

(* Get all variables *)
let vars (x : t) : SS.t =
  Hashtbl.fold (fun var _ ac -> SS.add var ac) x.var_types SS.empty

(* Get all logical variables *)
let lvars (x : t) : SS.t =
  Hashtbl.fold
    (fun var _ ac -> if is_lvar_name var then SS.add var ac else ac)
    x.var_types SS.empty

(* Get all variables of specific type *)
let get_vars_of_type (x : t) (tt : Type.t) : string list =
  Hashtbl.fold
    (fun var t ac_vars -> if t = tt then var :: ac_vars else ac_vars)
    x.var_types []

(* Get all var-type pairs as a list *)
let get_var_type_pairs (x : t) : (string * Type.t) Seq.t =
  Hashtbl.to_seq x.var_types

(* Iteration *)
let iter (x : t) (f : string -> Type.t -> unit) : unit =
  Hashtbl.iter f x.var_types

let fold (x : t) (f : string -> Type.t -> 'a -> 'a) (init : 'a) : 'a =
  Hashtbl.fold f x.var_types init

let pp fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%s: %s)" v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

let pp_by_need vars fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%s: %s)" v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  let bindings = List.filter (fun (v, _) -> SS.mem v vars) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

(* Update with removal *)

let update (te : t) (x : string) (t : Type.t) : unit =
  match get te x with
  | None -> Hashtbl.replace te.var_types x t
  | Some t' when t' = t -> ()
  | Some t' ->
      Fmt.failwith
        "Type_env update: Conflict: %s has type %s but required extension is %s"
        x (Type.str t') (Type.str t)

let remove (te : t) (x : string) : unit = Hashtbl.remove te.var_types x

(* Extend gamma with more_gamma *)
let extend (x : t) (y : t) : unit =
  iter y (fun v t ->
      match Hashtbl.find_opt x.var_types v with
      | None -> Hashtbl.replace x.var_types v t
      | Some t' ->
          if t <> t' then
            raise (Failure "Typing environment cannot be extended."))

(* Filter using function on variables *)
let filter (x : t) (f : string -> bool) : t =
  let new_gamma = init () in
  iter x (fun v v_type -> if f v then update new_gamma v v_type);
  new_gamma

(* Filter using function on variables *)
let filter_in_place (x : t) (f : string -> bool) : unit =
  iter x (fun v _ -> if not (f v) then remove x v)

(* Filter for specific variables *)
let filter_vars (gamma : t) (vars : SS.t) : t =
  filter gamma (fun v -> SS.mem v vars)

(* Filter for specific variables *)
let filter_vars_in_place (gamma : t) (vars : SS.t) : unit =
  filter_in_place gamma (fun v -> SS.mem v vars)

(* Perform substitution, return new typing environment *)
let substitution (x : t) (subst : SESubst.t) (partial : bool) : t =
  let new_gamma = init () in
  iter x (fun var v_type ->
      let evar = Expr.from_var_name var in
      let new_var = SESubst.get subst evar in
      match new_var with
      | Some (LVar new_var) -> update new_gamma new_var v_type
      | Some _ -> if partial then update new_gamma var v_type
      | None ->
          if partial then update new_gamma var v_type
          else if Names.is_lvar_name var then (
            let new_lvar = LVar.alloc () in
            SESubst.put subst evar (LVar new_lvar);
            update new_gamma new_lvar v_type));
  new_gamma

let to_list_expr (x : t) : (Expr.t * Type.t) list =
  let le_type_pairs =
    Hashtbl.fold
      (fun x t (pairs : (Expr.t * Type.t) list) ->
        if Names.is_lvar_name x then (LVar x, t) :: pairs
        else (PVar x, t) :: pairs)
      x.var_types []
  in
  le_type_pairs

let to_list (x : t) : (Var.t * Type.t) list =
  let le_type_pairs =
    Hashtbl.fold
      (fun x t (pairs : (Var.t * Type.t) list) -> (x, t) :: pairs)
      x.var_types []
  in
  le_type_pairs

let reset (x : t) (reset : (Var.t * Type.t) list) =
  Hashtbl.clear x.var_types;
  List.iter (fun (y, t) -> Hashtbl.replace x.var_types y t) reset

let is_well_formed (_ : t) : bool = true

let filter_with_info relevant_info (x : t) =
  let pvars, lvars, locs = relevant_info in
  let relevant = List.fold_left SS.union SS.empty [ pvars; lvars; locs ] in
  filter x (fun x -> SS.mem x relevant)

(*************************************)
(**     Datatype Functions          **)

(*************************************)

let get_constructor_type (x : t) (cname : string) : Type.t option =
  let constructor = Hashtbl.find_opt x.constructor_defs cname in
  Option.map
    (fun (c : Constructor.t) -> Type.DatatypeType c.constructor_datatype)
    constructor

let get_constructor_type_unsafe (x : t) (cname : string) : Type.t =
  let constructor = Hashtbl.find_opt x.constructor_defs cname in
  match constructor with
  | Some c -> Type.DatatypeType c.constructor_datatype
  | None ->
      raise
        (Failure
           ("Type_env.get_constructor_type_unsafe: constructor " ^ cname
          ^ " not found."))

let get_constructor_field_types (x : t) (cname : string) :
    Type.t option list option =
  let constructor = Hashtbl.find_opt x.constructor_defs cname in
  Option.map (fun (c : Constructor.t) -> c.constructor_fields) constructor

let keeping_datatypes (x : t) : t =
  let datatype_defs = Hashtbl.copy x.datatype_defs in
  init ~datatype_defs ()
