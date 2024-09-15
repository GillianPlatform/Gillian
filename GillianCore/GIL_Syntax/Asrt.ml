(** {b GIL logic assertions}. *)
type simple = TypeDef__.assertion_simple =
  | Emp  (** Empty heap             *)
  | Pred of string * Expr.t list  (** Predicates             *)
  | Pure of Formula.t  (** Pure formula           *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion       *)
  | CorePred of string * Expr.t list * Expr.t list
      (** Core assertion         *)
  | Wand of { lhs : string * Expr.t list; rhs : string * Expr.t list }
      (** Magic wand of the form [P(...) -* Q(...)] *)
[@@deriving eq]

type t = TypeDef__.assertion [@@deriving eq]

let simple_to_yojson = TypeDef__.assertion_simple_to_yojson
let simple_of_yojson = TypeDef__.assertion_simple_of_yojson
let to_yojson = TypeDef__.assertion_to_yojson
let of_yojson = TypeDef__.assertion_of_yojson

let compare x y =
  let cmp = Stdlib.compare in
  match (x, y) with
  | Pure (Eq (PVar x, _)), Pure (Eq (PVar y, _)) -> cmp x y
  | Pure (Eq (PVar _, _)), _ -> -1
  | _, Pure (Eq (PVar _, _)) -> 1
  | Pure _, Pure _ -> cmp x y
  | Pure _, _ -> -1
  | _, Pure _ -> 1
  | Types _, Types _ -> cmp x y
  | Types _, _ -> -1
  | _, Types _ -> 1
  | _, _ -> cmp x y

let prioritise (a1 : simple) (a2 : simple) =
  let lloc_aloc_pvar_lvar e1 e2 =
    match ((e1 : Expr.t), (e2 : Expr.t)) with
    | Lit (Loc _), Lit (Loc _) -> 0
    | Lit (Loc _), _ -> -1
    | _, Lit (Loc _) -> 1
    | ALoc _, ALoc _ -> 0
    | ALoc _, _ -> -1
    | _, ALoc _ -> 0
    | PVar _, PVar _ -> 0
    | PVar _, _ -> -1
    | _, PVar _ -> 1
    | LVar v, LVar v' -> (
        match (Names.is_spec_var_name v, Names.is_spec_var_name v') with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> Stdlib.compare e1 e2)
    | _, _ -> Stdlib.compare e1 e2
  in

  match (a1, a2) with
  | Types [ (e, _) ], Types [ (e', _) ] -> lloc_aloc_pvar_lvar e e'
  | Types _, _ -> -1
  | _, Types _ -> 1
  | Pred _, _ -> 1
  | _, Pred _ -> -1
  | _, _ -> Stdlib.compare a1 a2

module MyAssertion = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyAssertion)

(** Deprecated, use {!Visitors.endo} instead. *)
let map
    (f_a_before : (simple -> t) option)
    (f_a_after : (simple -> t) option)
    (f_e : (Expr.t -> Expr.t) option)
    (f_p : (Formula.t -> Formula.t) option)
    (a : t) : t =
  (* Map recursively to assertions and expressions *)
  let map_e = Option.value ~default:(fun x -> x) f_e in
  let map_p = Option.value ~default:(Formula.map None None (Some map_e)) f_p in
  let f_a_before = Option.value ~default:(fun x -> [ x ]) f_a_before in
  let f_a_after = Option.value ~default:(fun x -> [ x ]) f_a_after in
  let a' = List.concat_map f_a_before a in

  a'
  |> List.map (function
       | Emp -> Emp
       | Pred (s, le) -> Pred (s, List.map map_e le)
       | Pure form -> Pure (map_p form)
       | Types lt -> Types (List.map (fun (exp, typ) -> (map_e exp, typ)) lt)
       | CorePred (x, es1, es2) ->
           CorePred (x, List.map map_e es1, List.map map_e es2)
       | Wand { lhs = lhs_pred, lhs_args; rhs = rhs_pred, rhs_args } ->
           Wand
             {
               lhs = (lhs_pred, List.map map_e lhs_args);
               rhs = (rhs_pred, List.map map_e rhs_args);
             })
  |> List.concat_map f_a_after

(* Get all the logical expressions of --a-- that denote a list
   and are not logical variables *)
let list_lexprs : t -> Expr.Set.t =
  Formula.list_lexprs_collector#visit_assertion ()

(* Get all the logical variables in --a-- *)
let lvars : t -> SS.t =
  Visitors.Collectors.lvar_collector#visit_assertion SS.empty

(* Get all the program variables in --a-- *)
let pvars : t -> SS.t = Visitors.Collectors.pvar_collector#visit_assertion ()

(* Get all the abstract locations in --a-- *)
let alocs : t -> SS.t = Visitors.Collectors.aloc_collector#visit_assertion ()

(* Get all the concrete locations in [a] *)
let clocs : t -> SS.t = Visitors.Collectors.cloc_collector#visit_assertion ()

(* Get all the concrete locations in [a] *)
let locs : t -> SS.t = Visitors.Collectors.loc_collector#visit_assertion ()

(* Returns a list with the names of the predicates that occur in --a-- *)
let pred_names : t -> string list =
  let collector =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.non_ordered_list_monoid
      method! visit_Pred () name _ = [ name ]
    end
  in
  collector#visit_assertion ()

(* Returns a list with the pure assertions that occur in --a-- *)
let pure_asrts : t -> Formula.t list =
  let collector =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.non_ordered_list_monoid
      method! visit_Pure () f = [ f ]
    end
  in
  collector#visit_assertion ()

(* Returns a list with the simple assertions that occur in --a-- *)
(* TODO: remove *)
let simple_asrts : t -> t = List.filter (fun x -> x <> Emp)

(* Check if --a-- is a pure assertion *)
let is_pure_asrt : simple -> bool = function
  | Pred _ | CorePred _ | Wand _ -> false
  | _ -> true

(* Check if --a-- is a pure assertion & non-recursive assertion.
   It assumes that only pure assertions are universally quantified *)
(* TODO: remove *)
let is_pure_non_rec_asrt : simple -> bool = is_pure_asrt

(* Eliminate LStar and LTypes assertions.
   LTypes disappears. LStar is replaced by LAnd.
   This function expects its argument to be a PURE assertion. *)
let make_pure (a : t) : Formula.t =
  a
  |> List.filter_map (function
       | Pure f -> Some f
       | Emp -> None
       | _ -> raise (Failure "DEATH. make_pure received unpure assertion"))
  |> Formula.conjunct

(** GIL logic assertions *)
let _pp_simple ?(e_pp : Format.formatter -> Expr.t -> unit = Expr.pp) fmt =
  function
  | Emp -> Fmt.string fmt "emp"
  | Pred (name, params) ->
      let name = Pp_utils.maybe_quote_ident name in
      Fmt.pf fmt "@[<h>%s(%a)@]" name (Fmt.list ~sep:Fmt.comma e_pp) params
  | Types tls ->
      let pp_tl f (e, t) = Fmt.pf f "%a : %s" e_pp e (Type.str t) in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_tl) tls
  | Pure f -> Formula.pp fmt f
  | CorePred (a, ins, outs) ->
      let pp_e_l = Fmt.list ~sep:Fmt.comma e_pp in
      Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs
  | Wand { lhs = lname, largs; rhs = rname, rargs } ->
      let lname = Pp_utils.maybe_quote_ident lname in
      let rname = Pp_utils.maybe_quote_ident rname in
      Fmt.pf fmt "(%s(%a) -* %s(%a))" lname
        (Fmt.list ~sep:Fmt.comma e_pp)
        largs rname
        (Fmt.list ~sep:Fmt.comma e_pp)
        rargs

let _pp ~(e_pp : Format.formatter -> Expr.t -> unit) (fmt : Format.formatter) :
    t -> unit =
  Fmt.list ~sep:(Fmt.any " *@ ") (_pp_simple ~e_pp) fmt

let pp_simple = _pp_simple ~e_pp:Expr.pp
let pp_simple_full = _pp_simple ~e_pp:Expr.full_pp
let pp = _pp ~e_pp:Expr.pp
let full_pp = _pp ~e_pp:Expr.full_pp

let subst_clocs (subst : string -> Expr.t) : t -> t =
  map None None
    (Some (Expr.subst_clocs subst))
    (Some (Formula.subst_clocs subst))

let subst_expr_for_expr ~(to_subst : Expr.t) ~(subst_with : Expr.t) : t -> t =
  map None None
    (Some (Expr.subst_expr_for_expr ~to_subst ~subst_with))
    (Some (Formula.subst_expr_for_expr ~to_subst ~subst_with))

let pvars_to_lvars : t -> t =
  map None None (Some Expr.pvars_to_lvars) (Some Formula.pvars_to_lvars)

(* TODO: remove *)
let star : t list -> t = List.concat
