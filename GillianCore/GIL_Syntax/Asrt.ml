(** {b GIL logic assertions}. *)
type atom = TypeDef__.assertion_atom =
  | Emp  (** Empty heap *)
  | Pure of Expr.t  (** Pure formula *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion *)
  | CorePred of string * Expr.t list * Expr.t list  (** Core assertion *)
[@@deriving eq]

type t = TypeDef__.assertion [@@deriving eq]

let atom_to_yojson = TypeDef__.assertion_atom_to_yojson
let atom_of_yojson = TypeDef__.assertion_atom_of_yojson
let to_yojson = TypeDef__.assertion_to_yojson
let of_yojson = TypeDef__.assertion_of_yojson

(* User-defined predicates no longer have their own assertion variant: they are
   encoded as {!CorePred}s whose name is the user predicate name prefixed with
   [user_pred_prefix]. The prefix is defined {b once}, here. *)
let user_pred_prefix = "GILLIAN_USER_PRED__"

(** [user_pred_name p] is the core-predicate name that encodes the user-defined
    predicate [p]. *)
let user_pred_name (name : string) : string = user_pred_prefix ^ name

(** [as_user_pred_name s] returns [Some p] when the core-predicate name [s]
    encodes a user-defined predicate [p] (i.e. [s = user_pred_name p]), and
    [None] when [s] is a genuine core predicate. *)
let as_user_pred_name (name : string) : string option =
  let n = String.length user_pred_prefix in
  if String.length name >= n && String.sub name 0 n = user_pred_prefix then
    Some (String.sub name n (String.length name - n))
  else None

(** Builds a user-predicate assertion (a {!CorePred} with the encoded name). *)
let pred (name : string) (ins : Expr.t list) (outs : Expr.t list) : atom =
  CorePred (user_pred_name name, ins, outs)

(** Magic wands in GIL are simply core predicates whose name encodes the wand's
    left-hand and right-hand predicate names as
    [wand_prefix ^ lname ^ wand_sep ^ rname]. *)
let wand_prefix = "GILLIAN_WAND__"

let wand_sep = "___INTO___"

(** Index of the first occurrence of [sub] in [s], if any. *)
let string_find ~(sub : string) (s : string) : int option =
  let n = String.length s and m = String.length sub in
  let rec aux i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else aux (i + 1)
  in
  aux 0

(** [wand_name lname rname] is the core-predicate name that encodes the magic
    wand [lname(...) -* rname(...)]. *)
let wand_name (lname : string) (rname : string) : string =
  wand_prefix ^ lname ^ wand_sep ^ rname

(** [as_wand_name s] returns [Some (lname, rname)] when the core-predicate name
    [s] encodes a magic wand (i.e. [s = wand_name lname rname]), and [None]
    otherwise. The split is on the first [wand_sep] after the prefix. *)
let as_wand_name (name : string) : (string * string) option =
  let np = String.length wand_prefix in
  if String.length name >= np && String.sub name 0 np = wand_prefix then
    let rest = String.sub name np (String.length name - np) in
    match string_find ~sub:wand_sep rest with
    | None -> None
    | Some i ->
        let lname = String.sub rest 0 i in
        let ns = String.length wand_sep in
        let rname = String.sub rest (i + ns) (String.length rest - i - ns) in
        Some (lname, rname)
  else None

let wand_to_core_pred
    ((lname, largs) : string * Expr.t list)
    ((rname, r_ins) : string * Expr.t list)
    (r_outs : Expr.t list) : string * Expr.t list * Expr.t list =
  (wand_name lname rname, largs @ r_ins, r_outs)

(** Builds a magic-wand assertion atom. A wand's semantic {b ins} are the lhs
    args together with the {e in}-arguments of the rhs; its {b outs} are
    {e only} the {e out}-arguments of the rhs. The encoding therefore stores
    [largs @ r_ins] as the core-predicate ins and [r_outs] as its outs. *)
let wand
    ((lname, largs) : string * Expr.t list)
    ((rname, r_ins) : string * Expr.t list)
    (r_outs : Expr.t list) : atom =
  let n, i, o = wand_to_core_pred (lname, largs) (rname, r_ins) r_outs in
  CorePred (n, i, o)

(** [as_wand ~rhs_ins_number a] returns [Some (lhs, rhs)] when [a] is a
    wand-encoding {!CorePred}, recovering the raw [lhs = (lname, largs)] and
    [rhs = (rname, rargs)]. Since the stored ins are [largs @ r_ins] (with
    [r_ins] the last [rhs_ins_number] of them) and the stored outs are [r_outs],
    reconstructing [rargs = r_ins @ r_outs] requires the rhs predicate's number
    of in-parameters. *)
let as_wand ~(rhs_ins_number : int) (a : atom) :
    ((string * Expr.t list) * (string * Expr.t list)) option =
  match a with
  | CorePred (name, ins, outs) -> (
      match as_wand_name name with
      | Some (lname, rname) ->
          let n = List.length ins - rhs_ins_number in
          let largs = List.filteri (fun i _ -> i < n) ins in
          let r_ins = List.filteri (fun i _ -> i >= n) ins in
          Some ((lname, largs), (rname, r_ins @ outs))
      | None -> None)
  | _ -> None

(** Effect that looks up a predicate's number of in-parameters. Used only by the
    printer, to reconstruct a wand's surface form from its (semantic) ins/outs.
    Handled by {!Engine.MP.with_pred_table}; outside its scope,
    {!pred_ins_number} returns [None] and the printer falls back to the raw
    core-predicate form. *)
type _ Effect.t += Pred_ins_number : string -> int option Effect.t

let pred_ins_number (name : string) : int option =
  match Effect.perform (Pred_ins_number name) with
  | v -> v
  | exception Effect.Unhandled _ -> None

let compare x y =
  let cmp = Stdlib.compare in
  match (x, y) with
  | Pure (BinOp (PVar x, Equal, _)), Pure (BinOp (PVar y, Equal, _)) -> cmp x y
  | Pure (BinOp (PVar _, Equal, _)), _ -> -1
  | _, Pure (BinOp (PVar _, Equal, _)) -> 1
  | Pure _, Pure _ -> cmp x y
  | Pure _, _ -> -1
  | _, Pure _ -> 1
  | Types _, Types _ -> cmp x y
  | Types _, _ -> -1
  | _, Types _ -> 1
  | _, _ -> cmp x y

let prioritise (a1 : atom) (a2 : atom) =
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
  | CorePred (n1, _, _), _ when Option.is_some (as_user_pred_name n1) -> 1
  | _, CorePred (n2, _, _) when Option.is_some (as_user_pred_name n2) -> -1
  | _, _ -> Stdlib.compare a1 a2

module MyAssertion = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyAssertion)

(** Deprecated, use {!Visitors.endo} instead. *)
let map (f_e : Expr.t -> Expr.t) : t -> t =
  List.map (function
    | Emp -> Emp
    | Pure form -> Pure (f_e form)
    | Types lt -> Types (List.map (fun (exp, typ) -> (f_e exp, typ)) lt)
    | CorePred (x, es1, es2) -> CorePred (x, List.map f_e es1, List.map f_e es2))

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

      method! visit_CorePred () name _ _ =
        match as_user_pred_name name with
        | Some pred_name -> [ pred_name ]
        | None -> []
    end
  in
  collector#visit_assertion ()

(* Returns a list with the pure assertions that occur in --a-- *)
let pure_asrts : t -> Expr.t list =
  List.filter_map @@ function
  | Pure f -> Some f
  | _ -> None

(* Check if --a-- is a pure assertion *)
let is_pure_asrt : atom -> bool = function
  | CorePred _ -> false
  | _ -> true

(* Eliminate Emp assertions.
   Pure assertions are converted to a single formula.
   This function expects its argument to be a PURE assertion. *)
let make_pure (a : t) : Expr.t =
  a
  |> List.filter_map (function
       | Pure f -> Some f
       | Emp -> None
       | _ -> raise (Failure "DEATH. make_pure received unpure assertion"))
  |> Expr.conjunct

(** GIL logic assertions *)
let _pp_atom ?(e_pp : Format.formatter -> Expr.t -> unit = Expr.pp) fmt =
  let pp_e_l = Fmt.list ~sep:Fmt.comma e_pp in
  function
  | Emp -> Fmt.string fmt "emp"
  | Types tls ->
      let pp_tl f (e, t) = Fmt.pf f "%a : %s" e_pp e (Type.str t) in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_tl) tls
  | Pure f -> e_pp fmt f
  | CorePred (a, ins, outs) when Option.is_some (as_wand_name a) -> (
      let lname, rname = Option.get (as_wand_name a) in
      (* A magic wand: reconstruct and print in surface syntax
         [(lname(largs) -* rname(rargs))]. This needs the rhs predicate's number
         of in-parameters (to split the stored [largs @ r_ins] ins); when no
         predicate table is ambient, fall back to the raw core-predicate form. *)
      match pred_ins_number rname with
      | Some rhs_ins_number ->
          let (_, largs), (_, rargs) =
            Option.get (as_wand ~rhs_ins_number (CorePred (a, ins, outs)))
          in
          let lname = Pp_utils.maybe_quote_ident lname in
          let rname = Pp_utils.maybe_quote_ident rname in
          Fmt.pf fmt "(%s(%a) -* %s(%a))" lname pp_e_l largs rname pp_e_l rargs
      | None -> Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs)
  | CorePred (a, ins, outs) when Option.is_some (as_user_pred_name a) ->
      let pred_name = Option.get (as_user_pred_name a) in
      (* A user-defined predicate: printed [name(ins; outs)]. *)
      let pred_name = Pp_utils.maybe_quote_ident pred_name in
      Fmt.pf fmt "@[<h>%s(%a; %a)@]" pred_name pp_e_l ins pp_e_l outs
  | CorePred (a, ins, outs) ->
      (* A genuine core predicate: printed [<name>(ins; outs)]. *)
      Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs

let _pp ~(e_pp : Format.formatter -> Expr.t -> unit) (fmt : Format.formatter) :
    t -> unit =
  Fmt.list ~sep:(Fmt.any " *@ ") (_pp_atom ~e_pp) fmt

let pp_atom = _pp_atom ~e_pp:Expr.pp
let pp_atom_full = _pp_atom ~e_pp:Expr.full_pp
let pp = _pp ~e_pp:Expr.pp
let full_pp = _pp ~e_pp:Expr.full_pp

let subst_clocs (subst : string -> Expr.t) : t -> t =
  map (Expr.subst_clocs subst)

let subst_expr_for_expr ~(to_subst : Expr.t) ~(subst_with : Expr.t) : t -> t =
  map (Expr.subst_expr_for_expr ~to_subst ~subst_with)

let pvars_to_lvars : t -> t = map Expr.pvars_to_lvars
