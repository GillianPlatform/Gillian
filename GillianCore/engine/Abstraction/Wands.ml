module Subst = SVal.SESubst

type wand = { lhs : string * Expr.t list; rhs : string * Expr.t list }
[@@deriving yojson]

type t = wand list ref [@@deriving yojson]

(** Returns the number of wand assertions *)
let length x = List.length !x

let init (wands : wand list) : t = ref wands
let to_list wands = !wands
let copy wands = ref !wands

let is_empty wands =
  match !wands with
  | [] -> true
  | _ -> false

let substitution_in_place subst wands =
  let subst_in_val (v : Expr.t) : Expr.t =
    Subst.subst_in_expr subst ~partial:true v
  in

  let subst_wand { lhs = lname, largs; rhs = rname, rargs } =
    {
      lhs = (lname, List.map subst_in_val largs);
      rhs = (rname, List.map subst_in_val rargs);
    }
  in
  wands := List.map subst_wand !wands

let pp_wand ft t =
  let { lhs = lname, largs; rhs = rname, rargs } = t in
  Fmt.pf ft "%s(%a) -* %s(%a)" lname
    (Fmt.list ~sep:(Fmt.any ", ") Expr.pp)
    largs rname
    (Fmt.list ~sep:(Fmt.any ", ") Expr.pp)
    rargs

let pp ft t = (Fmt.list ~sep:(Fmt.any "@\n") pp_wand) ft !t

let get_lvars t =
  let lvars_val_list el =
    List.fold_left (fun acc expr -> SS.union acc (Expr.lvars expr)) SS.empty el
  in
  List.fold_left
    (fun acc { lhs = _, largs; rhs = _, rargs } ->
      acc |> SS.union (lvars_val_list largs) |> SS.union (lvars_val_list rargs))
    SS.empty !t

let get_alocs t =
  let alocs_val_list el =
    List.fold_left (fun acc v -> SS.union acc (Expr.alocs v)) SS.empty el
  in
  List.fold_left
    (fun acc { lhs = _, largs; rhs = _, rargs } ->
      acc |> SS.union (alocs_val_list largs) |> SS.union (alocs_val_list rargs))
    SS.empty !t

let to_assertions (wands : t) =
  let wand_to_asrt { lhs; rhs } = Asrt.Wand { lhs; rhs } in
  List.map wand_to_asrt !wands
