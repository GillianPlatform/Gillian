module type S = sig
  type vt [@@deriving yojson]
  type st
  type t [@@deriving yojson]

  type wand = { lhs : string * vt list; rhs : string * vt list }
  [@@deriving yojson]

  val length : t -> int
  val init : wand list -> t
  val to_list : t -> wand list
  val copy : t -> t
  val is_empty : t -> bool
  val get_lvars : t -> SS.t
  val get_alocs : t -> SS.t
  val substitution_in_place : st -> t -> unit
  val pp : Format.formatter -> t -> unit
  val to_assertions : t -> Asrt.t list
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et) :
  S with type vt = Val.t and type st = ESubst.t = struct
  type vt = Val.t [@@deriving yojson]
  type st = ESubst.t [@@deriving yojson]

  type wand = { lhs : string * vt list; rhs : string * vt list }
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
    let subst_in_val (v : vt) : vt =
      let le' = ESubst.subst_in_expr subst ~partial:true (Val.to_expr v) in
      Option.fold ~some:(fun v -> v) ~none:v (Val.from_expr le')
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
      (Fmt.list ~sep:(Fmt.any ", ") Val.pp)
      largs rname
      (Fmt.list ~sep:(Fmt.any ", ") Val.pp)
      rargs

  let pp ft t = (Fmt.list ~sep:(Fmt.any "@\n") pp_wand) ft !t

  let get_lvars t =
    let lvars_val_list el =
      List.fold_left
        (fun acc v ->
          let expr = Val.to_expr v in
          SS.union acc (Expr.lvars expr))
        SS.empty el
    in
    List.fold_left
      (fun acc { lhs = _, largs; rhs = _, rargs } ->
        acc
        |> SS.union (lvars_val_list largs)
        |> SS.union (lvars_val_list rargs))
      SS.empty !t

  let get_alocs t =
    let alocs_val_list el =
      List.fold_left
        (fun acc v ->
          let expr = Val.to_expr v in
          SS.union acc (Expr.alocs expr))
        SS.empty el
    in
    List.fold_left
      (fun acc { lhs = _, largs; rhs = _, rargs } ->
        acc
        |> SS.union (alocs_val_list largs)
        |> SS.union (alocs_val_list rargs))
      SS.empty !t

  let to_assertions (wands : t) =
    let wand_to_asrt { lhs = lname, largs; rhs = rname, rargs } =
      let largs = List.map Val.to_expr largs in
      let rargs = List.map Val.to_expr rargs in
      Asrt.Wand { lhs = (lname, largs); rhs = (rname, rargs) }
    in
    List.map wand_to_asrt !wands
end

module SWands = Make (SVal.M) (SVal.SESubst)
