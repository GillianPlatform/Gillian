(** @canonical Gillian.Concrete.Values *)
module rec M :
  (Val.S with type t = Literal.t and type st = CSubst.t and type et = CESubst.t) =
struct
  type t = Literal.t [@@deriving yojson]
  type st = CSubst.t
  type et = CESubst.t

  let equal = Literal.equal
  let pp = Literal.pp
  let full_pp = Literal.pp
  let full_pp_list = Fmt.list ~sep:(Fmt.any "; ") full_pp
  let to_literal lit = Some lit
  let from_literal lit = lit
  let to_expr lit = Expr.Lit lit

  let from_expr (le : Expr.t) =
    match le with
    | Lit l -> Some l
    | _ -> None

  let from_list = Literal.from_list

  let from_lvar_name _ =
    failwith "Cannot create a concrete value from a logical variable"

  let to_list = Literal.to_list
end

(** @canonical Gillian.Concrete.Subst *)
and CSubst : (Subst.S with type vt = M.t) = Subst.Make (M)

and CESubst : (ESubst.S with type vt = M.t) = ESubst.Make (M)
