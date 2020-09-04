(*********************)
(*                   *)
(*  Symbolic values  *)
(*                   *)
(*********************)

module L = Logging

module rec M :
  (Val.S with type t = Expr.t and type st = SSubst.t and type et = SESubst.t) =
struct
  type t = Expr.t

  type st = SSubst.t

  type et = SESubst.t

  let pp = Expr.pp

  let full_pp = Expr.full_pp

  let full_pp_list = Fmt.list ~sep:(Fmt.any "; ") full_pp

  let to_literal le =
    match (le : Expr.t) with
    | Lit lit -> Some lit
    | _       -> None

  let from_literal lit = Expr.Lit lit

  let to_expr le = le

  let from_expr le = Some le

  let from_list = Expr.from_list

  let from_lvar_name x = Expr.LVar x

  let to_list = Expr.to_list

  let is_concrete = Expr.is_concrete
end

and SSubst : (Subst.S with type vt = M.t) = Subst.Make (M)
and SESubst : (ESubst.S with type vt = M.t) = ESubst.Make (M)
