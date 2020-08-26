(*********************)
(*                   *)
(*  Concrete values  *)
(*                   *)
(*********************)

module rec M : (Val.S with type st = CSubst.t and type t = Literal.t) = struct
  type t = Literal.t

  type st = CSubst.t

  let pp = Literal.pp

  let full_pp = Literal.pp

  let full_pp_list = Fmt.list ~sep:(Fmt.any "; ") full_pp

  let to_literal lit = Some lit

  let from_literal lit = lit

  let to_expr lit = Expr.Lit lit

  let from_expr (le : Expr.t) =
    match le with
    | Lit l -> Some l
    | _     -> None

  let from_list = Literal.from_list

  let from_lvar_name x =
    failwith "Cannot create a concrete value from a logical variable"

  let to_list = Literal.to_list

  let is_concrete (v : t) = true
end

and CSubst : (Subst.S with type vt = M.t) = Subst.Make (M)
