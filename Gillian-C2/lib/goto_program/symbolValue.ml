type t = Expr of Expr.t | Stmt of Stmt.t | SVNone

let pp ft t =
  match t with
  | Expr e -> Expr.pp ft e
  | Stmt s -> Stmt.pp ft s
  | SVNone -> Fmt.string ft "Nil"

let of_irep ~(machine : Machine_model.t) ~(type_ : Type.t) (irep : Irep.t) =
  if Irep.is_nil irep then SVNone
  else
    match type_ with
    | Code _ ->
        let stmt = Stmt.of_irep ~machine irep in
        Stmt stmt
    | _ ->
        let expr = Expr.of_irep ~machine irep in
        Expr expr
