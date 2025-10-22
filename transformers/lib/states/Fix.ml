open Gillian.Gil_syntax

type 'cp atom = 'cp * Expr.t list * Expr.t list [@@deriving yojson, show]
type 'cp t = 'cp atom list [@@deriving yojson, show]

let lvars (fix : 'cp t) =
  let open Gillian.Utils.Containers in
  List.fold_left
    (fun acc (_, ins, outs) ->
      let acc =
        List.fold_left (fun acc e -> SS.union acc (Expr.lvars e)) acc ins
      in
      List.fold_left (fun acc e -> SS.union acc (Expr.lvars e)) acc outs)
    SS.empty fix

let alocs (fix : 'cp t) =
  let open Gillian.Utils.Containers in
  List.fold_left
    (fun acc (_, ins, outs) ->
      let acc =
        List.fold_left (fun acc e -> SS.union acc (Expr.alocs e)) acc ins
      in
      List.fold_left (fun acc e -> SS.union acc (Expr.alocs e)) acc outs)
    SS.empty fix

let subst (subst : Gillian.Symbolic.Subst.t) (fix : 'a t) : 'a t =
  let le_subst = Gillian.Symbolic.Subst.subst_in_expr subst ~partial:true in
  let subst_atom (cp, ins, outs) =
    (cp, List.map le_subst ins, List.map le_subst outs)
  in
  List.map subst_atom fix
