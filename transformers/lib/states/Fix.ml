open Gillian.Gil_syntax

type 'cp atom = Res of 'cp * Expr.t list * Expr.t list | Ty of Expr.t * Type.t
[@@deriving yojson, show]

type 'cp t = 'cp atom list [@@deriving yojson, show]

let lvars (fix : 'cp t) =
  let open Gillian.Utils.Containers in
  List.fold_left
    (fun acc atom ->
      match atom with
      | Res (_, ins, outs) ->
          let acc =
            List.fold_left (fun acc e -> SS.union acc (Expr.lvars e)) acc ins
          in
          List.fold_left (fun acc e -> SS.union acc (Expr.lvars e)) acc outs
      | Ty (e, _) -> SS.union acc (Expr.lvars e))
    SS.empty fix

let alocs (fix : 'cp t) =
  let open Gillian.Utils.Containers in
  List.fold_left
    (fun acc atom ->
      match atom with
      | Res (_, ins, outs) ->
          let acc =
            List.fold_left (fun acc e -> SS.union acc (Expr.alocs e)) acc ins
          in
          List.fold_left (fun acc e -> SS.union acc (Expr.alocs e)) acc outs
      | Ty (s, _) -> SS.union (Expr.alocs s) acc)
    SS.empty fix

let subst (subst : Gillian.Symbolic.Subst.t) (fix : 'a t) : 'a t =
  let le_subst = Gillian.Symbolic.Subst.subst_in_expr subst ~partial:true in
  let subst_atom atom =
    match atom with
    | Res (cp, ins, outs) ->
        Res (cp, List.map le_subst ins, List.map le_subst outs)
    | Ty (e, t) -> Ty (le_subst e, t)
  in
  List.map subst_atom fix

let to_asrt ~pred_to_str fix : Asrt.t =
  ListLabels.map fix ~f:(function
    | Res (cp, ins, outs) -> Asrt.CorePred (pred_to_str cp, ins, outs)
    | Ty (e, t) -> Asrt.Types [ (e, t) ])

let deep_map_cps f fixess =
  List.map
    (List.map (function
      | Res (p, i, o) ->
          let p', i', o' = f (p, i, o) in
          Res (p', i', o')
      | Ty (e, t) -> Ty (e, t)))
    fixess
