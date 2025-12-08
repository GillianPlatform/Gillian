open Gillian.Utils
open Gillian.Monadic
open Gillian.Symbolic
open Gil_syntax
module DR = Delayed_result
module ExpMap = MyUtils.ExpMap

module Make (S : MyMonadicSMemory.S) :
  MyMonadicSMemory.S with type t = S.t ExpMap.t * Expr.t option = struct
  type t = S.t ExpMap.t * Expr.t option [@@deriving yojson]

  let pp fmt ((b, n) : t) =
    Fmt.pf fmt "@[<v>BOUND: %a@ %a@]"
      (Fmt.option ~none:(Fmt.any "NONE") Expr.pp)
      n
      (Fmt.braces @@ Fmt.vbox
      @@ Fmt.iter_bindings ~sep:Fmt.sp ExpMap.iter
      @@ fun ft (o, v) -> Fmt.pf ft "%a: %a" Expr.pp o S.pp v)
      b

  type err_t =
    | OutOfBounds of Expr.t * Expr.t (* Accessed index, list length *)
    | MissingLength
    | SubError of Expr.t * S.err_t
  [@@deriving show, yojson]

  type action = SubAction of S.action

  let action_from_str str =
    Option.map (fun a -> SubAction a) (S.action_from_str str)

  let action_to_str = function
    | SubAction a -> S.action_to_str a

  let list_actions () =
    List.map
      (fun (a, args, ret) -> (SubAction a, "offset" :: args, ret))
      (S.list_actions ())

  type pred = Length | SubPred of S.pred

  let pred_from_str = function
    | "length" -> Some Length
    | str -> Option.map (fun p -> SubPred p) (S.pred_from_str str)

  let pred_to_str = function
    | Length -> "length"
    | SubPred p -> S.pred_to_str p

  let list_preds () =
    (Length, [], [ "length" ])
    :: List.map
         (fun (p, args, ret) -> (SubPred p, "offset" :: args, ret))
         (S.list_preds ())

  let empty () : t = (ExpMap.empty, None)

  let validate_index ((b, n) : t) idx =
    let open Delayed.Syntax in
    let* res = ExpMap.sym_find_opt idx b in
    match res with
    | Some (idx', ss) -> DR.ok (idx', ss)
    | None -> (
        match n with
        | Some n ->
            if%sat Expr.Infix.(Expr.zero_i <= idx && idx < n) then
              DR.ok (idx, S.empty ())
            else DR.error (OutOfBounds (idx, n))
        | None -> DR.ok (idx, S.empty ()))

  let[@inline] execute_action action ((b, n) : t) (args : Values.t list) :
      (t * Values.t list, err_t) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    match (action, args) with
    | SubAction a, idx :: args -> (
        let** idx', ss = validate_index (b, n) idx in
        let+ r = S.execute_action a ss args in
        match r with
        | Ok (ss', v) -> Ok ((ExpMap.add idx' ss' b, n), idx' :: v)
        | Error e -> Error (SubError (idx', e)))
    | SubAction _, [] -> failwith "Missing index for sub-action"

  let[@inline] consume pred (b, n) ins =
    let open DR.Syntax in
    let open Delayed.Syntax in
    match (pred, ins) with
    | SubPred p, idx :: ins -> (
        let** idx', ss = validate_index (b, n) idx in
        let+ r = S.consume p ss ins in
        match r with
        | Ok (ss', outs) when S.is_empty ss' ->
            Ok ((ExpMap.remove idx' b, n), outs)
        | Ok (ss', outs) -> Ok ((ExpMap.add idx' ss' b, n), outs)
        | Error e -> Error (SubError (idx', e)))
    | SubPred _, [] -> failwith "Missing index for sub-predicate consume"
    | Length, [] -> (
        match n with
        | Some n -> DR.ok ((b, None), [ n ])
        | None -> DR.error MissingLength)
    | Length, _ -> failwith "Invalid arguments for length consume"

  let[@inline] produce pred (b, n) args =
    let open Delayed.Syntax in
    let open MyUtils.Syntax in
    match (pred, args) with
    | SubPred p, idx :: args ->
        let*? idx', ss = validate_index (b, n) idx in
        let* ss' = S.produce p ss args in
        Delayed.return (ExpMap.add idx' ss' b, n)
    | SubPred _, [] -> failwith "Missing index for sub-predicate produce"
    | Length, [ n' ] -> (
        match n with
        | Some _ -> Delayed.vanish ()
        | None -> Delayed.return (b, Some n'))
    | Length, _ -> failwith "Invalid arguments for length produce"

  let compose s1 s2 =
    match (s1, s2) with
    | (b1, n), (b2, None) | (b1, None), (b2, n) ->
        let open Delayed.Syntax in
        let* b' = ExpMap.sym_merge S.compose b1 b2 in
        Delayed.return (b', n)
    | (_, Some _), (_, Some _) -> Delayed.vanish ()

  let is_exclusively_owned s e =
    let open Delayed.Syntax in
    match s with
    | b, Some n ->
        (* This does the assumption that all indices are different values *)
        if%sat Expr.Infix.(n == Expr.int (ExpMap.cardinal b)) then
          let rec check l acc =
            let* acc = acc in
            match (acc, l) with
            | false, _ -> Delayed.return false
            | true, [] -> Delayed.return true
            | true, (_, hd) :: tl -> check tl (S.is_exclusively_owned hd e)
          in
          check (ExpMap.bindings b) (Delayed.return true)
        else Delayed.return false
    | _, None -> Delayed.return false

  let is_empty = function
    | b, None -> ExpMap.for_all (fun _ v -> S.is_empty v) b
    | _ -> false

  let is_concrete = function
    | b, None -> ExpMap.for_all (fun _ v -> S.is_concrete v) b
    | b, Some n ->
        Expr.is_concrete n && ExpMap.for_all (fun _ v -> S.is_concrete v) b

  let instantiate : Expr.t list -> t * Expr.t list = function
    | n :: args ->
        let length =
          match n with
          | Expr.Lit (Int n) -> Z.to_int n
          | _ -> failwith "Invalid length for list instantiation"
        in
        let rec aux acc i =
          if i = length then acc
          else
            aux (ExpMap.add (Expr.int i) (fst (S.instantiate args)) acc) (i + 1)
        in
        let b = aux ExpMap.empty 0 in
        ((b, Some n), [ Expr.zero_i ])
    | [] -> failwith "Invalid arguments for list instantiation"

  let substitution_in_place sub (b, n) =
    let open Delayed.Syntax in
    let subst = Subst.subst_in_expr sub ~partial:true in
    let mapper (idx, s) =
      let+ s' = S.substitution_in_place sub s in
      let idx' = subst idx in
      (idx', s')
    in
    let* sub_entries = ExpMap.bindings b |> List.map mapper |> Delayed.all in
    let+ b' = ExpMap.sym_compose S.compose sub_entries ExpMap.empty in
    let n' = Option.map subst n in
    (b', n')

  let lvars (b, n) =
    let open Containers.SS in
    let lvars_map =
      ExpMap.fold
        (fun k v acc -> S.lvars v |> union (Expr.lvars k) |> union acc)
        b empty
    in
    match n with
    | Some n -> union lvars_map (Expr.lvars n)
    | None -> lvars_map

  let alocs (b, n) =
    let open Containers.SS in
    let alocs_map =
      ExpMap.fold
        (fun k v acc -> union (union (Expr.alocs k) (S.alocs v)) acc)
        b empty
    in
    match n with
    | Some n -> union alocs_map (Expr.alocs n)
    | None -> alocs_map

  let lift_corepred k (p, i, o) = (SubPred p, k :: i, o)

  let assertions (b, n) =
    let sub_asrts =
      ExpMap.fold
        (fun k v acc -> acc @ List.map (lift_corepred k) (S.assertions v))
        b []
    in
    match n with
    | Some n -> (Length, [], [ n ]) :: sub_asrts
    | None -> sub_asrts

  let assertions_others (b, _) =
    List.concat_map (fun (_, v) -> S.assertions_others v) (ExpMap.bindings b)

  let get_recovery_tactic = function
    | SubError (idx, e) ->
        Gillian.General.Recovery_tactic.merge
          (Gillian.General.Recovery_tactic.try_unfold [ idx ])
          (S.get_recovery_tactic e)
    | _ -> Gillian.General.Recovery_tactic.none

  let can_fix = function
    | SubError (_, e) -> S.can_fix e
    | MissingLength -> true
    | OutOfBounds _ -> false

  let get_fixes = function
    | SubError (idx, e) ->
        S.get_fixes e |> MyUtils.deep_map (MyAsrt.map_cp (lift_corepred idx))
    | MissingLength ->
        let lvar = Expr.LVar (LVar.alloc ()) in
        [
          [
            MyAsrt.CorePred (Length, [], [ lvar ]);
            MyAsrt.Types [ (lvar, Type.IntType) ];
          ];
        ]
    | _ -> failwith "Called get_fixes on unfixable error"
end
