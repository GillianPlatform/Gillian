open Utils
open Gil_syntax
module Delayed = Gillian.Monadic.Delayed
module DR = Gillian.Monadic.Delayed_result
module Global_env = Cgil_lib.Global_env

(* Import C-specific constructs *)
module BlockTree = C_states.BlockTree.M
module CGEnv = C_states.CGEnv.M

(* Base memories *)
module BaseBlock = Freeable (BlockTree)

module type C_PMapType = OpenPMapType with type entry = BaseBlock.t

module BaseMemory : C_PMapType = OpenPMap (LocationIndex) (BaseBlock)
module SplitMemory : C_PMapType = OpenSplitPMap (LocationIndex) (BaseBlock)
module ALocMemory : C_PMapType = OpenALocPMap (BaseBlock)

(* Add move action implementation *)
module ExtendMemory (S : C_PMapType) = struct
  module Addition : ActionAddition with type t = S.t = struct
    type t = S.t
    type action = Move | SetZeros

    type err_t =
      | BaseError of S.err_t
      | BlockTreeErr of (Expr.t * Expr.t * BlockTree.err_t)
      | MoveOnMissing
      | MoveOnFreed
    [@@deriving show, yojson]

    let ( let**^ ) x f =
      Delayed.bind x (function
        | Ok x -> f x
        | Error e -> Delayed.return (Error (BaseError e)))

    let list_actions () =
      [ (Move, [ "?" ], [ "?" ]); (SetZeros, [ "?" ], [ "?" ]) ]

    let action_from_str = function
      | "move" -> Some Move
      | "setZeros" -> Some SetZeros
      | _ -> None

    let action_to_str = function
      | Move -> "move"
      | SetZeros -> "setZeros"

    let exec_move s args =
      match args with
      | [ dst_loc; dst_ofs; src_loc; src_ofs; size ] -> (
          let open DR.Syntax in
          let open Expr.Infix in
          if%sat size == Expr.zero_i then DR.ok (s, [])
          else
            let**^ s, _, src = S.get s src_loc in
            let**^ s, dst_loc', dest = S.get s dst_loc in
            match (src, dest) with
            | States.Freeable.None, _ | _, States.Freeable.None ->
                DR.error MoveOnMissing
            | States.Freeable.Freed, _ | _, States.Freeable.Freed ->
                DR.error MoveOnFreed
            | States.Freeable.SubState src, States.Freeable.SubState dest ->
                let** dest =
                  DR.map_error (BlockTree.move dest dst_ofs src src_ofs size)
                    (fun e -> BlockTreeErr (src_loc, dst_loc, e))
                in
                let s' =
                  S.set ~idx:dst_loc ~idx':dst_loc'
                    (States.Freeable.SubState dest) s
                in
                DR.ok (s', []))
      | _ -> failwith "Invalid arguments for mem_move"

    let pred_zero = S.pred_from_str "zeros" |> Option.get

    let exec_set_zeros s args =
      let s' = S.produce pred_zero s args in
      Delayed.map s' (fun s' -> Ok (s', []))

    let[@inline] execute_action = function
      | Move -> exec_move
      | SetZeros -> exec_set_zeros

    let can_fix = function
      | BaseError e -> S.can_fix e
      | BlockTreeErr (_, _, e) -> BlockTree.can_fix e
      | _ -> false

    let map_fixes mapper =
      States.MyUtils.deep_map
        (States.MyAsrt.map_cp (fun (p, i, o) -> (mapper p, i, o)))

    let get_fixes = function
      | BaseError e -> S.get_fixes e |> map_fixes S.pred_to_str
      | BlockTreeErr (_, _, e) ->
          BlockTree.get_fixes e |> map_fixes BlockTree.pred_to_str
      | _ -> []

    let get_recovery_tactic = function
      | BaseError e -> S.get_recovery_tactic e
      | BlockTreeErr (dest_idx, src_idx, _) ->
          Gillian.General.Recovery_tactic.try_unfold [ dest_idx; src_idx ]
      | _ -> Gillian.General.Recovery_tactic.none
  end

  include ActionAdder (Addition) (S)

  let[@inline] execute_action a s args =
    let open Delayed.Syntax in
    let action = action_to_str a in
    let args =
      (* Move index to be the first argument *)
      match (action, args) with
      | "load", c :: loc :: rest | "store", c :: loc :: rest -> loc :: c :: rest
      | _ -> args
    in
    let+ r = execute_action a s args in
    match (action, r) with
    (* remove returned index (not needed in C) *)
    | "alloc", r -> r
    | _, Ok (s', _ :: rest) -> Ok (s', rest)
    | _, r -> r
end

module Wrap (S : C_PMapType) = struct
  module CMapMemory = ExtendMemory (S)

  include
    Product
      (struct
        let id1 = "mem_"
        let id2 = "genv_"
      end)
      (CMapMemory)
      (CGEnv)

  let pp f (s1, _) = CMapMemory.pp f s1
end

module MonadicSMemory_Base = Wrap (BaseMemory)
module MonadicSMemory_ALoc = Wrap (ALocMemory)
module MonadicSMemory_Split = Wrap (SplitMemory)
module ParserAndCompiler = ParserAndCompiler.Dummy

module ExternalSemantics =
  Gillian.General.External.Dummy (ParserAndCompiler.Annot)

module InitData = Cgil_lib.Global_env

module MyInitData = struct
  type t = InitData.t

  let init = C_states.CGEnv.set_init_data
end
