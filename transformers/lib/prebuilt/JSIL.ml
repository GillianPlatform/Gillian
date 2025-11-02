open Gillian.Monadic
open Utils
open Gil_syntax
module ExpMap = States.MyUtils.ExpMap

(* Default instantiation is Nono *)
module StringIndex = struct
  include StringIndex

  let default_instantiation = [ Expr.Lit Nono ]
end

(* left side is a PMap that doesn't need any arguments, while
   the right hand side is an Agreement that requires the value.
   Split accordingly (unpatched product gives the args to both sides) *)
module PatchedProduct
    (IDs : IDs)
    (S1 : States.MyMonadicSMemory.S)
    (S2 : States.MyMonadicSMemory.S) :
  States.MyMonadicSMemory.S with type t = S1.t * S2.t = struct
  include Product (IDs) (S1) (S2)

  let instantiate v =
    let s1, v1 = S1.instantiate [] in
    let s2, v2 = S2.instantiate v in
    ((s1, s2), v1 @ v2)

  let pp ft (s1, s2) = Fmt.pf ft "%a with metadata %a" S1.pp s1 S2.pp s2
end

(* Add an action to delete objects *)

module DeleteActionAddition (S : MyMonadicSMemory) :
  ActionAddition with type t = S.t = struct
  type t = S.t
  type action = Delete
  type err_t = unit [@@deriving show, yojson]

  let list_actions () = [ (Delete, [], []) ]

  let action_from_str = function
    | "delete" -> Some Delete
    | _ -> None

  let action_to_str Delete = "delete"
  let[@inline] execute_action Delete _ _ = Delayed.return (Ok (S.empty (), []))
  let can_fix () = false
  let get_fixes () = []
  let get_recovery_tactic () = Gillian.General.Recovery_tactic.none
end

(* the "Props" predicate considers its out an in, so it must be removed
   from consumption and then checked for equality. *)
module MoveInToOut (S : States.MyMonadicSMemory.S) :
  States.MyMonadicSMemory.S with type t = S.t = struct
  include S

  let[@inline] consume pred s ins =
    match (pred_to_str pred, ins) with
    | "domainset", [ out ] -> (
        let open Delayed_result.Syntax in
        let** s', outs = S.consume pred s [] in
        match outs with
        | [ out' ] ->
            if%ent Expr.Infix.(out == out') then Delayed_result.ok (s', [])
            else
              Fmt.failwith
                "Mismatch in domainset (Props) consumption - got: %a, expected \
                 %a"
                Expr.pp out' Expr.pp out
        | _ -> Delayed_result.ok (s', outs))
    | _ -> consume pred s ins
end

(* Outer substitutions for JS *)
module JSSubst : NameMap = struct
  let action_substitutions =
    [
      ("GetCell", "left_load");
      ("SetCell", "left_store");
      ("GetAllProps", "left_inner_get_domainset");
      ("Alloc", "alloc");
      ("DeleteObject", "delete");
      ("GetMetadata", "right_load");
    ]

  let pred_substitutions =
    [
      ("Cell", "left_ex");
      ("Props", "left_inner_domainset");
      ("Metadata", "right_ag");
    ]
end

(* Substitutions for internal PMap (avoids name clash) *)
module JSSubstInner : NameMap = struct
  let action_substitutions = [ ("inner_get_domainset", "get_domainset") ]
  let pred_substitutions = [ ("inner_domainset", "domainset") ]
end

(* Outer pred/action filter *)
module JSFilter : FilterVals = struct
  let mode : filter_mode = ShowOnly

  let action_filters =
    [
      "GetCell";
      "SetCell";
      "Alloc";
      "DeleteObject";
      "GetMetadata";
      "GetAllProps";
    ]

  let preds_filters = [ "Cell"; "Props"; "Metadata" ]
end

(* Patch domainset to filter out all Nonos *)
module PatchDomainsetObject (S : sig
  include MyMonadicSMemory

  val is_not_nono : t -> Expr.t -> bool
end) =
  Injector
    (struct
      include DummyInject (S)

      let post_execute_action a (s, args, rets) =
        match (a, rets) with
        | "get_domainset", [ (Expr.Lit (LList _) as dom) ]
        | "get_domainset", [ (Expr.EList _ as dom) ] ->
            let dom =
              (match dom with
              | Expr.Lit (LList lits) -> List.map Expr.lit lits
              | Expr.EList lits -> lits
              | _ -> failwith "Unexpected domainset type")
              |> List.filter (S.is_not_nono s)
              |> Expr.list
            in
            Delayed.return (s, args, [ dom ])
        | _ -> Delayed.return (s, args, rets)

      let post_consume p (s, outs) =
        match (p, outs) with
        | "domainset", [ Expr.ESet dom ] ->
            let dom = List.filter (S.is_not_nono s) dom in
            Delayed.return (s, [ Expr.ESet dom ])
        | _ -> Delayed.return (s, outs)
    end)
    (S)

(* The content of memory at a given location: object * metadata address *)
module BaseMemoryContent (S : MyMonadicSMemory) = struct
  module S =
    PatchedProduct (IDs) (Mapper (JSSubstInner) (MoveInToOut (S))) (Agreement)

  include ActionAdder (DeleteActionAddition (S)) (S)
end

(* Override pretty printing, implement `is_not_nono` *)
module ObjectBase = struct
  include PMap (StringIndex) (Exclusive)

  let pp ft ((h, d) : t) =
    let d =
      match d with
      | Some (Expr.ESet l) -> Some (Expr.ESet (List.sort Expr.compare l))
      | e -> e
    in
    let open Fmt in
    let pp_bindings =
      iter_bindings ~sep:comma ExpMap.iter
        (hbox (parens (pair ~sep:(any " :") Expr.pp Exclusive.pp)))
    in
    pf ft "[ @[%a@] | @[%a@] ]" pp_bindings h (option Expr.pp) d

  let is_not_nono (h, _) idx =
    match ExpMap.find_opt idx h with
    | Some (Some (Expr.Lit Nono)) -> false
    | _ -> true
end

(* Override pretty printing, implement `is_not_nono` *)
module SplitObjectBase = struct
  include SplitPMap (StringIndex) (Exclusive)

  let pp ft (((ch, sh), d) : t) =
    let h = ExpMap.union (fun _ a _ -> Some a) ch sh in
    let d =
      match d with
      | Some (Expr.ESet l) -> Some (Expr.ESet (List.sort Expr.compare l))
      | e -> e
    in
    let open Fmt in
    let pp_bindings =
      iter_bindings ~sep:comma ExpMap.iter
        (hbox (parens (pair ~sep:(any " :") Expr.pp Exclusive.pp)))
    in
    pf ft "[ @[%a@] | @[%a@] ]" pp_bindings h (option Expr.pp) d

  let is_not_nono ((ch, sh), _) idx =
    match ExpMap.find_opt idx ch with
    | Some (Some (Expr.Lit Nono)) -> false
    | _ -> (
        match ExpMap.find_opt idx sh with
        | Some (Some (Expr.Lit Nono)) -> false
        | _ -> true)
end

(* Patch map pretty-printing for nicer diffs *)
module PatchedBasePMap (S : MyMonadicSMemory) :
  OpenPMapType with type entry = S.t = struct
  include OpenPMap (LocationIndex) (S)

  let pp ft (h : t) =
    let open Fmt in
    let sorted_locs_with_vals =
      ExpMap.bindings h |> List.sort (fun (k1, _) (k2, _) -> Expr.compare k1 k2)
    in
    let pp_one ft (loc, fv_pairs) =
      pf ft "@[%a |-> %a@]" Expr.pp loc S.pp fv_pairs
    in
    (list ~sep:(any "@\n") pp_one) ft sorted_locs_with_vals
end

(* Patch map pretty-printing for nicer diffs *)
module PatchedALocPMap (S : MyMonadicSMemory) = struct
  include OpenALocPMap (S)

  let pp ft (h : t) =
    let open Fmt in
    let sorted_locs_with_vals =
      States.MyUtils.SMap.bindings h
      |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
    in
    let pp_one ft (loc, fv_pairs) = pf ft "@[%s |-> %a@]" loc S.pp fv_pairs in
    (list ~sep:(any "@\n") pp_one) ft sorted_locs_with_vals
end

(* When allocating, two params are given:
    - the address to allocate into (can be 'empty' to generate new address) - defaults to empty
    - the metadata address, which is the value of the agreement (rhs of the object product) - defaults to null
   Need to take that into consideration + similarly to WISL, return the index on each action. *)
module PatchAlloc
    (Obj : MyMonadicSMemory)
    (Map : OpenPMapType with type entry = Obj.t) =
struct
  include Map
  module SS = Gillian.Utils.Containers.SS
  module SMap = States.MyUtils.SMap

  (* Patch the alloc action *)
  let[@inline] execute_action a s args =
    let open Delayed.Syntax in
    match (action_to_str a, args) with
    | "alloc", [ idx; v ] ->
        let* idx =
          match idx with
          | Expr.Lit Empty -> Delayed.return (Some (ALoc.alloc ()))
          | _ -> States.MyUtils.get_loc idx
        in
        let idx =
          match idx with
          | Some idx -> idx
          | None ->
              failwith "Invalid index given for alloc (no error handles this)"
        in
        let idx = Expr.loc_from_loc_name idx in
        let ss, v = Obj.instantiate [ v ] in
        let s' = set ~idx ~idx':idx ss s in
        Delayed_result.ok (s', idx :: v)
    | _ -> execute_action a s args
end

(* Helper to escape (or create) functor hell *)
module Object = BaseMemoryContent (PatchDomainsetObject (ObjectBase))
module SplitObject = BaseMemoryContent (PatchDomainsetObject (SplitObjectBase))

module Wrap
    (Obj : MyMonadicSMemory)
    (Map : OpenPMapType with type entry = Obj.t) =
  Filter (JSFilter) (Mapper (JSSubst) (PatchAlloc (Obj) (Map)))

(* Actual exports *)

module ParserAndCompiler = Js2jsil_lib.JS2GIL_ParserAndCompiler
module ExternalSemantics = Semantics.External
module MonadicSMemory_Base = Wrap (Object) (PatchedBasePMap (Object))
module MonadicSMemory_ALoc = Wrap (Object) (PatchedALocPMap (Object))
module MonadicSMemory_Split = Wrap (SplitObject) (PatchedBasePMap (SplitObject))

module MonadicSMemory_ALocSplit =
  Wrap (SplitObject) (PatchedALocPMap (SplitObject))
