open Gil_syntax
open Gillian.Monadic
module Subst = Gillian.Symbolic.Subst
module DR = Delayed_result
module DO = Delayed_option

(*
    There are three layers to PMaps: the functor that creates a MyMonadicSMemory module, the implementation
    module that handles the data structure and operations on it, and (sometimes) the indexing module that
    handles index creation and validation.

    1. The functor to create the state model transformer, with the actual implementation that satisfies
       MyMonadicSMemory. There are two variants for it, Make and MakeOpen, creating a state transformer
       either with or without a domain set, respectively. MakeOpen only works on static PMap implementations.

    2. The module for a PMap implementation, PMapImpl; here by implementation we mean the part of the code
       that handles the actual data structure. It must expose get/set functions to access the map, some
       common traversal functions (for_all, fold), and functions relative to indexing. In particular, an
       implementation is either static or dynamic; if it's static, new indices must be created via
       allocation, and the module must provide a make_fresh function to create a new address. If it's
       dynamic, there is no allocation, and new locations are instead created when accessed (eg. object
       entries in JavaScript), in which case the module has a default_instantiation list, with the
       arguments to instantiate sub-states with.
       We note that substitutions and composition are handled by PMapImpl too, to allow for optimisations
       specific to the data structure; it would be interesting to see if a shared implementation has a
       good performance too.
       Currently there exist three PMap implementations: the "base" one that just uses an Expr map, the
       "split" optimisation that splits the map into a concrete and symbolic part, and the ALoc one that
       uses abstract locations and indexes on strings.

    3. Some PMap implementations (the "base" one and the split one) also accept a module as input, that
       represents how the indexing works; this is done via the PMapIndex module type, that specifies
       if indexing is static or dynamic, and how to generate and validate new indices. Some implementations
       don't need it (ALoc, to name it), since they rely on a specific indexing method.

    TODO: It would be nice if instead of having Make and MakeOpen, we'd have one "Make" functor
          that then receives a "discriminator" module that can choose how to handle misses
          (e.g. a DomainSetDiscriminator, OpenDiscriminator, BoundedDiscriminator...). Would need
          to assess the performance cost of this, though.
*)

type index_mode = Static | Dynamic

module type PMapImpl = sig
  type entry
  type t [@@deriving yojson]

  val mode : index_mode

  (** Creates a new address, for allocating new state. Only used in static mode
  *)
  val make_fresh : unit -> Expr.t Delayed.t

  (** The arguments used when instantiating new state. Only used in dynamic mode
  *)
  val default_instantiation : Expr.t list

  (* Note for the below two functions we use option Delayed rather than
     result Delayed, to avoid the headache of handling additional errors. *)

  (** Validates the index, by returning possibly a new index (or that same
      index). Returns None if the index is not valid. *)
  val validate_index : Expr.t -> Expr.t option Delayed.t

  (** Returns (symbolically) the state at the given index if it's found, or None
      if no state is there, in which case an empty binding could be created
      there instead. It's important this function doesn't return an empty state
      (with MyMonadicSMemory.empty ()), but None, as the caller might need to
      distinguish these situations (eg. checking a domain set).

      This function should assume the index is valid (ie. it was returned by
      `validate_index`). *)
  val get : t -> Expr.t -> (Expr.t * entry) option Delayed.t

  (** Updates the entry with the given state; `idx` represents the previous
      index of the state, in case a new index was found for it. In other words,
      after this operation the map must store nothing at `idx`, and the new
      state at `idx'`. `idx` and `idx'` can be equal, in which case the state is
      just added/updated. *)
  val set : idx:Expr.t -> idx':Expr.t -> entry -> t -> t

  val empty : t
  val fold : (Expr.t -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (entry -> bool) -> t -> bool
  val compose : t -> t -> t Delayed.t
  val substitution_in_place : Subst.t -> t -> t Delayed.t
end

module type OpenPMapType = sig
  include MyMonadicSMemory.S

  type entry

  val get : t -> Expr.t -> (t * Expr.t * entry, err_t) DR.t
  val set : idx:Expr.t -> idx':Expr.t -> entry -> t -> t
end

module type PMapType = sig
  include OpenPMapType

  val domain_add : Expr.t -> t -> t
end

module Make
    (I_Cons : functor (S : MyMonadicSMemory.S) -> PMapImpl with type entry = S.t)
    (S : MyMonadicSMemory.S) =
struct
  module I = I_Cons (S)

  type entry = S.t
  type t = I.t * Expr.t option [@@deriving yojson]

  let pp fmt ((h, d) : t) =
    let iter f h = I.fold (fun k v () -> f k v) h () in
    Format.pp_open_vbox fmt 0;
    MyUtils.pp_bindings ~pp_k:Expr.pp ~pp_v:S.pp iter fmt h;
    Format.pp_close_box fmt ();
    match d with
    | None -> Format.fprintf fmt "@\nDomainSet: None"
    | Some (Expr.ESet l) ->
        let l' = List.sort Expr.compare l in
        Format.fprintf fmt "@\nDomainSet: -{ %a }-"
          (Fmt.list ~sep:Fmt.comma Expr.pp)
          l'
    | Some d ->
        (* shouldn't happen *)
        Format.fprintf fmt "@\nDomainSet: %a" Expr.pp d

  type err_t =
    | NotAllocated of Expr.t
    | InvalidIndexValue of Expr.t
    | MissingDomainSet
    | DomainSetNotFullyOwned
    | SubError of
        Expr.t * Expr.t * S.err_t (* Original index, map index, error *)
  [@@deriving show, yojson]

  type action = Alloc | GetDomainSet | SubAction of S.action

  let action_from_str = function
    | "alloc" -> Some Alloc
    | "get_domainset" -> Some GetDomainSet
    | s -> Option.map (fun a -> SubAction a) (S.action_from_str s)

  let action_to_str = function
    | SubAction a -> S.action_to_str a
    | Alloc -> "alloc"
    | GetDomainSet -> "get_domainset"

  let list_actions () =
    (match I.mode with
    | Static -> [ (Alloc, [ "params" ], [ "address" ]) ]
    | Dynamic -> [])
    @ [ (GetDomainSet, [], [ "domainset" ]) ]
    @ List.map
        (fun (a, args, ret) -> (SubAction a, "index" :: args, ret))
        (S.list_actions ())

  type pred = DomainSet | SubPred of S.pred

  let pred_from_str = function
    | "domainset" -> Some DomainSet
    | s -> Option.map (fun p -> SubPred p) (S.pred_from_str s)

  let pred_to_str = function
    | SubPred p -> S.pred_to_str p
    | DomainSet -> "domainset"

  let list_preds () =
    (DomainSet, [], [ "domainset" ])
    :: List.map
         (fun (p, ins, outs) -> (SubPred p, "index" :: ins, outs))
         (S.list_preds ())

  let domain_add k ((h, d) : t) =
    match d with
    | None -> (h, d)
    | Some (Expr.ESet d) -> (h, Some (Expr.ESet (k :: d)))
    | Some _ ->
        (* Currently we only support "concrete" domain sets (ie. using Expr.ESet, instead of
           as logical variables). To add support to "symbolic" domain sets one would need
           to make this function return a Expr.t Delayed.t, adding to the PC that the key is in
           the set. *)
        failwith "Invalid index set; expected a set"

  let[@inline] get ((h, d) as s) idx =
    let open Delayed.Syntax in
    let* idx_opt = I.validate_index idx in
    match idx_opt with
    | None -> DR.error (InvalidIndexValue idx)
    | Some idx' -> (
        let* res = I.get h idx' in
        match (res, d) with
        | Some (idx', ss), _ -> DR.ok (s, idx', ss)
        | None, None -> DR.ok (s, idx', S.empty ())
        | None, Some d -> (
            if%sat Expr.BinOp (idx', SetMem, d) then DR.ok (s, idx', S.empty ())
            else
              match I.mode with
              | Static ->
                  Logging.normal (fun f ->
                      f "STATIC NotAllocated: %a" Expr.pp idx');
                  DR.error (NotAllocated idx')
              | Dynamic ->
                  let ss, _ = S.instantiate I.default_instantiation in
                  let h' = I.set ~idx:idx' ~idx':idx ss h in
                  let s' = (h', Some d) |> domain_add idx' in
                  DR.ok (s', idx', ss)))

  let set ~idx ~idx' entry ((h, d) : t) = (I.set ~idx ~idx' entry h, d)

  let lifting_err idx idx' v fn =
    match v with
    | Ok v -> Ok (fn v)
    | Error e -> Error (SubError (idx, idx', e))

  let empty () : t = (I.empty, None)

  let[@inline] execute_action action (s : t) args =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (action, args) with
    | SubAction _, [] -> failwith "Missing index for sub-action"
    | SubAction action, idx :: args ->
        let** s, idx', ss = get s idx in
        let* () = Delayed.return ~learned:[ Expr.Infix.(idx == idx') ] () in
        let+ r = S.execute_action action ss args in
        let ( let+^ ) = lifting_err idx idx' in
        let+^ ss', v = r in
        let s' = set ~idx ~idx' ss' s in
        (s', idx' :: v)
    | Alloc, args ->
        if I.mode = Dynamic then
          failwith "Alloc not allowed using dynamic indexing";
        let* idx = I.make_fresh () in
        let ss, v = S.instantiate args in
        let s' = set ~idx ~idx':idx ss s |> domain_add idx in
        DR.ok (s', idx :: v)
    | GetDomainSet, [] -> (
        match s with
        (* Implementation taken from JSIL:
           - ensure domain set is there
           - ensure the domain set is exactly the set of keys in the map
           - filter keys to remove empty cells (for JS: Nono)
           - return as a list *)
        | h, Some d ->
            let keys = I.fold (fun k _ acc -> k :: acc) h [] in
            if%ent Expr.Infix.(d == Expr.ESet keys) then
              let keys =
                I.fold
                  (fun k v acc -> if S.is_empty v then acc else k :: acc)
                  h []
              in
              DR.ok (s, [ Expr.list keys ])
            else DR.error DomainSetNotFullyOwned
        | _, None -> DR.error MissingDomainSet)
    | GetDomainSet, _ -> failwith "Invalid arguments for get_domainset"

  let[@inline] consume pred s ins =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (pred, ins) with
    | SubPred _, [] -> failwith "Missing index for sub-predicate"
    | SubPred pred, idx :: ins ->
        let** s, idx', ss = get s idx in
        let+ r = S.consume pred ss ins in
        let ( let+^ ) = lifting_err idx idx' in
        let+^ ss', v = r in
        let s' = set ~idx ~idx' ss' s in
        (s', v)
    | DomainSet, [] -> (
        match s with
        | h, Some d -> DR.ok ((h, None), [ d ])
        | _, None -> DR.error MissingDomainSet)
    | DomainSet, _ -> failwith "Invalid number of ins for domainset"

  let[@inline] produce pred s args =
    let open Delayed.Syntax in
    let open MyUtils.Syntax in
    match (pred, args) with
    | SubPred _, [] -> failwith "Missing index for sub-predicate"
    | SubPred pred, idx :: args ->
        let*? s, idx', ss = get s idx in
        let+ ss' = S.produce pred ss args in
        set ~idx ~idx' ss' s
    | DomainSet, [ d' ] -> (
        match s with
        | _, Some _ -> Delayed.vanish ()
        | h, None ->
            (* This would be the correct implementation, but the handling of sets is bad so
               it creates all sorts of issues (eg. in matching plans)...
               let dom = ExpMap.bindings h |> List.map fst in
               let dom = Expr.ESet dom in*)
            Delayed.return (*~learned:[ Formula.SetSub (dom, d') ]*) (h, Some d')
        )
    | DomainSet, _ -> failwith "Invalid arguments for domainset produce"

  let compose (h1, d1) (h2, d2) =
    let open Delayed.Syntax in
    match (d1, d2) with
    | None, d | d, None ->
        let+ h = I.compose h1 h2 in
        (h, d)
    | Some _, Some _ -> Delayed.vanish ()

  let is_exclusively_owned (h, d) e =
    match d with
    | None -> Delayed.return false
    | Some (Expr.ESet d) ->
        let size = I.fold (fun _ _ acc -> acc + 1) h 0 in
        (* Here we do the assumption that all indices in the heap correspond to semantically
           different indices. This is not always the case (eg. ALocs)! But good enough in many
           cases and avoids crazy O(N!) branching. *)
        if size == List.length d then
          let open Delayed.Syntax in
          I.fold
            (fun _ v acc ->
              let* acc_v = acc in
              if not acc_v then acc else S.is_exclusively_owned v e)
            h (Delayed.return true)
        else Delayed.return false
    | Some _ -> failwith "Invalid domain set"

  let is_empty = function
    | _, Some _ -> false
    | h, None -> I.for_all S.is_empty h

  let is_concrete = function
    | h, Some d -> Expr.is_concrete d && I.for_all S.is_concrete h
    | h, None -> I.for_all S.is_concrete h

  let instantiate = function
    | [] -> ((I.empty, Some (Expr.ESet [])), [])
    | [ Expr.EList fields ] ->
        let ss, _ = S.instantiate [] in
        let h =
          List.fold_left
            (fun acc k -> I.set ~idx:k ~idx':k ss acc)
            I.empty fields
        in
        let d = Expr.ESet fields in
        ((h, Some d), [])
    | _ -> failwith "Invalid arguments for instantiation"

  let substitution_in_place sub (h, d) =
    let open Delayed.Syntax in
    let d' = Option.map (Subst.subst_in_expr sub ~partial:true) d in
    let+ h' = I.substitution_in_place sub h in
    (h', d')

  let accumulate ~fn_k ~fn_v h =
    let open Utils.Containers.SS in
    I.fold (fun k s acc -> fn_v s |> union @@ fn_k k |> union acc) h empty

  let lvars (h, d) =
    let open Utils.Containers.SS in
    accumulate ~fn_k:Expr.lvars ~fn_v:S.lvars h
    |> union @@ Option.fold ~none:empty ~some:Expr.lvars d

  let alocs (h, d) =
    let open Utils.Containers.SS in
    accumulate ~fn_k:Expr.alocs ~fn_v:S.alocs h
    |> union @@ Option.fold ~none:empty ~some:Expr.alocs d

  let lift_corepred k (p, i, o) = (SubPred p, k :: i, o)

  let assertions (h, d) =
    I.fold
      (fun k s acc -> List.map (lift_corepred k) (S.assertions s) @ acc)
      h []
    @ Option.fold ~none:[] ~some:(fun d -> [ (DomainSet, [], [ d ]) ]) d

  let assertions_others (h, _) =
    I.fold (fun _ s acc -> S.assertions_others s @ acc) h []

  let get_recovery_tactic = function
    | SubError (_, idx, e) ->
        Gillian.General.Recovery_tactic.merge (S.get_recovery_tactic e)
          (Gillian.General.Recovery_tactic.try_unfold [ idx ])
    | NotAllocated idx | InvalidIndexValue idx ->
        Gillian.General.Recovery_tactic.try_unfold [ idx ]
    | _ -> Gillian.General.Recovery_tactic.none

  let can_fix = function
    | SubError (_, _, e) -> S.can_fix e
    | MissingDomainSet -> true
    | DomainSetNotFullyOwned -> false
    | InvalidIndexValue _ -> false
    | NotAllocated _ -> false

  let get_fixes = function
    | SubError (idx, idx', e) ->
        S.get_fixes e
        |> MyUtils.deep_map @@ MyAsrt.map_cp @@ lift_corepred idx'
        |> List.map @@ List.cons @@ MyAsrt.Pure Expr.Infix.(idx == idx')
    | MissingDomainSet ->
        let lvar = Expr.LVar (LVar.alloc ()) in
        [
          [
            MyAsrt.CorePred (DomainSet, [], [ lvar ]);
            MyAsrt.Types [ (lvar, Type.SetType) ];
          ];
        ]
    | _ -> failwith "Called get_fixes on unfixable error"
end

module MakeOpen
    (I_Cons : functor (S : MyMonadicSMemory.S) -> PMapImpl with type entry = S.t)
    (S : MyMonadicSMemory.S) =
struct
  module I = I_Cons (S)

  let () =
    if I.mode = Dynamic then failwith "Dynamic mode not supported for OpenPMap"

  type entry = S.t
  type t = I.t [@@deriving yojson]

  let pp fmt (h : t) =
    let iter f h = I.fold (fun k v () -> f k v) h () in
    Format.pp_open_vbox fmt 0;
    MyUtils.pp_bindings ~pp_k:Expr.pp ~pp_v:S.pp iter fmt h;
    Format.pp_close_box fmt ()

  type err_t =
    | InvalidIndexValue of Expr.t
    | SubError of
        Expr.t * Expr.t * S.err_t (* Original index, map index, error *)
  [@@deriving show, yojson]

  type action = Alloc | SubAction of S.action

  let action_from_str = function
    | "alloc" -> Some Alloc
    | s -> Option.map (fun a -> SubAction a) (S.action_from_str s)

  let action_to_str = function
    | SubAction a -> S.action_to_str a
    | Alloc -> "alloc"

  let list_actions () =
    [ (Alloc, [ "params" ], [ "address" ]) ]
    @ List.map
        (fun (a, args, ret) -> (SubAction a, "index" :: args, ret))
        (S.list_actions ())

  type pred = S.pred

  let pred_from_str = S.pred_from_str
  let pred_to_str = S.pred_to_str

  let list_preds () =
    List.map (fun (p, ins, outs) -> (p, "index" :: ins, outs)) (S.list_preds ())

  let[@inline] get s idx =
    let open Delayed.Syntax in
    let* idx_opt = I.validate_index idx in
    match idx_opt with
    | None -> DR.error (InvalidIndexValue idx)
    | Some idx' -> (
        let* res = I.get s idx' in
        match res with
        | None -> DR.ok (s, idx', S.empty ())
        | Some (idx', ss) -> DR.ok (s, idx', ss))

  let set = I.set

  let[@inline] lifting_err idx idx' v fn =
    match v with
    | Ok v -> Ok (fn v)
    | Error e -> Error (SubError (idx, idx', e))

  let empty () : t = I.empty

  let[@inline] execute_action action (s : t) args =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (action, args) with
    | SubAction _, [] -> failwith "Missing index for sub-action"
    | SubAction action, idx :: args ->
        let** s, idx', ss = get s idx in
        let+ r = S.execute_action action ss args in
        let ( let+^ ) = lifting_err idx idx' in
        let+^ ss', v = r in
        let s' = set ~idx ~idx' ss' s in
        (s', idx' :: v)
    | Alloc, args ->
        let* idx = I.make_fresh () in
        let ss, v = S.instantiate args in
        let s' = set ~idx ~idx':idx ss s in
        DR.ok (s', idx :: v)

  let[@inline] consume pred s ins =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match ins with
    | [] -> failwith "Missing index for sub-predicate"
    | idx :: ins ->
        let** s, idx', ss = get s idx in
        let+ r = S.consume pred ss ins in
        let ( let+^ ) = lifting_err idx idx' in
        let+^ ss', v = r in
        let s' = set ~idx ~idx' ss' s in
        (s', v)

  let[@inline] produce pred s args =
    let open Delayed.Syntax in
    let open MyUtils.Syntax in
    match args with
    | [] -> failwith "Missing index for sub-predicate"
    | idx :: args ->
        let*? s, idx', ss = get s idx in
        let+ ss' = S.produce pred ss args in
        set ~idx ~idx' ss' s

  let compose = I.compose
  let is_exclusively_owned _ _ = Delayed.return false
  let is_empty = I.for_all S.is_empty
  let is_concrete = I.for_all S.is_concrete

  let instantiate = function
    | [] -> (I.empty, [])
    | _ -> failwith "Invalid arguments for instantiation"

  let substitution_in_place = I.substitution_in_place

  let accumulate ~fn_k ~fn_v h =
    let open Utils.Containers.SS in
    I.fold (fun k s acc -> fn_v s |> union @@ fn_k k |> union acc) h empty

  let lvars = accumulate ~fn_k:Expr.lvars ~fn_v:S.lvars
  let alocs = accumulate ~fn_k:Expr.alocs ~fn_v:S.alocs
  let lift_corepred k (p, i, o) = (p, k :: i, o)

  let assertions h =
    I.fold
      (fun k s acc -> List.map (lift_corepred k) (S.assertions s) @ acc)
      h []

  let assertions_others h =
    I.fold (fun _ s acc -> S.assertions_others s @ acc) h []

  let get_recovery_tactic = function
    | SubError (_, idx, e) ->
        Gillian.General.Recovery_tactic.merge (S.get_recovery_tactic e)
          (Gillian.General.Recovery_tactic.try_unfold [ idx ])
    | InvalidIndexValue idx ->
        Gillian.General.Recovery_tactic.try_unfold [ idx ]

  let can_fix = function
    | SubError (_, _, e) -> S.can_fix e
    | InvalidIndexValue _ -> false

  let get_fixes = function
    | SubError (idx, idx', e) ->
        S.get_fixes e
        |> MyUtils.deep_map @@ MyAsrt.map_cp @@ lift_corepred idx'
        |> List.map @@ List.cons @@ MyAsrt.Pure Expr.Infix.(idx == idx')
    | _ -> failwith "Called get_fixes on unfixable error"
end

(** Type for the domain of a PMap. Allows configuring it to either have static
    or dynamic indexing:
    - Static: indexes are created by the state model on allocation (eg. the heap
      in C)
    - Dynamic: indexes are given by the user on allocation (eg. objects in JS)

    The user must provide the index on allocation in dynamic mode, and mustn't
    provide it in static mode. is_valid_index must always be implemented, while
    make_fresh is only needed in static mode. *)
module type PMapIndex = sig
  val mode : index_mode

  (** If the given expression is a valid index for the map. Returns a possibly
      simplified index, and None if it's not valid. *)
  val is_valid_index : Expr.t -> Expr.t option Delayed.t

  (** Creates a new address, for allocating new state. Only used in static mode
  *)
  val make_fresh : unit -> Expr.t Delayed.t

  (** The arguments used when instantiating new state. Only used in dynamic mode
  *)
  val default_instantiation : Expr.t list
end

module LocationIndex : PMapIndex = struct
  let mode = Static

  let make_fresh () =
    let loc = ALoc.alloc () in
    Expr.loc_from_loc_name loc |> Delayed.return

  let is_valid_index e =
    Delayed_option.map (MyUtils.get_loc e) Expr.loc_from_loc_name

  let default_instantiation = []
end

module StringIndex : PMapIndex = struct
  let mode = Dynamic
  let is_valid_index = DO.some ~learned:[]
  let make_fresh () = failwith "Invalid in dynamic mode"
  let default_instantiation = []
end

module IntegerIndex : PMapIndex = struct
  open Expr.Infix

  let mode = Static
  let last_index = ref None

  let make_fresh () =
    let lvar = LVar.alloc () in
    let e = Expr.LVar lvar in
    let learnt =
      match !last_index with
      | None -> []
      | Some last -> [ e == last + Expr.one_i ]
    in
    last_index := Some e;
    Delayed.return ~learned:learnt
      ~learned_types:[ (lvar, Type.IntType) ]
      (Expr.LVar lvar)

  let is_valid_index = function
    | l -> Delayed.return (Some l)

  let default_instantiation = []
end

(** "Base" implementation of an open PMap, with no particular optimisation.
    Takes as modules the backing ExpMap used (%sat or %ent), and the PMapIndex
    used for validating and generating indices. Because this PMap is *open*, it
    is only compatible with static indexing, as dynamic indexing requires a
    domain set to be sound. *)
module MakeBaseImpl
    (ExpMap : MyUtils.SymExprMap)
    (I : PMapIndex)
    (S : MyMonadicSMemory.S) =
struct
  type entry = S.t
  type t = S.t ExpMap.t [@@deriving yojson]

  let mode = I.mode
  let make_fresh = I.make_fresh
  let default_instantiation = I.default_instantiation
  let empty = ExpMap.empty
  let fold = ExpMap.fold
  let for_all f = ExpMap.for_all (fun _ v -> f v)
  let validate_index = I.is_valid_index
  let[@inline] get idx h = ExpMap.sym_find_opt h idx

  let[@inline] set ~idx ~idx' s h =
    if S.is_empty s then ExpMap.remove idx h
    else if Expr.equal idx idx' then ExpMap.add idx s h
    else ExpMap.remove idx h |> ExpMap.add idx' s

  let compose = ExpMap.sym_merge S.compose

  let substitution_in_place sub h =
    let open Delayed.Syntax in
    let mapper (idx, s) =
      let+ s' = S.substitution_in_place sub s in
      let idx' = Subst.subst_in_expr sub ~partial:true idx in
      (idx', s')
    in
    let* sub_entries = ExpMap.bindings h |> List.map mapper |> Delayed.all in
    ExpMap.sym_compose S.compose sub_entries ExpMap.empty
end

module BaseImplSat = MakeBaseImpl (MyUtils.ExpMap)
module BaseImplEnt = MakeBaseImpl (MyUtils.ExpMapEnt)

(** Implementation of an open PMap with concrete/symbolic split. *)
module MakeSplitImpl
    (ExpMap : MyUtils.SymExprMap)
    (I : PMapIndex)
    (S : MyMonadicSMemory.S) =
struct
  type entry = S.t
  type t = S.t ExpMap.t * S.t ExpMap.t [@@deriving yojson]

  let mode = I.mode
  let make_fresh = I.make_fresh
  let default_instantiation = I.default_instantiation
  let empty = (ExpMap.empty, ExpMap.empty)

  let fold f (ch, sh) acc =
    let acc = ExpMap.fold f ch acc in
    ExpMap.fold f sh acc

  let for_all f (ch, sh) =
    ExpMap.for_all (fun _ v -> f v) ch && ExpMap.for_all (fun _ v -> f v) sh

  let validate_index = I.is_valid_index

  let[@inline] get (ch, sh) idx =
    let open Delayed.Syntax in
    (* This check might not be needed if we know idx' is not concrete *)
    match ExpMap.find_opt idx ch with
    | Some v -> DO.some (idx, v)
    | None -> (
        match ExpMap.find_opt idx sh with
        | Some v -> DO.some (idx, v)
        | None -> (
            let merged = ExpMap.union (fun _ v1 _ -> Some v1) ch sh in
            let* match_val = ExpMap.sym_find_opt idx merged in
            match match_val with
            | Some (idx'', v) -> DO.some (idx'', v)
            | None -> DO.none ()))

  let[@inline] set ~idx ~idx' s (ch, sh) =
    (* remove from both (dont know where it was) *)
    let ch', sh' = (ExpMap.remove idx ch, ExpMap.remove idx sh) in
    if S.is_empty s then (ch', sh')
    else if Expr.is_concrete idx' && S.is_concrete s then
      (ExpMap.add idx' s ch', sh')
    else (ch', ExpMap.add idx' s sh')

  let compose (ch1, sh1) (ch2, sh2) =
    let open Delayed.Syntax in
    (* This is not sound, we don't take into account when, eg,
       ch1 has [i -> x] and sh2 has [i -> y], where we need to compose
       x • y and check if it's concrete. *)
    let* ch = ExpMap.sym_merge S.compose ch1 ch2 in
    let+ sh = ExpMap.sym_merge S.compose sh1 sh2 in
    (ch, sh)

  let substitution_in_place sub (ch, sh) =
    let open Delayed.Syntax in
    let subst = Subst.subst_in_expr sub ~partial:true in
    let mapper (idx, s) =
      let+ s' = S.substitution_in_place sub s in
      let idx' = subst idx in
      (idx', s')
    in
    let map_entries = ExpMap.bindings sh in
    let* sub_entries = Delayed.all (List.map mapper map_entries) in
    let ch_new, sh_new =
      List.partition
        (fun (k, v) -> Expr.is_concrete k && S.is_concrete v)
        sub_entries
    in
    let* ch' = ExpMap.sym_compose S.compose ch_new ch in
    let+ sh' = ExpMap.sym_compose S.compose sh_new ExpMap.empty in
    (ch', sh')
end

module SplitImplSat = MakeSplitImpl (MyUtils.ExpMap)
module SplitImplEnt = MakeSplitImpl (MyUtils.ExpMapEnt)

(** Implementation of an open PMap with abstract locations. *)
module ALocImpl (S : MyMonadicSMemory.S) = struct
  module SMap = MyUtils.SMap

  type entry = S.t
  type t = S.t MyUtils.SMap.t [@@deriving yojson]

  let mode : index_mode = Static
  let make_fresh () = ALoc.alloc () |> Expr.loc_from_loc_name |> Delayed.return
  let default_instantiation = []
  let empty = SMap.empty
  let fold f = SMap.fold (fun k v acc -> f (Expr.loc_from_loc_name k) v acc)
  let for_all f = SMap.for_all (fun _ v -> f v)

  let[@inline] get_loc_fast = function
    | Expr.Lit (Loc loc) -> loc
    | Expr.ALoc loc -> loc
    | e ->
        Fmt.failwith
          "ALocImpl: get_loc_fast: non-trivial location passed to \
           get_loc_fast: %a"
          Expr.pp e

  let[@inline] validate_index idx =
    DO.map (MyUtils.get_loc idx) Expr.loc_from_loc_name

  let[@inline] get h idx =
    let idx_s = get_loc_fast idx in
    match SMap.find_opt idx_s h with
    | Some v -> DO.some (idx, v)
    | None -> DO.none ()

  let[@inline] set ~idx:_ ~idx' s h =
    let idx_s = get_loc_fast idx' in
    if S.is_empty s then SMap.remove idx_s h else SMap.add idx_s s h

  let compose h1 h2 =
    let open Delayed.Syntax in
    let compose_binding m (k, v) =
      let* m = m in
      match SMap.find_opt k m with
      | Some v' ->
          let+ v'' = S.compose v v' in
          SMap.add k v'' m
      | None -> Delayed.return (SMap.add k v m)
    in
    List.fold_left compose_binding (Delayed.return h1) (SMap.bindings h2)

  let substitution_in_place sub h =
    let open Delayed.Syntax in
    let aloc_subst =
      Subst.fold sub
        (fun l r acc ->
          match l with
          | ALoc aloc -> (aloc, r) :: acc
          | _ -> acc)
        []
    in
    let* substituted =
      SMap.fold
        (fun k v acc ->
          let* acc = acc in
          let+ s' = S.substitution_in_place sub v in
          SMap.add k s' acc)
        h
        (Delayed.return SMap.empty)
    in
    List.fold_left
      (fun acc (idx, idx') ->
        let* acc = acc in
        match SMap.find_opt idx acc with
        | None -> Delayed.return acc
        | Some s -> (
            let idx' = get_loc_fast idx' in
            match SMap.find_opt idx' acc with
            | None -> Delayed.return (SMap.remove idx acc |> SMap.add idx' s)
            | Some s' ->
                let+ s'' = S.compose s s' in
                SMap.remove idx acc |> SMap.add idx' s''))
      (Delayed.return substituted)
      aloc_subst
end
