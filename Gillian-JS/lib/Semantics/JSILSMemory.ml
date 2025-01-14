open Gillian
open Gillian.Gil_syntax
open Javert_utils
open Js2jsil_lib
module GAsrt = Asrt
module SSubst = Gillian.Symbolic.Subst
module L = Logging
module SVal = Gillian.Symbolic.Values
module PFS = Gillian.Symbolic.Pure_context
module Type_env = Gillian.Symbolic.Type_env
module Recovery_tactic = Gillian.General.Recovery_tactic
open Gillian.Logic

module M = struct
  type init_data = unit
  type vt = SVal.t [@@deriving yojson, show]

  (** Type of JSIL general states *)
  type t = SHeap.t [@@deriving yojson]

  let sure_is_nonempty _ = (* TODO: Implement *) false

  (** Type of JSIL substitutions *)
  type st = SSubst.t

  (** Errors *)
  type i_fix_t =
    | FLoc of vt
    | FCell of vt * vt
    | FMetadata of vt
    | FPure of Expr.t
  [@@deriving yojson, show]

  type err_t = vt list * i_fix_t list list * Expr.t [@@deriving yojson, show]

  type action_ret =
    ( (t * vt list * Expr.t list * (string * Type.t) list) list,
      err_t list )
    result

  let pp_i_fix ft (i_fix : i_fix_t) : unit =
    let open Fmt in
    match i_fix with
    | FLoc loc -> pf ft "@[<h>MIFLoc(%a)@]" SVal.pp loc
    | FCell (loc, prop) ->
        pf ft "@[<h>MIFCell(%a, %a)@]" SVal.pp loc SVal.pp prop
    | FMetadata loc -> pf ft "@[<h>MIFMetadata(%a)@]" SVal.pp loc
    | FPure f -> pf ft "@[<h>MIFPure(%a)@]" Expr.pp f

  let get_failing_constraint (err : err_t) : Expr.t =
    let _, _, f = err in
    f

  let pp_err ft (err : err_t) : unit =
    let open Fmt in
    let vs, fixes, f = err in
    let pp_fixes ft fix = pf ft "[%a]" (list ~sep:comma pp_i_fix) fix in
    pf ft "@[<h><[%a], %a, %a>@]" (list ~sep:comma SVal.pp) vs Expr.pp f
      (list ~sep:semi pp_fixes) fixes

  let get_recovery_tactic (heap : t) (err : err_t) =
    let ufl_vs, _, _ = err in
    L.verbose (fun fmt ->
        fmt "JSIL SMemory: Recovery values: %a"
          Fmt.(brackets (list ~sep:comma Expr.pp))
          ufl_vs);
    let ufl_alocs =
      List.filter
        (fun e ->
          match e with
          | Expr.ALoc _ | Lit (Loc _) -> true
          | _ -> false)
        ufl_vs
    in
    let values =
      match ufl_alocs with
      | [] -> ufl_vs
      | alocs ->
          let imeta = SHeap.get_inv_metadata heap in
          (* Perhaps we are looking for metadata? *)
          let metadata_recovery_vals =
            List.fold_left
              (fun mrvs aloc ->
                match Hashtbl.find_opt imeta aloc with
                | Some md -> md :: mrvs
                | _ -> mrvs)
              [] alocs
          in
          ufl_vs @ metadata_recovery_vals
    in
    Recovery_tactic.try_unfold values

  let assertions ?to_keep:_ (heap : t) : GAsrt.t = SHeap.assertions heap
  let lvars (heap : t) : Containers.SS.t = SHeap.lvars heap
  let alocs (heap : t) : Containers.SS.t = SHeap.alocs heap

  let clean_up ?(keep = Expr.Set.empty) (heap : t) : Expr.Set.t * Expr.Set.t =
    SHeap.clean_up heap;
    (Expr.Set.empty, keep)

  let substitution_in_place ~pfs:_ ~gamma:_ (subst : st) (heap : t) =
    SHeap.substitution_in_place subst heap;
    [ (heap, Expr.Set.empty, []) ]

  let pp fmt (heap : t) : unit = SHeap.pp fmt heap
  let pp_by_need locs fmt heap = SHeap.pp_by_need locs fmt heap
  let get_print_info = SHeap.get_print_info
  let copy (heap : t) : t = SHeap.copy heap
  let init () : t = SHeap.init ()
  let get_init_data _ = ()
  let clear (_ : t) = init () (* We don't maintain any context *)

  let get_loc_name pfs gamma =
    Gillian.Logic.FOSolver.resolve_loc_name ~pfs ~gamma

  let fresh_loc ?(loc : vt option) (pfs : PFS.t) (gamma : Type_env.t) :
      string * vt * Expr.t list =
    match loc with
    | Some loc -> (
        let loc_name = get_loc_name pfs gamma loc in
        match loc_name with
        | Some loc_name ->
            if Names.is_aloc_name loc_name then
              (loc_name, Expr.ALoc loc_name, [])
            else (loc_name, Expr.Lit (Loc loc_name), [])
        | None ->
            let al = ALoc.alloc () in
            (al, ALoc al, [ Expr.BinOp (ALoc al, Equal, loc) ]))
    | None ->
        let al = ALoc.alloc () in
        (al, ALoc al, [])

  let alloc
      (heap : t)
      (pfs : PFS.t)
      (loc : vt option)
      ?is_empty:(ie = false)
      (mv : vt option) : action_ret =
    let (loc_name : string), (loc : Expr.t) =
      match (loc : Expr.t option) with
      | None ->
          let loc_name = ALoc.alloc () in
          (loc_name, ALoc loc_name)
      | Some (Lit (Loc loc)) -> (loc, Lit (Loc loc))
      | Some (ALoc loc) -> (loc, ALoc loc)
      | Some (LVar v) ->
          let loc_name = ALoc.alloc () in
          PFS.extend pfs (BinOp (LVar v, Equal, ALoc loc_name));
          (loc_name, ALoc loc_name)
      | Some le ->
          raise
            (Failure
               (Printf.sprintf "Alloc with a non-loc loc argument: %s"
                  ((Fmt.to_to_string Expr.pp) le)))
    in
    SHeap.init_object heap loc_name ~is_empty:ie mv;
    Ok [ (heap, [ loc ], [], []) ]

  let set_cell
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (prop : vt)
      (v : vt) : action_ret =
    let loc_name, _, new_pfs = fresh_loc ~loc pfs gamma in
    SHeap.set_fv_pair heap loc_name prop v;
    Ok [ (heap, [], new_pfs, []) ]

  let get_cell
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (prop : vt) : action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    L.tmi (fun m ->
        m "@[<h>GetCell: resolved location: %a -> %a@]" SVal.pp loc
          Fmt.(option ~none:(any "None") string)
          loc_name);

    let make_gc_error
        (loc_name : string)
        (prop : vt)
        (props : vt list)
        (dom : vt option) : err_t =
      let loc = Expr.loc_from_loc_name loc_name in
      (*  failing_constraint *)
      let ff =
        Expr.conjunct
          (List.map
             (fun prop' -> Expr.UnOp (Not, BinOp (prop, Equal, prop')))
             props)
      in

      let fixes_exist_props : i_fix_t list list =
        List.map
          (fun prop' -> [ FPure (Expr.BinOp (prop, Equal, prop')) ])
          props
      in
      let fix_new_property : i_fix_t list = [ FCell (loc, prop); FPure ff ] in

      match dom with
      | Some dom ->
          let ff' : Expr.t = BinOp (prop, SetMem, dom) in
          let ff'' : Expr.t = BinOp (ff, And, ff') in
          let fix_new_property' : i_fix_t list =
            FPure ff' :: fix_new_property
          in
          ([ loc ], fix_new_property' :: fixes_exist_props, ff'')
      | None -> ([ loc; prop ], fix_new_property :: fixes_exist_props, ff)
    in

    let get_cell_from_loc loc_name =
      Option.fold
        ~some:(fun ((fv_list, dom), mtdt) ->
          L.tmi (fun m -> m "fv_list: %a" SFVL.pp fv_list);
          L.tmi (fun m ->
              m "domain: %a" Fmt.(option ~none:(any "None") Expr.pp) dom);
          L.tmi (fun m ->
              m "metadata: %a" Fmt.(option ~none:(any "None") Expr.pp) mtdt);
          match SFVL.get prop fv_list with
          | Some ffv -> Ok [ (heap, [ loc; prop; ffv ], [], []) ]
          | None -> (
              match
                ( dom,
                  SFVL.get_first
                    (fun name -> FOSolver.is_equal ~pfs ~gamma name prop)
                    fv_list )
              with
              | None, None ->
                  Error
                    [
                      make_gc_error loc_name prop (SFVL.field_names fv_list)
                        None;
                    ]
              | _, Some (ffn, ffv) -> Ok [ (heap, [ loc; ffn; ffv ], [], []) ]
              | Some dom, None ->
                  let a_set_inclusion : Expr.t =
                    UnOp (Not, BinOp (prop, SetMem, dom))
                  in
                  if
                    FOSolver.check_entailment Containers.SS.empty pfs
                      [ a_set_inclusion ] gamma
                  then (
                    let new_domain : Expr.t =
                      NOp (SetUnion, [ dom; ESet [ prop ] ])
                    in
                    let new_domain =
                      Reduction.reduce_lexpr ?gamma:(Some gamma) ?pfs:(Some pfs)
                        new_domain
                    in
                    let fv_list' = SFVL.add prop (Lit Nono) fv_list in
                    SHeap.set heap loc_name fv_list' (Some new_domain) mtdt;
                    Ok [ (heap, [ loc; prop; Lit Nono ], [], []) ])
                  else
                    let f_names : Expr.t list = SFVL.field_names fv_list in
                    let full_knowledge : Expr.t =
                      BinOp (dom, Equal, ESet f_names)
                    in
                    if
                      FOSolver.check_entailment Containers.SS.empty pfs
                        [ full_knowledge ] gamma
                    then (
                      L.verbose (fun m -> m "GET CELL will branch\n");
                      let rets : (t * vt list * Expr.t list * 'a) option list =
                        List.map
                          (fun (f_name, f_value) ->
                            let new_f : Expr.t = BinOp (f_name, Equal, prop) in
                            let sat =
                              FOSolver.check_satisfiability
                                ~time:"JS getCell branch: heap"
                                (new_f :: PFS.to_list pfs) gamma
                            in
                            match sat with
                            | false -> None
                            | true ->
                                (* Cases in which the prop exists *)
                                let heap' = SHeap.copy heap in
                                Some
                                  ( heap',
                                    [ loc; f_name; f_value ],
                                    [ new_f ],
                                    [] ))
                          (SFVL.to_list fv_list)
                      in

                      let rets =
                        List.map Option.get (List.filter Option.is_some rets)
                      in

                      (* I need the case in which the prop does not exist *)
                      let new_f : Expr.t =
                        UnOp (Not, BinOp (prop, SetMem, dom))
                      in
                      let sat =
                        FOSolver.check_satisfiability
                          ~time:"JS getCell branch: domain"
                          (new_f :: PFS.to_list pfs) gamma
                      in
                      let dom_ret =
                        match sat with
                        | false -> []
                        | true ->
                            [ (heap, [ loc; prop; Lit Nono ], [ new_f ], []) ]
                      in
                      Ok (rets @ dom_ret))
                    else
                      Error
                        [
                          make_gc_error loc_name prop (SFVL.field_names fv_list)
                            (Some dom);
                        ]))
        ~none:(Error [ ([], [ [ FLoc loc; FCell (loc, prop) ] ], Expr.false_) ])
        (SHeap.get heap loc_name)
    in

    let result =
      Option.fold ~some:get_cell_from_loc
        ~none:(Error [ ([], [ [ FLoc loc; FCell (loc, prop) ] ], Expr.false_) ])
        loc_name
    in
    result

  let remove_cell
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (prop : vt) : action_ret =
    let heap = SHeap.copy heap in
    let f (loc_name : string) : unit =
      Option.fold
        ~some:(fun ((fv_list, dom), mtdt) ->
          SHeap.set heap loc_name (SFVL.remove prop fv_list) dom mtdt;
          ())
        ~none:() (SHeap.get heap loc_name)
    in
    Option.fold ~some:f ~none:() (get_loc_name pfs gamma loc);
    Ok [ (heap, [], [], []) ]

  let set_domain
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (dom : vt) : action_ret =
    let loc_name, _, new_pfs = fresh_loc ~loc pfs gamma in

    (match SHeap.get heap loc_name with
    | None -> SHeap.set heap loc_name SFVL.empty (Some dom) None
    | Some ((fv_list, _), mtdt) ->
        (* TODO: This probably needs to be a bit more sophisticated *)
        SHeap.set heap loc_name fv_list (Some dom) mtdt);
    Ok [ (heap, [], new_pfs, []) ]

  let get_metadata (heap : t) (pfs : PFS.t) (gamma : Type_env.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    let make_gm_error (loc_name : string) : err_t =
      let loc = Expr.loc_from_loc_name loc_name in
      ([ loc ], [ [ FMetadata loc ] ], Expr.false_)
    in

    let f loc_name =
      let loc =
        if Names.is_aloc_name loc_name then Expr.ALoc loc_name
        else Expr.Lit (Loc loc_name)
      in
      match SHeap.get heap loc_name with
      | None -> Error [ make_gm_error loc_name ]
      | Some ((_, _), mtdt) ->
          Option.fold
            ~some:(fun mtdt -> Ok [ (heap, [ loc; mtdt ], [], []) ])
            ~none:(Error [ make_gm_error loc_name ])
            mtdt
    in

    Option.fold ~some:f
      ~none:(Error [ ([ loc ], [ [ FLoc loc; FMetadata loc ] ], Expr.false_) ])
      loc_name

  let set_metadata
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (mtdt : vt) : action_ret =
    L.tmi (fun m -> m "Trying to set metadata.");
    let loc_name, _, new_pfs = fresh_loc ~loc pfs gamma in

    (match SHeap.get heap loc_name with
    | None -> SHeap.set heap loc_name SFVL.empty None (Some mtdt)
    | Some ((fv_list, dom), None) ->
        SHeap.set heap loc_name fv_list dom (Some mtdt)
    | Some ((fv_list, dom), Some omet) ->
        if omet <> Option.get (SVal.from_expr (Lit Null)) then
          PFS.extend pfs (BinOp (mtdt, Equal, omet))
        else SHeap.set heap loc_name fv_list dom (Some mtdt));
    L.tmi (fun m -> m "Done setting metadata.");
    Ok [ (heap, [], new_pfs, []) ]

  let delete_object (heap : t) (pfs : PFS.t) (gamma : Type_env.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    match loc_name with
    | Some loc_name ->
        if SHeap.has_loc heap loc_name then (
          SHeap.remove heap loc_name;
          Ok [ (heap, [], [], []) ])
        else raise (Failure "delete_obj. Unknown Location")
    | None -> raise (Failure "delete_obj. Unknown Location")

  let get_partial_domain
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (loc : vt)
      (e_dom : vt) : action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    L.verbose (fun fmt -> fmt "Get partial domain");
    L.verbose (fun fmt -> fmt "Expected domain: %a" SVal.pp e_dom);

    let f loc_name =
      let loc = Expr.loc_from_loc_name loc_name in
      match SHeap.get heap loc_name with
      | None -> raise (Failure "DEATH. get_partial_domain. illegal loc_name")
      | Some ((_, None), _) ->
          raise (Failure "DEATH. get_partial_domain. missing domain")
      | Some ((fv_list, Some dom), mtdt) -> (
          L.verbose (fun fmt -> fmt "Domain: %a" Expr.pp dom);
          let none_fv_list, pos_fv_list =
            SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
          in
          (* Called from the entailment - compute all negative resource associated with
             the location whose name is loc_name *)
          let none_props = SFVL.field_names none_fv_list in
          L.verbose (fun fmt ->
              fmt "None-props in heap: %a"
                Fmt.(brackets (list ~sep:comma Expr.pp))
                none_props);
          let dom' = Expr.BinOp (dom, SetDiff, ESet none_props) in
          let dom'' =
            Reduction.reduce_lexpr ?gamma:(Some gamma) ?pfs:(Some pfs) dom'
          in

          (* Expected dom - dom *)
          let dom_diff = Expr.BinOp (e_dom, SetDiff, dom'') in
          let dom_diff' =
            Reduction.reduce_lexpr ?gamma:(Some gamma) ?pfs:(Some pfs) dom_diff
          in

          (* if dom_diff' != {} then we have to put the excess properties in the heap as nones *)
          match dom_diff' with
          | ESet props ->
              let new_fv_list =
                List.fold_left
                  (fun fv_list prop -> SFVL.add prop (Lit Nono) fv_list)
                  pos_fv_list props
              in
              SHeap.set heap loc_name new_fv_list (Some e_dom) mtdt;
              Ok [ (heap, [ loc; e_dom ], [], []) ]
          | _ -> raise (Failure "DEATH. get_partial_domain. dom_diff"))
    in
    let result =
      Option.fold ~some:f ~none:(Error [ ([ loc ], [], Expr.false_) ]) loc_name
    in
    result

  let get_full_domain (heap : t) (pfs : PFS.t) (gamma : Type_env.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in
    let f loc_name =
      let loc = Expr.loc_from_loc_name loc_name in
      match SHeap.get heap loc_name with
      | None ->
          (* This should never happen *)
          raise (Failure "DEATH. get_full_domain. illegal loc_name")
      | Some ((_, None), _) ->
          (* This is not correct *)
          raise (Failure "DEATH. TODO. get_full_domain. missing domain")
      | Some ((fv_list, Some dom), _) ->
          let props = SFVL.field_names fv_list in
          let a_set_equality : Expr.t = BinOp (dom, Equal, ESet props) in
          let solver_ret =
            FOSolver.check_entailment Containers.SS.empty pfs [ a_set_equality ]
              gamma
          in
          if solver_ret then
            let _, pos_fv_list =
              SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
            in
            Ok [ (heap, [ loc; EList (SFVL.field_names pos_fv_list) ], [], []) ]
          else raise (Failure "DEATH. TODO. get_full_domain. incomplete domain")
    in

    let result =
      Option.fold ~some:f ~none:(Error [ ([ loc ], [], Expr.false_) ]) loc_name
    in
    result

  let remove_domain (heap : t) (pfs : PFS.t) (gamma : Type_env.t) (loc : vt) :
      action_ret =
    let f (loc_name : string) : unit =
      Option.fold
        ~some:(fun ((fv_list, _), mtdt) ->
          SHeap.set heap loc_name fv_list None mtdt;
          ())
        ~none:() (SHeap.get heap loc_name)
    in
    Option.fold ~some:f ~none:() (get_loc_name pfs gamma loc);
    Ok [ (heap, [], [], []) ]

  let execute_action
      ?matching:_
      (action : string)
      (heap : t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (args : vt list) : action_ret =
    if action = JSILNames.getCell then
      match args with
      | [ loc; prop ] -> get_cell heap pfs gamma loc prop
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.setCell then
      match args with
      | [ loc; prop; v ] -> set_cell heap pfs gamma loc prop v
      | _ -> raise (Failure "Internal Error. execute_action. setCell")
    else if action = JSILNames.delCell then
      match args with
      | [ loc; prop ] -> remove_cell heap pfs gamma loc prop
      | _ -> raise (Failure "Internal Error. execute_action. delCell")
    else if action = JSILNames.alloc then
      match args with
      | [ Lit Empty; m_loc ] -> alloc heap pfs None (Some m_loc)
      | [ loc; m_loc ] -> alloc heap pfs (Some loc) (Some m_loc)
      | _ -> raise (Failure "Internal Error. execute_action. alloc")
    else if action = JSILNames.delObj then
      match args with
      | [ loc ] -> delete_object heap pfs gamma loc
      | _ -> raise (Failure "Internal Error. execute_action. delObj")
    else if action = JSILNames.getAllProps then
      match args with
      | [ loc ] -> get_full_domain heap pfs gamma loc
      | _ -> raise (Failure "Internal Error. execute_action. getAllProps")
    else if action = JSILNames.getMetadata then
      match args with
      | [ loc ] -> get_metadata heap pfs gamma loc
      | _ -> raise (Failure "Internal Error. execute_action. getMetadata")
    else if action = JSILNames.setMetadata then
      match args with
      | [ loc; loc_m ] -> set_metadata heap pfs gamma loc loc_m
      | _ -> raise (Failure "Internal Error. execute_action. setMetadata")
    else if action = JSILNames.delMetadata then
      match args with
      | [ _ ] -> Ok [ (heap, [], [], []) ]
      | _ -> raise (Failure "Internal Error. execute_action. delMetadata")
    else if action = JSILNames.getProps then
      match args with
      | [ loc; props ] -> get_partial_domain heap pfs gamma loc props
      | _ -> raise (Failure "Internal Error. execute_action. getProps")
    else if action = JSILNames.setProps then
      match args with
      | [ loc; props ] -> set_domain heap pfs gamma loc props
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.delProps then
      match args with
      | [ loc; _ ] -> remove_domain heap pfs gamma loc
      | _ -> raise (Failure "Internal Error. execute_action. remove_domain")
    else raise (Failure "Internal Error. execute_action")

  let ga_to_setter (a_id : string) : string =
    if a_id = JSILNames.aCell then JSILNames.setCell
    else if a_id = JSILNames.aMetadata then JSILNames.setMetadata
    else if a_id = JSILNames.aProps then JSILNames.setProps
    else raise (Failure "DEATH. ga_to_setter")

  let ga_to_getter (a_id : string) : string =
    if a_id = JSILNames.aCell then JSILNames.getCell
    else if a_id = JSILNames.aMetadata then JSILNames.getMetadata
    else if a_id = JSILNames.aProps then JSILNames.getProps
    else raise (Failure "DEATH. ga_to_setter")

  let ga_to_deleter (a_id : string) : string =
    if a_id = JSILNames.aCell then JSILNames.delCell
    else if a_id = JSILNames.aMetadata then JSILNames.delMetadata
    else if a_id = JSILNames.aProps then JSILNames.delProps
    else raise (Failure "DEATH. ga_to_setter")

  let mem_constraints (state : t) : Expr.t list = SHeap.wf_assertions state

  let is_overlapping_asrt (a : string) : bool =
    if a = JSILNames.aMetadata then true else false

  let prop_abduce_none_in_js = [ "@call" ]
  let prop_abduce_both_in_js = [ "hasOwnProperty" ]

  let complete_fix_js (i_fix : i_fix_t) : Asrt.t list =
    match i_fix with
    | FLoc v ->
        (* Get a fresh location *)
        (* This is dodgy, as the old instantiation does a bit more than this for this fix,
           however it only seemed to add the binding without creating any state, so did it really
           "do" anything? Bi-abduction is broken for Gillian-JS anyways. *)
        let al = ALoc.alloc () in
        [ [ Asrt.Pure (BinOp (ALoc al, Equal, v)) ] ]
    | FCell (l, p) -> (
        let none_fix () =
          [ Asrt.CorePred (JSILNames.aCell, [ l; p ], [ Lit Nono ]) ]
        in

        let some_fix () =
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          let asrt_empty : Expr.t = UnOp (Not, BinOp (v, Equal, Lit Empty)) in
          let asrt_none : Expr.t = UnOp (Not, BinOp (v, Equal, Lit Nono)) in
          let asrt_list : Expr.t =
            UnOp (Not, BinOp (UnOp (TypeOf, v), Equal, Lit (Type ListType)))
          in
          let descriptor : Expr.t =
            EList
              [
                Lit (String "d");
                v;
                Lit (Bool true);
                Lit (Bool true);
                Lit (Bool true);
              ]
          in
          [
            Asrt.CorePred (JSILNames.aCell, [ l; p ], [ descriptor ]);
            Asrt.Pure asrt_empty;
            Asrt.Pure asrt_none;
            Asrt.Pure asrt_list;
          ]
        in

        match p with
        | Lit (String x) when List.mem x prop_abduce_none_in_js ->
            [ none_fix () ]
        | Lit (String x) when List.mem x prop_abduce_both_in_js ->
            [ none_fix (); some_fix () ]
        | _ -> [ some_fix () ])
    | FMetadata l ->
        let al = ALoc.alloc () in
        let mloc = Expr.ALoc al in
        [
          [
            Asrt.Pure (BinOp (ALoc al, Equal, l));
            Asrt.CorePred (JSILNames.aMetadata, [ l ], [ mloc ]);
            Asrt.CorePred (JSILNames.aMetadata, [ mloc ], [ Lit Null ]);
            Asrt.CorePred
              ( JSILNames.aCell,
                [ mloc; Lit (String "@class") ],
                [ Lit (String "Object") ] );
            Asrt.CorePred
              ( JSILNames.aCell,
                [ mloc; Lit (String "@extensible") ],
                [ Lit (Bool true) ] );
            Asrt.CorePred
              ( JSILNames.aCell,
                [ mloc; Lit (String "@proto") ],
                [ Lit (Loc JS2JSIL_Helpers.locObjPrototype) ] );
          ];
        ]
    | FPure f -> [ [ Asrt.Pure f ] ]

  (* Fix completion: simple *)
  let complete_fix_jsil (i_fix : i_fix_t) : Asrt.t list =
    match i_fix with
    | FLoc v ->
        (* Get a fresh location *)
        let al = ALoc.alloc () in
        [ [ Asrt.Pure (BinOp (ALoc al, Equal, v)) ] ]
    | FCell (l, p) ->
        (* Fresh variable to denote the property value *)
        let vvar = LVar.alloc () in
        let v : vt = LVar vvar in
        (* Value is not none - we always bi-abduce presence *)
        let not_none : Expr.t = UnOp (Not, BinOp (v, Equal, Lit Nono)) in
        [
          [
            Asrt.CorePred (JSILNames.aCell, [ l; p ], [ v ]); Asrt.Pure not_none;
          ];
        ]
    | FMetadata l ->
        (* Fresh variable to denote the property value *)
        let vvar = LVar.alloc () in
        let v : vt = LVar vvar in
        let not_none : Expr.t = UnOp (Not, BinOp (v, Equal, Lit Nono)) in
        [
          [
            Asrt.CorePred (JSILNames.aMetadata, [ l ], [ v ]);
            Asrt.Pure not_none;
          ];
        ]
    | FPure f -> [ [ Asrt.Pure f ] ]

  (* An error can have multiple fixes *)
  let get_fixes (err : err_t) : Asrt.t list =
    let pp_fix ft res =
      let open Fmt in
      pf ft "@[<v 2>@[<h>[[ %a ]]@]@\n@]" Asrt.pp res
    in
    let _, fixes, _ = err in
    L.verbose (fun m ->
        m "@[<v 2>Memory: Fixes found:@\n%a@]"
          Fmt.(
            list ~sep:(any "@\n")
              (brackets (brackets (hbox (list ~sep:comma pp_i_fix)))))
          fixes);

    let complete =
      if !Js_config.js then complete_fix_js else complete_fix_jsil
    in

    let complete_ifixes (ifixes : i_fix_t list) : Asrt.t list =
      let completed_ifixes = List.map complete ifixes in
      let completed_ifixes = List_utils.list_product completed_ifixes in
      let completed_ifixes : Asrt.t list =
        List.map
          (fun fixes -> List.fold_right List.append fixes [])
          completed_ifixes
      in

      L.verbose (fun m ->
          m "@[<v 2>Memory: i-fixes completed: %d@\n%a"
            (List.length completed_ifixes)
            Fmt.(list ~sep:(any "@\n") pp_fix)
            completed_ifixes);

      completed_ifixes
    in

    (* Fixes hold lists of lists of i_fixes, *)
    List.concat_map complete_ifixes fixes

  let can_fix _ = true

  let sorted_locs_with_vals (smemory : t) =
    let sorted_locs = Containers.SS.elements (SHeap.domain smemory) in
    List.map (fun loc -> (loc, Option.get (SHeap.get smemory loc))) sorted_locs
end
