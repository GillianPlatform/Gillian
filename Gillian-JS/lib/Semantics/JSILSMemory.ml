open Gillian
open Gillian.Gil_syntax
open Javert_utils
open Js2jsil_lib
module GAsrt = Asrt
module SSubst = Gillian.Symbolic.Subst
module L = Logging
module SVal = Gillian.Symbolic.Values
module PFS = Gillian.Symbolic.PureContext
module TypEnv = Gillian.Symbolic.TypEnv
open Gillian.Logic

module M : Gillian.Symbolic.Memory_S = struct
  type vt = SVal.t

  (** Type of JSIL general states *)
  type t = SHeap.t

  (** Type of JSIL substitutions *)
  type st = SSubst.t

  (** Errors *)
  type i_fix_t =
    | FLoc      of vt
    | FCell     of vt * vt
    | FMetadata of vt
    | FDomain   of vt * vt
    | FPure     of Formula.t

  type c_fix_t =
    | CFLoc      of string
    | CFCell     of vt * vt * vt
    | CFMetadata of vt * vt
    | CFDomain   of vt * vt

  type err_t = vt list * i_fix_t list list * Formula.t

  type action_ret =
    | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
    | AFail of err_t list

  let pp_i_fix ft (i_fix : i_fix_t) : unit =
    let open Fmt in
    match i_fix with
    | FLoc loc          -> pf ft "@[<h>MIFLoc(%a)@]" SVal.pp loc
    | FCell (loc, prop) ->
        pf ft "@[<h>MIFCell(%a, %a)@]" SVal.pp loc SVal.pp prop
    | FMetadata loc     -> pf ft "@[<h>MIFMetadata(%a)@]" SVal.pp loc
    | FDomain (loc, v)  -> pf ft "@[<h>MIFDomain(%a, %a)@]" SVal.pp loc SVal.pp v
    | FPure f           -> pf ft "@[<h>MIFPure(%a)@]" Formula.pp f

  let pp_c_fix ft (c_fix : c_fix_t) : unit =
    let open Fmt in
    match c_fix with
    | CFLoc loc_name        -> pf ft "@[<h>MCFLoc(%s)@]" loc_name
    | CFCell (loc, prop, v) ->
        pf ft "@[<h>MCFCell(%a, %a, %a)@]" SVal.pp loc SVal.pp prop SVal.pp v
    | CFMetadata (loc, v)   ->
        pf ft "@[<h>MCFMetadata(%a, %a)@]" SVal.pp loc SVal.pp v
    | CFDomain (loc, v)     ->
        pf ft "@[<h>MCFDomain(%a, %a)@]" SVal.pp loc SVal.pp v

  let get_failing_constraint (err : err_t) : Formula.t =
    let _, _, f = err in
    f

  let pp_err ft (err : err_t) : unit =
    let open Fmt in
    let vs, fixes, f = err in
    let pp_fixes ft fix = pf ft "[%a]" (list ~sep:comma pp_i_fix) fix in
    pf ft "@[<h><[%a], %a, %a>@]" (list ~sep:comma SVal.pp) vs Formula.pp f
      (list ~sep:semi pp_fixes) fixes

  let get_recovery_vals (err : err_t) : vt list =
    let ufl_vs, _, _ = err in
    ufl_vs

  let assertions ?(to_keep : Containers.SS.t option) (heap : t) : GAsrt.t list =
    List.map JSIL2GIL.jsil2gil_asrt (SHeap.assertions heap)

  let lvars (heap : t) : Containers.SS.t = SHeap.lvars heap

  let clean_up (heap : t) : unit = SHeap.clean_up heap

  let fresh_val (heap : t) : vt = LVar (LVar.alloc ())

  let substitution_in_place (subst : st) (heap : t) : unit =
    SHeap.substitution_in_place subst heap

  let pp fmt (heap : t) : unit = SHeap.pp fmt heap

  let copy (heap : t) : t = SHeap.copy heap

  let init () : t = SHeap.init ()

  let get_loc_name (pfs : PFS.t) (gamma : TypEnv.t) (loc : Expr.t) :
      string option =
    L.(tmi (fun m -> m "get_loc_name: %s" ((Fmt.to_to_string Expr.pp) loc)));
    let lpfs = PFS.to_list pfs in
    match Reduction.reduce_lexpr ~pfs ~gamma loc with
    | Lit (Loc loc) | ALoc loc -> Some loc
    | LVar x                   -> (
        match Reduction.resolve_expr_to_location lpfs (LVar x) with
        | Some (loc_name, _) -> Some loc_name
        | _                  -> None )
    | loc'                     -> (
        match Reduction.resolve_expr_to_location lpfs loc' with
        | Some (loc_name, _) -> Some loc_name
        | None               ->
            let msg =
              Fmt.str
                "@[<v 2>JSILSMemory: Unsupported location: %a with pfs:@\n%a@]"
                Expr.pp loc' PFS.pp pfs
            in
            L.verboser (fun m -> m "%s" msg);
            raise (Failure msg) )

  let fresh_loc ?(loc : vt option) (pfs : PFS.t) (gamma : TypEnv.t) :
      string * vt * Formula.t list =
    match loc with
    | Some loc -> (
        let loc_name = get_loc_name pfs gamma loc in
        match loc_name with
        | Some loc_name ->
            if Names.is_aloc_name loc_name then
              (loc_name, Expr.ALoc loc_name, [])
            else (loc_name, Expr.Lit (Loc loc_name), [])
        | None          ->
            let al = ALoc.alloc () in
            (al, ALoc al, [ Formula.Eq (ALoc al, loc) ]) )
    | None     ->
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
      | None                 ->
          let loc_name = ALoc.alloc () in
          (loc_name, ALoc loc_name)
      | Some (Lit (Loc loc)) -> (loc, Lit (Loc loc))
      | Some (ALoc loc)      -> (loc, ALoc loc)
      | Some (LVar v)        ->
          let loc_name = ALoc.alloc () in
          PFS.extend pfs (Eq (LVar v, ALoc loc_name));
          (loc_name, ALoc loc_name)
      | Some le              ->
          raise
            (Failure
               (Printf.sprintf "Alloc with a non-loc loc argument: %s"
                  ((Fmt.to_to_string Expr.pp) le)))
    in
    SHeap.init_object heap loc_name ~is_empty:ie mv;
    ASucc [ (heap, [ loc ], [], []) ]

  let set_cell
      (heap : t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (loc : vt)
      (prop : vt)
      (v : vt option) : action_ret =
    let loc_name, _, new_pfs = fresh_loc ~loc pfs gamma in
    ( match v with
    | None   -> SHeap.set_fv_pair heap loc_name prop (Lit Nono)
    | Some v -> SHeap.set_fv_pair heap loc_name prop v );
    ASucc [ (heap, [], new_pfs, []) ]

  let get_cell
      (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) (prop : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    L.tmi (fun m ->
        m "@[<h>GetCell: resolved location: %a -> %a@]" SVal.pp loc
          Fmt.(option ~none:(any "None") string)
          loc_name);

    let make_gc_error
        (loc_name : string) (prop : vt) (props : vt list) (dom : vt option) :
        err_t =
      let loc = Expr.loc_from_loc_name loc_name in
      (*  failing_constraint *)
      let ff =
        Formula.conjunct
          (List.map (fun prop' -> Formula.Not (Eq (prop, prop'))) props)
      in

      let fixes_exist_props : i_fix_t list list =
        List.map (fun prop' -> [ FPure (Formula.Eq (prop, prop')) ]) props
      in
      let fix_new_property : i_fix_t list = [ FCell (loc, prop); FPure ff ] in

      match dom with
      | Some dom ->
          let ff' : Formula.t = SetMem (prop, dom) in
          let ff'' : Formula.t = And (ff, ff') in
          let fix_new_property' : i_fix_t list =
            FPure ff' :: fix_new_property
          in
          ([ loc ], fix_new_property' :: fixes_exist_props, ff'')
      | None     -> ([ loc; prop ], fix_new_property :: fixes_exist_props, ff)
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
          | Some ffv -> ASucc [ (heap, [ loc; prop; ffv ], [], []) ]
          | None     -> (
              match
                ( dom,
                  SFVL.get_first
                    (fun name -> FOSolver.is_equal name prop pfs gamma)
                    fv_list )
              with
              | None, None         ->
                  AFail
                    [
                      make_gc_error loc_name prop (SFVL.field_names fv_list)
                        None;
                    ]
              | _, Some (ffn, ffv) ->
                  ASucc [ (heap, [ loc; ffn; ffv ], [], []) ]
              | Some dom, None     ->
                  let a_set_inclusion : Formula.t = Not (SetMem (prop, dom)) in
                  if
                    FOSolver.check_entailment Containers.SS.empty
                      (PFS.to_list pfs) [ a_set_inclusion ] gamma
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
                    ASucc [ (heap, [ loc; prop; Lit Nono ], [], []) ] )
                  else
                    let f_names : Expr.t list = SFVL.field_names fv_list in
                    let full_knowledge : Formula.t = Eq (dom, ESet f_names) in
                    if
                      FOSolver.check_entailment Containers.SS.empty
                        (PFS.to_list pfs) [ full_knowledge ] gamma
                    then (
                      L.verboser (fun m -> m "GET CELL will branch\n");
                      let rets : (t * vt list * Formula.t list * 'a) option list
                          =
                        List.map
                          (fun (f_name, f_value) ->
                            let new_f : Formula.t = Eq (f_name, prop) in
                            let sat =
                              FOSolver.check_satisfiability
                                (new_f :: PFS.to_list pfs) gamma
                            in
                            match sat with
                            | false -> None
                            | true  ->
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
                      let new_f : Formula.t = Not (SetMem (prop, dom)) in
                      let sat =
                        FOSolver.check_satisfiability (new_f :: PFS.to_list pfs)
                          gamma
                      in
                      let dom_ret =
                        match sat with
                        | false -> []
                        | true  ->
                            [ (heap, [ loc; prop; Lit Nono ], [ new_f ], []) ]
                      in
                      ASucc (rets @ dom_ret) )
                    else
                      AFail
                        [
                          make_gc_error loc_name prop (SFVL.field_names fv_list)
                            (Some dom);
                        ] ))
        ~none:(AFail [ ([], [ [ FLoc loc; FCell (loc, prop) ] ], False) ])
        (SHeap.get heap loc_name)
    in

    let result =
      Option.fold ~some:get_cell_from_loc
        ~none:(AFail [ ([], [ [ FLoc loc; FCell (loc, prop) ] ], False) ])
        loc_name
    in
    result

  let remove_cell
      (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) (prop : vt) :
      action_ret =
    let heap = SHeap.copy heap in
    let f (loc_name : string) : unit =
      Option.fold
        ~some:(fun ((fv_list, dom), mtdt) ->
          SHeap.set heap loc_name (SFVL.remove prop fv_list) dom mtdt;
          ())
        ~none:() (SHeap.get heap loc_name)
    in
    Option.fold ~some:f ~none:() (get_loc_name pfs gamma loc);
    ASucc [ (heap, [], [], []) ]

  let get_full_domain (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in
    L.verboser (fun m -> m "%a" SHeap.pp heap);

    let make_gd_error (loc_name : string) (props : vt list) (dom : vt option) :
        err_t =
      ([], [], False)
    in

    let f loc_name =
      let loc = Expr.loc_from_loc_name loc_name in
      match SHeap.get heap loc_name with
      | None -> AFail [ ([ loc ], [], False) ]
      | Some ((fv_list, None), _) ->
          AFail [ make_gd_error loc_name (SFVL.field_names fv_list) None ]
      | Some ((fv_list, Some dom), mtdt) ->
          let none_fv_list, pos_fv_list =
            SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
          in
          (* Called from the semantics -> we need full knowledge *)
          let props = SFVL.field_names fv_list in
          let a_set_equality : Formula.t = Eq (dom, ESet props) in
          let solver_ret =
            FOSolver.check_entailment Containers.SS.empty (PFS.to_list pfs)
              [ a_set_equality ] gamma
          in
          if solver_ret then
            ASucc
              [ (heap, [ loc; EList (SFVL.field_names pos_fv_list) ], [], []) ]
          else AFail [ make_gd_error loc_name props (Some dom) ]
    in

    let result =
      Option.fold ~some:f ~none:(AFail [ ([ loc ], [], False) ]) loc_name
    in
    result

  let set_domain
      (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) (dom : vt) :
      action_ret =
    let loc_name, loc', new_pfs = fresh_loc ~loc pfs gamma in

    ( match SHeap.get heap loc_name with
    | None                      -> SHeap.set heap loc_name SFVL.empty (Some dom)
                                     None
    | Some ((fv_list, _), mtdt) ->
        (* TODO: This probably needs to be a bit more sophisticated *)
        SHeap.set heap loc_name fv_list (Some dom) mtdt );
    ASucc [ (heap, [], new_pfs, []) ]

  let get_metadata (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    let make_gm_error (loc_name : string) : err_t =
      let loc = Expr.loc_from_loc_name loc_name in
      ([ loc ], [ [ FMetadata loc ] ], False)
    in

    let f loc_name =
      let loc =
        if Names.is_aloc_name loc_name then Expr.ALoc loc_name
        else Expr.Lit (Loc loc_name)
      in
      match SHeap.get heap loc_name with
      | None -> AFail [ make_gm_error loc_name ]
      | Some ((fv_list, dom), mtdt) ->
          Option.fold
            ~some:(fun mtdt -> ASucc [ (heap, [ loc; mtdt ], [], []) ])
            ~none:(AFail [ make_gm_error loc_name ])
            mtdt
    in

    Option.fold ~some:f
      ~none:(AFail [ ([ loc ], [ [ FLoc loc; FMetadata loc ] ], False) ])
      loc_name

  let set_metadata
      (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) (mtdt : vt) :
      action_ret =
    L.tmi (fun m -> m "Trying to set metadata.");
    let loc_name, loc', new_pfs = fresh_loc ~loc pfs gamma in

    ( match SHeap.get heap loc_name with
    | None -> SHeap.set heap loc_name SFVL.empty None (Some mtdt)
    | Some ((fv_list, dom), None) ->
        SHeap.set heap loc_name fv_list dom (Some mtdt)
    | Some ((fv_list, dom), Some omet) ->
        if omet <> Option.get (SVal.from_expr (Lit Null)) then
          PFS.extend pfs (Eq (mtdt, omet))
        else SHeap.set heap loc_name fv_list dom (Some mtdt) );
    L.tmi (fun m -> m "Done setting metadata.");
    ASucc [ (heap, [], new_pfs, []) ]

  let remove_metadata (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let f (loc_name : string) : unit =
      Option.fold
        ~some:(fun ((fv_list, dom), mtdt) ->
          SHeap.set heap loc_name fv_list dom None;
          ())
        ~none:() (SHeap.get heap loc_name)
    in
    Option.fold ~some:f ~none:() (get_loc_name pfs gamma loc);
    ASucc [ (heap, [], [], []) ]

  let delete_object (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    match loc_name with
    | Some loc_name ->
        if SHeap.has_loc heap loc_name then (
          SHeap.remove heap loc_name;
          ASucc [ (heap, [], [], []) ] )
        else raise (Failure "delete_obj. Unknown Location")
    | None          -> raise (Failure "delete_obj. Unknown Location")

  let get_partial_domain
      (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) (e_dom : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in

    let f loc_name =
      let loc = Expr.loc_from_loc_name loc_name in
      match SHeap.get heap loc_name with
      | None -> raise (Failure "DEATH. get_partial_domain. illegal loc_name")
      | Some ((fv_list, None), _) ->
          raise (Failure "DEATH. get_partial_domain. missing domain")
      | Some ((fv_list, Some dom), mtdt) -> (
          let none_fv_list, pos_fv_list =
            SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
          in
          (* Called from the entailment - compute all negative resource associated with
             the location whose name is loc_name *)
          let none_props = SFVL.field_names none_fv_list in
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
              ASucc [ (heap, [ loc; e_dom ], [], []) ]
          | _          -> raise (Failure "DEATH. get_partial_domain. dom_diff")
          )
    in
    let result =
      Option.fold ~some:f ~none:(AFail [ ([ loc ], [], False) ]) loc_name
    in
    result

  let get_full_domain (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let loc_name = get_loc_name pfs gamma loc in
    let f loc_name =
      let loc = Expr.loc_from_loc_name loc_name in
      match SHeap.get heap loc_name with
      | None ->
          (* This should never happen *)
          raise (Failure "DEATH. get_full_domain. illegal loc_name")
      | Some ((fv_list, None), _) ->
          (* This is not correct *)
          raise (Failure "DEATH. TODO. get_full_domain. missing domain")
      | Some ((fv_list, Some dom), mtdt) ->
          let props = SFVL.field_names fv_list in
          let a_set_equality : Formula.t = Eq (dom, ESet props) in
          let solver_ret =
            FOSolver.check_entailment Containers.SS.empty (PFS.to_list pfs)
              [ a_set_equality ] gamma
          in
          if solver_ret then
            let _, pos_fv_list =
              SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
            in
            ASucc
              [ (heap, [ loc; EList (SFVL.field_names pos_fv_list) ], [], []) ]
          else raise (Failure "DEATH. TODO. get_full_domain. incomplete domain")
    in

    let result =
      Option.fold ~some:f ~none:(AFail [ ([ loc ], [], False) ]) loc_name
    in
    result

  let remove_domain (heap : t) (pfs : PFS.t) (gamma : TypEnv.t) (loc : vt) :
      action_ret =
    let f (loc_name : string) : unit =
      Option.fold
        ~some:(fun ((fv_list, dom), mtdt) ->
          SHeap.set heap loc_name fv_list dom None;
          ())
        ~none:() (SHeap.get heap loc_name)
    in
    Option.fold ~some:f ~none:() (get_loc_name pfs gamma loc);
    ASucc [ (heap, [], [], []) ]

  let execute_action
      (action : string)
      (heap : t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (args : vt list) : action_ret =
    if action = JSILNames.getCell then
      match args with
      | [ loc; prop ] -> get_cell heap pfs gamma loc prop
      | _             -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.setCell then
      match args with
      | [ loc; prop; v ] -> set_cell heap pfs gamma loc prop (Some v)
      | _                -> raise
                              (Failure "Internal Error. execute_action. setCell")
    else if action = JSILNames.delCell then
      match args with
      | [ loc; prop ] -> remove_cell heap pfs gamma loc prop
      | _             -> raise
                           (Failure "Internal Error. execute_action. delCell")
    else if action = JSILNames.alloc then
      match args with
      | [ Lit Empty; m_loc ] -> alloc heap pfs None (Some m_loc)
      | [ loc; m_loc ]       -> alloc heap pfs (Some loc) (Some m_loc)
      | _                    -> raise
                                  (Failure
                                     "Internal Error. execute_action. alloc")
    else if action = JSILNames.delObj then
      match args with
      | [ loc ] -> delete_object heap pfs gamma loc
      | _       -> raise (Failure "Internal Error. execute_action. delObj")
    else if action = JSILNames.getAllProps then
      match args with
      | [ loc ] -> get_full_domain heap pfs gamma loc
      | _       -> raise (Failure "Internal Error. execute_action. getAllProps")
    else if action = JSILNames.getMetadata then
      match args with
      | [ loc ] -> get_metadata heap pfs gamma loc
      | _       -> raise (Failure "Internal Error. execute_action. getMetadata")
    else if action = JSILNames.setMetadata then
      match args with
      | [ loc; loc_m ] -> set_metadata heap pfs gamma loc loc_m
      | _              -> raise
                            (Failure
                               "Internal Error. execute_action. setMetadata")
    else if action = JSILNames.delMetadata then
      match args with
      | [ loc ] -> ASucc [ (heap, [], [], []) ]
      | _       -> raise (Failure "Internal Error. execute_action. delMetadata")
    else if action = JSILNames.getProps then
      match args with
      | [ loc; props ] -> get_partial_domain heap pfs gamma loc props
      | _              -> raise
                            (Failure "Internal Error. execute_action. getProps")
    else if action = JSILNames.setProps then
      match args with
      | [ loc; props ] -> set_domain heap pfs gamma loc props
      | _              -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.delProps then
      match args with
      | [ loc; _ ] -> remove_domain heap pfs gamma loc
      | _          -> raise
                        (Failure "Internal Error. execute_action. remove_domain")
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

  let ga_loc_indexes (a_id : string) : int list =
    if a_id = JSILNames.aCell then [ 0 ]
    else if a_id = JSILNames.aMetadata then [ 0 ]
    else if a_id = JSILNames.aProps then [ 0 ]
    else raise (Failure "DEATH. ga_to_setter")

  let mem_constraints (state : t) : Formula.t list = SHeap.wf_assertions state

  let is_overlapping_asrt (a : string) : bool =
    if a = JSILNames.aMetadata then true else false

  let prop_abduce_none_in_js = [ "@call" ]

  let prop_abduce_both_in_js = [ "hasOwnProperty" ]

  type fix_result =
    c_fix_t list * Formula.t list * Containers.SS.t * GAsrt.t list

  let complete_fix_simple_js (pfs : PFS.t) (gamma : TypEnv.t) (i_fix : i_fix_t)
      : fix_result list =
    match i_fix with
    | FLoc v       ->
        (* Get a fresh location *)
        let loc_name, loc', new_pfs = fresh_loc ~loc:v pfs gamma in
        (* TODO: If the initial value denoting the location was a variable, we may need to save it as a spec var *)
        [ ([ CFLoc loc_name ], new_pfs, Containers.SS.empty, []) ]
    | FCell (l, p) -> (
        let none_fix () =
          ([ CFCell (l, p, Lit Nono) ], [], Containers.SS.empty, [])
        in

        let some_fix () =
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          let asrt_empty : Formula.t = Not (Eq (v, Lit Empty)) in
          let asrt_none : Formula.t = Not (Eq (v, Lit Nono)) in
          let asrt_list : Formula.t =
            Not (Eq (UnOp (TypeOf, v), Lit (Type ListType)))
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
          ( [ CFCell (l, p, descriptor) ],
            [ asrt_empty; asrt_none; asrt_list ],
            Containers.SS.singleton vvar,
            [] )
        in

        match p with
        | Lit (String x) when List.mem x prop_abduce_none_in_js ->
            [ none_fix () ]
        | Lit (String x) when List.mem x prop_abduce_both_in_js ->
            [ none_fix (); some_fix () ]
        | _ -> [ some_fix () ] )
    | FMetadata l  ->
        let mloc_name, mloc, _ = fresh_loc pfs gamma in
        [
          ( [
              CFLoc mloc_name;
              CFMetadata (l, mloc);
              CFMetadata (mloc, Lit Null);
              CFCell (mloc, Lit (String "@class"), Lit (String "Object"));
              CFCell (mloc, Lit (String "@extensible"), Lit (Bool true));
              CFCell
                ( mloc,
                  Lit (String "@proto"),
                  Lit (Loc JS2JSIL_Helpers.locObjPrototype) );
            ],
            [],
            Containers.SS.empty,
            [] );
        ]
    | FDomain _    ->
        raise (Failure "Complete fix: domain fix currently unsupported ")
    | FPure f      -> [ ([], [ f ], Containers.SS.empty, []) ]

  (* Fix completion: simple *)
  let complete_fix_simple_jsil
      (pfs : PFS.t) (gamma : TypEnv.t) (i_fix : i_fix_t) : fix_result list =
    if !Js_config.js then complete_fix_simple_js pfs gamma i_fix
    else
      match i_fix with
      | FLoc v       ->
          (* Get a fresh location *)
          let loc_name, loc', new_pfs = fresh_loc ~loc:v pfs gamma in
          (* TODO: If the initial value denoting the location was a variable, we may need to save it as a spec var *)
          [ ([ CFLoc loc_name ], new_pfs, Containers.SS.empty, []) ]
      | FCell (l, p) ->
          (* Fresh variable to denote the property value *)
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          (* Value is not none - we always bi-abduce presence *)
          let not_none : Formula.t = Not (Eq (v, Lit Nono)) in
          [
            ( [ CFCell (l, p, v) ],
              [ not_none ],
              Containers.SS.singleton vvar,
              [] );
          ]
      | FMetadata l  ->
          (* Fresh variable to denote the property value *)
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          let not_none : Formula.t = Not (Eq (v, Lit Nono)) in
          [
            ( [ CFMetadata (l, v) ],
              [ not_none ],
              Containers.SS.singleton vvar,
              [] );
          ]
      | FDomain _    ->
          raise (Failure "Complete fix: domain fix currently unsupported ")
      | FPure f      -> [ ([], [ f ], Containers.SS.empty, []) ]

  let complete_fix_full_js (pfs : PFS.t) (gamma : TypEnv.t) (i_fix : i_fix_t) :
      fix_result list =
    match i_fix with
    | _ -> complete_fix_simple_js pfs gamma i_fix

  let complete_fix_full_jsil (pfs : PFS.t) (gamma : TypEnv.t) (i_fix : i_fix_t)
      : fix_result list =
    if !Js_config.js then complete_fix_full_js pfs gamma i_fix
    else
      match i_fix with
      | FLoc v       ->
          (* Get a fresh location *)
          let loc_name, loc', new_pfs = fresh_loc ~loc:v pfs gamma in
          (* TODO: If the initial value denoting the location was a variable, we may need to save it as a spec var *)
          [ ([ CFLoc loc_name ], new_pfs, Containers.SS.empty, []) ]
      | FCell (l, p) ->
          (* Fresh variable to denote the property value *)
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          (* Value is not none - we always bi-abduce presence *)
          let not_none : Formula.t = Not (Eq (v, Lit Nono)) in
          [
            ( [ CFCell (l, p, v) ],
              [ not_none ],
              Containers.SS.singleton vvar,
              [] );
          ]
      | FMetadata l  ->
          (* Fresh variable to denote the property value *)
          let vvar = LVar.alloc () in
          let v : vt = LVar vvar in
          let not_none : Formula.t = Not (Eq (v, Lit Nono)) in
          [
            ( [ CFMetadata (l, v) ],
              [ not_none ],
              Containers.SS.singleton vvar,
              [] );
          ]
      | FDomain _    ->
          raise (Failure "Complete fix: domain fix currently unsupported ")
      | FPure f      -> [ ([], [ f ], Containers.SS.empty, []) ]

  (* An error can have multiple fixes *)
  let get_fixes
      ?simple_fix:(sf = true)
      (mem : t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (err : err_t) : fix_result list =
    let pp_fix_result ft res =
      let open Fmt in
      let fixes, pfs, svars, gasrts = res in
      pf ft
        "@[<v 2>@[<h>[[ %a ]]@]@\n\
         @[<h>with PFS:%a@]@\n\
         @[<h>spec vars: %a@]@\n\
         @[<h>predicates: %a@]@]" (list ~sep:comma pp_c_fix) fixes
        (list ~sep:comma Formula.pp)
        pfs
        (iter ~sep:comma Containers.SS.iter string)
        svars (list ~sep:comma GAsrt.pp) gasrts
    in
    let _, fixes, _ = err in
    L.verboser (fun m ->
        m "@[<v 2>Memory: Fixes found:@\n%a@]"
          Fmt.(
            list ~sep:(any "@\n")
              (brackets (brackets (hbox (list ~sep:comma pp_i_fix)))))
          fixes);

    let complete =
      if sf then complete_fix_simple_jsil else complete_fix_full_jsil
    in

    let complete_ifixes (ifixes : i_fix_t list) : fix_result list =
      let completed_ifixes = List.map (complete pfs gamma) ifixes in
      let completed_ifixes = List_utils.list_product completed_ifixes in
      let completed_ifixes : fix_result list =
        List.map
          (fun fixes ->
            List.fold_right
              (fun (mfix, pfs, svars, gasrts) (mfix', pfs', svars', gasrts') ->
                ( mfix @ mfix',
                  pfs @ pfs',
                  Containers.SS.union svars svars',
                  gasrts @ gasrts' ))
              fixes
              ([], [], Containers.SS.empty, []))
          completed_ifixes
      in

      L.verboser (fun m ->
          m "@[<v 2>Memory: i-fixes completed: %d@\n%a"
            (List.length completed_ifixes)
            Fmt.(list ~sep:(any "@\n") pp_fix_result)
            completed_ifixes);

      completed_ifixes
    in

    (* Fixes hold lists of lists of i_fixes, *)
    let completed_fixes = List.concat (List.map complete_ifixes fixes) in

    completed_fixes

  let apply_fix (mem : t) (pfs : PFS.t) (gamma : TypEnv.t) (fix : c_fix_t) : t =
    match fix with
    (* Missing metadata: create new, no new variables *)
    | CFMetadata (l, v) -> (
        match set_metadata mem pfs gamma l v with
        | ASucc [ (mem, [], new_pfs, []) ] ->
            List.iter (fun f -> PFS.extend pfs f) new_pfs;
            mem
        | _ -> raise (Failure "Bi-abduction: cannot fix metadata.") )
    (* Missing location: create new *)
    | CFLoc loc_name -> (
        L.verboser (fun m -> m "CFLoc: %s" loc_name);
        let loc : vt =
          if Names.is_aloc_name loc_name then ALoc loc_name
          else Lit (Loc loc_name)
        in
        match alloc mem pfs (Some loc) ~is_empty:true None with
        | ASucc [ (mem, [ loc' ], [], []) ] when loc' = loc -> mem
        | _ -> raise (Failure "Bi-abduction: cannot fix missing location.") )
    (* Missing cell: create new *)
    | CFCell (l, p, v) -> (
        match set_cell mem pfs gamma l p (Some v) with
        | ASucc [ (mem, [], new_pfs, []) ] ->
            List.iter (fun f -> PFS.extend pfs f) new_pfs;
            mem
        | _ -> raise (Failure "Bi-abduction: cannot fix cell.") )
    | CFDomain _ ->
        raise (Failure "Bi-abduction: domain fix currently unsupported")
end
