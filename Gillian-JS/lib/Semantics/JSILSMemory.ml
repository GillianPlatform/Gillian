open Gillian
open Gillian.Gil_syntax
open Gillian.Monadic
open Delayed.Syntax
open Delayed_result.Syntax
open Javert_utils
open Js2jsil_lib
module GAsrt = Asrt
module SSubst = Gillian.Symbolic.Subst
module L = Logging
module SVal = Gillian.Symbolic.Values
module Recovery_tactic = Gillian.General.Recovery_tactic
module DR = Delayed_result

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
  type action_ret = (t * vt list, err_t) result

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
                match Expr.Map.find_opt aloc imeta with
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

  let clean_up ?(keep = Expr.Set.empty) (_ : t) : Expr.Set.t * Expr.Set.t =
    (Expr.Set.empty, keep)

  let substitution_in_place (subst : st) (heap : t) : t Delayed.t =
    Delayed.return (SHeap.substitution subst heap)

  let pp fmt (heap : t) : unit = SHeap.pp fmt heap
  let pp_by_need locs fmt heap = SHeap.pp_by_need locs fmt heap
  let get_print_info = SHeap.get_print_info
  let copy (heap : t) : t = heap
  let init () : t = SHeap.init ()
  let get_init_data _ = ()
  let clear (_ : t) = init () (* We don't maintain any context *)

  (** Resolves the location to its name, or creates a new abstract location
      equal to it if it cannot be resolved. *)
  let fresh_loc ?(loc : vt option) () : (string * vt) Delayed.t =
    match loc with
    | Some loc -> (
        let* loc_name = Delayed.resolve_loc loc in
        match loc_name with
        | Some loc_name ->
            Delayed.return (loc_name, Expr.loc_from_loc_name loc_name)
        | None ->
            let al = ALoc.alloc () in
            Delayed.return
              ~learned:[ Expr.BinOp (ALoc al, Equal, loc) ]
              (al, Expr.ALoc al))
    | None ->
        let al = ALoc.alloc () in
        Delayed.return (al, Expr.ALoc al)

  let alloc (heap : t) (loc : vt option) ?is_empty:(ie = false) (mv : vt option)
      : action_ret Delayed.t =
    let* loc_name, loc =
      match (loc : Expr.t option) with
      | None ->
          let loc_name = ALoc.alloc () in
          Delayed.return (loc_name, Expr.ALoc loc_name)
      | Some (Lit (Loc loc)) -> Delayed.return (loc, Expr.Lit (Loc loc))
      | Some (ALoc loc) -> Delayed.return (loc, Expr.ALoc loc)
      | Some (LVar v) ->
          let loc_name = ALoc.alloc () in
          Delayed.return
            ~learned:[ Expr.BinOp (LVar v, Equal, ALoc loc_name) ]
            (loc_name, Expr.ALoc loc_name)
      | Some le ->
          raise
            (Failure
               (Printf.sprintf "Alloc with a non-loc loc argument: %s"
                  ((Fmt.to_to_string Expr.pp) le)))
    in
    let heap = SHeap.init_object heap loc_name ~is_empty:ie mv in
    DR.ok (heap, [ loc ])

  let set_cell (heap : t) (loc : vt) (prop : vt) (v : vt) : action_ret Delayed.t
      =
    let* loc_name, _ = fresh_loc ~loc () in
    DR.ok (SHeap.set_fv_pair heap loc_name prop v, [])

  (** Returns the first field of [fv_list] whose name is provably equal to
      [prop], if any. *)
  let get_equal_field (prop : vt) (fv_list : SFVL.t) :
      (Expr.t * Expr.t) option Delayed.t =
    let rec aux = function
      | [] -> Delayed.return None
      | (name, value) :: rest ->
          let* eq = Delayed.entails [] (Expr.BinOp (name, Equal, prop)) in
          if eq then Delayed.return (Some (name, value)) else aux rest
    in
    aux (SFVL.to_list fv_list)

  let get_cell (heap : t) (loc : vt) (prop : vt) : action_ret Delayed.t =
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

    (* The cases in which the property might exist in the field-value list *)
    let get_cell_branches loc_name fv_list dom mtdt =
      match SFVL.get prop fv_list with
      | Some ffv -> DR.ok (heap, [ loc; prop; ffv ])
      | None -> (
          let* equal_field = get_equal_field prop fv_list in
          match (dom, equal_field) with
          | None, None ->
              DR.error
                (make_gc_error loc_name prop (SFVL.field_names fv_list) None)
          | _, Some (ffn, ffv) -> DR.ok (heap, [ loc; ffn; ffv ])
          | Some dom, None ->
              let not_in_dom = Expr.UnOp (Not, BinOp (prop, SetMem, dom)) in
              Delayed.if_sure not_in_dom
                ~then_:(fun () ->
                  (* The property is certainly absent: it is added to the
                     object as None, and to its domain *)
                  let* new_domain =
                    Delayed.reduce (NOp (SetUnion, [ dom; ESet [ prop ] ]))
                  in
                  let fv_list' = SFVL.add prop (Lit Nono) fv_list in
                  let heap' =
                    SHeap.set heap loc_name fv_list' (Some new_domain) mtdt
                  in
                  DR.ok (heap', [ loc; prop; Lit Nono ]))
                ~else_:(fun () ->
                  let f_names : Expr.t list = SFVL.field_names fv_list in
                  let full_knowledge : Expr.t =
                    BinOp (dom, Equal, ESet f_names)
                  in
                  Delayed.if_sure full_knowledge
                    ~then_:(fun () ->
                      (* The domain is fully known: the property is either one
                         of the fields it is equal to, or it is absent *)
                      L.verbose (fun m -> m "GET CELL will branch\n");
                      let field_branches =
                        List.map
                          (fun (f_name, f_value) ->
                            let this_prop : Expr.t =
                              BinOp (f_name, Equal, prop)
                            in
                            let* sat = Delayed.check_sat this_prop in
                            if sat then
                              DR.ok ~learned:[ this_prop ]
                                (heap, [ loc; f_name; f_value ])
                            else Delayed.vanish ())
                          (SFVL.to_list fv_list)
                      in
                      let none_branch =
                        let not_in_dom : Expr.t =
                          UnOp (Not, BinOp (prop, SetMem, dom))
                        in
                        let* sat = Delayed.check_sat not_in_dom in
                        if sat then
                          DR.ok ~learned:[ not_in_dom ]
                            (heap, [ loc; prop; Lit Nono ])
                        else Delayed.vanish ()
                      in
                      Delayed.branches (field_branches @ [ none_branch ]))
                    ~else_:(fun () ->
                      DR.error
                        (make_gc_error loc_name prop (SFVL.field_names fv_list)
                           (Some dom)))))
    in

    let* loc_name = Delayed.resolve_loc loc in
    L.tmi (fun m ->
        m "@[<h>GetCell: resolved location: %a -> %a@]" SVal.pp loc
          Fmt.(option ~none:(any "None") string)
          loc_name);
    match Option.map (fun ln -> (ln, SHeap.get heap ln)) loc_name with
    | None | Some (_, None) ->
        DR.error ([], [ [ FLoc loc; FCell (loc, prop) ] ], Expr.false_)
    | Some (loc_name, Some ((fv_list, dom), mtdt)) ->
        L.tmi (fun m -> m "fv_list: %a" SFVL.pp fv_list);
        L.tmi (fun m ->
            m "domain: %a" Fmt.(option ~none:(any "None") Expr.pp) dom);
        L.tmi (fun m ->
            m "metadata: %a" Fmt.(option ~none:(any "None") Expr.pp) mtdt);
        get_cell_branches loc_name fv_list dom mtdt

  let remove_cell (heap : t) (loc : vt) (prop : vt) : action_ret Delayed.t =
    let+ loc_name = Delayed.resolve_loc loc in
    let heap =
      match Option.map (fun ln -> (ln, SHeap.get heap ln)) loc_name with
      | None | Some (_, None) -> heap
      | Some (loc_name, Some ((fv_list, dom), mtdt)) ->
          SHeap.set heap loc_name (SFVL.remove prop fv_list) dom mtdt
    in
    Ok (heap, [])

  let set_domain (heap : t) (loc : vt) (dom : vt) : action_ret Delayed.t =
    let+ loc_name, _ = fresh_loc ~loc () in
    let heap =
      match SHeap.get heap loc_name with
      | None -> SHeap.set heap loc_name SFVL.empty (Some dom) None
      | Some ((fv_list, _), mtdt) ->
          (* TODO: This probably needs to be a bit more sophisticated *)
          SHeap.set heap loc_name fv_list (Some dom) mtdt
    in
    Ok (heap, [])

  let get_metadata (heap : t) (loc : vt) : action_ret Delayed.t =
    let make_gm_error (loc_name : string) : err_t =
      let loc = Expr.loc_from_loc_name loc_name in
      ([ loc ], [ [ FMetadata loc ] ], Expr.false_)
    in
    let* loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> DR.error ([ loc ], [ [ FLoc loc; FMetadata loc ] ], Expr.false_)
    | Some loc_name -> (
        let loc = Expr.loc_from_loc_name loc_name in
        match SHeap.get heap loc_name with
        | None | Some (_, None) -> DR.error (make_gm_error loc_name)
        | Some (_, Some mtdt) -> DR.ok (heap, [ loc; mtdt ]))

  let set_metadata (heap : t) (loc : vt) (mtdt : vt) : action_ret Delayed.t =
    let* loc_name, _ = fresh_loc ~loc () in
    match SHeap.get heap loc_name with
    | None -> DR.ok (SHeap.set heap loc_name SFVL.empty None (Some mtdt), [])
    | Some ((fv_list, dom), None) ->
        DR.ok (SHeap.set heap loc_name fv_list dom (Some mtdt), [])
    | Some ((fv_list, dom), Some omet) ->
        if omet <> Expr.Lit Null then
          DR.ok ~learned:[ Expr.BinOp (mtdt, Equal, omet) ] (heap, [])
        else DR.ok (SHeap.set heap loc_name fv_list dom (Some mtdt), [])

  let delete_object (heap : t) (loc : vt) : action_ret Delayed.t =
    let* loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | Some loc_name ->
        if SHeap.has_loc heap loc_name then
          DR.ok (SHeap.remove heap loc_name, [])
        else raise (Failure "delete_obj. Unknown Location")
    | None -> raise (Failure "delete_obj. Unknown Location")

  let get_partial_domain (heap : t) (loc : vt) (e_dom : vt) :
      action_ret Delayed.t =
    L.verbose (fun fmt -> fmt "Get partial domain");
    L.verbose (fun fmt -> fmt "Expected domain: %a" SVal.pp e_dom);
    let* loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> DR.error ([ loc ], [], Expr.false_)
    | Some loc_name -> (
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
            (* Called from the entailment - compute all negative resource
               associated with the location whose name is loc_name *)
            let none_props = SFVL.field_names none_fv_list in
            L.verbose (fun fmt ->
                fmt "None-props in heap: %a"
                  Fmt.(brackets (list ~sep:comma Expr.pp))
                  none_props);
            let* dom' =
              Delayed.reduce (Expr.BinOp (dom, SetDiff, ESet none_props))
            in
            (* Expected dom - dom *)
            let* dom_diff =
              Delayed.reduce (Expr.BinOp (e_dom, SetDiff, dom'))
            in
            (* if dom_diff != {} then we have to put the excess properties in
               the heap as nones *)
            match dom_diff with
            | ESet props ->
                let new_fv_list =
                  List.fold_left
                    (fun fv_list prop -> SFVL.add prop (Lit Nono) fv_list)
                    pos_fv_list props
                in
                let heap' =
                  SHeap.set heap loc_name new_fv_list (Some e_dom) mtdt
                in
                DR.ok (heap', [ loc; e_dom ])
            | _ -> raise (Failure "DEATH. get_partial_domain. dom_diff")))

  let get_full_domain (heap : t) (loc : vt) : action_ret Delayed.t =
    let* loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> DR.error ([ loc ], [], Expr.false_)
    | Some loc_name -> (
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
            Delayed.if_sure a_set_equality
              ~then_:(fun () ->
                let _, pos_fv_list =
                  SFVL.partition (fun _ fv -> fv = Lit Nono) fv_list
                in
                DR.ok (heap, [ loc; EList (SFVL.field_names pos_fv_list) ]))
              ~else_:(fun () ->
                raise
                  (Failure "DEATH. TODO. get_full_domain. incomplete domain")))

  let remove_domain (heap : t) (loc : vt) : action_ret Delayed.t =
    let+ loc_name = Delayed.resolve_loc loc in
    let heap =
      match Option.map (fun ln -> (ln, SHeap.get heap ln)) loc_name with
      | None | Some (_, None) -> heap
      | Some (loc_name, Some ((fv_list, _), mtdt)) ->
          SHeap.set heap loc_name fv_list None mtdt
    in
    Ok (heap, [])

  let execute_action ~action_name:(action : string) (heap : t) (args : vt list)
      : action_ret Delayed.t =
    if action = JSILNames.getCell then
      match args with
      | [ loc; prop ] -> get_cell heap loc prop
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.setCell then
      match args with
      | [ loc; prop; v ] -> set_cell heap loc prop v
      | _ -> raise (Failure "Internal Error. execute_action. setCell")
    else if action = JSILNames.delCell then
      match args with
      | [ loc; prop ] -> remove_cell heap loc prop
      | _ -> raise (Failure "Internal Error. execute_action. delCell")
    else if action = JSILNames.alloc then
      match args with
      | [ Lit Empty; m_loc ] -> alloc heap None (Some m_loc)
      | [ loc; m_loc ] -> alloc heap (Some loc) (Some m_loc)
      | _ -> raise (Failure "Internal Error. execute_action. alloc")
    else if action = JSILNames.delObj then
      match args with
      | [ loc ] -> delete_object heap loc
      | _ -> raise (Failure "Internal Error. execute_action. delObj")
    else if action = JSILNames.getAllProps then
      match args with
      | [ loc ] -> get_full_domain heap loc
      | _ -> raise (Failure "Internal Error. execute_action. getAllProps")
    else if action = JSILNames.getMetadata then
      match args with
      | [ loc ] -> get_metadata heap loc
      | _ -> raise (Failure "Internal Error. execute_action. getMetadata")
    else if action = JSILNames.setMetadata then
      match args with
      | [ loc; loc_m ] -> set_metadata heap loc loc_m
      | _ -> raise (Failure "Internal Error. execute_action. setMetadata")
    else if action = JSILNames.delMetadata then
      match args with
      | [ _ ] -> DR.ok (heap, [])
      | _ -> raise (Failure "Internal Error. execute_action. delMetadata")
    else if action = JSILNames.getProps then
      match args with
      | [ loc; props ] -> get_partial_domain heap loc props
      | _ -> raise (Failure "Internal Error. execute_action. getProps")
    else if action = JSILNames.setProps then
      match args with
      | [ loc; props ] -> set_domain heap loc props
      | _ -> raise (Failure "Internal Error. execute_action")
    else if action = JSILNames.delProps then
      match args with
      | [ loc; _ ] -> remove_domain heap loc
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

  (* Consuming a core predicate is achieved by getting it and then deleting
     it. *)
  let consume ~(core_pred : string) (heap : t) (args : vt list) :
      action_ret Delayed.t =
    let getter = ga_to_getter core_pred in
    let deleter = ga_to_deleter core_pred in
    let** heap', vs = execute_action ~action_name:getter heap args in
    let vs_ins, vs_outs = List_utils.split_at vs (List.length args) in
    let++ heap'', _ = execute_action ~action_name:deleter heap' vs_ins in
    (heap'', vs_outs)

  (* Producing a core predicate is achieved by setting it; failing producers
     are allowed to vanish, there is no unsoundness *)
  let produce ~(core_pred : string) (heap : t) (args : vt list) : t Delayed.t =
    let setter = ga_to_setter core_pred in
    let* set_res = execute_action ~action_name:setter heap args in
    match set_res with
    | Error _ -> Delayed.vanish ()
    | Ok (heap', _) -> Delayed.return heap'

  let split_further _ _ _ _ = None
  let mem_constraints (heap : t) : Expr.t list = SHeap.wf_assertions heap

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

  let sorted_locs_with_vals (heap : t) =
    let sorted_locs = Containers.SS.elements (SHeap.domain heap) in
    List.map (fun loc -> (loc, Option.get (SHeap.get heap loc))) sorted_locs
end
