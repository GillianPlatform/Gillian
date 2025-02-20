open Config
open Location
module L = Logging

let unfolded_preds : (string, Pred.t) Hashtbl.t = Hashtbl.create small_tbl_size

(*
 *  Auto-Unfolding Non-recursive Predicates in Assertions
 * 	-----------------------------------------------------
 * *)
let rec auto_unfold
    ?(unfold_rec_predicates = false)
    ?loc
    (predicates : (string, Pred.t) Hashtbl.t)
    (rec_tbl : (string, bool) Hashtbl.t)
    (asrt : Asrt.t) : Asrt.t list =
  asrt
  |> List.map (function
       (* We don't unfold:
           - Recursive predicates (except in some very specific cases)
           - predicates marked with no-unfold
           - predicates with a guard *)
       | Asrt.Pred (name, _) as asrt
         when (Hashtbl.find rec_tbl name && not unfold_rec_predicates)
              ||
              let pred = Hashtbl.find predicates name in
              pred.pred_nounfold || Option.is_some pred.pred_guard ->
           [ [ asrt ] ]
       | Pred (name, args) when Hashtbl.mem unfolded_preds name ->
           L.verbose (fun fmt ->
               fmt "Unfolding predicate: %s with nounfold %b" name
                 (Hashtbl.find predicates name).pred_nounfold);
           let pred = Hashtbl.find unfolded_preds name in
           let params, _ = List.split pred.pred_params in
           let combined =
             try List.combine params args
             with Invalid_argument _ ->
               let msg =
                 Fmt.str
                   "Impossible to auto unfold predicate %s. Used with %i args \
                    instead of %i"
                   name (List.length args) (List.length params)
               in
               raise (Gillian_result.Exc.verification_failure ?loc msg)
           in
           let subst = SVal.SSubst.init combined in
           let defs = List.map (fun (_, def) -> def) pred.pred_definitions in
           List.map (SVal.SSubst.substitute_asrt subst ~partial:false) defs
       | Pred (name, args) as asrt -> (
           try
             L.tmi (fun fmt -> fmt "AutoUnfold: %a : %s" Asrt.pp_atom asrt name);
             let pred : Pred.t = Hashtbl.find predicates name in
             (* If it is not, replace the predicate assertion for the list of its definitions
                substituting the formal parameters of the predicate with the corresponding
                logical expressions in the argument list *)
             let params, _ = List.split pred.pred_params in
             let subst = SVal.SSubst.init (List.combine params args) in
             L.tmi (fun fmt ->
                 fmt "PREDICATE %s has %d definitions" pred.pred_name
                   (List.length pred.pred_definitions));
             let new_asrts =
               List.map
                 (fun (_, a) ->
                   L.tmi (fun fmt -> fmt "Before Auto Unfolding: %a" Asrt.pp a);
                   let facts =
                     List.map (fun fact -> Asrt.Pure fact) pred.pred_facts
                   in
                   let a = a @ facts in
                   let result =
                     SVal.SSubst.substitute_asrt subst ~partial:false a
                   in
                   L.tmi (fun fmt ->
                       fmt "After Auto Unfolding: %a" Asrt.pp result);
                   result)
                 pred.pred_definitions
             in

             (* FIXME:
                If we processed the predicate definitions in order the recursive call to auto unfold
                would be avoided *)
             let au_no_rec =
               auto_unfold ~unfold_rec_predicates:false predicates rec_tbl
             in
             let result = List.concat_map au_no_rec new_asrts in
             List.filter Simplifications.admissible_assertion result
           with Not_found ->
             raise (Failure ("Error: Can't auto_unfold predicate " ^ name)))
       | asrt -> [ [ asrt ] ])
  (* Now that all assertions have been unfolded to multiple options, do the
     cross products of options
     e.g. [[a1; a2]; [b1; b2]] -> [[a1; b1]; [a1; b2]; [a2; b1]; [a2; b2]] *)
  |> List.fold_left
       (fun acc asrts ->
         List.concat_map
           (fun asrt -> List.map (fun asrt' -> asrt' @ asrt) asrts)
           acc)
       [ [] ]
  |> List.filter Simplifications.admissible_assertion

(*
 * Return: Hashtbl from predicate name to boolean
 * meaning "recursive" or "not recursive"
 *)
let find_recursive_preds (preds : (string, Pred.t) Hashtbl.t) :
    (string, bool) Hashtbl.t =
  let count = ref 0 in
  let max_index = 100_000 in

  (* mark visited predicates and remember the smallest index they can go to *)
  let is_recursive_pred = Hashtbl.create small_tbl_size in
  let open_pred = Hashtbl.create small_tbl_size in
  let visited = Hashtbl.create small_tbl_size in

  (* remember which predicates are still in our DFS stack (to detect cycles) *)
  (* Tarjan's SCC algorithm on predicate nodes; if recursive,
     returns the index where recursion starts, otherwise None *)
  let rec explore pred_name =
    match Hashtbl.find_opt visited pred_name with
    | Some min_index ->
        (* Already explored *)
        if Hashtbl.find open_pred pred_name then
          (* Part of the current component: recursivity detected *)
          Some min_index
        else (* A previously explored component *)
          None
    | None ->
        (* Exploring for the first time *)
        let index = !count in
        incr count;
        Hashtbl.add visited pred_name index;
        Hashtbl.add open_pred pred_name true;
        (* make sure that the hash table is well-formed *)
        assert (Hashtbl.mem preds pred_name);
        let pred = Hashtbl.find preds pred_name in
        (* Find the names of all predicates that the current predicate uses *)
        let neighbours =
          List.concat
            (List.map
               (fun (_, asrt) -> Asrt.pred_names asrt)
               pred.pred_definitions)
        in
        (* Compute the smallest index reachable from its neighbours *)
        let min_index =
          List.fold_left
            (fun min_so_far neighbour_name ->
              match explore neighbour_name with
              | None -> min_so_far
              | Some index -> min min_so_far index)
            max_index neighbours
        in
        Hashtbl.replace open_pred pred_name false;
        (* This predicate is recursive if we have seen an index smaller or equal than its own *)
        if min_index <= index then (
          Hashtbl.replace visited pred_name min_index;
          Hashtbl.add is_recursive_pred pred_name true;
          Some min_index)
        else (
          Hashtbl.add is_recursive_pred pred_name false;
          None)
  in
  (* Launch the exploration from each predicate, unless it's already been visited in a previous one *)
  Hashtbl.iter
    (fun name _ -> if not (Hashtbl.mem visited name) then ignore (explore name))
    preds;
  is_recursive_pred

let simplify_and_prune (pred : Pred.t) : Pred.t =
  L.verbose (fun fmt ->
      fmt "Predicate %s has %d definitions before pruning." pred.pred_name
        (List.length pred.pred_definitions));
  let new_defs =
    List.map
      (fun (oc, x) -> (oc, Reduction.reduce_assertion x))
      pred.pred_definitions
  in
  let new_defs =
    List.filter (fun (_, x) -> Simplifications.admissible_assertion x) new_defs
  in
  L.verbose (fun fmt ->
      fmt "Predicate %s left with %d definitions after pruning." pred.pred_name
        (List.length new_defs));
  { pred with pred_definitions = new_defs }

(*
 * Return: Hashtbl from predicate name to boolean
 * meaning "pure" or "not pure"
 *)
let find_pure_preds (preds : (string, Pred.t) Hashtbl.t) :
    (string, bool) Hashtbl.t =
  let is_pure_pred = Hashtbl.create small_tbl_size in
  (* we mark visited predicates and remember their pureness at the same time *)
  let explore pred_name =
    match Hashtbl.find_opt is_pure_pred pred_name with
    | Some is_pure ->
        (* predicate already visited *)
        is_pure
    | None ->
        (* discovering new predicate *)
        Hashtbl.add is_pure_pred pred_name true;
        (* assume predicates are pure until proven otherwise,
             for recursive calls *)
        let pred = Hashtbl.find preds pred_name in
        let is_pure =
          List.for_all
            (fun (_, asrt) -> List.for_all Asrt.is_pure_asrt asrt)
            pred.pred_definitions
        in

        Hashtbl.replace is_pure_pred pred_name is_pure;
        is_pure
  in
  Hashtbl.iter (fun pred_name _ -> ignore (explore pred_name)) preds;
  is_pure_pred

let unfold_preds (preds : (string, Pred.t) Hashtbl.t) :
    (string, Pred.t) Hashtbl.t * (string, bool) Hashtbl.t =
  (* Detect recursive and pure predicates *)
  let copy_preds = Hashtbl.create small_tbl_size in
  let recursion_info = find_recursive_preds preds in

  let pred_dependency_info : (string * string list) list =
    Hashtbl.fold
      (fun name (pred : Pred.t) res ->
        let deps =
          List.sort_uniq compare
            (List.concat_map
               (fun (_, asrt) -> Asrt.pred_names asrt)
               pred.pred_definitions)
        in
        (name, deps) :: res)
      preds []
  in
  let pred_dependency_info =
    List.sort
      (fun (a, x) (b, y) ->
        match (List.mem b x, List.mem a y) with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> compare (List.length x) (List.length y))
      pred_dependency_info
  in
  L.verbose (fun fmt ->
      fmt "Unfold order: %a"
        Fmt.(list ~sep:comma string)
        (fst (List.split pred_dependency_info)));
  List.iter
    (fun (name, _) ->
      let pred = Hashtbl.find preds name in
      L.verbose (fun fmt -> fmt "Unfolding predicate: %s" pred.pred_name);
      let definitions' : ((string * string list) option * Asrt.t) list =
        List.concat_map
          (fun (os, a) ->
            List.map (fun a -> (os, a)) (auto_unfold preds recursion_info a))
          pred.pred_definitions
      in
      L.verbose (fun fmt ->
          fmt "Definitions' has a length of %d" (List.length definitions'));
      let ret_pred = { pred with pred_definitions = definitions' } in
      L.verbose (fun fmt -> fmt "Pruning.");
      let ret_pred = simplify_and_prune ret_pred in
      L.verbose (fun fmt -> fmt "Done pruning.");
      Hashtbl.replace copy_preds ret_pred.pred_name ret_pred;
      Hashtbl.replace unfolded_preds ret_pred.pred_name ret_pred)
    pred_dependency_info;
  (copy_preds, recursion_info)

let unfold_spec
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (spec : Spec.t) : Spec.t =
  let aux (sspec : Spec.st) : Spec.st list =
    let pres : Asrt.t located list =
      concat_map_fst (auto_unfold preds rec_info) sspec.ss_pre
    in
    L.verbose (fun fmt -> fmt "Pre admissibility: %s" spec.spec_name);
    let pres =
      List.filter
        (fun (pre, _) -> Simplifications.admissible_assertion pre)
        pres
    in
    let posts : Asrt.t located list =
      List.concat_map
        (concat_map_fst @@ auto_unfold preds rec_info)
        sspec.ss_posts
    in
    let posts = List.map (map_fst Reduction.reduce_assertion) posts in
    L.verbose (fun fmt -> fmt "Post admissibility: %s" spec.spec_name);
    L.tmi (fun fmt ->
        fmt "@[<hov 2>Testing admissibility for assertions:@.%a@]"
          (Fmt.list Asrt.pp) (List.map fst posts));
    let posts =
      List.filter
        (fun (post, _) -> Simplifications.admissible_assertion post)
        posts
    in
    if posts = [] then
      Fmt.failwith
        "Unfolding: Postcondition of %s seems invalid, it has been reduced to \
         no postcondition"
        spec.spec_name;
    List.map
      (fun pre -> Spec.{ sspec with ss_pre = pre; ss_posts = posts })
      pres
  in
  let spec_sspecs = List.concat_map aux spec.spec_sspecs in
  match spec_sspecs with
  | [] -> Fmt.failwith "unfolding in spec at preprocessing led to no spec!"
  | _ ->
      ();
      { spec with spec_sspecs }

let unfold_lemma
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (lemma : Lemma.t) : Lemma.t =
  let unfold_lemma_spec (spec : Lemma.spec) =
    L.verbose (fun fmt ->
        fmt "Unfolding spec of lemma: %s with pre-condition\n%a"
          lemma.lemma_name Asrt.pp (fst spec.lemma_hyp));
    let lemma_hyps : Asrt.t located list =
      let unfolded_lemma_pre =
        concat_map_fst (auto_unfold preds rec_info) spec.lemma_hyp
      in
      List.filter
        (fun (pre, _) -> Simplifications.admissible_assertion pre)
        unfolded_lemma_pre
    in
    let lemma_concs : Asrt.t located list =
      List.concat_map
        (concat_map_fst (auto_unfold preds rec_info))
        spec.lemma_concs
    in
    List.map
      (fun lemma_hyp ->
        Lemma.
          { lemma_hyp; lemma_concs; lemma_spec_variant = lemma.lemma_variant })
      lemma_hyps
  in
  {
    lemma with
    lemma_specs = List.concat_map unfold_lemma_spec lemma.lemma_specs;
  }

let unfold_bispec
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (bi_spec : BiSpec.t) : BiSpec.t =
  let depth = !bi_unfold_depth in
  let rec unfold_pres curr_depth curr_pres =
    if curr_depth <= 0 then curr_pres
    else
      let new_pres =
        List.concat_map
          (concat_map_fst
             (auto_unfold ~unfold_rec_predicates:true preds rec_info))
          curr_pres
      in
      unfold_pres (curr_depth - 1) new_pres
  in
  let bispec_pres : Asrt.t located list =
    unfold_pres depth bi_spec.bispec_pres
  in
  { bi_spec with bispec_pres }

let remove_equalities_between_binders_and_lvars binders assertion =
  let module Union_find = Preprocessing_utils.Union_find in
  let is_binder x = List.mem x binders in
  let priority x y =
    match (is_binder x, is_binder y) with
    | true, true | false, false -> `Eq
    | true, false -> `Greater
    | false, true -> `Lower
  in
  let equal = String.equal in
  let uf = Union_find.init ~priority ~equal in
  let rec union_expr (e1 : Expr.t) (e2 : Expr.t) =
    match (e1, e2) with
    | (LVar x | PVar x), (LVar y | PVar y) -> Union_find.union uf x y
    | EList x, EList y -> (
        try List.iter2 union_expr x y
        with Invalid_argument _ ->
          Fmt.failwith "Equality between two lists of different sizes")
    | _ -> ()
  in
  let uf_maker =
    object
      inherit [_] Visitors.iter
      method! visit_BinOp _ e1 op e2 = if op = Equal then union_expr e1 e2
    end
  in
  uf_maker#visit_assertion () assertion;
  let substitutor =
    object
      inherit [_] Visitors.endo as super

      method! visit_expr () e =
        match e with
        | LVar x | PVar x ->
            let rep = Union_find.rep uf x in
            if String.equal x rep then e else Expr.var_to_expr rep
        | _ -> super#visit_expr () e
    end
  in
  substitutor#visit_assertion () assertion

let unfold_cmd
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (cmd : int Cmd.t) : int Cmd.t =
  match cmd with
  | Logic (SL (SepAssert (a, binders))) -> (
      let asrts = auto_unfold preds rec_info a in
      match asrts with
      | [ a ] -> Logic (SL (SepAssert (a, binders)))
      | _ -> Logic (SL (SepAssert (a, binders))))
  | Logic (SL (Invariant (a, binders))) ->
      let a =
        match auto_unfold preds rec_info a with
        | [ a ] -> a
        | _ -> a
      in
      let a = remove_equalities_between_binders_and_lvars binders a in
      Logic (SL (Invariant (a, binders)))
  | _ -> cmd

let unfold_proc
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (proc : ('a, int) Proc.t) : ('a, int) Proc.t =
  Logging.normal (fun f ->
      f "UNFOLD_PROC ! %a" (Proc.pp_indexed ~pp_annot:Fmt.nop) proc);
  let new_spec = Option.map (unfold_spec preds rec_info) proc.proc_spec in
  let new_body =
    Array.map
      (fun (annot, lab, cmd) -> (annot, lab, unfold_cmd preds rec_info cmd))
      proc.proc_body
  in
  { proc with proc_spec = new_spec; proc_body = new_body }

let explicit_param_types
    (procs : (string, ('a, int) Proc.t) Hashtbl.t)
    (preds : (string, Pred.t) Hashtbl.t)
    (lemmas : (string, Lemma.t) Hashtbl.t) :
    (string, ('a, int) Proc.t) Hashtbl.t
    * (string, Pred.t) Hashtbl.t
    * (string, Lemma.t) Hashtbl.t =
  let copy_preds = Hashtbl.create small_tbl_size in
  let copy_procs = Hashtbl.create small_tbl_size in
  let copy_lemmas = Hashtbl.create small_tbl_size in

  let join_preds (pred1 : Pred.t) (pred2 : Pred.t) : Pred.t =
    L.tmi (fun fmt -> fmt "Join preds: %s, %s" pred1.pred_name pred2.pred_name);
    if
      pred1.pred_name <> pred2.pred_name
      || pred1.pred_num_params <> pred2.pred_num_params
    then
      let msg =
        Printf.sprintf
          "Incompatible predicate definitions for: %s\n\
           \tName:%s\tName:%s\n\
           \tParams:%d\tParams:%d"
          pred1.pred_name pred1.pred_name pred2.pred_name pred1.pred_num_params
          pred2.pred_num_params
      in
      Logging.fail msg
    else
      let p1_params, _ = List.split pred1.pred_params in
      let p2_params, _ = List.split pred2.pred_params in
      let subst =
        SVal.SSubst.init
          (List.combine p2_params
             (List.map (fun var -> Expr.PVar var) p1_params))
      in
      let defs =
        pred1.pred_definitions
        @ List.map
            (fun (oid, a) ->
              (oid, SVal.SSubst.substitute_asrt subst ~partial:true a))
            pred2.pred_definitions
      in
      { pred1 with pred_definitions = defs }
  in

  (* Explicit Parameter Types *)
  Hashtbl.iter
    (fun name pred ->
      (* Substitute literals in the head for logical variables *)
      let pred =
        match Pred.explicit_param_types preds pred with
        | Ok pred -> pred
        | Error msg ->
            raise
              (Gillian_result.Exc.verification_failure ?loc:pred.pred_loc msg)
      in
      (* Join the new predicate definition with all previous for the same predicate (if any) *)
      try
        let current_pred = Hashtbl.find copy_preds name in
        Hashtbl.replace copy_preds name (join_preds current_pred pred)
      with
      | Not_found -> Hashtbl.replace copy_preds name pred
      | Failure reason ->
          raise (Failure ("Error in predicate " ^ name ^ ": " ^ reason)))
    preds;

  Hashtbl.iter
    (fun name (proc : ('a, int) Proc.t) ->
      match proc.proc_spec with
      | None -> Hashtbl.replace copy_procs name proc
      | Some spec ->
          let spec' = Spec.parameter_types preds spec in
          let proc' = { proc with proc_spec = Some spec' } in
          Hashtbl.replace copy_procs name proc')
    procs;

  Hashtbl.iter
    (fun name (lemma : Lemma.t) ->
      let lemma' = Lemma.parameter_types preds lemma in
      Hashtbl.replace copy_lemmas name lemma')
    lemmas;

  (copy_procs, copy_preds, copy_lemmas)

let unfold_procs
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (procs : (string, ('a, int) Proc.t) Hashtbl.t) :
    (string, ('a, int) Proc.t) Hashtbl.t =
  let copy_procs = Hashtbl.create small_tbl_size in

  Hashtbl.iter
    (fun name (proc : ('a, int) Proc.t) ->
      let proc' = unfold_proc preds rec_info proc in
      Hashtbl.replace copy_procs name proc')
    procs;

  copy_procs

let unfold_specs
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (specs : (string, Spec.t) Hashtbl.t) : (string, Spec.t) Hashtbl.t =
  let copy_specs = Hashtbl.create small_tbl_size in

  Hashtbl.iter
    (fun name (spec : Spec.t) ->
      let spec' = unfold_spec preds rec_info spec in
      Hashtbl.replace copy_specs name spec')
    specs;

  copy_specs

let unfold_lemmas
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (lemmas : (string, Lemma.t) Hashtbl.t) : (string, Lemma.t) Hashtbl.t =
  let copy_lemmas = Hashtbl.create small_tbl_size in

  Hashtbl.iter
    (fun name (lemma : Lemma.t) ->
      let lemma' = unfold_lemma preds rec_info lemma in
      Hashtbl.replace copy_lemmas name lemma')
    lemmas;

  copy_lemmas

let unfold_bispecs
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (bispecs : (string, BiSpec.t) Hashtbl.t) : (string, BiSpec.t) Hashtbl.t =
  let copy_bispecs = Hashtbl.create small_tbl_size in

  Hashtbl.iter
    (fun name (bispec : BiSpec.t) ->
      let bispec' = unfold_bispec preds rec_info bispec in
      Hashtbl.replace copy_bispecs name bispec')
    bispecs;

  copy_bispecs

(* let create_partial_matches
     (procs    : (string, Proc.t) Hashtbl.t) : unit =

   Hashtbl.iter
     (fun name (proc : Proc.t) ->
       match proc.spec with
         | None -> ()
         | Some spec ->
             match MP.create_partial_match_spec spec with
             | None -> ()
             | Some sspec ->
                 Hashtbl.replace procs name { proc with Proc.spec = Some { spec with sspecs = spec.sspecs @ [ sspec ] } }
     ) procs *)

(* For each guarded predicate, we add an abstract close token to the predicate table. *)
let add_closing_tokens preds =
  let guarded_predicates =
    Hashtbl.to_seq_values preds
    |> Seq.filter (fun (pred : Pred.t) -> Option.is_some pred.pred_guard)
    |> List.of_seq
    (* Can't keep the sequence, or we'd be modifying the hashtbl while iterating over it. *)
  in
  (* We add the the close_token to each definition. *)
  List.to_seq guarded_predicates
  |> Seq.map (fun (pred : Pred.t) ->
         {
           pred with
           pred_definitions =
             List.map
               (fun (x, def) -> (x, Pred.close_token_call pred :: def))
               pred.pred_definitions;
         })
  |> Seq.iter (fun (pred : Pred.t) -> Hashtbl.replace preds pred.pred_name pred);
  (* We add the close tokens to the table. *)
  List.iter
    (fun (pred : Pred.t) ->
      let pred_name = Pred.close_token_name pred in
      let pred_params = Pred.in_args pred pred.pred_params in
      let pred_num_params = List.length pred_params in
      let pred_ins = List.init pred_num_params Fun.id in
      let close_token =
        Pred.
          {
            pred_name;
            pred_source_path = pred.pred_source_path;
            pred_loc = pred.pred_loc;
            pred_internal = false;
            pred_num_params;
            pred_params;
            pred_ins;
            pred_facts = [];
            pred_definitions = [];
            pred_guard = None;
            pred_pure = false;
            pred_abstract = true;
            pred_nounfold = true;
            pred_normalised = false;
          }
      in
      Hashtbl.replace preds pred_name close_token)
    guarded_predicates

let preprocess (prog : ('a, int) Prog.t) (unfold : bool) : ('a, int) Prog.t =
  let f (prog : ('a, int) Prog.t) unfold =
    let procs = prog.procs in
    let preds = prog.preds in
    let lemmas = prog.lemmas in
    let onlyspecs = prog.only_specs in

    let procs', preds', lemmas' = explicit_param_types procs preds lemmas in

    let () =
      Hashtbl.filter_map_inplace
        (fun _ lemma ->
          let lemma = Lemma.add_param_bindings lemma in
          Some lemma)
        lemmas'
    in

    let preds'', procs'', bi_specs, lemmas'', onlyspecs' =
      match unfold with
      | false -> (preds', procs', prog.bi_specs, lemmas', onlyspecs)
      | true ->
          let preds'', rec_info = unfold_preds preds' in
          let procs'' = unfold_procs preds'' rec_info procs' in
          let bi_specs = unfold_bispecs preds'' rec_info prog.bi_specs in
          let lemmas'' = unfold_lemmas preds'' rec_info lemmas' in
          let onlyspecs' = unfold_specs preds'' rec_info onlyspecs in
          (* create_partial_matches procs'';  *)
          (preds'', procs'', bi_specs, lemmas'', onlyspecs')
    in
    add_closing_tokens preds'';
    {
      prog with
      preds = preds'';
      procs = procs'';
      bi_specs;
      lemmas = lemmas'';
      only_specs = onlyspecs';
    }
  in
  L.Phase.with_normal ~title:"Logic preprocessing" (fun () -> f prog unfold)
