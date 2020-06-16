open Config
module L = Logging

(*
 *  Auto-Unfolding Non-recursive Predicates in Assertions
 * 	-----------------------------------------------------
 * *)
let rec auto_unfold
    ?(unfold_rec_predicates = false)
    (predicates : (string, Pred.t) Hashtbl.t)
    (rec_tbl : (string, bool) Hashtbl.t)
    (asrt : Asrt.t) : Asrt.t list =
  let au_rec = auto_unfold ~unfold_rec_predicates predicates rec_tbl in
  let au_no_rec = auto_unfold ~unfold_rec_predicates:false predicates rec_tbl in
  match (asrt : Asrt.t) with
  | Star (a1, a2)     ->
      List_utils.cross_product (au_rec a1) (au_rec a2) (fun asrt1 asrt2 ->
          Asrt.Star (asrt1, asrt2))
  | Pred (name, args) -> (
      try
        (* L.verbose (fun fmt -> fmt "AutoUnfold: %a" Asrt.pp asrt); *)
        let pred : Pred.t = Hashtbl.find predicates name in
        if Hashtbl.find rec_tbl pred.pred_name && not unfold_rec_predicates then
          [ asrt ]
        else
          (* If it is not, replace the predicate assertion for the list of its definitions
             substituting the formal parameters of the predicate with the corresponding
             logical expressions in the argument list *)
          let params, _ = List.split pred.pred_params in
          let subst = SVal.SSubst.init (List.combine params args) in

          let new_asrts =
            List.map
              (fun (_, a) ->
                (* L.verbose (fun fmt -> fmt "Before: %a" Asrt.pp a); *)
                let result = SVal.SSubst.substitute_asrt subst false a in
                (* L.verbose (fun fmt -> fmt "After: %a" Asrt.pp result); *)
                result)
              pred.pred_definitions
          in

          (* FIXME:
             If we processed the predicate definitions in order the recursive call to auto unfold
             would be avoided *)
          let result = List.concat (List.map au_no_rec new_asrts) in
          let result =
            List.filter Simplifications.admissible_assertion result
          in
          result
      with Not_found ->
        raise (Failure ("Error: Can't auto_unfold predicate " ^ name)) )
  | _                 -> [ asrt ]

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
    | None           ->
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
              | None       -> min_so_far
              | Some index -> min min_so_far index)
            max_index neighbours
        in
        Hashtbl.replace open_pred pred_name false;
        (* This predicate is recursive if we have seen an index smaller or equal than its own *)
        if min_index <= index then (
          Hashtbl.replace visited pred_name min_index;
          Hashtbl.add is_recursive_pred pred_name true;
          Some min_index )
        else (
          Hashtbl.add is_recursive_pred pred_name false;
          None )
  in
  (* Launch the exploration from each predicate, unless it's already been visited in a previous one *)
  Hashtbl.iter
    (fun name _ -> if not (Hashtbl.mem visited name) then ignore (explore name))
    preds;
  is_recursive_pred

let simplify_and_prune (pred : Pred.t) : Pred.t =
  let new_defs =
    List.map
      (fun (oc, x) -> (oc, Reduction.reduce_assertion x))
      pred.pred_definitions
  in
  let new_defs =
    List.filter (fun (oc, x) -> Simplifications.admissible_assertion x) new_defs
  in
  { pred with pred_definitions = new_defs }

(*
 * Return: Hashtbl from predicate name to boolean
 * meaning "pure" or "not pure"
 *)
let find_pure_preds (preds : (string, Pred.t) Hashtbl.t) :
    (string, bool) Hashtbl.t =
  let is_pure_pred = Hashtbl.create small_tbl_size in
  (* we mark visited predicates and remember their pureness at the same time *)
  let rec explore pred_name =
    match Hashtbl.find_opt is_pure_pred pred_name with
    | Some is_pure ->
        (* predicate already visited *)
        is_pure
    | None         ->
        (* discovering new predicate *)
        let is_pure_assertion (a : Asrt.t) =
          let f_ac a _ _ ac =
            match (a : Asrt.t) with
            | Pred (pred_name, _) -> explore pred_name
            | Emp | GA _          -> false
            | _                   -> List.for_all (fun b -> b) ac
          in
          Asrt.fold None None f_ac None None a
        in

        Hashtbl.add is_pure_pred pred_name true;
        (* assume predicates are pure until proven otherwise,
             for recursive calls *)
        let pred = Hashtbl.find preds pred_name in
        let is_pure =
          List.for_all
            (fun (_, asrt) -> is_pure_assertion asrt)
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

  Hashtbl.iter
    (fun name (pred : Pred.t) ->
      L.verbose (fun fmt -> fmt "Unfolding predicate: %s" pred.pred_name);
      let definitions' : ((string * string list) option * Asrt.t) list =
        List.flatten
          (List.map
             (fun (os, a) ->
               List.map (fun a -> (os, a)) (auto_unfold preds recursion_info a))
             pred.pred_definitions)
      in
      let ret_pred = { pred with pred_definitions = definitions' } in
      let ret_pred = simplify_and_prune ret_pred in
      Hashtbl.replace copy_preds ret_pred.pred_name ret_pred)
    preds;
  (copy_preds, recursion_info)

let unfold_spec
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (spec : Spec.t) : Spec.t =
  let aux spec_name (sspec : Spec.st) : Spec.st list =
    let pres : Asrt.t list = auto_unfold preds rec_info sspec.ss_pre in
    let pres = List.filter Simplifications.admissible_assertion pres in
    let posts : Asrt.t list =
      List.concat (List.map (auto_unfold preds rec_info) sspec.ss_posts)
    in
    L.verbose (fun fmt -> fmt "Testing for admissibility");
    let posts = List.filter Simplifications.admissible_assertion posts in
    if posts = [] then
      Fmt.failwith
        "Unfolding: Postcondition of %s seems invalid, it has been reduced to \
         no postcondition"
        spec_name;
    List.map
      (fun pre -> Spec.{ sspec with ss_pre = pre; ss_posts = posts })
      pres
  in

  {
    spec with
    spec_sspecs = List.concat (List.map (aux spec.spec_name) spec.spec_sspecs);
  }

let unfold_lemma
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (lemma : Lemma.t) : Lemma.t =
  L.verbose (fun fmt ->
      fmt "Unfolding lemma: %s with pre-condition\n%a" lemma.lemma_name Asrt.pp
        lemma.lemma_hyp);
  let lemma_hyp : Asrt.t =
    let unfolded_lemma = auto_unfold preds rec_info lemma.lemma_hyp in
    let unfolded_lemma =
      List.filter Simplifications.admissible_assertion unfolded_lemma
    in
    match unfolded_lemma with
    | [ pre ] -> pre
    | other   ->
        L.verbose (fun fmt ->
            fmt "Lemma unfolded to %d preconditions" (List.length other));
        lemma.lemma_hyp
  in
  let lemma_concs : Asrt.t list =
    List.concat (List.map (auto_unfold preds rec_info) lemma.lemma_concs)
  in
  { lemma with lemma_hyp; lemma_concs }

let unfold_bispec
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (bi_spec : BiSpec.t) : BiSpec.t =
  let depth = !bi_unfold_depth in
  let rec unfold_pres curr_depth curr_pres =
    if curr_depth <= 0 then curr_pres
    else
      let new_pres =
        List.concat
          (List.map
             (auto_unfold ~unfold_rec_predicates:true preds rec_info)
             curr_pres)
      in
      unfold_pres (curr_depth - 1) new_pres
  in
  let bispec_pres : Asrt.t list = unfold_pres depth bi_spec.bispec_pres in
  { bi_spec with bispec_pres }

let unfold_cmd
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (cmd : int Cmd.t) : int Cmd.t =
  match cmd with
  | Logic (SL (SepAssert (a, binders))) -> (
      let asrts = auto_unfold preds rec_info a in
      match asrts with
      | [ a ] -> Logic (SL (SepAssert (a, binders)))
      | _     -> Logic (SL (SepAssert (a, binders))) )
  | Logic (SL (Invariant (a, existentials))) -> (
      let asrts = auto_unfold preds rec_info a in
      match asrts with
      | [ a ] -> Logic (SL (Invariant (a, existentials)))
      | _     -> Logic (SL (Invariant (a, existentials))) )
  | _ -> cmd

let unfold_proc
    (preds : (string, Pred.t) Hashtbl.t)
    (rec_info : (string, bool) Hashtbl.t)
    (proc : ('a, int) Proc.t) : ('a, int) Proc.t =
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
            (fun (oid, a) -> (oid, SVal.SSubst.substitute_asrt subst true a))
            pred2.pred_definitions
      in
      { pred1 with pred_definitions = defs }
  in

  (* Explicit Parameter Types *)
  Hashtbl.iter
    (fun name pred ->
      (* Substitute literals in the head for logical variables *)
      let pred = Pred.explicit_param_types preds pred in
      (* Join the new predicate definition with all previous for the same predicate (if any) *)
      try
        let current_pred = Hashtbl.find copy_preds name in
        Hashtbl.replace copy_preds name (join_preds current_pred pred)
      with
      | Not_found      -> Hashtbl.replace copy_preds name pred
      | Failure reason ->
          raise (Failure ("Error in predicate " ^ name ^ ": " ^ reason)))
    preds;

  Hashtbl.iter
    (fun name (proc : ('a, int) Proc.t) ->
      match proc.proc_spec with
      | None      -> Hashtbl.replace copy_procs name proc
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
            match UP.create_partial_match_spec spec with
            | None -> ()
            | Some sspec ->
                Hashtbl.replace procs name { proc with Proc.spec = Some { spec with sspecs = spec.sspecs @ [ sspec ] } }
    ) procs *)

let preprocess (prog : ('a, int) Prog.t) (unfold : bool) : ('a, int) Prog.t =
  let f (prog : ('a, int) Prog.t) unfold =
    let procs = prog.procs in
    let preds = prog.preds in
    let lemmas = prog.lemmas in
    let onlyspecs = prog.only_specs in

    let procs', preds', lemmas' = explicit_param_types procs preds lemmas in

    let preds'', procs'', bi_specs, lemmas'', onlyspecs' =
      match unfold with
      | false -> (preds', procs', prog.bi_specs, lemmas', onlyspecs)
      | true  ->
          let preds'', rec_info = unfold_preds preds' in
          let procs'' = unfold_procs preds'' rec_info procs' in
          let bi_specs = unfold_bispecs preds'' rec_info prog.bi_specs in
          let lemmas'' = unfold_lemmas preds'' rec_info lemmas' in
          let onlyspecs' = unfold_specs preds'' rec_info onlyspecs in
          (* create_partial_matches procs'';  *)
          (preds'', procs'', bi_specs, lemmas'', onlyspecs')
    in
    {
      prog with
      preds = preds'';
      procs = procs'';
      bi_specs;
      lemmas = lemmas'';
      only_specs = onlyspecs';
    }
  in
  L.with_normal_phase ~title:"Logic preprocessing" (fun () -> f prog unfold)
