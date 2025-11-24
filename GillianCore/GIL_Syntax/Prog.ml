type ('annot, 'label) t = {
  imports : (string * bool) list;
      (** List of tuples consisting of the file path and a boolean indicating
          whether the procedures in the file should be verified. The latter
          should be [false] for runtime files. *)
  lemmas : (string, Lemma.t) Hashtbl.t;
  (* Lemmas *)
  preds : (string, Pred.t) Hashtbl.t;
  (* Predicates = Name : String --> Definition *)
  only_specs : (string, Spec.t) Hashtbl.t;
  (* Specs = Name : String --> Spec *)
  procs : (string, ('annot, 'label) Proc.t) Hashtbl.t;
  (* GIL extended procedures = Name : String --> Procedure *)
  macros : (string, Macro.t) Hashtbl.t;
  (* macros *)
  bi_specs : (string, BiSpec.t) Hashtbl.t;
  (* Hints for bi-abduction *)
  proc_names : string list;
  predecessors : (string * int * int, int) Hashtbl.t;
}

let make
    ~imports
    ~lemmas
    ~preds
    ~only_specs
    ~procs
    ~macros
    ~bi_specs
    ~proc_names
    ~predecessors
    () =
  {
    imports;
    lemmas;
    preds;
    only_specs;
    procs;
    macros;
    bi_specs;
    proc_names;
    predecessors;
  }

let make_labeled ~(procs : (string, ('annot, string) Proc.t) Hashtbl.t) =
  make ~procs ~predecessors:(Hashtbl.create 1)

let make_indexed
    ~(procs : ('a, int) Proc.t list)
    ~(predecessors : (string * int * int * int) list) =
  let procs_tbl = Hashtbl.create Config.big_tbl_size in
  let predecessors_tbl = Hashtbl.create Config.big_tbl_size in
  List.iter
    (fun (proc : ('a, int) Proc.t) ->
      Hashtbl.replace procs_tbl proc.proc_name proc)
    procs;
  List.iter
    (fun (name, j, i, k) -> Hashtbl.replace predecessors_tbl (name, j, i) k)
    predecessors;
  (* FIXME: Why would proc_names be empty? pp wouldn't work properly *)
  make ~predecessors:predecessors_tbl ~procs:procs_tbl ~imports:[]
    ~proc_names:[]

let create () =
  let open Config in
  make_labeled ~imports:[]
    ~lemmas:(Hashtbl.create medium_tbl_size)
    ~preds:(Hashtbl.create big_tbl_size)
    ~only_specs:(Hashtbl.create medium_tbl_size)
    ~procs:(Hashtbl.create big_tbl_size)
    ~macros:(Hashtbl.create small_tbl_size)
    ~bi_specs:(Hashtbl.create big_tbl_size)
    ~proc_names:[] ()

let get_lemmas (prog : ('a, 'b) t) : Lemma.t list =
  Hashtbl.fold (fun _ lemma ac -> lemma :: ac) prog.lemmas []

let get_preds (prog : ('a, 'b) t) : Pred.t list =
  Hashtbl.fold (fun _ lemma ac -> lemma :: ac) prog.preds []

let get_ospecs (prog : ('a, 'b) t) : Spec.t list =
  Hashtbl.fold (fun _ spec ac -> spec :: ac) prog.only_specs []

let get_specs (prog : ('a, 'b) t) : Spec.t list =
  Hashtbl.fold
    (fun _ (proc : ('a, 'b) Proc.t) ac ->
      Option.fold ~some:(fun spec -> spec :: ac) ~none:ac proc.proc_spec)
    prog.procs (get_ospecs prog)

let get_procs ?(proc_names : string list option) (prog : ('a, 'b) t) :
    ('a, 'b) Proc.t list =
  let proc_names =
    match proc_names with
    | Some proc_names ->
        (* Printf.printf "GET PROCS with proc_names: %s\n" (String.concat ", " proc_names); *)
        proc_names
    | None ->
        Hashtbl.fold
          (fun proc_name _ proc_names -> proc_name :: proc_names)
          prog.procs []
  in
  List.map (fun proc_name -> Hashtbl.find prog.procs proc_name) proc_names

let get_bispecs (prog : ('a, 'b) t) : BiSpec.t list =
  Hashtbl.fold (fun _ bi_spec ac -> bi_spec :: ac) prog.bi_specs []

let get_proc_names (prog : ('a, 'b) t) : string list =
  Hashtbl.fold (fun name _ acc -> name :: acc) prog.procs []

let get_noninternal_proc_names (prog : ('a, 'b) t) : string list =
  Hashtbl.fold
    (fun name (proc : ('a, 'b) Proc.t) acc ->
      if not proc.proc_internal then name :: acc else acc)
    prog.procs []

let get_noninternal_pred_names (prog : ('a, 'b) t) : string list =
  Hashtbl.fold
    (fun name (pred : Pred.t) acc ->
      if not pred.pred_internal then name :: acc else acc)
    prog.preds []

let get_noninternal_lemma_names (prog : ('a, 'b) t) : string list =
  Hashtbl.fold
    (fun name (lemma : Lemma.t) acc ->
      if not lemma.lemma_internal then name :: acc else acc)
    prog.lemmas []

let get_proc (prog : ('a, 'b) t) (name : string) : ('a, 'b) Proc.t option =
  Hashtbl.find_opt prog.procs name

let get_proc_exn (prog : ('a, 'b) t) (name : string) : ('a, 'b) Proc.t =
  match get_proc prog name with
  | Some proc -> proc
  | None -> failwith (Printf.sprintf "could not find proc %s" name)

let get_pred (prog : ('a, 'b) t) (name : string) : Pred.t option =
  Hashtbl.find_opt prog.preds name

let get_pred_exn (prog : ('a, 'b) t) (name : string) : Pred.t =
  match get_pred prog name with
  | Some pred -> pred
  | None -> failwith (Printf.sprintf "could not find pred %s" name)

let get_bispec (prog : ('a, 'b) t) (name : string) : BiSpec.t option =
  Hashtbl.find_opt prog.bi_specs name

let get_bispec_exn (prog : ('a, 'b) t) (name : string) : BiSpec.t =
  match get_bispec prog name with
  | Some bispec -> bispec
  | None -> failwith (Printf.sprintf "could not find bispec %s" name)

let get_lemma (prog : ('a, 'b) t) (name : string) : Lemma.t option =
  Hashtbl.find_opt prog.lemmas name

let get_lemma_exn (prog : ('a, 'b) t) (name : string) : Lemma.t =
  match get_lemma prog name with
  | Some lemma -> lemma
  | None -> failwith (Printf.sprintf "could not find lemma %s" name)

let pp
    ~(show_labels : bool)
    ~(pp_label : 'b Fmt.t)
    ?(pp_annot : 'a Fmt.t option)
    fmt
    (prog : ('a, 'b) t) =
  let proc_names = Hashtbl.to_seq_keys prog.procs in
  let pp_list ppp = Fmt.list ~sep:(Fmt.any "@\n") ppp in
  let npp pp =
    let open Fmt in
    any "@\n" ++ pp
  in
  let pp_import_paths fmt prefix = function
    | [] -> ()
    | paths ->
        let pp_str fmt = Fmt.pf fmt "\"%a\"" Fmt.string in
        Fmt.pf fmt "%s @[%a@];@\n" prefix (Fmt.list ~sep:Fmt.comma pp_str) paths
  in
  let pp_imports fmt imports =
    let reg_imps, imps_to_verify =
      List.partition (fun (_, should_verify) -> not should_verify) imports
    in
    let reg_paths = List.map fst reg_imps in
    let paths_to_verify = List.map fst imps_to_verify in
    pp_import_paths fmt "import" reg_paths;
    pp_import_paths fmt "import verify" paths_to_verify
  in
  let pp_only_spec fmt' spec = Fmt.pf fmt' "axiomatic %a" Spec.pp spec in
  Fmt.pf fmt "%a@\n%a%a%a%a%a" pp_imports prog.imports
    (pp_list (npp Lemma.pp))
    (get_lemmas prog)
    (pp_list (npp Pred.pp))
    (get_preds prog)
    (pp_list (npp pp_only_spec))
    (get_ospecs prog)
    (pp_list (npp BiSpec.pp))
    (get_bispecs prog)
    (pp_list (npp (Proc.pp ~show_labels ~pp_label ?pp_annot)))
    (get_procs ~proc_names:(List.of_seq proc_names) prog)

let pp_labeled fmt ?pp_annot c =
  pp ~show_labels:true ~pp_label:Fmt.string ?pp_annot fmt c

let pp_indexed fmt ?pp_annot c =
  pp ~show_labels:false ~pp_label:Fmt.int ?pp_annot fmt c

(* let perform_syntax_checks (prog : t) : unit =
   if (!Config.perform_syntax_checks)
   then (
     L.(normal (fun m -> m "Running syntax checks:"));
     L.(normal (fun m -> m "Checking predicate definitions only use program variables they are allowed to."));
     Pred.check_pvars prog.preds;
     L.(normal (fun m -> m "Checking spec definitions only use program variables they're allowed to."));
     LabProc.check_spec_pvars prog.procs;
     L.(normal (fun m -> m "Checking specs correspond directly to procedures"));
     LabProc.check_proc_spec_correspondence prog.procs;
   ) *)

let update_specs (to_update : ('a, 'b) t) (update_with : ('c, 'd) t) : unit =
  Hashtbl.iter
    (fun (proc_name : string) (proc : ('c, 'd) Proc.t) ->
      match (proc.proc_spec, Hashtbl.find_opt to_update.procs proc_name) with
      | Some spec, Some eproc ->
          Hashtbl.replace to_update.procs proc_name
            { eproc with proc_spec = Some spec }
      | _ -> ())
    update_with.procs;
  Hashtbl.clear to_update.bi_specs

let update_imports (prog : ('a, 'b) t) (imports : (string * bool) list) :
    ('a, 'b) t =
  { prog with imports }

let add_lemma (prog : ('a, 'b) t) (lemma : Lemma.t) : ('a, 'b) t =
  Hashtbl.add prog.lemmas lemma.lemma_name lemma;
  prog

let add_pred (prog : ('a, 'b) t) (pred : Pred.t) : ('a, 'b) t =
  Hashtbl.add prog.preds pred.pred_name pred;
  prog

let add_ospec (prog : ('a, 'b) t) (spec : Spec.t) : ('a, 'b) t =
  Hashtbl.add prog.only_specs spec.spec_name spec;
  prog

let add_proc (prog : ('a, 'b) t) (proc : ('a, 'b) Proc.t) : ('a, 'b) t =
  Hashtbl.add prog.procs proc.proc_name proc;
  { prog with proc_names = proc.proc_name :: prog.proc_names }

let add_macro (prog : ('a, 'b) t) (macro : Macro.t) : ('a, 'b) t =
  Hashtbl.add prog.macros macro.macro_name macro;
  prog

let add_bispec (prog : ('a, 'b) t) (bi_spec : BiSpec.t) : ('a, 'b) t =
  Hashtbl.add prog.bi_specs bi_spec.bispec_name bi_spec;
  prog

let make_callgraph (prog : ('a, 'b) t) =
  let fcalls =
    Hashtbl.fold
      (fun _ proc acc ->
        Proc.(proc.proc_name, proc.proc_aliases, proc.proc_calls) :: acc)
      prog.procs []
  in
  let call_graph = Call_graph.make () in
  let fnames = Hashtbl.create (List.length fcalls * 2) in
  fcalls
  |> List.iter (fun (sym, aliases, _) ->
         Call_graph.add_proc call_graph sym;
         Hashtbl.add fnames sym sym;
         aliases |> List.iter (fun alias -> Hashtbl.add fnames alias sym));
  fcalls
  |> List.iter (fun (caller, _, callees) ->
         callees
         |> List.iter (fun callee ->
                match Hashtbl.find_opt fnames callee with
                | Some callee ->
                    Call_graph.add_proc_call call_graph caller callee
                | None -> ()));
  call_graph
