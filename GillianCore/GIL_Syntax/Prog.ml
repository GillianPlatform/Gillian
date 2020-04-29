module L = Logging

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
    | None            ->
        Hashtbl.fold
          (fun proc_name _ proc_names -> proc_name :: proc_names)
          prog.procs []
  in
  List.map (fun proc_name -> Hashtbl.find prog.procs proc_name) proc_names

let get_proc (prog : ('a, 'b) t) (proc_name : string) : ('a, 'b) Proc.t option =
  Hashtbl.find_opt prog.procs proc_name

let get_bispecs (prog : ('a, 'b) t) : BiSpec.t list =
  Hashtbl.fold (fun _ bi_spec ac -> bi_spec :: ac) prog.bi_specs []

let pp ~(show_labels : bool) ~(pp_label : 'b Fmt.t) fmt (prog : ('a, 'b) t) =
  let pp_list ppp = Fmt.list ~sep:(Fmt.any "@\n") ppp in
  let npp pp =
    let open Fmt in
    any "@\n" ++ pp
  in
  let pp_import_paths fmt prefix = function
    | []    -> ()
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
  let pp_only_spec fmt' spec = Fmt.pf fmt' "only %a" Spec.pp spec in
  Fmt.pf fmt "%a@\n%a%a%a%a%a" pp_imports prog.imports
    (pp_list (npp Lemma.pp))
    (get_lemmas prog)
    (pp_list (npp Pred.pp))
    (get_preds prog)
    (pp_list (npp pp_only_spec))
    (get_ospecs prog)
    (pp_list (npp BiSpec.pp))
    (get_bispecs prog)
    (pp_list (npp (Proc.pp ~show_labels ~pp_label)))
    (get_procs ~proc_names:prog.proc_names prog)

let pp_labeled fmt x = pp ~show_labels:true ~pp_label:Fmt.string fmt x

let pp_indexed fmt x = pp ~show_labels:false ~pp_label:Fmt.int fmt x

let line_info (prog : ('a, 'b) t) : (string * int * int) list =
  List.concat (List.map Proc.line_info (get_procs prog))

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
      | _                     -> ())
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

let get_bispec (prog : ('a, 'b) t) : string -> BiSpec.t option =
  Hashtbl.find_opt prog.bi_specs

let get_lemma (prog : ('a, 'b) t) (name : string) : Lemma.t =
  try Hashtbl.find prog.lemmas name
  with Not_found ->
    raise (Failure (Printf.sprintf "DEATH. Lemma %s does not exist" name))

let get_proc_specs (prog : ('a, 'b) t) : Spec.t list =
  List.rev
    (Hashtbl.fold
       (fun _ (proc : ('a, 'b) Proc.t) ac ->
         Option.fold ~some:(fun spec -> spec :: ac) ~none:ac proc.proc_spec)
       prog.procs [])
