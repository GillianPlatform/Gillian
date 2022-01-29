module L = Logging

type t = {
  imports : (string * bool) list;
  (* Import statements = [Filename : String] *)
  lemmas : (string, Lemma.t) Hashtbl.t;
  (* Lemmas *)
  preds : (string, Pred.t) Hashtbl.t;
  (* Predicates = Name : String --> Definition *)
  only_specs : (string, Spec.t) Hashtbl.t;
  (* Specs = Name : String --> Spec *)
  procs : (string, EProc.t) Hashtbl.t;
  (* JSIL extended procedures = Name : String --> Procedure *)
  macros : (string, Macro.t) Hashtbl.t;
  (* macros *)
  bi_specs : (string, BiSpec.t) Hashtbl.t;
  (* Hints for bi-abduction *)
  proc_names : string list;
}

let init
    (imports : (string * bool) list)
    (lemmas : (string, Lemma.t) Hashtbl.t)
    (preds : (string, Pred.t) Hashtbl.t)
    (only_specs : (string, Spec.t) Hashtbl.t)
    (procs : (string, EProc.t) Hashtbl.t)
    (macros : (string, Macro.t) Hashtbl.t)
    (bi_specs : (string, BiSpec.t) Hashtbl.t)
    (proc_names : string list) : t =
  { imports; lemmas; preds; only_specs; procs; macros; bi_specs; proc_names }

let full_init () =
  let open Config in
  let imports = [] in
  let lemmas = Hashtbl.create medium_tbl_size in
  let preds = Hashtbl.create big_tbl_size in
  let only_specs = Hashtbl.create medium_tbl_size in
  let procs = Hashtbl.create big_tbl_size in
  let macros = Hashtbl.create small_tbl_size in
  let bi_specs = Hashtbl.create big_tbl_size in
  let proc_names = [] in
  init imports lemmas preds only_specs procs macros bi_specs proc_names

let get_lemmas (prog : t) : Lemma.t list =
  Hashtbl.fold (fun _ lemma ac -> lemma :: ac) prog.lemmas []

let get_preds (prog : t) : Pred.t list =
  Hashtbl.fold (fun _ lemma ac -> lemma :: ac) prog.preds []

let get_ospecs (prog : t) : Spec.t list =
  Hashtbl.fold (fun _ spec ac -> spec :: ac) prog.only_specs []

let get_specs (prog : t) : Spec.t list =
  Hashtbl.fold
    (fun _ (proc : EProc.t) ac ->
      Option.fold ~some:(fun spec -> spec :: ac) ~none:ac proc.spec)
    prog.procs (get_ospecs prog)

let get_procs ?(proc_names : string list option) (prog : t) : EProc.t list =
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

let get_proc (prog : t) (proc_name : string) : EProc.t option =
  Hashtbl.find_opt prog.procs proc_name

let get_bispecs (prog : t) : BiSpec.t list =
  Hashtbl.fold (fun _ bi_spec ac -> bi_spec :: ac) prog.bi_specs []

let pp fmt prog =
  let pp_list ppp = Fmt.list ~sep:(Fmt.any "@\n@\n") ppp in
  let pp_imports fmt = function
    | [] -> ()
    | imps -> Fmt.pf fmt "import %a;" (Fmt.list ~sep:Fmt.comma Fmt.string) imps
  in
  (* let _ = List.for_all (fun name -> Hashtbl.mem prog.procs name) (prog.proc_names) in *)
  let pp_only_spec fmt' spec = Fmt.pf fmt' "only %a" Spec.pp spec in
  Fmt.pf fmt "%a@\n@\n@\n%a@\n@\n@\n%a@\n@\n@\n%a@\n@\n@\n%a@\n@\n@\n%a"
    pp_imports
    (let imports, _ = List.split prog.imports in
     imports)
    (pp_list Lemma.pp) (get_lemmas prog) (pp_list Pred.pp) (get_preds prog)
    (pp_list pp_only_spec) (get_ospecs prog) (pp_list BiSpec.pp)
    (get_bispecs prog) (pp_list EProc.pp)
    (get_procs ~proc_names:prog.proc_names prog)

let update_imports (prog : t) (imports : string list) : t =
  { prog with imports = List.map (fun i -> (i, false)) imports }

let add_lemma (prog : t) (lemma : Lemma.t) : t =
  Hashtbl.add prog.lemmas lemma.name lemma;
  prog

let add_pred (prog : t) (pred : Pred.t) : t =
  Hashtbl.add prog.preds pred.name pred;
  prog

let add_ospec (prog : t) (spec : Spec.t) : t =
  Hashtbl.add prog.only_specs spec.name spec;
  prog

let add_proc (prog : t) (proc : EProc.t) : t =
  Hashtbl.add prog.procs proc.name proc;
  { prog with proc_names = proc.name :: prog.proc_names }

let add_macro (prog : t) (macro : Macro.t) : t =
  Hashtbl.add prog.macros macro.name macro;
  prog

let add_bispec (prog : t) (bi_spec : BiSpec.t) : t =
  Hashtbl.add prog.bi_specs bi_spec.name bi_spec;
  prog
