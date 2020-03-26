open Compcert
open Config_compcert
open CConstants

let burn_csm = ref false

let current_arch =
  if Archi.ptr64 then Architecture.Arch64 else Architecture.Arch32

module TargetLangOptions = struct
  open Cmdliner

  type t = { burn_csm : bool; hide_genv : bool; warnings : bool }

  let term =
    let docs = Manpage.s_common_options in
    let doc = "If you want to write the intermediate C#m program into a file" in
    let bcsm = Arg.(value & flag & info [ "burn-csm" ] ~docs ~doc) in
    let doc =
      "If you want to hide the global environment from the reporting of heap"
    in
    let hgenv = Arg.(value & flag & info [ "hide-genv" ] ~docs ~doc) in
    let doc = "If you want to silent warnings from CompCert and GCC" in
    let no_warnings = Arg.(value & flag & info [ "no-warnings" ] ~docs ~doc) in
    let f burn_csm hide_genv no_warnings =
      { burn_csm; hide_genv; warnings = not no_warnings }
    in
    Term.(const f $ bcsm $ hgenv $ no_warnings)

  let apply { burn_csm = bcsm; hide_genv; warnings } =
    burn_csm := bcsm;
    Config.hide_genv := hide_genv;
    Config.warnings := warnings
end

type err = Errors.errmsg

let pp_err fmt e = Driveraux.print_error fmt e

let get_or_print_and_die = function
  | Errors.OK e    -> e
  | Errors.Error e ->
      Format.printf "%a\n@?" Driveraux.print_error e;
      exit 1

let parse_annots file =
  let parse_with_error token lexbuf =
    try token Annot_lexer.read lexbuf with
    | Annot_lexer.SyntaxError message -> failwith ("SYNTAX ERROR : " ^ message)
    | Annot_parser.Error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let message =
          Printf.sprintf "unexpected token : %s at loc (%i, %i)"
            (Lexing.lexeme lexbuf) curr.pos_lnum
            (curr.pos_cnum - curr.pos_bol + 1)
        in
        failwith ("PARSER ERROR : " ^ message)
  in
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let wprog = parse_with_error Annot_parser.prog lexbuf in
  let () = close_in inx in
  wprog

let parse_and_compile_file path exec_mode =
  let () = Frontend.init () in
  let () =
    if !Config.warnings then Warnings.as_error () else Warnings.silence_all ()
  in
  let () = Optim.disable_all () in
  (* Disable all optims *)
  let pathi = Filename.chop_extension path ^ ".i" in
  let () = Frontend.preprocess path pathi in
  let s = Frontend.parse_c_file path pathi in
  let s = get_or_print_and_die (SimplExpr.transl_program s) in
  let last_clight = get_or_print_and_die (SimplLocals.transf_program s) in
  let csm = get_or_print_and_die (Cshmgen.transl_program last_clight) in
  let () =
    if !burn_csm then
      let pathcsm = Filename.chop_extension path ^ ".csm" in
      let oc = open_out pathcsm in
      let fmt = Format.formatter_of_out_channel oc in
      let () = Format.fprintf fmt "%a" PrintCsharpminor.print_program csm in
      close_out oc
  in
  let annots = parse_annots path in
  Gilgen.trans_program_with_annots exec_mode last_clight csm annots

module Symbol_set = Set.Make (String)

let linker_error msg symbols =
  let () =
    Symbol_set.iter (fun sym -> Printf.printf (msg ^^ " '%s'\n") sym) symbols
  in
  failwith "linker error"

let parse_and_compile_files paths =
  let exec_mode = !Gillian.Utils.Config.current_exec_mode in
  (* Compile all C input files to GIL *)
  let compiled_progs = Hashtbl.create 1 in
  let () =
    List.iter
      (fun path ->
        Hashtbl.add compiled_progs path (parse_and_compile_file path exec_mode))
      paths
  in

  (* Attempt to match all symbol references to definitions *)
  let rec link paths unresolved_syms defined_syms =
    match paths with
    | []           -> unresolved_syms
    | path :: rest ->
        let _, _, symbols = Hashtbl.find compiled_progs path in
        let def, undef = List.partition Gilgen.is_def_sym symbols in
        let def_set = Symbol_set.of_list (List.map Gilgen.sym_name def) in
        let undef_set = Symbol_set.of_list (List.map Gilgen.sym_name undef) in
        let conflicting_defs = Symbol_set.inter defined_syms def_set in
        let () =
          if not (Symbol_set.is_empty conflicting_defs) then
            linker_error "multiple definitions of" conflicting_defs
        in
        let unresolved = Symbol_set.diff unresolved_syms def_set in
        let new_unresolved = Symbol_set.union unresolved undef_set in
        let new_defined = Symbol_set.union defined_syms def_set in
        link rest new_unresolved new_defined
  in
  let unresolved_syms = link paths Symbol_set.empty Symbol_set.empty in
  let () =
    if not (Symbol_set.is_empty unresolved_syms) then
      linker_error "undefined reference to" unresolved_syms
  in

  (* Create main GIL program with references to all other files *)
  let open Gillian.Gil_syntax in
  let open Gillian.Utils in
  let rec combine paths combined_imports combined_init_cmds =
    match paths with
    | []           -> (combined_imports, combined_init_cmds)
    | path :: rest ->
        let prog, init_cmds, _ = Hashtbl.find compiled_progs path in
        let gil_path = Filename.chop_extension path ^ ".gil" in
        let () = Io_utils.save_file_pp gil_path Prog.pp_labeled prog in
        combine rest
          (combined_imports @ [ gil_path ])
          (combined_init_cmds @ init_cmds)
  in
  let imports, init_cmds = combine (List.tl paths) [] [] in
  let main_prog, main_init_cmds, _ =
    Hashtbl.find compiled_progs (List.hd paths)
  in
  let all_imports = Imports.imports current_arch exec_mode @ imports in
  let init_proc = Gilgen.make_init_proc (main_init_cmds @ init_cmds) in
  let init_proc_name = init_proc.Proc.proc_name in
  let () = Hashtbl.add main_prog.procs init_proc_name init_proc in
  Ok
    {
      main_prog with
      imports = all_imports;
      proc_names = init_proc_name :: main_prog.proc_names;
    }

let other_imports = []

let env_var_import_path = Some CConstants.Imports.env_path_var

let initialize = function
  | Gillian.Utils.ExecMode.BiAbduction ->
      let () = Gillian.Utils.Config.bi_unfold_depth := 2 in
      Gillian.Utils.Config.delay_entailment := true
  | Verification -> Gillian.Utils.Config.delay_entailment := false
  | _ -> ()
