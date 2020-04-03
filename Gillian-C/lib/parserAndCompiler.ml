open Compcert
open Config_compcert
open CConstants

module TargetLangOptions = struct
  open Cmdliner

  type t = {
    include_dirs : string list;
    source_dirs : string list;
    burn_csm : bool;
    hide_genv : bool;
    warnings : bool;
    hide_undef : bool;
    hide_mult_def : bool;
  }

  let term =
    let docs = Manpage.s_common_options in
    let docv = "DIR" in
    let doc =
      "Add $(docv) to the list of directories used to search for included .h \
       files."
    in
    let include_dirs =
      Arg.(value & opt_all dir [] & info [ "I" ] ~docs ~doc ~docv)
    in

    let doc =
      "Add $(docv) to the list of directories used to find .c files to link \
       against. These are searched recursively."
    in
    let source_dirs =
      Arg.(value & opt_all dir [] & info [ "S"; "source" ] ~docs ~doc ~docv)
    in

    let doc = "Write the intermediate C#minor program to a file." in
    let bcsm = Arg.(value & flag & info [ "burn-csm" ] ~docs ~doc) in

    let doc = "Hide the global environment from the heap report." in
    let hgenv = Arg.(value & flag & info [ "hide-genv" ] ~docs ~doc) in

    let doc = "Silence CompCert warnings." in
    let no_warnings = Arg.(value & flag & info [ "no-warnings" ] ~docs ~doc) in

    let doc =
      "Suppress linker errors about undefined symbols. Warning: might cause \
       the C program to crash unexpectedly."
    in
    let hundef = Arg.(value & flag & info [ "ignore-undef" ] ~docs ~doc) in

    let doc =
      "Suppress linker errors about multiple symbol definitions. Warning: \
       might cause the C program to crash unexpectedly."
    in
    let hmultdef = Arg.(value & flag & info [ "ignore-multdef" ] ~docs ~doc) in

    let opt
        include_dirs
        source_dirs
        burn_csm
        hide_genv
        no_warnings
        hide_undef
        hide_mult_def =
      {
        include_dirs;
        source_dirs;
        burn_csm;
        hide_genv;
        warnings = not no_warnings;
        hide_undef;
        hide_mult_def;
      }
    in
    Term.(
      const opt $ include_dirs $ source_dirs $ bcsm $ hgenv $ no_warnings
      $ hundef $ hmultdef)

  let apply
      {
        include_dirs;
        source_dirs;
        burn_csm;
        hide_genv;
        warnings;
        hide_undef;
        hide_mult_def;
      } =
    let rec get_c_paths dirs =
      match dirs with
      | []          -> []
      | dir :: rest ->
          let files = Gillian.Utils.Io_utils.get_files dir in
          let c_files =
            List.filter (fun p -> Filename.extension p = ".c") files
          in
          c_files @ get_c_paths rest
    in
    let c_source_paths = get_c_paths source_dirs in
    Config.include_dirs := include_dirs;
    Config.source_paths := c_source_paths;
    Config.burn_csm := burn_csm;
    Config.hide_genv := hide_genv;
    Config.warnings := warnings;
    Config.hide_undef := hide_undef;
    Config.hide_mult_def := hide_mult_def
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

module Str_set = Gillian.Utils.Containers.SS

let mangle_clashing_vars proc =
  let reserved_names = Str_set.of_list [ "pred"; "pure" ] in
  let mangle var =
    let suffix = if Str_set.mem var reserved_names then "__" else "" in
    var ^ suffix
  in
  let varname_mangler =
    object
      inherit [_] Gillian.Gil_syntax.Visitors.map as super

      method! visit_proc env proc =
        let proc_params = List.map mangle proc.proc_params in
        let proc = super#visit_proc env proc in
        { proc with proc_params }

      method! visit_PVar _ var = Gillian.Gil_syntax.Expr.PVar (mangle var)
    end
  in
  varname_mangler#visit_proc () proc

let parse_and_compile_file path exec_mode =
  let () = Frontend.init () in
  let () =
    if !Config.warnings then Warnings.as_error () else Warnings.silence_all ()
  in
  let () = Optim.disable_all () in
  (* Disable all optimisations *)
  let () = Include.add_include_dirs !Config.include_dirs in
  let pathi = Filename.chop_extension path ^ ".i" in
  let () = Frontend.preprocess path pathi in
  let s = Frontend.parse_c_file path pathi in
  let s = get_or_print_and_die (SimplExpr.transl_program s) in
  let last_clight = get_or_print_and_die (SimplLocals.transf_program s) in
  let csm = get_or_print_and_die (Cshmgen.transl_program last_clight) in
  let () =
    if !Config.burn_csm then
      let pathcsm = Filename.chop_extension path ^ ".csm" in
      let oc = open_out pathcsm in
      let fmt = Format.formatter_of_out_channel oc in
      let () = Format.fprintf fmt "%a" PrintCsharpminor.print_program csm in
      close_out oc
  in
  let annots = parse_annots path in
  let prog, init_asrts, init_cmds, symbols =
    Gilgen.trans_program_with_annots exec_mode last_clight csm annots
  in
  let trans_procs = Hashtbl.create 1 in
  let () =
    Hashtbl.iter
      (fun proc_name proc_body ->
        Hashtbl.add trans_procs proc_name (mangle_clashing_vars proc_body))
      prog.procs
  in
  ({ prog with procs = trans_procs }, init_asrts, init_cmds, symbols)

exception Linker_error

let linker_error msg symbols =
  let () =
    Gilgen.Symbol_set.iter
      (fun sym -> Printf.printf (msg ^^ " '%s'\n") sym)
      symbols
  in
  raise Linker_error

let current_arch =
  if Archi.ptr64 then Architecture.Arch64 else Architecture.Arch32

let add_init_pred exec_mode =
  ExecMode.verification_exec exec_mode || ExecMode.biabduction_exec exec_mode

let parse_and_compile_files paths =
  let open Gilgen in
  let exec_mode = !Gillian.Utils.Config.current_exec_mode in
  (* Compile all C input files to GIL *)
  let paths = paths @ !Config.source_paths in
  let compiled_progs = Hashtbl.create 1 in
  let () =
    List.iter
      (fun path ->
        Hashtbl.add compiled_progs path (parse_and_compile_file path exec_mode))
      paths
  in

  (* Attempt to match all symbol references to definitions *)
  let hide_undef = !Config.hide_undef in
  let hide_mult_def = !Config.hide_mult_def in
  let rec link paths unresolved_syms defined_syms =
    match paths with
    | []           -> unresolved_syms
    | path :: rest ->
        let _, _, _, symbols = Hashtbl.find compiled_progs path in
        let def, undef = List.partition is_def_sym symbols in
        let def_set = Symbol_set.of_list (List.map sym_name def) in
        let undef_set = Symbol_set.of_list (List.map sym_name undef) in
        let conflicting_defs = Symbol_set.inter defined_syms def_set in
        let () =
          if (not (Symbol_set.is_empty conflicting_defs)) && not hide_mult_def
          then linker_error "multiple definitions of" conflicting_defs
        in
        let unresolved = Symbol_set.union unresolved_syms undef_set in
        let new_defined = Symbol_set.union defined_syms def_set in
        let new_unresolved = Symbol_set.diff unresolved new_defined in
        link rest new_unresolved new_defined
  in
  let unresolved_syms = link paths Symbol_set.empty Symbol_set.empty in
  let () =
    if (not (Symbol_set.is_empty unresolved_syms)) && not hide_undef then
      linker_error "undefined reference to" unresolved_syms
  in

  (* Create main GIL program with references to all other files *)
  let open Gillian.Gil_syntax in
  let open Gillian.Utils in
  let rec combine paths comb_imports comb_init_asrts comb_init_cmds =
    match paths with
    | []           -> (comb_imports, comb_init_asrts, comb_init_cmds)
    | path :: rest ->
        let prog, init_asrts, init_cmds, _ = Hashtbl.find compiled_progs path in
        let gil_path = Filename.chop_extension path ^ ".gil" in
        let () = Io_utils.save_file_pp gil_path Prog.pp_labeled prog in
        combine rest
          (comb_imports @ [ (gil_path, true) ])
          (comb_init_asrts @ init_asrts)
          (comb_init_cmds @ init_cmds)
  in
  let imports, init_asrts, init_cmds = combine (List.tl paths) [] [] [] in
  let main_prog, main_init_asrts, main_init_cmds, _ =
    Hashtbl.find compiled_progs (List.hd paths)
  in
  let all_imports = Imports.imports current_arch exec_mode @ imports in
  let init_proc = Gilgen.make_init_proc (main_init_cmds @ init_cmds) in
  let init_proc_name = init_proc.Proc.proc_name in
  let () = Hashtbl.add main_prog.procs init_proc_name init_proc in
  let () =
    if add_init_pred exec_mode then
      let init_pred =
        Gil_logic_gen.make_global_env_pred (main_init_asrts @ init_asrts)
      in
      Hashtbl.add main_prog.preds init_pred.Pred.pred_name init_pred
  in
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
