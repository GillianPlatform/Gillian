open Compcert
open Config_compcert
open CConstants
module SS = Gillian.Utils.Containers.SS

let small_tbl_size = Gillian.Utils.Config.small_tbl_size

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
    verbose_compcert : bool;
    fstruct_passing : bool;
    fpacked_structs : bool;
    pp_full_trees : bool;
    allocated_functions : bool;
    alloc_can_fail : bool;
    cbmc : bool;
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

    let doc = "Verbose Compcert invocation" in
    let verbose_compcert =
      Arg.(value & flag & info [ "verbose-compcert" ] ~docs ~doc)
    in
    let doc =
      "Enable CompCert's struct-passing feature, which lets one pass \
       structures and unions as parameters and return values"
    in
    let fstruct_passing =
      Arg.(value & flag & info [ "fstruct-passing" ] ~docs ~doc)
    in
    let doc =
      "Enable CompCert's packed-struct feature, which lets one specify \
       'packed' structs, which ensures no gaps beween fields."
    in
    let fpacked_structs =
      Arg.(value & flag & info [ "fpacked-structs" ] ~docs ~doc)
    in
    let doc =
      "Show full symbolic heap trees in the log, otherwise, just shows a \
       flattened version"
    in
    let pp_full_trees =
      Arg.(value & flag & info [ "pp-full-trees" ] ~docs ~doc)
    in
    let doc =
      "Allocate some memory for every function. This is the correct behaviour \
       of C, but it is only relevant if the analysed program compares function \
       pointers. If enabled, this has a significant impact on performances \
       since a memory core predicate has to be consumed and then produced at \
       every function call, for every function existing in the program."
    in
    let allocated_functions =
      Arg.(value & flag & info [ "allocated-functions" ] ~docs ~doc)
    in
    let doc = "If flag is set, allocation may fail and return NULL." in
    let alloc_can_fail =
      Arg.(value & flag & info [ "alloc-can-fail" ] ~docs ~doc)
    in
    let doc =
      "Enable CBMC builtin functions for assert, assume, nondet, etc."
    in
    let cbmc = Arg.(value & flag & info [ "cbmc" ] ~docs ~doc) in
    let opt
        include_dirs
        source_dirs
        burn_csm
        hide_genv
        no_warnings
        hide_undef
        hide_mult_def
        verbose_compcert
        fstruct_passing
        fpacked_structs
        pp_full_trees
        allocated_functions
        alloc_can_fail
        cbmc =
      {
        include_dirs;
        source_dirs;
        burn_csm;
        hide_genv;
        warnings = not no_warnings;
        hide_undef;
        hide_mult_def;
        verbose_compcert;
        fstruct_passing;
        fpacked_structs;
        pp_full_trees;
        allocated_functions;
        alloc_can_fail;
        cbmc;
      }
    in
    Term.(
      const opt $ include_dirs $ source_dirs $ bcsm $ hgenv $ no_warnings
      $ hundef $ hmultdef $ verbose_compcert $ fstruct_passing $ fpacked_structs
      $ pp_full_trees $ allocated_functions $ alloc_can_fail $ cbmc)

  let apply
      {
        include_dirs;
        source_dirs;
        burn_csm;
        hide_genv;
        warnings;
        hide_undef;
        hide_mult_def;
        verbose_compcert;
        fstruct_passing;
        fpacked_structs;
        pp_full_trees;
        allocated_functions;
        alloc_can_fail;
        cbmc;
      } =
    let rec get_c_paths dirs =
      match dirs with
      | [] -> []
      | dir :: rest ->
          let files = Gillian.Utils.Io_utils.get_files dir in
          let c_files =
            List.filter (fun p -> Filename.extension p = ".c") files
          in
          c_files @ get_c_paths rest
    in
    Config.include_dirs := include_dirs @ !Config.include_dirs;
    Config.source_paths := get_c_paths source_dirs;
    Config.burn_csm := burn_csm;
    Config.hide_genv := hide_genv;
    Config.warnings := warnings;
    Config.hide_undef := hide_undef;
    Config.hide_mult_def := hide_mult_def;
    Config.verbose_compcert := verbose_compcert;
    Config_compcert.Features.set_fstruct_passing fstruct_passing;
    Config_compcert.Features.set_fpacked_structs fpacked_structs;
    Config.pp_full_tree := pp_full_trees;
    Config.allocated_functions := allocated_functions;
    Config.alloc_can_fail := alloc_can_fail;
    Config.cbmc := cbmc
end

type init_data = Global_env.t

(** Cache of compiled but unlinked GIL programs and their compilation data. *)
let compiled_progs = Hashtbl.create small_tbl_size

(** Cache of header paths included in each source .c path. *)
let included_headers_cache = Hashtbl.create small_tbl_size

let get_gil_path c_path = Filename.chop_extension c_path ^ ".gil"
let get_deps_path c_path = Filename.chop_extension c_path ^ ".deps"

type err = Errors.errmsg

(* TODO: Include some form of the C AST *)
type tl_ast = unit

module Annot = Gillian.Gil_syntax.Annot.Basic

let pp_err fmt e = Driveraux.print_error fmt e

let get_or_print_and_die = function
  | Errors.OK e -> e
  | Errors.Error e ->
      Format.printf "%a\n@?" Driveraux.print_error e;
      exit 1

let parse_annots file =
  let parse_with_error token lexbuf =
    try token Annot_lexer.read lexbuf with
    | Annot_lexer.SyntaxError message ->
        Fmt.failwith "SYNTAX ERROR in %s, line %i: %s" file
          lexbuf.Lexing.lex_curr_p.pos_lnum message
    | Annot_parser.Error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let message =
          Printf.sprintf "unexpected token : %s at loc (%i, %i)"
            (Lexing.lexeme lexbuf) curr.pos_lnum
            (curr.pos_cnum - curr.pos_bol + 1)
        in
        failwith ("PARSER ERROR in " ^ file ^ " : " ^ message)
  in
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let wprog = parse_with_error Annot_parser.prog lexbuf in
  let () = close_in inx in
  wprog

let mangle_proc proc mangled_syms =
  let reserved_names = SS.of_list [ "pred"; "pure" ] in
  let mangle_var var =
    let prefix = if SS.mem var reserved_names then "v__" else "" in
    prefix ^ var
  in
  let mangle_symbol sym =
    match Hashtbl.find_opt mangled_syms sym with
    | Some mangled_sym -> mangled_sym
    | None -> sym
  in
  let mangling_visitor =
    object
      inherit [_] Gillian.Gil_syntax.Visitors.endo as super

      method! visit_proc env proc =
        let proc_params = List.map mangle_var proc.proc_params in
        let proc = super#visit_proc env proc in
        { proc with proc_params }

      method! visit_PVar _ _ var = Gillian.Gil_syntax.Expr.PVar (mangle_var var)

      method! visit_Loc _ _ str =
        Gillian.Gil_syntax.Literal.Loc (mangle_symbol str)
    end
  in
  mangling_visitor#visit_proc () proc

let optimise_calls_in_proc proc genv =
  let get_fname loc = Global_env.find_function_opt genv loc in
  let call_replacer =
    object
      inherit [_] Gillian.Gil_syntax.Visitors.endo

      method! visit_cmd _ cmd =
        match cmd with
        | Call
            ( x,
              Lit (String fname),
              [ EList [ Lit (Loc l); Lit (Int z) ] ],
              None,
              None )
          when fname = Internal_Functions.get_function_name && Z.(equal z zero)
          -> (
            match get_fname l with
            | Some fname -> Assignment (x, Lit (String fname))
            | None ->
                cmd
                (* Can only happen when hide_undef is true, in which case we safely ignore.
                   If hide_undef was false, the missing definition would be caught earlier *)
            )
        | _ -> cmd
    end
  in
  call_replacer#visit_proc () proc

let optimise_calls prog genv =
  Hashtbl.filter_map_inplace
    (fun _ proc -> Some (optimise_calls_in_proc proc genv))
    prog.Gil_syntax.Prog.procs

let write_dependencies_file c_path =
  let prev_options = Preprocessor.get_options () in
  Preprocessor.set_output_dependencies_opts (get_deps_path c_path);
  Frontend.preprocess c_path "-";
  Preprocessor.restore_options prev_options

let parse_and_compile_file path exec_mode =
  let pathi = Filename.chop_extension path ^ ".i" in
  let () = Frontend.preprocess path pathi in
  let () = write_dependencies_file path in
  let c_prog = Frontend.parse_c_file path pathi in
  let clight = get_or_print_and_die (SimplExpr.transl_program c_prog) in
  let last_clight = get_or_print_and_die (SimplLocals.transf_program clight) in
  let csm = get_or_print_and_die (Cshmgen.transl_program last_clight) in
  let () =
    if !Config.burn_csm then
      let pathcsm = Filename.chop_extension path ^ ".csm" in
      let oc = open_out pathcsm in
      let fmt = Format.formatter_of_out_channel oc in
      let () = Format.fprintf fmt "%a" PrintCsharpminor.print_program csm in
      close_out oc
  in
  let mangled_syms = Hashtbl.create small_tbl_size in
  let annots = parse_annots path in
  let prog, compilation_data =
    Gilgen.trans_program_with_annots ~exec_mode ~clight_prog:last_clight
      ~filepath:path ~mangled_syms csm annots
  in
  let trans_procs = Hashtbl.create small_tbl_size in
  let () =
    Hashtbl.iter
      (fun proc_name proc_body ->
        Hashtbl.add trans_procs proc_name (mangle_proc proc_body mangled_syms))
      prog.procs
  in
  ({ prog with procs = trans_procs }, compilation_data)

exception Linker_error

let linker_error msg symbols =
  let () = SS.iter (fun sym -> Fmt.pr "%s '%s'\n@?" msg sym) symbols in
  raise Linker_error

let current_arch =
  if Archi.ptr64 then Architecture.Arch64 else Architecture.Arch32

let parse_dependencies_file deps_file =
  let file_str = Gillian.Utils.Io_utils.load_file deps_file in
  let delims = Str.regexp "[ \\( \\\\\\)\n\r\t]+" in
  let parts = Str.split delims file_str in
  let header_path = Str.regexp ".*\\.h" in
  List.filter (fun part -> Str.string_match header_path part 0) parts

let get_included_headers deps_file =
  if Hashtbl.mem included_headers_cache deps_file then
    Hashtbl.find included_headers_cache deps_file
  else
    let included_headers = parse_dependencies_file deps_file in
    Hashtbl.add included_headers_cache deps_file included_headers;
    included_headers

let create_compilation_result gil_progs genv =
  let open IncrementalAnalysis in
  let open Command_line.ParserAndCompiler in
  let source_files = SourceFiles.make () in
  let gil_progs =
    List.map
      (fun (path, prog) ->
        let () = SourceFiles.add_source_file source_files ~path in
        let headers = get_included_headers (get_deps_path path) in
        let () =
          List.iter
            (fun header ->
              SourceFiles.add_dependency source_files ~path:header
                ~dependent_path:path)
            headers
        in
        (get_gil_path path, prog))
      gil_progs
  in
  { gil_progs; source_files; tl_ast = (); init_data = genv }

let parse_and_compile_files paths =
  let exec_mode = !Gillian.Utils.Config.current_exec_mode in
  (* Compile all C input files to GIL *)
  let paths = paths @ !Config.source_paths in
  let () =
    List.iter
      (fun path ->
        if not (Hashtbl.mem compiled_progs path) then
          Hashtbl.add compiled_progs path
            (parse_and_compile_file path exec_mode))
      paths
  in
  (* Attempt to match all symbol references to definitions *)
  let hide_undef = !Config.hide_undef in
  let hide_mult_def = !Config.hide_mult_def in
  let rec link paths unresolved_syms defined_syms =
    match paths with
    | [] -> unresolved_syms
    | path :: rest ->
        let _, Gilgen.{ symbols; _ } = Hashtbl.find compiled_progs path in
        let def, undef = List.partition Gilgen.is_def_sym symbols in
        let def_set = SS.of_list (List.map Gilgen.sym_name def) in
        let undef_set = SS.of_list (List.map Gilgen.sym_name undef) in
        let conflicting_defs = SS.inter defined_syms def_set in
        let () =
          if (not (SS.is_empty conflicting_defs)) && not hide_mult_def then
            linker_error "multiple definitions of" conflicting_defs
        in
        let unresolved = SS.union unresolved_syms undef_set in
        let new_defined = SS.union defined_syms def_set in
        let new_unresolved = SS.diff unresolved new_defined in
        link rest new_unresolved new_defined
  in
  let unresolved_syms = link paths SS.empty SS.empty in
  let to_ignore =
    let compcert_ignores = Config_compcert.references_to_ignore in
    let cbmc_ignores =
      if !Config.cbmc then
        CBMC_Builtin_Functions.[ assert_; assume; nondet_int ]
      else []
    in
    compcert_ignores @ cbmc_ignores |> SS.of_list
  in
  let unresolved_syms = SS.diff unresolved_syms to_ignore in
  let () =
    if (not (SS.is_empty unresolved_syms)) && not hide_undef then
      linker_error "undefined reference to" unresolved_syms
  in

  (* Create main GIL program with references to all other files *)
  let open Gillian.Gil_syntax in
  let rec combine paths comb_imports comb_defs comb_init_cmds =
    match paths with
    | [] -> (comb_imports, comb_defs, comb_init_cmds)
    | path :: rest ->
        let _, Gilgen.{ genv_defs; genv_init_cmds; _ } =
          Hashtbl.find compiled_progs path
        in
        combine rest
          (comb_imports @ [ (get_gil_path path, true) ])
          (comb_defs @ genv_defs)
          (comb_init_cmds @ genv_init_cmds)
  in
  let imports, genv_defs, init_cmds = combine (List.tl paths) [] [] [] in
  (* defs and init_cmds can contain duplicated things
      in the case of memcpy or equivalents. We need to remove those duplicates *)
  let genv_defs =
    List.sort_uniq (fun (n1, _) (n2, _) -> String.compare n1 n2) genv_defs
  in
  let main_path = List.hd paths in
  let main_prog, Gilgen.{ genv_defs = main_genv_defs; genv_init_cmds; _ } =
    Hashtbl.find compiled_progs (List.hd paths)
  in
  let init_cmds =
    List.sort_uniq
      (fun a b ->
        match (a, b) with
        | ( Cmd.Call (_, _, Lit (Loc a) :: _, _, _),
            Cmd.Call (_, _, Lit (Loc b) :: _, _, _) ) -> String.compare a b
        | _ -> failwith "Wrong init cmd")
      (init_cmds @ genv_init_cmds)
  in
  let all_imports = Imports.imports current_arch exec_mode @ imports in
  let init_proc = Gilgen.make_init_proc init_cmds in
  let init_proc_name = init_proc.Proc.proc_name in
  let all_proc_names = init_proc_name :: main_prog.proc_names in
  let () = Hashtbl.add main_prog.procs init_proc_name init_proc in
  let main_prog =
    {
      main_prog with
      imports = all_imports @ main_prog.imports;
      proc_names = all_proc_names;
    }
  in
  let paths_set = SS.of_list paths in
  let all_other_progs =
    Hashtbl.fold
      (fun path (prog, _) acc ->
        if (not (String.equal path main_path)) && SS.mem path paths_set then
          (path, prog) :: acc
        else acc)
      compiled_progs []
  in
  let all_defs = genv_defs @ main_genv_defs in
  let genv = Global_env.of_definition_list all_defs in
  let all_progs = (main_path, main_prog) :: all_other_progs in
  let () = List.iter (fun (_, prog) -> optimise_calls prog genv) all_progs in
  Ok (create_compilation_result all_progs genv)

let other_imports = []
let default_import_paths = Some Runtime_sites.Sites.runtime

let init_compcert () =
  Frontend.init ();
  if !Config.warnings then Warnings.as_error () else Warnings.silence_all ();
  if !Config.verbose_compcert then Config_compcert.set_verbose_invocation ();
  Optimisations.disable_all ();
  Preprocessor.add_include_dirs !Config.include_dirs;
  Preprocessor.set_gnuc_for_macos ()

let initialize exec_mode =
  init_compcert ();
  match exec_mode with
  | Gillian.Utils.Exec_mode.BiAbduction ->
      Gillian.Utils.Config.bi_unfold_depth := 2;
      Gillian.Utils.Config.delay_entailment := true
  | Verification -> Gillian.Utils.Config.delay_entailment := true
  | _ -> ()
