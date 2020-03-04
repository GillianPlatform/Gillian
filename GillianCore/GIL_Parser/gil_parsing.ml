open Lexing

module Preprocess_GCmd = PreProcessing_Utils.M (struct
  type t = int Cmd.t

  let successors = Cmd.successors
end)

let log_verboser = Logging.verboser

let col pos = pos.pos_cnum - pos.pos_bol + 1

let parse start lexbuf =
  try start GIL_Lexer.read lexbuf with
  | GIL_Lexer.Syntax_error message -> failwith ("Syntax error: " ^ message)
  | GIL_Parser.Error ->
      let loc_start = lexeme_start_p lexbuf in
      let loc_end = lexeme_end_p lexbuf in
      let unexpected_token = lexeme lexbuf in
      let message =
        Printf.sprintf
          "unexpected token : %s at loc %i:%i-%i:%i while reading %s"
          unexpected_token loc_start.pos_lnum (col loc_start) loc_end.pos_lnum
          (col loc_end)
          ( if String.equal loc_start.pos_fname "" then "a string"
          else loc_start.pos_fname )
      in
      failwith ("Parser Error, " ^ message)

let parse_from_string start str =
  let lexbuf = from_string str in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" } in
  try parse start lexbuf with
  | Failure msg ->
      failwith
        (Printf.sprintf "Failed while trying to parse the string :@\n%s@\n%s"
           str msg)
  | _           ->
      failwith
        (Printf.sprintf "Unkown parsing error while parsing the string :@\n%s"
           str)

let parse_eprog_from_string : string -> (Annot.t, string) Prog.t =
  parse_from_string GIL_Parser.gmain_target

let parse_expr_from_string : string -> Expr.t =
  parse_from_string GIL_Parser.top_level_expr_target

let parse_eprog_from_file (path : string) : (Annot.t, string) Prog.t =
  print_endline ("Parsing: " ^ path);
  let extension = List.hd (List.rev (Str.split (Str.regexp "\\.") path)) in
  let file_previously_normalised = String.equal "ngil" extension in
  Config.previously_normalised := file_previously_normalised;
  (* Check that the file is of a valid type *)
  ( match file_previously_normalised || String.equal "gil" extension with
  | true  -> ()
  | false ->
      raise
        (Failure
           (Printf.sprintf "Failed to import %s: not a .gil or .ngil file."
              path)) );
  let inx = open_in path in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path };
  let prog = parse GIL_Parser.gmain_target lexbuf in
  close_in inx;
  prog

(**  Converts a string Prog.t to a Prog.t. Resolving the imports in the meantime.
     The parameter [other_imports] is a association list, that maps extensions to a parser and compiler.
     For example, it is possible to import jsil file in a gil program, using [import file.jsil].
     In order to do so, the [other_imports] list should contain the double [("jsil", parse_and_compile_jsil_file)] where
     [parse_and_compile_jsil_file] is a function that takes a file name, parses that jsil file and compiles it to a gil program.
*)
let eprog_to_prog
    ~(other_imports : (string * (string -> (Annot.t, string) Prog.t)) list)
    (ext_program : (Annot.t, string) Prog.t) : (Annot.t, int) Prog.t =
  let open Prog in
  (* ----------------------------------------------------
      Add the declarations in 'program_from' to 'program_to'.
      -----------------------------------------------------
  *)
  let extend_declarations
      (program_to : (Annot.t, string) Prog.t)
      (program_from : (Annot.t, string) Prog.t) : unit =
    (* Step 1 - Extend the predicates
       * -----------------------------------------------------------------------------------
    *)
    Hashtbl.iter
      (fun pred_name pred ->
        if Hashtbl.mem program_to.preds pred_name then
          log_verboser (fun m ->
              m "*** WARNING: Predicate %s already exists.@\n" pred_name)
        else
          log_verboser (fun m ->
              m "*** MESSAGE: Adding predicate %s.@\n" pred_name);
        Hashtbl.add program_to.preds pred_name pred)
      program_from.preds;
    (* Step 2 - Extend the procedures, except where a procedure with the same name already exists
     * -----------------------------------------------------------------------------------
     *)
    Hashtbl.iter
      (fun proc_name (proc : (Annot.t, string) Proc.t) ->
        if not (Hashtbl.mem program_to.procs proc_name) then (
          log_verboser (fun m ->
              m "*** MESSAGE: Adding procedure: %s.@\n" proc_name);
          let adjusted_proc : (Annot.t, string) Proc.t =
            Proc.
              {
                proc with
                proc_spec =
                  Option.map
                    (fun (spec : Spec.t) ->
                      Spec.{ spec with spec_to_verify = false })
                    proc.proc_spec;
              }
          in
          Hashtbl.add program_to.procs proc_name adjusted_proc )
        else
          log_verboser (fun m ->
              m "*** WARNING: Procedure %s already exists.@\n" proc_name))
      program_from.procs;
    (* Step 3 - Extend the onlyspecs
       * -----------------------------------------------------------------------------------
    *)
    Hashtbl.iter
      (fun proc_name proc ->
        if not (Hashtbl.mem program_to.only_specs proc_name) then (
          log_verboser (fun m ->
              m "*** MESSAGE: Adding onlyspec procedure: %s.@\n" proc_name);
          Hashtbl.add program_to.only_specs proc_name proc )
        else
          log_verboser (fun m ->
              m "*** WARNING: Procedure %s already exists.@\n" proc_name))
      program_from.only_specs;
    (* Step 4 - Extend the macros
       * -----------------------------------------------------------------------------------
    *)
    Hashtbl.iter
      (fun macro_name macro ->
        if not (Hashtbl.mem program_to.macros macro_name) then (
          log_verboser (fun m ->
              m "*** MESSAGE: Adding macro: %s.@\n" macro_name);
          Hashtbl.add program_to.macros macro_name macro )
        else
          log_verboser (fun m ->
              m "*** WARNING: Procedure %s already exists.@\n" macro_name))
      program_from.macros
  in
  let resolve_imports (program : (Annot.t, string) Prog.t) : unit =
    (* 'added_imports' keeps track of the loaded files *)
    (* Step 1 - Create a hashtable 'added_imports' which keeps track of the loaded files
     * -----------------------------------------------------------------------------------
     *)
    let added_imports = Hashtbl.create 32 in
    (* Step 2 - Extend the program with each of the programs in imports
     * -----------------------------------------------------------------------------------
     *)
    let resolve_import_path fname =
      let list_paths = "." :: Config.get_runtime_paths () in
      let rec find fn l =
        match l with
        | []     -> failwith ("Cannot resolve \"" ^ fname ^ "\"")
        | p :: r -> (
            try
              let complete_path = Filename.concat p fname in
              let _ = Unix.stat complete_path in
              complete_path
            with Unix.Unix_error (Unix.ENOENT, "stat", _) -> find fn r )
      in
      find fname list_paths
    in
    let rec resolve_imports_iter imports =
      match imports with
      | []                   -> ()
      | file :: rest_imports ->
          let open Prog in
          if not (Hashtbl.mem added_imports file) then (
            let file = resolve_import_path file in
            let () = Hashtbl.replace added_imports file true in
            let extension = Filename.extension file in
            let imported_program =
              if String.equal extension ".gil" then parse_eprog_from_file file
              else
                match
                  List.assoc_opt
                    (String.sub extension 1 (String.length extension - 1))
                    other_imports
                with
                | None                   -> raise
                                              (Failure "DEATH.resolve_imports")
                | Some parse_and_compile -> parse_and_compile file
            in
            extend_declarations program imported_program;
            resolve_imports_iter (rest_imports @ imported_program.imports) )
    in
    resolve_imports_iter program.imports
  in
  (* Step 1 - Add the declarations from the imported files
     * -----------------------------------------------------------------------------------
  *)
  let () = resolve_imports ext_program in
  let proc_of_ext_proc (proc : (Annot.t, string) Proc.t) :
      (Annot.t, int) Proc.t * (string * int * int * int) list =
    let open Proc in
    let name = proc.proc_name in
    (* Step 2.1 - Desugar labels
     * -----------------------------------------------------------------------------------
     *)
    let proc = Proc.indexed_of_labeled proc in
    (* Step 2.2 - Get the succ and pred tables
     * -----------------------------------------------------------------------------------
     *)
    let succ_table, pred_table =
      Preprocess_GCmd.get_succ_pred proc.Proc.proc_body
    in
    (* Step 2.3 - Compute the which_pred table
     * -----------------------------------------------------------------------------------
     *)
    let predecessors = Preprocess_GCmd.compute_which_preds pred_table in
    (* Step 2.4 - Update the global_which_pred table with the correct indexes
       * -----------------------------------------------------------------------------------
    *)
    let predecessors' =
      List.map
        (fun (prev_cmd, cur_cmd, i) -> (name, prev_cmd, cur_cmd, i))
        predecessors
    in
    (proc, predecessors')
  in
  let procs, predecessors =
    Hashtbl.fold
      (fun (name : string) (proc : (Annot.t, string) Proc.t)
           (procs, predecessors) ->
        let proc, new_predecessors = proc_of_ext_proc proc in
        (proc :: procs, new_predecessors @ predecessors))
      ext_program.procs ([], [])
  in
  Prog.make_indexed ~lemmas:ext_program.lemmas ~preds:ext_program.preds
    ~only_specs:ext_program.only_specs ~procs ~predecessors
    ~macros:ext_program.macros ~bi_specs:ext_program.bi_specs ()

(** ----------------------------------------------------
    Parse a line_numbers file.
    Proc: proc_name
    (0, 0)
    ...
    -----------------------------------------------------
*)
let parse_line_numbers (ln_str : string) : (string * int, int * bool) Hashtbl.t
    =
  let strs = Str.split (Str.regexp_string "Proc: ") ln_str in
  let line_info = Hashtbl.create Config.big_tbl_size in
  List.iter
    (fun str ->
      let memory = Hashtbl.create Config.small_tbl_size in
      let index = String.index str '\n' in
      let proc_name = String.sub str 0 index in
      let proc_line_info =
        String.sub str (index + 1) (String.length str - (index + 1))
      in
      let lines = Str.split (Str.regexp_string "\n") proc_line_info in
      List.iter
        (fun line ->
          Scanf.sscanf line "(%d, %d)" (fun x y ->
              if Hashtbl.mem memory y then
                Hashtbl.replace line_info (proc_name, x) (y, false)
              else (
                Hashtbl.replace memory y true;
                Hashtbl.replace line_info (proc_name, x) (y, true) )))
        lines)
    strs;
  line_info
