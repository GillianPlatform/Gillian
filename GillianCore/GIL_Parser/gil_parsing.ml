open Lexing
module L = Logging

module Preprocess_GCmd = PreProcessing_Utils.M (struct
  type t = int Cmd.t

  let successors = Cmd.successors
end)

module SS = Containers.SS

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

let trans_procs procs path internal_file =
  let procs' = Hashtbl.create Config.small_tbl_size in
  let () =
    Hashtbl.iter
      (fun pname (proc : (Annot.t, string) Proc.t) ->
        let proc_source_path =
          if SS.mem pname !Parser_state.procs_with_no_paths then None
          else Some path
        in
        let proc_internal = proc.proc_internal || internal_file in
        Hashtbl.add procs' pname { proc with proc_source_path; proc_internal })
      procs
  in
  procs'

let trans_preds preds path internal_file =
  let preds' = Hashtbl.create Config.small_tbl_size in
  let internal_file = !Parser_state.internal_file in
  let () =
    Hashtbl.iter
      (fun pname (pred : Pred.t) ->
        let pred_source_path =
          if SS.mem pname !Parser_state.preds_with_no_paths then None
          else Some path
        in
        let pred_internal = pred.pred_internal || internal_file in
        Hashtbl.add preds' pname { pred with pred_source_path; pred_internal })
      preds
  in
  preds'

let parse_eprog_from_file (path : string) : (Annot.t, string) Prog.t =
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
  let internal_file = !Parser_state.internal_file in
  let procs = trans_procs prog.procs path internal_file in
  let preds = trans_preds prog.preds path internal_file in
  Parser_state.reset ();
  { prog with procs; preds }

let cached_progs = Hashtbl.create Config.small_tbl_size

let cache_gil_prog path prog = Hashtbl.add cached_progs path prog

let resolve_path path =
  let lookup_paths = "." :: Config.get_runtime_paths () in
  let rec find fname paths =
    match paths with
    | []           -> failwith (Printf.sprintf "Cannot resolve \"%s\"" fname)
    | path :: rest ->
        let complete_path = Filename.concat path fname in
        if Sys.file_exists complete_path then complete_path else find fname rest
  in
  find path lookup_paths

let remove_dot file_ext = String.sub file_ext 1 (String.length file_ext - 1)

let fetch_imported_prog path other_imports : (Annot.t, string) Prog.t =
  if Hashtbl.mem cached_progs path then Hashtbl.find cached_progs path
  else
    let file = resolve_path path in
    let extension = Filename.extension file in
    let prog =
      if String.equal extension ".gil" then parse_eprog_from_file file
      else
        match List.assoc_opt (remove_dot extension) other_imports with
        | None                   -> failwith
                                      (Printf.sprintf
                                         "Cannot import file \"%s\"" file)
        | Some parse_and_compile -> parse_and_compile file
    in
    cache_gil_prog path prog;
    prog

let combine
    (existing_components : (string, 'a) Hashtbl.t)
    (new_components : (string, 'a) Hashtbl.t)
    (transform : 'a -> 'a)
    (component_type : string) : unit =
  Hashtbl.iter
    (fun comp_name comp ->
      if not (Hashtbl.mem existing_components comp_name) then (
        L.verbose (fun m ->
            m "*** MESSAGE: Adding %s: %s.@\n" component_type comp_name);
        Hashtbl.add existing_components comp_name (transform comp) )
      else
        L.verbose (fun m ->
            m "*** WARNING: %s %s already exists.@\n"
              (String.capitalize_ascii component_type)
              comp_name))
    new_components

let extend_program
    (prog : (Annot.t, string) Prog.t)
    (other_prog : (Annot.t, string) Prog.t)
    (verify_other_prog : bool) : unit =
  let transform_proc proc =
    let open Proc in
    let new_spec =
      Option.map
        (fun spec -> Spec.{ spec with spec_to_verify = verify_other_prog })
        proc.proc_spec
    in
    { proc with proc_spec = new_spec }
  in
  let id x = x in
  combine prog.procs other_prog.procs transform_proc "procedure";
  combine prog.preds other_prog.preds id "predicate";
  combine prog.only_specs other_prog.only_specs id "spec-only procedure";
  combine prog.macros other_prog.macros id "macro";
  combine prog.bi_specs other_prog.bi_specs id "bi-abduction spec"

let resolve_imports
    (program : (Annot.t, string) Prog.t)
    (other_imports : (string * (string -> (Annot.t, string) Prog.t)) list) :
    unit =
  let rec resolve imports added_imports =
    match imports with
    | [] -> ()
    | (file, should_verify) :: rest ->
        if not (SS.mem file added_imports) then
          let imported_prog = fetch_imported_prog file other_imports in
          let () = extend_program program imported_prog should_verify in
          let new_added_imports = SS.add file added_imports in
          resolve (rest @ imported_prog.imports) new_added_imports
        else resolve rest added_imports
  in
  resolve program.imports SS.empty

(** Converts a string-labelled [Prog.t] to an index-labelled [Prog.t], 
    resolving the imports in the meantime. The parameter [other_imports] is an
    association list that maps extensions to a parser and compiler. For example,
    it is possible to import a JSIL file in a GIL program using 
    [import "file.jsil";]. In order to do so, the [other_imports] list should
    contain the tuple [("jsil", parse_and_compile_jsil_file)] where 
    [parse_and_compile_jsil_file] is a function that takes a file path, parses 
    the file as a JSIL program, and compiles this to a GIL program. *)
let eprog_to_prog
    ~(other_imports : (string * (string -> (Annot.t, string) Prog.t)) list)
    (ext_program : (Annot.t, string) Prog.t) : (Annot.t, int) Prog.t =
  let open Prog in
  let () = resolve_imports ext_program other_imports in
  let proc_of_ext_proc (proc : (Annot.t, string) Proc.t) :
      (Annot.t, int) Proc.t * (string * int * int * int) list =
    let open Proc in
    let name = proc.proc_name in
    (* Desugar labels *)
    let proc = Proc.indexed_of_labeled proc in
    (* Get the succ and pred tables *)
    let succ_table, pred_table =
      Preprocess_GCmd.get_succ_pred proc.Proc.proc_body
    in
    (* Compute the which_pred table *)
    let predecessors = Preprocess_GCmd.compute_which_preds pred_table in
    (* Update the global_which_pred table with the correct indexes *)
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
    ----------------------------------------------------- *)
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
