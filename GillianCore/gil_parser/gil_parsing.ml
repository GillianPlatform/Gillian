open Lexing
open Syntaxes.Result
module L = Logging
include Gil_parsing_intf

module Preprocess_GCmd = Preprocessing_utils.M (struct
  type t = int Cmd.t

  let successors = Cmd.successors
end)

module Make (Annot : Annot.S) = struct
  type annot = Annot.t

  module GIL_Lexer = GIL_Lexer.Make (Annot)
  module GIL_Parser = GIL_Parser.Make (Annot)

  type parsing_result = {
    labeled_prog : (annot, string) Prog.t;
    init_data : Yojson.Safe.t;  (** Will be `Null if no [init_data] is parsed *)
  }

  let get_loc_from_lexbuf lexbuf =
    let to_position loc : Location.position =
      { pos_line = loc.pos_lnum; pos_column = loc.pos_cnum - loc.pos_bol }
    in
    let loc_source, loc_start =
      let loc_start = lexeme_start_p lexbuf in
      (loc_start.pos_fname, to_position loc_start)
    in
    let loc_end = to_position (lexeme_end_p lexbuf) in
    Location.{ loc_start; loc_end; loc_source }

  let parse start lexbuf =
    let open Gillian_result in
    try Ok (start GIL_Lexer.read lexbuf) with
    | GIL_Lexer.Syntax_error message ->
        let loc = get_loc_from_lexbuf lexbuf in
        let msg = "Syntax error: " ^ message in
        compilation_error ~loc msg
    | GIL_Parser.Error ->
        let loc = get_loc_from_lexbuf lexbuf in
        compilation_error ~loc
          ("Syntax error: Unexpected token " ^ Lexing.lexeme lexbuf)

  let parse_from_string start str =
    let open Gillian_result in
    let lexbuf = from_string str in
    let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" } in
    try
      match parse start lexbuf with
      | Error (Gillian_result.Error.CompilationError { msg; _ }) ->
          let msg =
            Fmt.str "Failed while trying to parse the string:@\n%s@\n%s" str msg
          in
          compilation_error msg
      | res -> res
    with _ ->
      let msg =
        Fmt.str "Unkown parsing error while parsing the string:@\n%s" str
      in
      compilation_error msg

  let parse_eprog_from_string str : parsing_result Gillian_result.t =
    let+ labeled_prog, init_data =
      parse_from_string GIL_Parser.gmain_target str
    in
    { labeled_prog; init_data }

  let trans_procs procs path internal_file =
    let procs' = Hashtbl.create Config.small_tbl_size in
    let () =
      Hashtbl.iter
        (fun name (proc : (annot, string) Proc.t) ->
          let proc_source_path =
            if SS.mem name !Parser_state.procs_with_no_paths then None
            else Some path
          in
          let proc_internal = proc.proc_internal || internal_file in
          Hashtbl.add procs' name { proc with proc_source_path; proc_internal })
        procs
    in
    procs'

  let trans_preds preds path internal_file =
    let preds' = Hashtbl.create Config.small_tbl_size in
    let () =
      Hashtbl.iter
        (fun name (pred : Pred.t) ->
          let pred_source_path =
            if SS.mem name !Parser_state.preds_with_no_paths then None
            else Some path
          in
          let pred_internal = pred.pred_internal || internal_file in
          Hashtbl.add preds' name { pred with pred_source_path; pred_internal })
        preds
    in
    preds'

  let trans_lemmas lemmas path internal_file =
    let lemmas' = Hashtbl.create Config.small_tbl_size in
    let () =
      Hashtbl.iter
        (fun name (lemma : Lemma.t) ->
          let lemma_source_path =
            if SS.mem name !Parser_state.lemmas_with_no_paths then None
            else Some path
          in
          let lemma_internal = lemma.lemma_internal || internal_file in
          Hashtbl.add lemmas' name
            { lemma with lemma_source_path; lemma_internal })
        lemmas
    in
    lemmas'

  let parse_eprog_from_file (path : string) : parsing_result Gillian_result.t =
    let f path =
      (* Check that the file is of a valid type *)
      let extension = Filename.extension path in
      let prev_normalised = String.equal extension ".ngil" in
      let () = Config.previously_normalised := prev_normalised in
      let () =
        if not (prev_normalised || String.equal extension ".gil") then
          failwith (Printf.sprintf "Error: %s is not a .gil or .ngil file" path)
      in
      (* Parse file *)
      let in_channel = open_in path in
      let lexbuf = Lexing.from_channel in_channel in
      let () =
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path }
      in
      let+ prog, init_data =
        try parse GIL_Parser.gmain_target lexbuf
        with exn ->
          Fmt.epr "In file at path: %s\n" path;
          raise exn
      in
      let () = close_in in_channel in

      (* Correctly label components that have @internal and/or @nopath directives *)
      let internal_file = !Parser_state.internal_file in
      let procs = trans_procs prog.procs path internal_file in
      let preds = trans_preds prog.preds path internal_file in
      let lemmas = trans_lemmas prog.lemmas path internal_file in
      Parser_state.reset ();
      { labeled_prog = { prog with procs; preds; lemmas }; init_data }
    in
    L.Phase.with_normal ~title:"Program parsing" (fun () -> f path)

  let cached_progs = Hashtbl.create Config.small_tbl_size
  let cache_gil_prog path prog = Hashtbl.add cached_progs path prog

  let cache_labelled_progs (progs : (string * (annot, string) Prog.t) list) =
    List.iter (fun (path, prog) -> cache_gil_prog path prog) progs

  let resolve_path ?rel path =
    let rel =
      match rel with
      | Some rel -> Filename.dirname rel
      | None -> "."
    in
    let runtime_paths = Config.get_runtime_paths () in
    if Filename.is_relative path then
      let lookup_paths = rel :: runtime_paths in
      let rec find fname paths =
        match paths with
        | [] ->
            Fmt.failwith "Cannot resolve \"%s\", looked in %a and ." fname
              Fmt.(list ~sep:(any ", ") string)
              runtime_paths
        | path :: rest ->
            let complete_path = Filename.concat path fname in
            if Sys.file_exists complete_path then complete_path
            else find fname rest
      in
      find path lookup_paths
    else if Sys.file_exists path then path
    else
      Fmt.failwith "Cannot resolve absolute path \"%s\", looked in %a and ."
        path
        Fmt.(list ~sep:(any ", ") string)
        runtime_paths

  let remove_dot file_ext = String.sub file_ext 1 (String.length file_ext - 1)

  let fetch_imported_prog ?rel path other_imports :
      (annot, string) Prog.t Gillian_result.t =
    match Hashtbl.find_opt cached_progs path with
    | Some prog -> Ok prog
    | None ->
        let file = resolve_path ?rel path in
        let extension = Filename.extension file in
        let+ prog =
          if String.equal extension ".gil" then
            let+ result = parse_eprog_from_file file in
            match result.init_data with
            | `Null -> result.labeled_prog
            | _ -> failwith "imported file had init_data, don't know what to do"
          else
            match List.assoc_opt (remove_dot extension) other_imports with
            | None -> failwith (Printf.sprintf "Cannot import file \"%s\"" file)
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
          Hashtbl.add existing_components comp_name (transform comp))
        else
          L.verbose (fun m ->
              m "*** WARNING: %s %s already exists.@\n"
                (String.capitalize_ascii component_type)
                comp_name))
      new_components

  let extend_program
      (prog : (annot, string) Prog.t)
      (other_prog : (annot, string) Prog.t)
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
    combine prog.lemmas other_prog.lemmas id "lemmas";
    combine prog.macros other_prog.macros id "macro";
    combine prog.bi_specs other_prog.bi_specs id "bi-abduction spec"

  let resolve_imports
      ?(prog_path : string option)
      (program : (annot, string) Prog.t)
      (other_imports :
        (string * (string -> (annot, string) Prog.t Gillian_result.t)) list) :
      unit Gillian_result.t =
    let rec resolve imports added_imports =
      match imports with
      | [] -> Ok ()
      | (file, should_verify) :: rest ->
          if not (SS.mem file added_imports) then
            let* imported_prog =
              fetch_imported_prog file ?rel:prog_path other_imports
            in
            let () = extend_program program imported_prog should_verify in
            let new_added_imports = SS.add file added_imports in
            resolve (rest @ imported_prog.imports) new_added_imports
          else resolve rest added_imports
    in
    resolve program.imports SS.empty

  let eprog_to_prog ?prog_path ~other_imports ext_program =
    let open Prog in
    let+ () = resolve_imports ?prog_path ext_program other_imports in
    let proc_of_ext_proc (proc : (annot, string) Proc.t) :
        (annot, int) Proc.t * (string * int * int * int) list =
      let open Proc in
      let name = proc.proc_name in
      (* Desugar labels *)
      let proc = Proc.indexed_of_labeled proc in
      (* Get the succ and pred tables *)
      let _, pred_table = Preprocess_GCmd.get_succ_pred proc.Proc.proc_body in
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
        (fun (_ : string) (proc : (annot, string) Proc.t) (procs, predecessors) ->
          let proc, new_predecessors = proc_of_ext_proc proc in
          (proc :: procs, new_predecessors @ predecessors))
        ext_program.procs ([], [])
    in
    Prog.make_indexed ~lemmas:ext_program.lemmas ~preds:ext_program.preds
      ~only_specs:ext_program.only_specs ~procs ~predecessors
      ~macros:ext_program.macros ~bi_specs:ext_program.bi_specs ()

  let parse_literal lexbuf = parse GIL_Parser.lit_target lexbuf
  let parse_expression lexbuf = parse GIL_Parser.top_level_expr_target lexbuf
end
