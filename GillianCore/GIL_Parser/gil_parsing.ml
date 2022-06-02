open Lexing
module L = Logging

module Preprocess_GCmd = PreProcessing_Utils.M (struct
  type t = int Cmd.t

  let successors = Cmd.successors
end)

(** Used to avoid redundant parsing. *)

(* let cached_progs = Hashtbl.create Config.small_tbl_size *)

(* let cache_gil_prog path prog = Hashtbl.add cached_progs path prog *)

(* let cache_labelled_progs (progs : (string * (Annot.t, string) Prog.t) list) =
   List.iter
     (fun (path, prog) ->
       if not (Hashtbl.mem cached_progs path) then cache_gil_prog path prog)
     progs *)

(** Used to avoid redundant parsing. *)

(* let cached_progs = Hashtbl.create Config.small_tbl_size *)

(* let cache_gil_prog path prog = Hashtbl.add cached_progs path prog *)

(* let cache_labelled_progs (progs : (string * (Annot.t, string) Prog.t) list) =
   List.iter
     (fun (path, prog) ->
       if not (Hashtbl.mem cached_progs path) then cache_gil_prog path prog)
     progs *)

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
          "unexpected token: %s at loc %i:%i-%i:%i while reading %s"
          unexpected_token loc_start.pos_lnum (col loc_start) loc_end.pos_lnum
          (col loc_end)
          (if String.equal loc_start.pos_fname "" then "a string"
          else loc_start.pos_fname)
      in
      failwith ("Parsing error: " ^ message)

let parse_from_string start str =
  let lexbuf = from_string str in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" } in
  try parse start lexbuf with
  | Failure msg ->
      failwith
        (Printf.sprintf "Failed while trying to parse the string:@\n%s@\n%s" str
           msg)
  | _ ->
      failwith
        (Printf.sprintf "Unkown parsing error while parsing the string:@\n%s"
           str)

let parse_eprog_from_string : string -> (Annot.t, string) Prog.t =
  parse_from_string GIL_Parser.gmain_target

let trans_procs procs path internal_file =
  let procs' = Hashtbl.create Config.small_tbl_size in
  let () =
    Hashtbl.iter
      (fun name (proc : (Annot.t, string) Proc.t) ->
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

let parse_eprog_from_file (path : string) : (Annot.t, string) Prog.t =
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
    let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } in
    let prog =
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
    { prog with procs; preds; lemmas }
  in
  L.with_normal_phase ~title:"Program parsing" (fun () -> f path)

let cached_progs = Hashtbl.create Config.small_tbl_size
let cache_gil_prog path prog = Hashtbl.add cached_progs path prog

let cache_labelled_progs (progs : (string * (Annot.t, string) Prog.t) list) =
  List.iter (fun (path, prog) -> cache_gil_prog path prog) progs

let resolve_path path =
  if Filename.is_relative path then
    let lookup_paths = "." :: Config.get_runtime_paths () in
    let rec find fname paths =
      match paths with
      | [] -> failwith (Printf.sprintf "Cannot resolve \"%s\"" fname)
      | path :: rest ->
          let complete_path = Filename.concat path fname in
          if Sys.file_exists complete_path then complete_path
          else find fname rest
    in
    find path lookup_paths
  else if Sys.file_exists path then path
  else failwith (Printf.sprintf "Cannot resolve \"%s\"" path)

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
  combine prog.lemmas other_prog.lemmas id "lemmas";
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

let eprog_to_prog ~other_imports ext_program =
  let open Prog in
  let () = resolve_imports ext_program other_imports in
  let proc_of_ext_proc (proc : (Annot.t, string) Proc.t) :
      (Annot.t, int) Proc.t * (string * int * int * int) list =
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
      (fun (_ : string) (proc : (Annot.t, string) Proc.t) (procs, predecessors) ->
        let proc, new_predecessors = proc_of_ext_proc proc in
        (proc :: procs, new_predecessors @ predecessors))
      ext_program.procs ([], [])
  in
  Prog.make_indexed ~lemmas:ext_program.lemmas ~preds:ext_program.preds
    ~only_specs:ext_program.only_specs ~procs ~predecessors
    ~macros:ext_program.macros ~bi_specs:ext_program.bi_specs ()

let parse_literal lexbuf = parse GIL_Parser.lit_target lexbuf
