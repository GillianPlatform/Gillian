open Linol_lwt

type buffer_state = { path : string; content : string }

let default_range =
  let start = Position.create ~line:0 ~character:0 in
  let end_ = start in
  Range.create ~start ~end_

let loc_to_range (loc : Utils.Location.t) : Range.t =
  let start =
    Position.create ~line:loc.loc_start.pos_line
      ~character:loc.loc_start.pos_column
  in
  let end_ =
    Position.create ~line:loc.loc_end.pos_line ~character:loc.loc_end.pos_column
  in
  Range.create ~start ~end_

let mk_diagnostic
    ~path
    ?(loc : Utils.Location.t option)
    ?(severity = DiagnosticSeverity.Error)
    msg =
  let range =
    match loc with
    | Some loc when loc.loc_source = path -> loc_to_range loc
    | _ -> default_range
  in
  Diagnostic.create ~severity ~range ~message:(`String msg) ()

let analysis_failure_to_diagnostic
    ~path
    ({ loc; msg; _ } : Gillian_result.Error.analysis_failure) =
  mk_diagnostic ~path ?loc msg

let result_to_diagnostics ~path : unit Gillian_result.t -> Diagnostic.t list =
  function
  | Ok () -> []
  | Error (AnalysisFailures failures) ->
      List.map (analysis_failure_to_diagnostic ~path) failures
  | Error (CompilationError { msg; loc; _ }) -> [ mk_diagnostic ~path ?loc msg ]
  | Error (OperationError msg | InternalError { msg; _ }) ->
      [ mk_diagnostic ~path msg ]

class lsp_server (f : string -> unit Gillian_result.t) =
  object (self)
    inherit Jsonrpc2.server

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, buffer_state) Hashtbl.t =
      Hashtbl.create 4

    method spawn_query_handler f = spawn f

    (* We define here a helper method that will:
        - process a document
        - store the state resulting from the processing
        - return the diagnostics from the new state
    *)
    method private _on_doc
        ~(notify_back : Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t)
        (contents : string) =
      let path = Lsp.Types.DocumentUri.to_path uri in
      let () = Hashtbl.replace Config.file_content_overrides path contents in
      let result = f path in
      let diags = result_to_diagnostics ~path result in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
        by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
        by the server each time a new document is opened. *)
    method on_notif_doc_did_change
        ~notify_back
        d
        _c
        ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
        hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit t =
      let path = Lsp.Types.DocumentUri.to_path d.uri in
      Hashtbl.remove Config.file_content_overrides path;
      Hashtbl.remove buffers d.uri;
      return ()
  end

let run analyse =
  let s = new lsp_server analyse in
  let server = Linol_lwt.Jsonrpc2.create_stdio ~env:() s in
  let task =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown server
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "Uncaught LSP error: %s\n%!" e;
      exit Gillian_result.(to_error_code (internal_error ""))
