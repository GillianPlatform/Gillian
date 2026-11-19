open Linol_lwt
open Linol.Lsp.Types

let default_range =
  let start = Position.create ~line:0 ~character:0 in
  let end_ = Position.create ~line:0 ~character:1 in
  Range.create ~start ~end_

let convert_pos ({ pos_line; pos_column; _ } : Utils.Location.position) =
  let line = pos_line - 1 in
  let character = pos_column in
  Position.create ~line ~character

let loc_to_range (loc : Utils.Location.t) =
  let start = convert_pos loc.loc_start in
  let end_ = convert_pos loc.loc_end in
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
  mk_diagnostic ~severity:DiagnosticSeverity.Information ~path ?loc msg

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
    val buffers : (DocumentUri.t, unit Gillian_result.t) Hashtbl.t =
      Hashtbl.create 4

    (* For a fresh progress token per analysis *)
    val mutable analyses = 0

    (* Each document's latest content not yet analysed, with the [notify_back]
       of the notification that brought it *)
    val pending : (DocumentUri.t, Jsonrpc2.notify_back * string) Hashtbl.t =
      Hashtbl.create 4

    (* Analyses share global state, so one at a time *)
    val lock = Lwt_mutex.create ()
    method spawn_query_handler f = spawn f

    (* We define here a helper method that will:
        - process a document
        - store the state resulting from the processing
        - return the diagnostics from the new state
    *)
    method
        private _analyse
        ~(notify_back : Jsonrpc2.notify_back)
        (uri : DocumentUri.t)
        (content : string) =
      let open Lwt.Syntax in
      let path = DocumentUri.to_path uri in
      let () = Hashtbl.replace Config.file_content_overrides path content in
      analyses <- analyses + 1;
      let token = `String (Fmt.str "gillian-analysis-%d" analyses) in
      let progress value =
        notify_back#send_notification (WorkDoneProgress { token; value })
      in
      (* Not waiting for the reply: notifications are handled one at a time,
         so the reply couldn't be read until this handler is done *)
      let* _ =
        notify_back#send_request
          (WorkDoneProgressCreate (WorkDoneProgressCreateParams.create ~token))
          (fun _ -> Lwt.return ())
      in
      let* () =
        progress
          (Begin
             (WorkDoneProgressBegin.create ~title:"Gillian"
                ~message:("Verifying " ^ Filename.basename path)
                ()))
      in
      (* The analysis blocks, so flush now rather than when Lwt next runs *)
      let* () = Lwt_io.flush Lwt_io.stdout in
      Lwt.finalize
        (fun () ->
          let result = f path in
          let old_result = Hashtbl.find_opt buffers uri in
          let () = Usage_logs.Lsp.log path ?old_result ~content result in
          let diags = result_to_diagnostics ~path result in
          let () = Config.reset_config () in
          Hashtbl.replace buffers uri result;
          notify_back#send_diagnostic diags)
        (fun () -> progress (End (WorkDoneProgressEnd.create ())))

    (* Clears the document's diagnostics, and schedules an analysis of its
       latest content. Edits arriving meanwhile (including during another
       analysis, which blocks) replace that content, so they're analysed once. *)
    method private _on_doc ~notify_back uri content =
      let open Lwt.Syntax in
      let scheduled = Hashtbl.mem pending uri in
      Hashtbl.replace pending uri (notify_back, content);
      let* () = notify_back#send_diagnostic [] in
      if not scheduled then
        Lwt.async (fun () ->
            Lwt_mutex.with_lock lock @@ fun () ->
            (* Waiting after any earlier analysis, so edits that queued during
               it are read first.
               ponytail: fixed debounce; make it configurable if it's felt *)
            let* () = Lwt_unix.sleep 0.2 in
            match Hashtbl.find_opt pending uri with
            | None -> Lwt.return () (* closed meanwhile *)
            | Some (notify_back, content) ->
                Hashtbl.remove pending uri;
                self#_analyse ~notify_back uri content);
      Lwt.return ()

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
      let path = DocumentUri.to_path d.uri in
      Hashtbl.remove Config.file_content_overrides path;
      Hashtbl.remove buffers d.uri;
      Hashtbl.remove pending d.uri;
      return ()
  end

let run analyse =
  Utils.Prelude.disable_stdout ();
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
