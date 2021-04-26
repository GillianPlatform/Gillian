(**
    Reporter which logs to a database
*)

(* let filename = "./database.log"

let fd = ref None

let initialize () =
  let () = if Sys.file_exists filename then Sys.remove filename else () in
  (* rw-r--r-- *)
  let permissions = 0o644 in
  fd :=
    Some (Unix.openfile filename [ O_WRONLY; O_APPEND; O_CREAT ] permissions)

let log (report : Report.t) =
  match !fd with
  | None    -> ()
  | Some fd -> (
      (* TODO: This should eventually log all report types *)
      let yojson = Report.to_yojson report in
          let yojson = Yojson.Safe.to_string yojson ^ "\n" in
          Unix.lockf fd F_LOCK 0;
          ignore (Unix.write_substring fd yojson 0 (String.length yojson));
          Unix.lockf fd F_ULOCK 0)
      (* match report.type_ with
      | type_ when type_ = LoggingConstants.ContentType.store ->
          let yojson = Report.to_yojson report in
          let yojson = Yojson.Safe.to_string yojson ^ "\n" in
          Unix.lockf fd F_LOCK 0;
          ignore (Unix.write_substring fd yojson 0 (String.length yojson));
          Unix.lockf fd F_ULOCK 0
      | _ -> ()) *)

let wrap_up () =
  match !fd with
  | None    -> ()
  | Some fd -> Unix.close fd *)

let db_name = "gillian-debugger.db"

let db = ref None

let create () =
  let new_db = Sqlite3.db_open db_name in
  let stmt =
    Sqlite3.prepare new_db
      "CREATE TABLE report ( id TEXT PRIMARY KEY, title TEXT NOT NULL, \
       elapsed_time REAL NOT NULL, previous TEXT, parent TEXT, content TEXT \
       NOT NULL, severity TEXT NOT NULL, type TEXT NOT NULL);"
  in
  let response = Sqlite3.step stmt in
  if Sqlite3.Rc.is_success response then db := Some new_db
  else
    failwith
      (Printf.sprintf "Unable to create report table, error=%s"
         (Sqlite3.Rc.to_string response))

let reset () = if Sys.file_exists db_name then Sys.remove db_name else ()

let bind_value stmt index value =
  let response = Sqlite3.bind stmt index value in
  if not (Sqlite3.Rc.is_success response) then
    failwith (Sqlite3.Rc.to_string response)

let store_report (report : Report.t) db =
  let stmt =
    Sqlite3.prepare db "INSERT INTO report VALUES (?, ?, ?, ?, ?, ?, ?, ?);"
  in
  bind_value stmt 1
    (Sqlite3.Data.opt_text (Some (Uuidm.to_string (snd report.id))));
  bind_value stmt 2 (Sqlite3.Data.opt_text (Some report.title));
  bind_value stmt 3 (Sqlite3.Data.opt_float (Some report.elapsed_time));
  let previous_opt =
    match report.previous with
    | None          -> None
    | Some previous -> Some (Uuidm.to_string (snd previous))
  in
  bind_value stmt 4 (Sqlite3.Data.opt_text previous_opt);
  let parent_opt =
    match report.parent with
    | None        -> None
    | Some parent -> Some (Uuidm.to_string (snd parent))
  in
  bind_value stmt 5 (Sqlite3.Data.opt_text parent_opt);
  bind_value stmt 6
    (Sqlite3.Data.opt_text
       (Some
          (Yojson.Safe.to_string (Loggable.loggable_to_yojson report.content))));
          (* (Bi_io.string_of_tree (Yojson_biniou.biniou_of_json (Loggable.loggable_to_yojson report.content))))); *)
          (* "Testing if it's still slow with no JSON conversion")); *)
  (* TODO: Use plain string for severity *)
  bind_value stmt 7
    (Sqlite3.Data.opt_text
       (Some (Yojson.Safe.to_string (Report.severity_to_yojson report.severity))));
       (* (Some (Bi_io.string_of_tree (Yojson_biniou.biniou_of_json (Report.severity_to_yojson report.severity))))); *)
       (* (Some "Testing if it's still slow with no JSON conversion")); *)
  bind_value stmt 8 (Sqlite3.Data.opt_text (Some report.type_));
  let response = Sqlite3.step stmt in
  if not (Sqlite3.Rc.is_success response) then
    failwith (Sqlite3.Rc.to_string response)


let initialize () =
  reset ();
  create ()

let log (report : Report.t) =
  (* match !db with
  | None    -> print_endline "I got no db"
  | Some db ->
    match report.type_ with
      | type_ when type_ = LoggingConstants.ContentType.store ->
        store_report report db
      | _ -> () *)
  match !db with
  | None    -> print_endline "I got no db"
  | Some db -> store_report report db

let wrap_up () = ()

(* let get_line line_num db =
  let stmt = Sqlite3.prepare db "SELECT content FROM log WHERE line_number=?;" in
  let response = Sqlite3.bind stmt 1 (Sqlite3.Data.opt_int (Some line_num)) in
  if not (Sqlite3.Rc.is_success response) then
    Error
      (Printf.sprintf
         "Cannot bind line_num=%d, error=%s"
         line_num
         (Sqlite3.Rc.to_string response))
  else
    let response = Sqlite3.step stmt in
    if response == Sqlite3.Rc.ROW then
      let rows = Sqlite3.row_blobs stmt in
      let content = rows.(0) in
      Ok content
    else
      Error
        (Printf.sprintf
           "No execution log found, error=%s"
           (Rc.to_string response)) *)
