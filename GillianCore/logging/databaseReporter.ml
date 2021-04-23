let filename = "./database.log"

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
      match report.type_ with
      | type_ when type_ = LoggingConstants.ContentType.store ->
          let yojson = Report.to_yojson report in
          let yojson = Yojson.Safe.to_string yojson ^ "\n" in
          Unix.lockf fd F_LOCK 0;
          ignore (Unix.write_substring fd yojson 0 (String.length yojson));
          Unix.lockf fd F_ULOCK 0
      | _ -> ())

let wrap_up () =
  match !fd with
  | None    -> ()
  | Some fd -> Unix.close fd
