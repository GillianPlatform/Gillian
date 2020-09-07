module Types = struct
  type conf = { filename : string }

  type state = { fd : Unix.file_descr }
end

include Reporter.Make (struct
  include Types

  let conf =
    let filename = "database.log" in
    if Sys.file_exists filename then Sys.remove filename;
    { filename }

  let initialize { filename } =
    (* rw-r--r-- *)
    let perm = 0o644 in
    { fd = Unix.openfile filename [ O_WRONLY; O_APPEND; O_CREAT ] perm }

  let wrap_up { fd } = Unix.close fd
end)

let get_fd () = (get_state ()).fd

let write yojson =
  let yojson = Yojson.Safe.to_string yojson ^ "\n" in
  Unix.lockf (get_fd ()) F_LOCK 0;
  ignore (Unix.write_substring (get_fd ()) yojson 0 (String.length yojson));
  Unix.lockf (get_fd ()) F_ULOCK 0

class virtual ['a] t =
  object (self)
    method log (report : 'a Report.t) =
      if enabled () then
        write (Report.yojson_of_t self#specific_serializer report)

    method wrap_up = wrap_up ()

    method virtual private specific_serializer : 'a -> Yojson.Safe.t
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t

    method private specific_serializer _ = `Null
  end
