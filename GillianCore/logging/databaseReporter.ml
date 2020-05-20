module Types = struct
  type conf = { filename : string }

  type state = {
    database : (module Sanddb.Database.T with type t = Report_j.t);
  }
end

include Reporter.Make (struct
  include Types

  let enabled = false

  let conf = { filename = "database.log" }

  let initialize { filename; _ } =
    if Sys.file_exists filename then Sys.remove "database.log";
    { database = Sanddb.create_json_database filename (module Report_j) }

  let wrap_up _ = ()
end)

let get_database () = (get_state ()).database

class virtual ['a] t =
  object (self)
    method log (report : 'a Report.t) =
      if enabled () then
        Report.yojson_of_t self#specific_serializer report
        |> Sanddb.insert_record self#database
        |> ignore

    method wrap_up = wrap_up ()

    method virtual private specific_serializer : 'a -> Yojson.Safe.t

    method private database = get_database ()
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t

    method private specific_serializer _ = `Null
  end
