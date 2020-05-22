module Types = struct
  type conf = { filename : string }

  type state = { out_channel : out_channel; mutable empty : bool }
end

include Reporter.Make (struct
  include Types

  let enabled = false

  let conf = { filename = "database.log" }

  let initialize { filename; _ } =
    let out_channel = open_out filename in
    Printf.fprintf out_channel "[";
    { out_channel; empty = true }

  let wrap_up { out_channel; _ } =
    Printf.fprintf out_channel "]";
    close_out out_channel
end)

let get_out_channel () = (get_state ()).out_channel

let is_empty () = (get_state ()).empty

let set_not_empty () = (get_state ()).empty <- false

class virtual ['a] t =
  object (self)
    method log (report : 'a Report.t) =
      if enabled () then
        if is_empty () then set_not_empty ()
        else Printf.fprintf self#out_channel ",\n";
      Report.yojson_of_t self#specific_serializer report
      |> Yojson.Safe.to_channel self#out_channel

    method wrap_up = wrap_up ()

    method virtual private specific_serializer : 'a -> Yojson.Safe.t

    method private out_channel = get_out_channel ()
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t

    method private specific_serializer _ = `Null
  end
