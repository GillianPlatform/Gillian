(* let filename = "file.log"

let out_channel = ref None

let formatter = ref None

let initialize () =
  out_channel := Some (open_out filename);
  formatter := Some (Format.formatter_of_out_channel (Option.get !out_channel))

let log (report : 'a Report.t) : unit =
  match report.content with
  | Report.Agnostic c ->
    (match !formatter with
    | None -> ()
    | Some formatter ->
      match c with
      | Debug msgf ->
        Report.PackedPP.pf formatter msgf;
        Format.fprintf formatter "@,@?"
      | Phase      -> Format.fprintf formatter "*** Phase ***@,@?")
  | Report.Specific _ -> ()

let wrap_up () = () *)

module Types = struct
  type conf = { filename : string }

  type state = { out_channel : out_channel; formatter : Format.formatter }
end

include Reporter.Make (struct
  include Types

  let conf = { filename = "file.log" }

  let initialize { filename; _ } =
    let out_channel = open_out filename in
    let formatter = Format.formatter_of_out_channel out_channel in
    { out_channel; formatter }

  let wrap_up { out_channel; _ } = close_out out_channel
end)

let get_formatter () =
  let state = get_state () in
  state.formatter

class virtual t =
  object (self)
    method log (report : Report.t) : unit =
      if enabled () then
        match report.type_ with
      | Debug | Phase ->
        let () = Loggable.pp report.content self#formatter in
        Format.fprintf self#formatter "@,@?"
      | _ -> ()

    method wrap_up = wrap_up ()

    method private formatter = get_formatter ()
  end

let default : unit -> t =
 fun () ->
  object
    inherit t
  end
