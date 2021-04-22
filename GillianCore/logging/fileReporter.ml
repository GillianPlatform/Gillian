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

class virtual ['a] t =
  object (self)
    method log (report : 'a Report.t) : unit =
      if enabled () then
        match report.content with
        | Agnostic c -> self#log_agnostic c
        | Specific _ -> ()

    method log_specific (_ : Loggable.loggable) : unit = ()

    method wrap_up = wrap_up ()

    method private log_agnostic =
      function
      | Debug msgf ->
          Report.PackedPP.pf self#formatter msgf;
          Format.fprintf self#formatter "@,@?"
      | Phase      -> Format.fprintf self#formatter "*** Phase ***@,@?"

    (* method virtual private log_specific : 'a -> unit *)
    method private formatter = get_formatter ()
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t
  end
