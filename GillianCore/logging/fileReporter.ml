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
    method log (report : 'a Report.t) =
      if enabled () then
        match report.content with
        | Agnostic c -> self#log_agnostic c
        | Specific c -> self#log_specific c

    method wrap_up = wrap_up ()

    method private log_agnostic =
      function
      | Debug msgf  ->
          Report.PackedPP.pf self#formatter msgf;
          Format.fprintf self#formatter "@,@?"
      | Phase phase ->
          Format.fprintf self#formatter "*** Phase %s ***@,@?" phase

    method virtual private log_specific : 'a -> unit

    method private formatter = get_formatter ()
  end

let default : type a. unit -> a t =
 fun () ->
  object
    inherit [a] t

    method private log_specific _ = ()
  end
