class t () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  object
    method log (report : Report.t) =
      match report.content with
      | Debug msgf  ->
          Report.PackedPP.pf formatter msgf;
          Format.fprintf formatter "@,@?"
      | Phase phase ->
          Format.fprintf formatter "*** Phase %s ***@,@?"
          @@ Report.string_of_phase phase

    method wrap_up = close_out out_channel
  end
