let filename = "file.log.html"

type state = {
  out : out_channel;
  fmt : Format.formatter;
  buf : Buffer.t;
  buf_fmt : Format.formatter;
  pre_div_pos : int option;
  mutable phase_depth : int;
  mutable line_number : int;
}

let state : state option ref = ref None

let html_pre =
  Fmt.str
    {|
  <!DOCTYPE html>
  <html>
  <head>
    <title>Gillian log</title>
    <style>%s</style>
    <script type="application/javascript">%s</script>
  </head>
  <body>
    <div class="container">
|}
    Html_reporter_aux.css Html_reporter_aux.js

let html_post = "</div></body></html>"

let log_str s state =
  let { fmt; _ } = state in
  Fmt.pf fmt {|<div class="msg" onclick="toggleCollapsedIfMultiline(this)">|};
  s |> String.trim |> String.split_on_char '\n'
  |> List.iter (fun line ->
         state.line_number <- state.line_number + 1;
         Fmt.pf fmt
           {|<pre class="line-num">%d</pre><pre class="msg-line">%s</pre>|}
           state.line_number line);
  Fmt.pf fmt "</div>"

let print_end state =
  let { out; fmt; phase_depth; _ } = state in
  let pos = pos_out out in
  for _ = 1 to phase_depth do
    Fmt.pf fmt "</div>"
  done;
  log_str "" state;
  Fmt.pf fmt "%s" html_post;
  Fmt.flush fmt ();
  seek_out out pos

let start_phase () =
  match !state with
  | None -> ()
  | Some state ->
      Fmt.pf state.fmt
        {|<div class="phase-button" onclick="toggleCollapsed(this)"></div><div class="phase">|};
      state.phase_depth <- state.phase_depth + 1

let end_phase () =
  match !state with
  | None -> ()
  | Some state ->
      if state.phase_depth = 0 then failwith "HORROR: phase_depth < 0";
      Fmt.pf state.fmt "</div>";
      state.phase_depth <- state.phase_depth - 1

let accepted_types =
  Logging_constants.Content_type.
    [ debug; assertion; phase; cmd; unify; unify_result ]

let initialize () =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  let buf = Buffer.create 16 in
  let buf_fmt = Format.formatter_of_buffer buf in
  Fmt.pf fmt "%s" html_pre;
  state :=
    Some
      {
        out;
        fmt;
        buf;
        buf_fmt;
        phase_depth = 0;
        pre_div_pos = None;
        line_number = 0;
      }

let will_log (type_ : string) = List.mem type_ accepted_types

let get_report_content (report : Report.t) { buf; buf_fmt; _ } =
  Buffer.clear buf;
  Loggable.pp_html report.content buf_fmt;
  Format.pp_print_flush buf_fmt ();
  Buffer.contents buf

let log (report : Report.t) : unit =
  match !state with
  | Some state ->
      if will_log report.type_ then
        let report_content = get_report_content report state in
        log_str report_content state
  | _ -> ()

let wrap_up () =
  match !state with
  | Some state ->
      print_end state;
      close_out state.out
  | None -> ()
