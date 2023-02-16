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

let css =
  {|
  html {
    --lh: 1rem;
    --side-color: #919191;
    line-height: var(--lh);
  }

  .container {
    margin-left: 50px;
    border-left: 1px solid var(--side-color);
    padding-left: 4px;
  }

  .phase {
    border-left: 1px solid black;
    padding-left: 4px;
    overflow: hidden;
  }

  .phase :nth-child(1) {
    margin-top: 0;
  }

  .phase-button {
    position: absolute;
    left: 50px;
    width: 1.2em;
    height: 1.2em;
    background-color: black;
    color: white;
    font-size: 0.8em;
    border-radius: 100%;
    box-sizing: border-box;
    padding-left: 1px;
    padding-top: 1px;
    cursor: pointer;
    transform: translateY(-50%);
  }

  .phase-button:has(+ .phase:empty),
  .phase-button[collapsed] + .phase > .phase-button{
    display: none;
  }

  .phase-button::before {
    content: "▼";
  }

  .phase-button[collapsed] {
    padding-top: 0px;
    padding-left: 3px;
  }

  .phase-button[collapsed]::before {
    content: "▶";
  }

  .phase-button:hover + .phase {
    border-color: red !important;
  }

  .phase-button[collapsed] + .phase {
    height: 0;
    border-top: 1px solid black;
  }

  pre {
    margin: 0;
  }

  .msg {
    margin-top: 1em;
  }

  .msg:hover {
    background-color: #f5f5f5;
  }

  .msg[collapsed] {
    --max-lines: 1;
    height: calc(var(--lh) * var(--max-lines));
    overflow-y: hidden;
    border-bottom: 1px dotted black;
  }

  .msg-line {
    min-height: var(--lh);
  }

  .line-num {
    position: absolute;
    color: var(--side-color);
    left: 5px;
    width: 40px;
    text-align: right;
    -webkit-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
  }

  .msg[collapsed] .line-num:not(:first-of-type),
  .phase-button[collapsed] + .phase .line-num {
    display: none;
  }
|}

let js =
  {|
  function toggleCollapsed(e) {
    e.toggleAttribute('collapsed');
  }

  function toggleCollapsedIfMultiline(e) {
    if (e.querySelectorAll('.msg-line').length > 1)
      toggleCollapsed(e);
  }
|}

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
    css js

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
