module L = Logging
include Lifter_intf

let make_executed_cmd_data
    ?(is_breakpoint = false)
    kind
    id
    cmd_report
    ?(matches = [])
    ?(errors = [])
    branch_path =
  { is_breakpoint; kind; id; cmd_report; matches; errors; branch_path }
