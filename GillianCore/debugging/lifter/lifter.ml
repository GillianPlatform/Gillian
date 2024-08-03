module L = Logging
include Lifter_intf

let make_executed_cmd_data
    ?(is_breakpoint = false)
    next_kind
    id
    cmd_report
    ?(matches = [])
    ?(errors = [])
    branch_path =
  { is_breakpoint; next_kind; id; cmd_report; matches; errors; branch_path }
