module L = Logging
include Lifter_intf

let make_executed_cmd_data
    next_kind
    id
    cmd_report
    ?(matches = [])
    ?(errors = [])
    branch_path =
  { next_kind; id; cmd_report; matches; errors; branch_path }
