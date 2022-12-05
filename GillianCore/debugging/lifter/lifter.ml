module L = Logging
include Lifter_intf

let make_executed_cmd_data
    kind
    id
    cmd_report
    ?(unifys = [])
    ?(errors = [])
    branch_path =
  { kind; id; cmd_report; unifys; errors; branch_path }
