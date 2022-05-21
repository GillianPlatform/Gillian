let internal_imports = [ "wisl_pointer_arith.gil"; "wisl_core.gil" ]
let internal_prefix = "i__"

module Prefix = struct
  let gvar = "gvar"
  let lgvar = "_lvar_"
  let loopinv_lab = "loopinv"
  let loop_lab = "loop"
  let ctn_lab = "continue"
  let fail_lab = "fail"
  let lbody_lab = "lbody"
  let end_lab = "end"
  let endif_lab = "endif"
  let then_lab = "then"
  let else_lab = "else"
end

module InternalProcs = struct
  let proc_prefix = ""
  let i x = internal_prefix ^ proc_prefix ^ x
  let internal_add = i "add"
  let internal_minus = i "minus"
  let internal_gt = i "gt"
  let internal_lt = i "lt"
  let internal_leq = i "leq"
  let internal_geq = i "geq"
end

module InternalPreds = struct
  let pred_prefix = "pred_"
  let i x = internal_prefix ^ pred_prefix ^ x
  let internal_pred_cell = i "cell"
  let internal_pred_add = i "add"
  let internal_pred_minus = i "minus"
  let internal_pred_gt = i "gt"
  let internal_pred_lt = i "lt"
  let internal_pred_geq = i "geq"
  let internal_pred_leq = i "leq"
end
