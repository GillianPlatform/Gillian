open Gillian.Concrete

type init_data = unit
type vt = Values.t
type st = Subst.t
type err_t = unit [@@deriving yojson, show]
type t = unit
type action_ret = (t * vt list, err_t) result

let init () = ()

type action = |

let action_from_str _ = None

let execute_action (a : action) _ _ =
  match a with
  | _ -> .

let copy () = ()
let pp _ () = ()
let pp_err _ () = ()
