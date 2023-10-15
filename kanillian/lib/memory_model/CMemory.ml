open Gillian.Concrete

type init_data = unit
type vt = Values.t
type st = Subst.t
type err_t = unit [@@deriving show]
type t = unit
type action_ret = (t * vt list, err_t) result

let init () = ()
let execute_action _ _ _ = failwith "c_memory not implemented in Kanillian"
let copy () = ()
let pp _ () = ()
let pp_err _ () = ()
