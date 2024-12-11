open Gillian.Concrete
module Expr = Gillian.Gil_syntax.Expr

module Make (InitData : Gillian.General.Init_data.S) :
  Gillian.Concrete.Memory_S with type init_data = InitData.t = struct
  type vt = Values.t
  type st = Subst.t
  type err_t = unit [@@deriving yojson, show]
  type t = unit
  type init_data = InitData.t
  type action_ret = (t * vt list, err_t) result

  let init _ = ()
  let copy () = ()

  (* val execute_action : action_name:string -> t -> vt list -> action_ret Delayed.t*)
  let execute_action (action_name : string) (_ : t) (_ : vt list) : action_ret =
    failwith
      (Printf.sprintf "Implement here (CMem.execute_action %s)" action_name)

  let pp _ _ = ()
  let pp_err _ _ = ()
end
