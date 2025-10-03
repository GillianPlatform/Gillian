type t = { pfs : PFS.t; gamma : Type_env.t; matching : bool }

let make ~matching ~pfs ~gamma () = { pfs; matching; gamma }

let copy t =
  { pfs = PFS.copy t.pfs; gamma = Type_env.copy t.gamma; matching = t.matching }
