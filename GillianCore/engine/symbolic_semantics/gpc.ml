type t = { pfs : PFS.t; gamma : Type_env.t }

let make ~pfs ~gamma () = { pfs; gamma }
let copy t = { pfs = PFS.copy t.pfs; gamma = Type_env.copy t.gamma }
