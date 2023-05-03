type t = { pfs : PFS.t; gamma : Type_env.t; unification : bool }

let make ~unification ~pfs ~gamma () = { pfs; unification; gamma }

let copy t =
  {
    pfs = PFS.copy t.pfs;
    gamma = Type_env.copy t.gamma;
    unification = t.unification;
  }
