open Gillian.Gil_syntax
module PureContext = Gillian.Symbolic.PureContext
module TypEnv = Gillian.Symbolic.TypEnv
module FOSolver = Gillian.Logic.FOSolver

let id_gen =
  let i = ref 0 in
  fun () ->
    let curr = !i in
    incr i;
    curr

type t = {
  id : int;
  pfs : PureContext.t;
  gamma : TypEnv.t;
  learned : Formula.Set.t;
}

let copy { id; pfs; gamma; learned } =
  { id; pfs = PureContext.copy pfs; gamma = TypEnv.copy gamma; learned }

let check_inconsistency { id = ida; _ } { id = idb; _ } =
  if not (Int.equal ida idb) then
    failwith "Inconsisten use of Action Path Conditions"

let make ~pfs ~gamma ?(learned = []) () =
  { id = id_gen (); pfs; gamma; learned = Formula.Set.of_list learned }

let init () = make ~pfs:(PureContext.init ()) ~gamma:(TypEnv.init ()) ()

let empty = init ()

let merge pca pcb =
  let { id; pfs; gamma; learned } = pca in
  let { learned = other_learned; _ } = pcb in
  check_inconsistency pca pcb;
  { id; pfs; gamma; learned = Formula.Set.union learned other_learned }

let extend pc fs =
  { pc with learned = Formula.Set.add_seq (List.to_seq fs) pc.learned }

let equal pca pcb =
  pca.pfs = pcb.pfs && pca.gamma = pcb.gamma
  && Formula.Set.equal pca.learned pcb.learned

let pp =
  Fmt.braces
    (Fmt.record ~sep:Fmt.semi
       [
         Fmt.field "pfs"
           (fun x -> x.pfs)
           (fun fmt pfs ->
             (Fmt.Dump.list Formula.pp) fmt (PureContext.to_list pfs));
         Fmt.field "gamma" (fun x -> x.gamma) TypEnv.pp;
         Fmt.field "learned"
           (fun x -> Formula.Set.to_seq x.learned)
           (Fmt.Dump.seq Formula.pp);
       ])
