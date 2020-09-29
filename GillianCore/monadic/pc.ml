open Gil_syntax
module PureContext = Engine.PFS
module TypEnv = Engine.TypEnv
module FOSolver = Engine.FOSolver

type t = {
  pfs : PureContext.t;
  gamma : TypEnv.t;
  learned : Formula.Set.t;
  learned_types : (string * Type.t) list;
}

let copy { pfs; gamma; learned; learned_types } =
  {
    pfs = PureContext.copy pfs;
    gamma = TypEnv.copy gamma;
    learned;
    learned_types;
  }

let make ~pfs ~gamma ?(learned = []) ?(learned_types = []) () =
  { pfs; gamma; learned = Formula.Set.of_list learned; learned_types }

let init () = make ~pfs:(PureContext.init ()) ~gamma:(TypEnv.init ()) ()

let empty = init ()

let pfs_to_pfs_and_gamma pfs =
  let expr_type_biding_to_gamma etb =
    match etb with
    | Expr.PVar s, t | Expr.LVar s, t -> Some (s, t)
    | _ -> None
  in
  let rec aux = function
    | [] -> ([], [])
    | Formula.Eq (UnOp (TypeOf, e), Lit (Type t)) :: r
    | Eq (Lit (Type t), UnOp (TypeOf, e)) :: r -> (
        let other_pfs, other_gamma = aux r in
        match expr_type_biding_to_gamma (e, t) with
        | None       ->
            ( Formula.Eq (Lit (Type t), UnOp (TypeOf, e)) :: other_pfs,
              other_gamma )
        | Some gamma -> (other_pfs, gamma :: other_gamma) )
    | f :: r ->
        let other_pfs, other_gamma = aux r in
        (f :: other_pfs, other_gamma)
  in
  aux pfs

let extend pc fs =
  let new_pfs, new_gamma = pfs_to_pfs_and_gamma fs in
  {
    pc with
    learned = Formula.Set.add_seq (List.to_seq new_pfs) pc.learned;
    learned_types = new_gamma @ pc.learned_types;
  }

let extend_types pc types = { pc with learned_types = types @ pc.learned_types }

let equal pca pcb =
  pca.pfs = pcb.pfs && pca.gamma = pcb.gamma
  && Formula.Set.equal pca.learned pcb.learned
  && List.for_all2
       (fun (n1, t1) (n2, t2) ->
         String.equal n1 n2 && String.equal (Type.str t1) (Type.str t2))
       pca.learned_types pcb.learned_types

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
         Fmt.field "learned_types"
           (fun x -> x.learned_types)
           (Fmt.Dump.list
              (Fmt.Dump.pair Fmt.string (Fmt.of_to_string Type.str)));
       ])
