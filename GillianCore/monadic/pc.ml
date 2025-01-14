open Gil_syntax
module Pure_context = Engine.PFS
module Type_env = Engine.Type_env
module FOSolver = Engine.FOSolver

type t = {
  pfs : Pure_context.t;
  gamma : Type_env.t;
  learned : Expr.Set.t;
  learned_types : (string * Type.t) list;
  matching : bool;
}

let copy { pfs; gamma; learned; learned_types; matching } =
  {
    pfs = Pure_context.copy pfs;
    gamma = Type_env.copy gamma;
    learned;
    learned_types;
    matching;
  }

let make ~pfs ~gamma ~matching ?(learned = []) ?(learned_types = []) () =
  { pfs; gamma; learned = Expr.Set.of_list learned; learned_types; matching }

let init ?(matching = false) () =
  make ~pfs:(Pure_context.init ()) ~gamma:(Type_env.init ()) ~matching ()

let empty = init ()

let pfs_to_pfs_and_gamma pfs =
  let expr_type_binding_to_gamma etb =
    match etb with
    | Expr.PVar s, t | Expr.LVar s, t -> Some (s, t)
    | _ -> None
  in
  let rec aux = function
    | [] -> ([], [])
    | Expr.BinOp (UnOp (TypeOf, e), Equal, Lit (Type t)) :: r
    | BinOp (Lit (Type t), Equal, UnOp (TypeOf, e)) :: r -> (
        let other_pfs, other_gamma = aux r in
        match expr_type_binding_to_gamma (e, t) with
        | None ->
            ( Expr.BinOp (Lit (Type t), Equal, UnOp (TypeOf, e)) :: other_pfs,
              other_gamma )
        | Some gamma -> (other_pfs, gamma :: other_gamma))
    | f :: r ->
        let other_pfs, other_gamma = aux r in
        (f :: other_pfs, other_gamma)
  in
  aux pfs

let extend pc fs =
  let rec split_conjunct : Expr.t -> Expr.t list = function
    | BinOp (f1, And, f2) -> split_conjunct f1 @ split_conjunct f2
    | UnOp (Not, BinOp (f1, Or, f2)) ->
        split_conjunct (BinOp (UnOp (Not, f1), And, UnOp (Not, f2)))
    | f -> [ f ]
  in
  let fs = List.concat_map split_conjunct fs in
  let pfs, gamma = (pc.pfs, pc.gamma) in
  let fs =
    List.filter_map
      (fun f ->
        match
          Engine.Reduction.reduce_lexpr ~matching:pc.matching ~pfs ~gamma f
        with
        | Expr.Lit (Bool true) -> None
        | f -> Some f)
      fs
  in
  let new_pfs, new_gamma = pfs_to_pfs_and_gamma fs in
  {
    pc with
    learned = Expr.Set.add_seq (List.to_seq new_pfs) pc.learned;
    learned_types = new_gamma @ pc.learned_types;
  }

let extend_types pc types = { pc with learned_types = types @ pc.learned_types }

let equal pca pcb =
  pca.pfs = pcb.pfs && pca.gamma = pcb.gamma
  && Expr.Set.equal pca.learned pcb.learned
  && List.for_all2
       (fun (n1, t1) (n2, t2) -> String.equal n1 n2 && Type.equal t1 t2)
       pca.learned_types pcb.learned_types

let pp =
  Fmt.braces
    (Fmt.record ~sep:Fmt.semi
       [
         Fmt.field "pfs"
           (fun x -> x.pfs)
           (fun fmt pfs ->
             (Fmt.Dump.list Expr.pp) fmt (Pure_context.to_list pfs));
         Fmt.field "gamma" (fun x -> x.gamma) Type_env.pp;
         Fmt.field "learned"
           (fun x -> Expr.Set.to_seq x.learned)
           (Fmt.Dump.seq Expr.pp);
         Fmt.field "learned_types"
           (fun x -> x.learned_types)
           (Fmt.Dump.list
              (Fmt.Dump.pair Fmt.string (Fmt.of_to_string Type.str)));
       ])

let diff pca pcb =
  (Expr.Set.diff pca.learned pcb.learned, Expr.Set.diff pcb.learned pca.learned)

let of_gpc (gpc : Engine.Gpc.t) =
  let Engine.Gpc.{ pfs; gamma; matching } = gpc in
  make ~pfs ~gamma ~matching ()

let to_gpc (pc : t) =
  let { pfs; gamma; matching; learned; learned_types } = pc in
  let pfs = Pure_context.copy pfs in
  let gamma = Type_env.copy gamma in
  Expr.Set.iter (Pure_context.extend pfs) learned;
  List.iter (fun (x, y) -> Type_env.update gamma x y) learned_types;
  Engine.Gpc.{ pfs; gamma; matching }
