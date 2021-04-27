(* A node of the disjoint set structure.
    The rank and parent fields are mutable to enable path compression
    and rank updates *)
type 'a t = {
  table : ('a, 'a) Hashtbl.t;
  priority : 'a -> 'a -> [ `Lower | `Greater | `Eq ];
  equal : 'a -> 'a -> bool;
}

let init ~priority ~equal = { table = Hashtbl.create 1; priority; equal }

let copy (uf : 'a t) : 'a t = { uf with table = Hashtbl.copy uf.table }

(* Creates a new union_find node, referencing
   itself as its parent. *)
let add (uf : 'a t) (elm : 'a) : unit =
  match Hashtbl.find_opt uf.table elm with
  | None   -> Hashtbl.replace uf.table elm elm
  | Some _ -> ()

(* Do a find() on a given node, searching
   for its representative *)
let rec rep (uf : 'a t) (elm : 'a) =
  match Hashtbl.find_opt uf.table elm with
  | None ->
      Hashtbl.replace uf.table elm elm;
      elm
  | Some elm' when uf.equal elm elm' -> elm
  | Some elm' ->
      let rep = rep uf elm' in
      Hashtbl.replace uf.table elm rep;
      rep

(* Union function, performs union by rank and
   uses find() for path compression *)
let union (uf : 'a t) (ela : 'a) (elb : 'a) : unit =
  let repa = rep uf ela in
  let repb = rep uf elb in
  match uf.priority repa repb with
  | `Eq | `Greater -> Hashtbl.replace uf.table repb repa
  | `Lower         -> Hashtbl.replace uf.table repa repb

(* Fold function *)
let fold (f : 'acc -> 'a * 'a -> 'acc) (ac : 'acc) (uf : 'a t) : 'acc =
  Hashtbl.fold (fun elt rep ac -> f ac (elt, rep)) uf.table ac
