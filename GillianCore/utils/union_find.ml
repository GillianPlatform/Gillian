(*
	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>
*)

(* A type with decidable equality, only requirement
   for the union_find data structure *)
module type EqType = sig
  type t

  val equal : t -> t -> bool

  val priority : t -> t -> int
end

module type S = sig
  type elt

  type t

  val init : unit -> t

  val copy : t -> t

  val add : t -> elt -> unit

  val rep : t -> elt -> elt

  val union : t -> elt -> elt -> unit

  val fold : ('a -> elt * elt -> 'a) -> 'a -> t -> 'a
end

(* A functor with one argument to construct a union-find
   structure. *)
module Make (Eq : EqType) = struct
  type elt = Eq.t

  (* A node of the disjoint set structure.
      The rank and parent fields are mutable to enable path compression
     and rank updates *)
  type t = (elt, elt) Hashtbl.t

  let init : unit -> t = function
    | _ -> Hashtbl.create 1

  let copy (uf : t) : t = Hashtbl.copy uf

  (* Creates a new union_find node, referencing
     itself as its parent. *)
  let add (uf : t) (elm : elt) : unit =
    match Hashtbl.find_opt uf elm with
    | None   -> Hashtbl.replace uf elm elm
    | Some _ -> ()

  (* Do a find() on a given node, searching
     for its representative *)
  let rec rep (uf : t) (elm : elt) =
    match Hashtbl.find_opt uf elm with
    | None ->
        Hashtbl.replace uf elm elm;
        elm
    | Some elm' when elm = elm' -> elm
    | Some elm' ->
        let rep = rep uf elm' in
        Hashtbl.replace uf elm rep;
        rep

  (* Union function, performs union by rank and
     uses find() for path compression *)
  let union (uf : t) (ela : elt) (elb : elt) : unit =
    let repa = rep uf ela in
    let repb = rep uf elb in
    match Eq.priority repa repb with
    | 0 | 1 -> Hashtbl.replace uf repb repa
    | -1    -> Hashtbl.replace uf repa repb
    | _     ->
        raise
          (Exceptions.Unsupported "union_find: priority not in { -1, 0, 1 }")

  (* Fold function *)
  let fold (f : 'a -> elt * elt -> 'a) (ac : 'a) (uf : t) : 'a =
    Hashtbl.fold (fun elt rep ac -> f ac (elt, rep)) uf ac
end
