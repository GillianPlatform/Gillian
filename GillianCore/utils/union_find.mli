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

module type EqType = sig
  type t

  val equal : t -> t -> bool

  val priority : t -> t -> int
end

module type S = sig
  type elt

  type t

  (** [init ()] initialises a new, empty union-find *)
  val init : unit -> t

  (** [copy uf] copies the given union-find *)
  val copy : t -> t

  (** [add uf e] creates a new element [e] in the union-find [uf] *)
  val add : t -> elt -> unit

  (** [rep uf e] finds the representative of node [e] in the union-find [uf] *)
  val rep : t -> elt -> elt

  (** [union uf e1 e2] merges elements [e1] and [e2] in the 
        union-find [uf], performing path compression along the way. *)
  val union : t -> elt -> elt -> unit

  (** [fold f ac uf] folds over the union find uf *)
  val fold : ('a -> elt * elt -> 'a) -> 'a -> t -> 'a
end

module Make (Eq : EqType) : S with type elt = Eq.t
