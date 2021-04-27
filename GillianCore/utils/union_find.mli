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

type 'a t

(** [init] initialises a new, empty union-find *)
val init :
  priority:('a -> 'a -> [ `Lower | `Greater | `Eq ]) ->
  equal:('a -> 'a -> bool) ->
  'a t

(** [copy uf] copies the given union-find *)
val copy : 'a t -> 'a t

(** [add uf e] creates a new element [e] in the union-find [uf] *)
val add : 'a t -> 'a -> unit

(** [rep uf e] finds the representative of node [e] in the union-find [uf] *)
val rep : 'a t -> 'a -> 'a

(** [union uf e1 e2] merges elements [e1] and [e2] in the 
        union-find [uf], performing path compression along the way. *)
val union : 'a t -> 'a -> 'a -> unit

(** [fold f ac uf] folds over the union find uf *)
val fold : ('acc -> 'a * 'a -> 'acc) -> 'acc -> 'a t -> 'acc
