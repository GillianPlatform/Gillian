(** @canonical Gillian.Utils.Ext_list

    A variable-length list

    A lot of functions here are self-explanatory analogs of {!List} functions *)

type 'a t [@@deriving yojson]

val make : unit -> 'a t
val clear : 'a t -> unit
val prepend : 'a -> 'a t -> unit
val append : 'a -> 'a t -> unit
val add : 'a -> 'a t -> unit
val add_all : 'a list -> 'a t -> unit
val length : 'a t -> int
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val mem : ?equal:('a -> 'a -> bool) -> 'a -> 'a t -> bool
val copy : 'a t -> 'a t

(** [concat dest src] moves all the elements in [src] to [dst]

    Elements are moved, not copied; [src] is left empty at the end *)
val concat : 'a t -> 'a t -> unit

val map_inplace : ('a -> 'a) -> 'a t -> unit
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val iter : ('a -> unit) -> 'a t -> unit

(** Unlike [List.for_all2], if the two lists given in parameters are not of the
    same size, it returns 0 instead of raising Invalid_argument *)
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val exists : ('a -> bool) -> 'a t -> bool
val assoc_opt : 'a -> ('a * 'b) t -> 'b option
val remove_duplicates : ?equal:('a -> 'a -> bool) -> 'a t -> unit

(** Filter-maps the list in place according to [f]

    For each element [x]:
    - If [f x = `Filter], drop [x] from the list and continue
    - If [f x = `Replace y], replace [x] with [y] in the list and continue
    - If [f x = `Stop], stop filtering, leaving the rest of the list unchanged

    Returns whether [`Stop] was encountered

    For example:
    {[
      let f =
       fun x ->
        if x < 5 then `Filter else if x = 10 then `Stop else `Replace (x + 1)
      in

      filter_map_stop_cond f
        [ 1; 2; 6; 7; 4 ] (* modifies the list into [7; 8] and returns false *)
        filter_map_stop_cond f [ 1; 2; 6; 7; 10; 8; 4 ]
      (* modifies the list into [ 7; 8; 10; 8; 4 ] and returns true *)
    ]} *)
val filter_map_stop :
  ('a -> [< `Replace of 'a | `Filter | `Stop ]) -> 'a t -> bool

(** Similar [filter_map_stop], except it cannot replace elements

  For each element [x]:
  - If [keep x = false], drop [x] from the list and continue
  - If [keep x = true && cond x = false], continue
  - If [keep x = true && cond x = true], stop filtering, leaving the rest of the list unchanged
  [keep] says if the element should be kept and [cond] if the filtering should stop

  If an element is not kept, the stop condition is not checked


  {[filter_stop_cond ~keep:(fun x -> x > 5) ~cond:(fun x -> x = 10) \[ 1; 2; 6; 10; 4 \]]} will return
  [\[ 6; 10; 4 \]]
*)
val filter_stop_cond : keep:('a -> bool) -> cond:('a -> bool) -> 'a t -> bool

(** Keep only elements that satisfy the predicate given as first parameter *)
val filter : ('a -> bool) -> 'a t -> unit

(** If the function returns None, the element is dropped, otherwise, it is
    replaced by the value *)
val filter_map : ('a -> 'a option) -> 'a t -> unit

(** Returns the nth element of the list in O(n). If n < 0 or n >= length, it
    returns None. *)
val nth : int -> 'a t -> 'a option

(** Pretty printer *)
val pp : sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
