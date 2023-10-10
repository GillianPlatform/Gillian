val of_option : none:'b -> 'a option -> ('a, 'b) Result.t
val all : ('a, 'b) Result.t list -> ('a list, 'b) Result.t

val fold_bind :
  ('a -> 'b -> ('a, 'c) Result.t) ->
  ('a, 'c) Result.t ->
  'b list ->
  ('a, 'c) Result.t

val map_bind : ('a -> ('b, 'c) Result.t) -> 'a list -> ('b list, 'c) Result.t

val bind_error :
  ('a, 'b) Result.t -> ('b -> ('a, 'c) Result.t) -> ('a, 'c) Result.t
