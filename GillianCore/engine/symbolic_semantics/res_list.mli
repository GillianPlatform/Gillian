type ('ok, 'err) t = ('ok, 'err) result list

val return : 'ok -> ('ok, 'err) t
val error_with : 'err -> ('ok, 'err) t
val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val vanish : 'a list

module Syntax : sig
  val ( let** ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let++ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
end
