type ('ok, 'err) t = ('ok, 'err) result list

val pp : ok:'ok Fmt.t -> error:'err Fmt.t -> ('ok, 'err) t Fmt.t
val return : 'ok -> ('ok, 'err) t
val error_with : 'err -> ('ok, 'err) t
val just_oks : 'ok list -> ('ok, 'err) t
val just_errors : 'err list -> ('ok, 'err) t
val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val iter_errors : ('b -> unit) -> ('a, 'b) t -> unit
val vanish : ('ok, 'err) t
val filter_errors : ('ok, 'err) t -> ('ok, 'err) t
val split : ('ok, 'err) t -> 'ok list * 'err list
val all : ('ok, 'err) t list -> ('ok list, 'err) t
val of_list_res : ('ok, 'err) List_res.t -> ('ok, 'err) t

module Syntax : sig
  val ( let** ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let++ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
  val ( let-- ) : ('c, 'a) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end
