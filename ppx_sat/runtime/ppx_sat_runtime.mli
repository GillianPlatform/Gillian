open Monadic.Delayed

val if_then_else :
  Gil_syntax.Formula.t ->
  then_branch:(unit -> 'a t) ->
  else_branch:(unit -> 'a t) ->
  'a t

val if_sure_then_else :
  Gil_syntax.Formula.t ->
  then_branch:(unit -> 'a t) ->
  else_branch:(unit -> 'a t) ->
  'a t
