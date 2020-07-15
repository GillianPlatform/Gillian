open Gillian.Gil_syntax
open Gillian.Symbolic

module Pc : sig
  type t

  val empty : t

  val make :
    pfs:PureContext.t -> gamma:TypEnv.t -> ?learned:Formula.t list -> unit -> t

  val extend : t -> Formula.t list -> t

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module FOSolver : sig
  val sat : pc:Pc.t -> Formula.t -> bool

  val is_equal : pc:Pc.t -> Expr.t -> Expr.t -> bool

  val is_different : pc:Pc.t -> Expr.t -> Expr.t -> bool

  val is_less_or_equal : pc:Pc.t -> Expr.t -> Expr.t -> bool

  val resolve_loc_name : pc:Pc.t -> Expr.t -> string option
end

module Results : sig
  type ('to_continue, 'terminated) t

  val pp : pp_cont:'a Fmt.t -> pp_term:'b Fmt.t -> ('a, 'b) t Fmt.t

  val equal :
    eq_cont:('a -> 'a -> bool) ->
    eq_term:('b -> 'b -> bool) ->
    ('a, 'b) t ->
    ('a, 'b) t ->
    bool

  val make : cont:'a list -> term:'b list -> ('a, 'b) t

  val to_continue : ('a, 'b) t -> 'a list

  val terminated : ('a, 'b) t -> 'b list

  val empty : ('a, 'b) t

  val return : 'a -> ('a, _) t

  val terminate : 'b -> (_, 'b) t

  val is_terminated : ('a, 'b) t -> bool

  val branch2 : 'a -> 'a -> ('a, _) t

  val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

  val map : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t

  module Infix : sig
    val ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

    val ( >>| ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
  end

  module Syntax : sig
    val ( let* ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

    val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
  end
end

module SatResults : sig
  module With_pc : sig
    type 'a t = 'a * Pc.t
  end

  type ('to_continue, 'terminated) t =
    ('to_continue With_pc.t, 'terminated With_pc.t) Results.t

  val return : pc:Pc.t -> ?learned:Formula.t list -> 'a -> ('a, _) t

  val terminate : pc:Pc.t -> ?learned:Formula.t list -> 'b -> (_, 'b) t

  val pp : pp_cont:'a Fmt.t -> pp_term:'b Fmt.t -> ('a, 'b) t Fmt.t

  val equal :
    eq_cont:('a -> 'a -> bool) ->
    eq_term:('b -> 'b -> bool) ->
    ('a, 'b) t ->
    ('a, 'b) t ->
    bool

  val empty : ('a, 'b) t

  val branch_on_sat :
    pc:Pc.t ->
    Formula.t ->
    then_branch:(Pc.t -> ('a, 'b) t) ->
    else_branch:(Pc.t -> ('a, 'b) t) ->
    ('a, 'b) t

  val bind : ('a, 'e) t -> ('a With_pc.t -> ('b, 'e) t) -> ('b, 'e) t

  val map : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t

  val of_res : pc:Pc.t -> ('a, 'b) result -> ('a, 'b) t

  val of_results : pc:Pc.t -> ('a, 'b) Results.t -> ('a, 'b) t

  val of_option : pc:Pc.t -> none:'b -> 'a option -> ('a, 'b) t

  module Syntax : sig
    val ( let** ) : ('a, 'e) t -> ('a With_pc.t -> ('b, 'e) t) -> ('b, 'e) t

    val ( let++ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
  end
end
