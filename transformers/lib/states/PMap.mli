open Gil_syntax
open Gillian.Monadic

type index_mode = Static | Dynamic

module type PMapImpl = sig
  type entry
  type t [@@deriving yojson]

  val mode : index_mode
  val make_fresh : unit -> Expr.t Delayed.t
  val default_instantiation : Expr.t list
  val validate_index : Expr.t -> Expr.t option Delayed.t
  val get : t -> Expr.t -> (Expr.t * entry) option Delayed.t
  val set : idx:Expr.t -> idx':Expr.t -> entry -> t -> t
  val empty : t
  val fold : (Expr.t -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (entry -> bool) -> t -> bool
  val compose : t -> t -> t Delayed.t
  val substitution_in_place : Gillian.Symbolic.Subst.t -> t -> t Delayed.t
end

module type OpenPMapType = sig
  include MyMonadicSMemory.S

  type entry

  val get : t -> Expr.t -> (t * Expr.t * entry, err_t) result Delayed.t
  val set : idx:Expr.t -> idx':Expr.t -> entry -> t -> t
end

module type PMapType = sig
  include OpenPMapType

  val domain_add : Expr.t -> t -> t
end

module Make
    (I_Cons : functor (S : MyMonadicSMemory.S) -> PMapImpl with type entry = S.t)
    (S : MyMonadicSMemory.S) :
  PMapType with type entry = S.t and type t = I_Cons(S).t * Expr.t option

module MakeOpen
    (I_Cons : functor (S : MyMonadicSMemory.S) -> PMapImpl with type entry = S.t)
    (S : MyMonadicSMemory.S) :
  OpenPMapType with type entry = S.t and type t = I_Cons(S).t

module type PMapIndex = sig
  val mode : index_mode
  val is_valid_index : Expr.t -> Expr.t option Delayed.t
  val make_fresh : unit -> Expr.t Delayed.t
  val default_instantiation : Expr.t list
end

module LocationIndex : PMapIndex
module StringIndex : PMapIndex
module IntegerIndex : PMapIndex

type 'e t_base_sat := 'e MyUtils.ExpMap.t
type 'e t_base_ent := 'e MyUtils.ExpMapEnt.t
type 'e t_split_sat := 'e MyUtils.ExpMap.t * 'e MyUtils.ExpMap.t
type 'e t_split_ent := 'e MyUtils.ExpMapEnt.t * 'e MyUtils.ExpMapEnt.t
type 'e t_aloc := 'e MyUtils.SMap.t

module BaseImplSat : functor (I : PMapIndex) (S : MyMonadicSMemory.S) ->
  PMapImpl with type t = S.t t_base_sat and type entry = S.t

module BaseImplEnt : functor (I : PMapIndex) (S : MyMonadicSMemory.S) ->
  PMapImpl with type t = S.t t_base_ent and type entry = S.t

module SplitImplSat : functor (I : PMapIndex) (S : MyMonadicSMemory.S) ->
  PMapImpl with type t = S.t t_split_sat and type entry = S.t

module SplitImplEnt : functor (I : PMapIndex) (S : MyMonadicSMemory.S) ->
  PMapImpl with type t = S.t t_split_ent and type entry = S.t

module ALocImpl : functor (S : MyMonadicSMemory.S) ->
  PMapImpl with type t = S.t t_aloc and type entry = S.t
