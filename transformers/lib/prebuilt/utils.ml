open States

(* Typings *)
module type ActionAddition = ActionAdder.ActionAddition
module type FilterVals = Filter.FilterVals
module type IDs = MyUtils.IDs
module type Injection = Injector.Injection
module type NameMap = Mapper.NameMap
module type MyMonadicSMemory = MyMonadicSMemory.S
module type PMapIndex = PMap.PMapIndex
module type PMapType = PMap.PMapType
module type OpenPMapType = PMap.OpenPMapType

type filter_mode = Filter.filter_mode
type index_mode = PMap.index_mode

(* Helpers *)
module DummyInject = Injector.DummyInject

module IDs : IDs = struct
  let id1 = "left_"
  let id2 = "right_"
end

(* Indices *)
module LocationIndex = PMap.LocationIndex
module IntegerIndex = PMap.IntegerIndex
module StringIndex = PMap.StringIndex

(* Leaves *)
module Agreement = Agreement
module Exclusive = Exclusive
module Fractional = Fractional

(* Transformers *)
module ActionAdder = ActionAdder.Make
module Filter = Filter.Make
module Freeable = Freeable.Make
module Injector = Injector.Make
module Logger = Logger.Make
module Mapper = Mapper.Make
module MList = MList.Make
module Product = Product.Make
module Sum = Sum.Make

(* PMaps *)
module ALocPMap = PMap.Make (PMap.ALocImpl)
module SplitPMap (I : PMapIndex) = PMap.Make (PMap.SplitImplSat (I))
module OpenALocPMap = PMap.MakeOpen (PMap.ALocImpl)
module OpenSplitPMap (I : PMapIndex) = PMap.MakeOpen (PMap.SplitImplSat (I))
module OpenPMap (I : PMapIndex) = PMap.MakeOpen (PMap.BaseImplSat (I))
module PMap (I : PMapIndex) = PMap.Make (PMap.BaseImplSat (I))
