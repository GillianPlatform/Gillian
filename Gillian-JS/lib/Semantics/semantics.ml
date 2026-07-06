module Base_symbolic = JSILSMemory.M
module Symbolic = Gillian.Monadic.MonadicSMemory.Lift (Base_symbolic)
module Concrete = JSILCMemory.M
module External = External.M
module SHeap = SHeap
