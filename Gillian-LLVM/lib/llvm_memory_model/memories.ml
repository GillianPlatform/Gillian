open Gillian

module LLVM_Base = struct
  module MonadicSMemory = LLVM.MonadicSMemory_Base
  module ParserAndCompiler = Gillian.ParserAndCompiler.Dummy
  module ExternalSemantics = LLVM.ExternalSemantics
  module InitData = LLVM.MyInitData
  module MyInitData = LLVM.MyInitData
end

module LLVM_ALoc = struct
  include LLVM_Base
  module MonadicSMemory = LLVM.MonadicSMemory_ALoc
end

module LLVM_Split = struct
  include LLVM_Base
  module MonadicSMemory = LLVM.MonadicSMemory_Split
end

module DefaultMem =
  States.MyMonadicSMemory.Make (LLVM_ALoc.MonadicSMemory) (LLVM.MyInitData)

module SMemory = Monadic.MonadicSMemory.Lift (DefaultMem)
