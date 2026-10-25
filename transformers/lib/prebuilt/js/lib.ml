module JSIL_Base = struct
  module MonadicSMemory = JSIL.MonadicSMemory_Base
  module ParserAndCompiler = JSIL.ParserAndCompiler
  module ExternalSemantics = JSIL.ExternalSemantics
  module InitData = Gillian.General.Init_data.Dummy
  module MyInitData = Gillian.Combinators.MyMonadicSMemory.DummyID
end

module JSIL_ALoc = struct
  include JSIL_Base
  module MonadicSMemory = JSIL.MonadicSMemory_ALoc
end

module JSIL_Split = struct
  include JSIL_Base
  module MonadicSMemory = JSIL.MonadicSMemory_Split
end

module JSIL_ALocSplit = struct
  include JSIL_Base
  module MonadicSMemory = JSIL.MonadicSMemory_ALocSplit
end
