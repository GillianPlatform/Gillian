module WISL_Base = struct
  module MonadicSMemory = WISL.MonadicSMemory_Base
  module ParserAndCompiler = WISL.ParserAndCompiler
  module ExternalSemantics = WISL.ExternalSemantics
  module InitData = Gillian.General.Init_data.Dummy
  module MyInitData = Gillian.Combinators.MyMonadicSMemory.DummyID
end

module WISL_ALoc = struct
  include WISL_Base
  module MonadicSMemory = WISL.MonadicSMemory_ALoc
end

module WISL_Split = struct
  include WISL_Base
  module MonadicSMemory = WISL.MonadicSMemory_Split
end

module WISLF_Base = struct
  module MonadicSMemory = WISLF.MonadicSMemory_Base
  module ParserAndCompiler = WISLF.ParserAndCompiler
  module ExternalSemantics = WISLF.ExternalSemantics
  module InitData = Gillian.General.Init_data.Dummy
  module MyInitData = Gillian.Combinators.MyMonadicSMemory.DummyID
end

module WISLF_ALoc = struct
  include WISLF_Base
  module MonadicSMemory = WISLF.MonadicSMemory_ALoc
end

module WISLF_Split = struct
  include WISLF_Base
  module MonadicSMemory = WISLF.MonadicSMemory_Split
end
