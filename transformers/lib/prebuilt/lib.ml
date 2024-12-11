module WISL_Base = struct
  module MonadicSMemory = WISL.MonadicSMemory_Base
  module ParserAndCompiler = WISL.ParserAndCompiler
  module ExternalSemantics = WISL.ExternalSemantics
  module InitData = Gillian.General.Init_data.Dummy
  module MyInitData = States.MyMonadicSMemory.DummyID
end

module WISL_ALoc = struct
  include WISL_Base
  module MonadicSMemory = WISL.MonadicSMemory_ALoc
end

module WISL_Split = struct
  include WISL_Base
  module MonadicSMemory = WISL.MonadicSMemory_Split
end

module JSIL_Base = struct
  module MonadicSMemory = JSIL.MonadicSMemory_Base
  module ParserAndCompiler = JSIL.ParserAndCompiler
  module ExternalSemantics = JSIL.ExternalSemantics
  module InitData = Gillian.General.Init_data.Dummy
  module MyInitData = States.MyMonadicSMemory.DummyID
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

module C_Base = struct
  module MonadicSMemory = C.MonadicSMemory_Base
  module ParserAndCompiler = Cgil_lib.CParserAndCompiler
  module ExternalSemantics = C.ExternalSemantics
  module InitData = Cgil_lib.Global_env
  module MyInitData = C.MyInitData
end

module C_ALoc = struct
  include C_Base
  module MonadicSMemory = C.MonadicSMemory_ALoc
end

module C_Split = struct
  include C_Base
  module MonadicSMemory = C.MonadicSMemory_Split
end
