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
