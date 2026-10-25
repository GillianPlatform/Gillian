module Utils = Prebuilt_utils
module WISL = Prebuilt_wisl.WISL
module WISLF = Prebuilt_wisl.WISLF
module JSIL = Prebuilt_js.JSIL
module C = Prebuilt_c.C
module C_states = Prebuilt_c.C_states

module Lib = struct
  include Prebuilt_wisl.Lib
  include Prebuilt_js.Lib
  include Prebuilt_c.Lib
end
