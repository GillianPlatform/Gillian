module Values = SVal.M
module Subst = SVal.SESubst
module PureContext = PFS
module TypEnv = TypEnv

module FOLogic = struct
  module Reduction = Reduction
end

module type Memory_S = SMemory.S
