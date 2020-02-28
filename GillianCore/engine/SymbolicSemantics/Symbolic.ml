module Values = SVal.M
module Subst = SVal.SSubst
module PureContext = PFS
module TypEnv = TypEnv

module FOLogic = struct
  module Reduction = Reduction
  module Simplifications = Simplifications
end

module type Memory_S = SMemory.S
