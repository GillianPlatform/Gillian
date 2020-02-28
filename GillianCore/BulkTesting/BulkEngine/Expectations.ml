module type S = sig
  type matcher

  type info

  type category

  type outcome

  val expectation : matcher -> (info, category) Test.t -> outcome -> unit
end
