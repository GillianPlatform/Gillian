open Monadic

let if_then_else guard ~then_branch ~else_branch =
  Delayed.branch_on guard ~then_branch ~else_branch
