open Monadic

let if_then_else guard ~then_branch ~else_branch =
  Delayed.branch_on guard ~then_:then_branch ~else_:else_branch

let if_sure_then_else guard ~then_branch ~else_branch =
  Delayed.if_sure guard ~then_:then_branch ~else_:else_branch

let branch_entailment = Delayed.branch_entailment
let true_formula = Gil_syntax.Formula.True
