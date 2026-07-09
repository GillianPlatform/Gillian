(** Transitional (predicate-refactor Phase 8): the matching walker knows the
    expected out-parameters of the core predicate it is about to consume (those
    already known through the substitution), and the legacy predicate
    consumption used them to {i score} candidate predicates. The
    [MonadicSMemory.consume] interface only carries the in-parameters, so this
    ambient, dynamically-scoped channel forwards the known outs to the
    predicate-carrying memory for candidate selection only — they are still
    checked (never assumed) by the walker afterwards.

    This dies together with [PState], when the memory interfaces can evolve. *)

let current : Expr.t option list option ref = ref None

let with_hint (h : Expr.t option list) (f : unit -> 'a) : 'a =
  let saved = !current in
  current := Some h;
  Fun.protect ~finally:(fun () -> current := saved) f

let take () : Expr.t option list option = !current
