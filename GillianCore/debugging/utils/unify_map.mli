(** A map of a unification.

  A unification is either a single string of assertions for most unifications
  ({!Direct}) or potentially multiple strings of assertions when folding a
  predicate ({!Fold}), each representing a case of the predicate.
  
  Each string of assertions (dubbed {!unify_seg}) is essentially a singly
  linked list of {!Assertion}s (each of which contains information about that
  assertion - see {!assertion_data}), terminated by a {!UnifyResult} that states
  whether that unification (or that case in the unification) was a success. *)

(** @inline *)
include Unify_map_intf.Intf
