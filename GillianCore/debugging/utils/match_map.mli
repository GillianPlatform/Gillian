(** A map of a matching.

    A matching is either a single string of assertions for most matches
    ({!Direct}) or potentially multiple strings of assertions when folding a
    predicate ({!Fold}), each representing a case of the predicate.

    Each string of assertions (dubbed {!match_seg}) is essentially a singly
    linked list of {!Assertion}s (each of which contains information about that
    assertion - see {!assertion_data}), terminated by a {!MatchResult} that
    states whether that matching (or that case in the matching) was a success.
*)

(** @inline *)
include Match_map_intf.Intf
