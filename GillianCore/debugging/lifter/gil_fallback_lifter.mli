(** A {!Lifter} implementation that acts as a proxy to another lifter
    ([TLLifter]), while also keeping a {!Gil_lifter} updated alongside.

    [TLLifter] can access the GIL lifter and its state via
    {!Gil_lifter_with_state}.

    Note that if the GIL lifter gives anything other than [Stop] when handling a
    command, it is considered skipped, and [TLLifter] won't be asked to handle
    it. *)

(** @inline *)
include Gil_fallback_lifter_intf.Intf
