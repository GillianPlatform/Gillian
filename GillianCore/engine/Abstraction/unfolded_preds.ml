(** Table of user predicates that {!Logic_preprocessing} has auto-unfolded.

    It is written by [LogicPreprocessing] (which lives in the separate [verify]
    library) but only read by {!Matcher} (which stays in the engine). Keeping
    the table itself in the engine lets [Matcher] read it without the engine
    having to depend on [verify] — which would be a dependency cycle. *)

let tbl : (string, Pred.t) Hashtbl.t = Hashtbl.create Config.small_tbl_size
