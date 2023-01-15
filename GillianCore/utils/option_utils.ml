(** Helper functions for [Option]s*)

(** Similar to {!Option.value}, but with a thunk for the default value *)
let or_else f = function
  | Some x -> x
  | None -> f ()

(** Returns the first option if it's [Some], otherwise the second option *)
let coalesce a b =
  match a with
  | Some _ -> a
  | None -> b
