(** Logging Level *)
type level =
  | Normal  (** Normal output *)
  | Verbose  (** Verbose output *)
  | Verboser  (** More verbose output *)
  | TMI  (** Too much information *)

val silent : bool ref
(** If true, logging is disabled. False by default. *)

val wrap_up : unit -> unit
(** Closes all the files *)

val log : level -> ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** This works like a very simplified version of opam's `Logs` library.
    `log TMI (fun m -> m "%a" pp_int 1)`
    will write `1` in the TMI file.
    `log <level>` will write at every level that is stronger than the given level.
    For example, `log Normal` will write in every file.
    The pattern is quite simple: it is as if you were formatting with a printf-like function except
    you get this function in the m argument of the function you give to the logging function.
 *)

val normal : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `normal` is just `log Normal` *)

val verbose : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `verbose` is just `log Verbose` *)

val verboser : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `verboser` is just `log Verboser` *)

val tmi : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `tmi` is just `log TMI` *)

val fail : string -> 'a
(** Writes the string and then raises a failure. *)

val print_to_all : string -> unit
(** Output the strings in every file and prints it to stdout *)
