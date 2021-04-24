(** Logging levels *)
type level =
  | Normal  (** Normal output *)
  | Verbose  (** Verbose output *)
  | TMI  (** Too much information *)

(** Type specifying the logging mode *)
type t = Disabled | Enabled of level

let logging_mode = ref @@ Enabled Verbose

(** Returns whether logging is enabled or not *)
let enabled () = !logging_mode <> Disabled

(** Sets the logging mode *)
let set_mode mode = logging_mode := mode

(** Returns whether a message of a given level should be logged *)
let should_log msg_lvl =
  match !logging_mode with
  | Disabled            -> false
  | Enabled enabled_lvl ->
      let int_of_level = function
        | Normal  -> 1000
        | Verbose -> 2000
        | TMI     -> 3000
      in
      int_of_level msg_lvl <= int_of_level enabled_lvl
