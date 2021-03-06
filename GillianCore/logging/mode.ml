(** Logging levels *)
type level =
  | Normal  (** Normal output *)
  | Verbose  (** Verbose output *)
  | TMI  (** Too much information *)

type t = Disabled | Enabled of level

let logging_mode = ref @@ Enabled Verbose

let enabled () = !logging_mode <> Disabled

let set_mode mode = logging_mode := mode

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
