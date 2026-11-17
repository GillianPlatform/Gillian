(** Errors can be printed briefly, e.g. for a tooltip, leaving out details such
    as why each attempt at something failed. *)

let brief_mode = ref false

(** [brief f] runs [f], printing errors briefly *)
let brief f =
  let old = !brief_mode in
  brief_mode := true;
  Fun.protect ~finally:(fun () -> brief_mode := old) f

(** [pp_details pp] prints with [pp], unless printing errors briefly *)
let pp_details pp ft x = if not !brief_mode then pp ft x
