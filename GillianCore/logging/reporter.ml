type 'a t =
  < log : 'a Report.t -> unit
  ; log_specific : Loggable.loggable -> unit
  ; wrap_up : unit >

module Make (P : sig
  type conf

  val conf : conf

  type state

  val initialize : conf -> state

  val wrap_up : state -> unit
end) =
struct
  let enabled, enable =
    let enabled = ref false in
    ((fun () -> !enabled), fun () -> enabled := true)

  type conf = P.conf

  type state = P.state

  let state = ref None

  let get_state () =
    match !state with
    | None   ->
        let s = P.initialize P.conf in
        state := Some s;
        s
    | Some s -> s

  let wrap_up () =
    match !state with
    | None   -> ()
    | Some s -> P.wrap_up s
end
