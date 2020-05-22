module Mode : sig
  (** Logging levels *)
  type level =
    | Normal  (** Normal output *)
    | Verbose  (** Verbose output *)
    | TMI  (** Too much information *)

  type t = Disabled | Enabled of level

  val enabled : unit -> bool

  val set_mode : t -> unit
end

module Report : sig
  type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification

  type severity = Info | Log | Success | Error | Warning

  type 'a t
end

module Reporter : sig
  type 'a t = < log : 'a Report.t -> unit ; wrap_up : unit >
end

module FileReporter : sig
  val enable : unit -> unit

  class virtual ['a] t :
    object
      method log : 'a Report.t -> unit

      method wrap_up : unit

      method virtual private log_specific : 'a -> unit

      method private formatter : Format.formatter
    end
end

module DatabaseReporter : sig
  val enable : unit -> unit

  class virtual ['a] t :
    object
      method log : 'a Report.t -> unit

      method wrap_up : unit

      method virtual private specific_serializer : 'a -> Yojson.Safe.t
    end
end

(** Closes all the files *)
val wrap_up : unit -> unit

(** `normal` is just `log Normal` *)
val normal :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** `verbose` is just `log Verbose` *)
val verbose :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** `tmi` is just `log TMI` *)
val tmi :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** Writes the string and then raises a failure. *)
val fail : string -> 'a

(** Output the strings in every file and prints it to stdout *)
val print_to_all : string -> unit

val normal_phase :
  ?title:string -> ?severity:Report.severity -> Report.phase -> unit

val verbose_phase :
  ?title:string -> ?severity:Report.severity -> Report.phase -> unit

val tmi_phase :
  ?title:string -> ?severity:Report.severity -> Report.phase -> unit

val end_phase : Report.phase -> unit

module Make : functor
  (TargetLang : sig
     type t

     val file_reporter : t FileReporter.t option

     val database_reporter : t DatabaseReporter.t option
   end)
  -> sig
  val normal :
    ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val verbose :
    ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val tmi : ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val wrap_up : unit -> unit
end

(*$
  let () =
    print_newline ();
    let dec i =
      let args =
        let rec aux = function
          | j when j == i + 1 -> Printf.sprintf "'a%d" j
          | j -> Printf.sprintf "'a%d -> %s" j @@ aux @@ (j + 1)
        in
        aux 1
      in
      Printf.printf
        "val with_phase%d : Mode.level -> ?title:string -> \
         ?severity:Report.severity -> Report.phase -> (%s) -> %s"
        i args args;
      print_newline ()
    in
    for i = 1 to 5 do
      dec i
    done
*)
val with_phase1 :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Report.phase ->
  ('a1 -> 'a2) ->
  'a1 ->
  'a2

val with_phase2 :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Report.phase ->
  ('a1 -> 'a2 -> 'a3) ->
  'a1 ->
  'a2 ->
  'a3

val with_phase3 :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Report.phase ->
  ('a1 -> 'a2 -> 'a3 -> 'a4) ->
  'a1 ->
  'a2 ->
  'a3 ->
  'a4

val with_phase4 :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Report.phase ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) ->
  'a1 ->
  'a2 ->
  'a3 ->
  'a4 ->
  'a5

val with_phase5 :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Report.phase ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) ->
  'a1 ->
  'a2 ->
  'a3 ->
  'a4 ->
  'a5 ->
  'a6

(*$*)
