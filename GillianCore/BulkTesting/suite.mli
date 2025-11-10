module type S = sig
  (** Additional information one may want to attach to a test case in order to
      help preprocessing it / interpreting its results *)
  type info

  val pp_info : Format.formatter -> info -> unit

  (** Tests will be launched by categories to help with the output *)
  type category

  val pp_category : Format.formatter -> category -> unit

  (** Return true to skip the given category of test *)
  val skip_category : category -> bool

  (** Takes the name of a file, decides if this file should be analysed. This
      should at least filter based on extention. *)
  val filter_source : string -> bool

  (** Takes the source of the file and its code, a list of name, info and
      category. Each element of the list corresponds to a new test to create *)
  val create_tests : string -> string -> (string * info * category) list

  (** Preprocessing that is done on each file *)
  val beforeTest : info -> string -> unit

  (** Lifecyle function that will be called before each test *)
  val beforeEach : unit -> unit

  (** Gives a list of files to setup. This should mostly be used to compute some
      information using only file names. Files will be read later *)
  val init_suite : string list -> unit

  (** This suite will be launched using [executable_name bulk cmd_name] (for
      example [gillian-js bulk test262] *)
  val cmd_name : string

  (** Execution mode that will be used for init in command-line *)
  val exec_mode : Exec_mode.t
end

module Dummy : S

module ByFolder (P : sig
  val max_depth : int
  val cmd_name : string
  val exec_mode : Exec_mode.t
end) : S with type category = string
