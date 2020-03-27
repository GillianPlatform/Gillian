module type S = sig
  module TargetLangOptions : sig
    (** Command line options specific to the target language. *)
    type t

    val term : t Cmdliner.Term.t
    (** A term that will be added to every command. *)

    val apply : t -> unit
    (** A side-effect function that will determine the behaviour of the target-language specific options *)
  end

  (** Type of error that can occur during parsing or compilation *)
  type err

  val pp_err : Format.formatter -> err -> unit
  (** Pretty printer for type {!err} *)

  val parse_and_compile_files :
    string list -> ((Annot.t, string) Prog.t, err) result
  (** Takes the name of the file and return an ast or an error *)

  val other_imports :
    (string * (string -> ((Annot.t, string) Prog.t, err) result)) list

  (** [other_imports] is a association list, that maps extensions to a parser and compiler functions.
     For example, it is possible to import jsil file in a gil program, using [import file.jsil].
     In order to do so, the [other_imports] list should contain the double [("jsil", parse_and_compile_jsil_file)] where
     [parse_and_compile_jsil_file] is a function that takes a file name, parses that jsil file and compiles it to a gil program *)

  val env_var_import_path : string option
  (** Contains the name of the environment variable which contains the path to where the runtime is stored. *)

  val initialize : ExecMode.t -> unit
  (** Function that will be executed at initialisation. It will be passed the current execution mode as parameter *)
end

module Dummy : S = struct
  module TargetLangOptions = struct
    type t = unit

    let term = Cmdliner.Term.(const ())

    let apply () = ()
  end

  type err = unit

  let pp_err _ _ =
    failwith
      "Please implement the compiling interface to use with the '-compile' \
       flag or test suites"

  let parse_and_compile_files _ =
    failwith
      "Please implement the compiling interface to use with the '-compile' \
       flag or test suites"

  let other_imports = []

  let import_paths = []

  let env_var_import_path = None

  let initialize _ = ()
end
