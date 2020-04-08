(** This defines an interface that allows a user to indicate how to parse their own programming language,
    preprocess the obtained language and compile it to GIL (type [LabProg.t]) *)
module type S = sig
  module TargetLangOptions : sig
    (** {2 Target-language specific options}
        For more help, see Cmdliner documentation *)

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
  (** Takes a path to a file, parses it with the user's language, and then compiles it to a GIL program.
      The obtained GIL program should be ready to be analysed. *)

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

(** Dummy ParserAndCompiler that will simply always fail. This is used when someone wants to build a command line interface
    to only reason about GIL. *)
module Dummy : S
