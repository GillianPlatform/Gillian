(** @canonical Gillian.Command_line.ParserAndCompiler

  This defines an interface that allows a user to indicate how to parse their own programming language,
  preprocess the obtained language and compile it to GIL (type [Prog.t]) *)

type ('annot, 'tl_ast, 'init_data) compiled_progs = {
  gil_progs : (string * ('annot, string) Prog.t) list;
  source_files : SourceFiles.t;
  tl_ast : 'tl_ast;
  init_data : 'init_data;
}

let get_progs_or_fail ~pp_err = function
  | Ok progs -> (
      match progs.gil_progs with
      | [] ->
          Fmt.pr "Error: expected at least one GIL program\n";
          exit 1
      | _ -> progs)
  | Error err ->
      Fmt.pr "Error during compilation to GIL:\n%a" pp_err err;
      exit 1

module type S = sig
  module TargetLangOptions : sig
    (** Command line options specific to the target language. *)
    type t

    (** A term that will be added to every command. *)
    val term : t Cmdliner.Term.t

    (** A side-effect function that will determine the behaviour of the target-language specific options *)
    val apply : t -> unit
  end

  type init_data

  (** Type of error that can occur during parsing or compilation *)
  type err

  (** Type of the target language AST *)
  type tl_ast

  module Annot : Annot.S

  (** Pretty printer for type {!err} *)
  val pp_err : Format.formatter -> err -> unit

  (** Takes a set of source file paths, parses them with the user's language, and
      then compiles them to a single or a set of GIL programs. The returned GIL
      program(s) should be ready to be analysed. *)
  val parse_and_compile_files :
    string list -> (Annot.t, tl_ast, init_data) compiled_progs Gillian_result.t

  (** [other_imports] is an association list that maps extensions to a parser
      and compiler. For example, it is possible to import a JSIL file in a GIL
      program using [import "file.jsil";]. In order to do so, the [other_imports]
      list should contain the tuple [("jsil", parse_and_compile_jsil_file)] where
      [parse_and_compile_jsil_file] is a function that takes a file path, parses
      the file as a JSIL program, and compiles this to a GIL program. *)
  val other_imports :
    (string * (string -> (Annot.t, string) Prog.t Gillian_result.t)) list

  (** Contains the name of the environment variable which contains the path to where the runtime is stored. *)
  val default_import_paths : string list option

  (** Function that will be executed at initialisation. It will be passed the current execution mode as parameter *)
  val initialize : Exec_mode.t -> unit
end

(** Dummy ParserAndCompiler that will simply always fail. This is used when someone wants to build a command line interface
    to only reason about GIL. *)
module Dummy :
  S with type init_data = unit and type Annot.t = Gil_syntax.Annot.Basic.t =
struct
  module TargetLangOptions = struct
    type t = unit

    let term = Cmdliner.Term.(const ())
    let apply () = ()
  end

  type init_data = unit
  type tl_ast = unit

  module Annot = Annot.Basic

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
  let default_import_paths = None
  let initialize _ = ()
end
