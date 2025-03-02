module type S = sig
  type annot

  type parsing_result = {
    labeled_prog : (annot, string) Prog.t;  (** The parsed program *)
    init_data : Yojson.Safe.t;  (** Will be `Null if no [init_data] is parsed *)
  }

  (** Takes a path to a file and returns the parsed GIL program with its global environment. *)
  val parse_eprog_from_file : string -> parsing_result Gillian_result.t

  (** Takes a string containing a GIL program and parses it. *)
  val parse_eprog_from_string : string -> parsing_result Gillian_result.t

  (** Converts a string-labelled [Prog.t] to an index-labelled [Prog.t],
      resolving the imports in the meantime. The parameter [other_imports] is an
      association list that maps extensions to a parser and compiler. For example,
      it is possible to import a JSIL file in a GIL program using
      [import "file.jsil";]. In order to do so, the [other_imports] list should
      contain the tuple [("jsil", parse_and_compile_jsil_file)] where
      [parse_and_compile_jsil_file] is a function that takes a file path, parses
      the file as a JSIL program, and compiles this to a GIL program. *)
  val eprog_to_prog :
    ?prog_path:string ->
    other_imports:
      (string * (string -> (annot, string) Prog.t Gillian_result.t)) list ->
    (annot, string) Prog.t ->
    (annot, int) Prog.t Gillian_result.t

  (** Caches a mapping from the output GIL filepaths to the corresponding
      sring-labelled GIL programs. Can be called before [eprog_to_prog] in order
      to allow the import-resolving mechanism to work without having to first
      write the GIL programs to file. *)
  val cache_labelled_progs : (string * (annot, string) Prog.t) list -> unit

  (** Parses a [Literal.t] from a lexbuf; raises [Failure] if parsing fails. *)
  val parse_literal : Lexing.lexbuf -> Literal.t Gillian_result.t

  (** Parses a [Expr.t] from a lexbuf; raises [Failure] if parsing fails. *)
  val parse_expression : Lexing.lexbuf -> Expr.t Gillian_result.t
end

module type Intf = sig
  module type S = S

  module Make (Annot : Annot.S) : S with type annot = Annot.t
end
