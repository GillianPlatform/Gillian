(** Takes a path to a file and returns the parsed GIL program. *)
val parse_eprog_from_file : string -> (Annot.t, string) Prog.t

(** Converts a string-labelled [Prog.t] to an index-labelled [Prog.t],
    resolving the imports in the meantime. The parameter [other_imports] is an
    association list that maps extensions to a parser and compiler. For example,
    it is possible to import a JSIL file in a GIL program using
    [import "file.jsil";]. In order to do so, the [other_imports] list should
    contain the tuple [("jsil", parse_and_compile_jsil_file)] where
    [parse_and_compile_jsil_file] is a function that takes a file path, parses
    the file as a JSIL program, and compiles this to a GIL program. *)
val eprog_to_prog :
  other_imports:(string * (string -> (Annot.t, string) Prog.t)) list ->
  (Annot.t, string) Prog.t ->
  (Annot.t, int) Prog.t

(** Caches a mapping from the output GIL filepaths to the corresponding
    sring-labelled GIL programs. Can be called before [eprog_to_prog] in order
    to allow the import-resolving mechanism to work without having to first
    write the GIL programs to file. *)
val cache_labelled_progs : (string * (Annot.t, string) Prog.t) list -> unit
