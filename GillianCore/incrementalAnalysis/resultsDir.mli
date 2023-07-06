val prev_results_exist : unit -> bool

val read_verif_results :
  unit -> SourceFiles.t * Call_graph.t * VerificationResults.t

val read_biabduction_results :
  unit -> SourceFiles.t * Call_graph.t * BiAbductionResults.t

val read_symbolic_results : unit -> SourceFiles.t * Call_graph.t

val read_bulk_symbolic_results :
  unit -> (string, SourceFiles.t) Hashtbl.t * (string, Call_graph.t) Hashtbl.t

val write_verif_results :
  SourceFiles.t -> Call_graph.t -> diff:string -> VerificationResults.t -> unit

val write_biabduction_results :
  SourceFiles.t -> Call_graph.t -> diff:string -> BiAbductionResults.t -> unit

val write_symbolic_results :
  SourceFiles.t -> Call_graph.t -> diff:string -> unit

val write_bulk_symbolic_results :
  tests_ran:string list ->
  (string, SourceFiles.t) Hashtbl.t ->
  (string, Call_graph.t) Hashtbl.t ->
  unit
