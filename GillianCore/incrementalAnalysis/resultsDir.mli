val prev_results_exist : unit -> bool

val read_verif_results :
  unit -> SourceFiles.t * CallGraph.t * VerificationResults.t

val read_biabduction_results :
  unit -> SourceFiles.t * CallGraph.t * BiAbductionResults.t

val read_symbolic_results : unit -> SourceFiles.t * CallGraph.t

val read_bulk_symbolic_results :
  unit -> (string, SourceFiles.t) Hashtbl.t * (string, CallGraph.t) Hashtbl.t

val write_verif_results :
  SourceFiles.t -> CallGraph.t -> diff:string -> VerificationResults.t -> unit

val write_biabduction_results :
  SourceFiles.t -> CallGraph.t -> diff:string -> BiAbductionResults.t -> unit

val write_symbolic_results : SourceFiles.t -> CallGraph.t -> diff:string -> unit

val write_bulk_symbolic_results :
  tests_ran:string list ->
  (string, SourceFiles.t) Hashtbl.t ->
  (string, CallGraph.t) Hashtbl.t ->
  unit
