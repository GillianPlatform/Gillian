val prev_results_exist : unit -> bool

val read_verif_results :
  unit -> SourceFiles.t * CallGraph.t * VerificationResults.t

val read_biabduction_results :
  unit -> SourceFiles.t * CallGraph.t * BiAbductionResults.t

val write_verif_results :
  SourceFiles.t -> CallGraph.t -> diff:string -> VerificationResults.t -> unit

val write_biabduction_results :
  SourceFiles.t -> CallGraph.t -> diff:string -> BiAbductionResults.t -> unit
