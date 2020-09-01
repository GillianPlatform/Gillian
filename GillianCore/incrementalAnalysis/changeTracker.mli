type proc_changes = {
  changed_procs : string list;
  new_procs : string list;
  deleted_procs : string list;
  dependent_procs : string list;
}

type lemma_changes = {
  changed_lemmas : string list;
  new_lemmas : string list;
  deleted_lemmas : string list;
  dependent_lemmas : string list;
}

val pp_proc_changes : Format.formatter -> proc_changes -> unit

val get_changes :
  ('a, 'b) Prog.t ->
  prev_source_files:SourceFiles.t ->
  prev_call_graph:CallGraph.t ->
  cur_source_files:SourceFiles.t ->
  proc_changes

val get_verif_changes :
  ('a, 'b) Prog.t ->
  prev_source_files:SourceFiles.t ->
  prev_call_graph:CallGraph.t ->
  cur_source_files:SourceFiles.t ->
  proc_changes * lemma_changes
