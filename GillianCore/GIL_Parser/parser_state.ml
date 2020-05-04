let procs_with_no_paths = ref Containers.SS.empty

let internal_file = ref false

let reset () =
  procs_with_no_paths := Containers.SS.empty;
  internal_file := false
