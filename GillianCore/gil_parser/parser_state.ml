let procs_with_no_paths = ref SS.empty
let preds_with_no_paths = ref SS.empty
let lemmas_with_no_paths = ref SS.empty
let internal_file = ref false

let reset () =
  procs_with_no_paths := SS.empty;
  preds_with_no_paths := SS.empty;
  lemmas_with_no_paths := SS.empty;
  internal_file := false
