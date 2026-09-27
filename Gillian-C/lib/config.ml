let include_dirs =
  ref
    [
      Gillian.Utils.Embedded.dir ~name:"gillian-c-includes"
        (List.map
           (fun f -> (f, Option.get (Include_files.read f)))
           Include_files.file_list);
    ]

let source_paths = ref ([] : string list)
let burn_csm = ref false
let hide_genv = ref false
let warnings = ref true
let hide_undef = ref false
let hide_mult_def = ref false
let verbose_compcert = ref false
let pp_full_tree = ref false
let allocated_functions = ref false
let alloc_can_fail = ref false
let cbmc = ref false
