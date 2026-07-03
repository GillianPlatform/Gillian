open struct
  let machine_model = ref Machine_model.archi64
end

let include_dirs : string list ref = ref []
let source_dirs : string list ref = ref []
let kstats_file : string option ref = ref None
let harness : string option ref = ref None
let archi = ref Archi.Arch64
let endianness : [ `LittleEndian | `BigEndian ] ref = ref `BigEndian
let hide_genv = ref false
let nondet_on_missing = ref true
let print_unhandled = ref false
let get_machine_model () = !machine_model

let set_machine_model m =
  Chunk.set_ptr_width m.Machine_model.pointer_width;
  machine_model := m
