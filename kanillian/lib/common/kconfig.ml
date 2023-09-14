let machine_model = ref Machine_model.archi64
let kstats_file : string option ref = ref None
let harness : string option ref = ref None
let archi = ref Archi.Arch64
let endianness : [ `LittleEndian | `BigEndian ] ref = ref `BigEndian
let hide_genv = ref false

let ptr64 () =
  match !archi with
  | Arch64 -> true
  | Arch32 -> false

let ptr_chunk () : Chunk.t =
  match !archi with
  | Arch64 -> U64
  | Arch32 -> U32

let nondet_on_missing = ref true
