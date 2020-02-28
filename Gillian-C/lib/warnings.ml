let options = Compcert.Diagnostics.warning_options

let rec find_unit_cmd pat = function
  | [] -> failwith "Couldn't find how to disable warnings"
  | (Compcert.Commandline.Exact patb, Compcert.Commandline.Unit wn) :: _
    when String.equal patb pat -> wn
  | _ :: r -> find_unit_cmd pat r

let wnothing = find_unit_cmd "-w" options

let silence_preprocess () =
  let open Compcert.Clflags in
  prepro_options := "-w" :: !prepro_options

let silence_all () =
  wnothing ();
  silence_preprocess ()
