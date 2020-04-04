module Warnings = struct
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

  let as_error = find_unit_cmd "-Werror" options
end

module Optim = struct
  open Compcert.Clflags

  let optimization_options =
    [
      option_ftailcalls;
      option_fifconversion;
      option_fconstprop;
      option_fcse;
      option_fredundancy;
      option_finline;
      option_finline_functions_called_once;
    ]

  let disable_all () = List.iter (fun r -> r := false) optimization_options
end

module Include = struct
  let add_include_dirs include_dirs =
    let open Compcert.Clflags in
    List.iter
      (fun dir -> prepro_options := dir :: "-I" :: !prepro_options)
      include_dirs
end
