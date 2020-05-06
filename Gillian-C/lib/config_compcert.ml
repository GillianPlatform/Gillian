open Compcert.Clflags

module Warnings = struct
  let options = Compcert.Diagnostics.warning_options

  let rec find_unit_cmd pat = function
    | [] -> failwith "Couldn't find how to disable warnings"
    | (Compcert.Commandline.Exact patb, Compcert.Commandline.Unit wn) :: _
      when String.equal patb pat -> wn
    | _ :: r -> find_unit_cmd pat r

  let wnothing = find_unit_cmd "-w" options

  let silence_preprocessor () = prepro_options := "-w" :: !prepro_options

  let silence_all () =
    wnothing ();
    silence_preprocessor ()

  let as_error = find_unit_cmd "-Werror" options
end

module Optimisations = struct
  let options =
    [
      option_ftailcalls;
      option_fifconversion;
      option_fconstprop;
      option_fcse;
      option_fredundancy;
      option_finline;
      option_finline_functions_called_once;
    ]

  let disable_all () = List.iter (fun r -> r := false) options
end

module Preprocessor = struct
  let add_include_dirs include_dirs =
    List.iter
      (fun dir -> prepro_options := dir :: "-I" :: !prepro_options)
      include_dirs

  let set_gnuc_for_macos () =
    let is_darwin =
      try
        let ic = Unix.open_process_in "uname" in
        let uname = input_line ic in
        let () = close_in ic in
        String.equal uname "Darwin"
      with _ -> false
    in
    if is_darwin then prepro_options := "__GNUC__=4" :: "-D" :: !prepro_options

  let set_output_dependencies_opts out_path =
    let target_opt = [ "files"; "-MT" ] in
    let output_file_opt = [ out_path; "-MF" ] in
    prepro_options := output_file_opt @ target_opt @ ("-MM" :: !prepro_options)

  let get_options () = !prepro_options

  let restore_options options = prepro_options := options
end
