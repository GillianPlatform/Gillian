open Compcert.Clflags

let set_verbose_invocation () = option_v := true

module Features = struct
  let set_fstruct_passing b = option_fstruct_passing := b
end

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


let references_to_ignore = ["__compcert_i64_dtos";
"__compcert_i64_dtou";
"__compcert_i64_sar";
"__compcert_i64_sdiv";
"__compcert_i64_shl";
"__compcert_i64_shr";
"__compcert_i64_smod";
"__compcert_i64_smulh";
"__compcert_i64_stod";
"__compcert_i64_stof";
"__compcert_i64_udiv";
"__compcert_i64_umod";
"__compcert_i64_umulh";
"__compcert_i64_utod";
"__compcert_i64_utof";
"__compcert_va_composite";
"__compcert_va_float64";
"__compcert_va_int32";
"__compcert_va_int64"]