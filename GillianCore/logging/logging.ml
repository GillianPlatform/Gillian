module Mode = Mode
module Report = Report

(** File prefix for log files *)
let log_prefix = "log_"

(** File extension for log files *)
let log_extension = "log"

(** 
  Log filenames 

  @param lvl Logging level
  @return Filename of the level
*)
let filename (lvl : Mode.level) : string =
  log_prefix
  ^ ( match lvl with
    | Normal   -> "normal"
    | Verbose  -> "verbose"
    | Verboser -> "verboser"
    | TMI      -> "TMI" )
  ^ "." ^ log_extension

(** File descriptors for log files *)

let oc_normal = open_out (filename Normal)

let oc_verbose = open_out (filename Verbose)

let oc_verboser = open_out (filename Verboser)

let oc_TMI = open_out (filename TMI)

let wrap_up () =
  close_out oc_normal;
  close_out oc_verbose;
  close_out oc_verboser;
  close_out oc_TMI

let log_string (lvl : Mode.level) msg =
  match lvl with
  | Normal   ->
      output_string oc_normal msg;
      output_string oc_verbose msg;
      output_string oc_verboser msg;
      output_string oc_TMI msg
  | Verbose  ->
      output_string oc_verbose msg;
      output_string oc_verboser msg;
      output_string oc_TMI msg
  | Verboser ->
      output_string oc_verboser msg;
      output_string oc_TMI msg
  | TMI      -> output_string oc_TMI msg

(* Unfortunately, since we're writing in several files,
   we have to write into a string first and then write the string several times *)
let log lvl msgf =
  if Mode.should_log lvl then
    msgf @@ fun fmt ->
    let k = log_string lvl in
    Format.kasprintf k (fmt ^^ "@\n@?")

let normal msgf = log Normal msgf

let verbose msgf = log Verbose msgf

let verboser msgf = log Verboser msgf

let tmi msgf = log TMI msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)
