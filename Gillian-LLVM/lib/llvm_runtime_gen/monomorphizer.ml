open Gil_syntax
open Gillian

type basic_proc = (Annot.Basic.t, string) Proc.t

type generator =
  | ValueOp of (string -> int -> basic_proc)
  | SimpleOp of (string -> basic_proc)

type op_template = { name : string; generator : generator }

module type OpTemplates = sig
  val operations : op_template list
end

module MonomorphizerCLI (OpT : OpTemplates) = struct
  open Cmdliner
  open Cmdliner.Term.Syntax

  let proc_name (name : string) (width : int option) =
    match width with
    | Some w -> Printf.sprintf "%s_%d" name w
    | None -> name

  let fl_to_widths (fl : string) : int list =
    let json = Yojson.Basic.from_file fl in
    let widths = Yojson.Basic.Util.to_list json in
    List.map
      (fun w ->
        match w with
        | `Int i -> i
        | _ -> failwith "Expected an integer")
      widths

  let apply_template (op : op_template) (widths : int list) : basic_proc list =
    match op.generator with
    | ValueOp f -> List.map (fun w -> f (proc_name op.name (Some w)) w) widths
    | SimpleOp f -> [ f (proc_name op.name None) ]

  let produce_file (name : string) (widths : int list) =
    let open Proc in
    let procs =
      List.map (fun op -> apply_template op widths) OpT.operations
      |> List.flatten
    in
    let proc_table =
      Hashtbl.of_seq
        (List.to_seq (List.map (fun proc -> (proc.proc_name, proc)) procs))
    in
    let prog = { (Prog.create ()) with procs = proc_table } in
    let fl = open_out name in
    Prog.pp ~show_labels:false ~pp_label:Fmt.string
      (Format.formatter_of_out_channel fl)
      prog;
    close_out fl

  let target_file =
    let info =
      Arg.info [] ~docv:"INPUT_FILE" ~doc:"List of widths to generate as json"
    in
    Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

  let output_file =
    let info =
      Arg.info [] ~docv:"OUTPUT_FILE"
        ~doc:"The file to write the generated code to"
    in
    Arg.required (Arg.pos 1 (Arg.some Arg.string) None info)

  let cmd_generate =
    Cmd.v (Cmd.info "Runtime code generator")
    @@
    let+ target_file = target_file and+ output_file = output_file in
    let widths = fl_to_widths target_file in
    produce_file output_file widths

  let () = if !Sys.interactive then () else exit (Cmd.eval cmd_generate)
end
