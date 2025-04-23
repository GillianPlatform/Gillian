open Gil_syntax
open Gillian

type basic_proc = (Annot.Basic.t, string) Proc.t

module Template = struct
  type flags = NoUnsignedWrap | NoSignedWrap [@@deriving yojson]

  type bv_op_shape = { args : int list; width_of_result : int option }
  [@@deriving yojson]

  type op_spec =
    | ValueSpec of { flags : flags list; shape : bv_op_shape }
    | SimpleSpec
  [@@deriving yojson]

  type generator =
    | ValueOp of (pointer_width:int -> string -> bv_op_shape -> basic_proc)
    | SimpleOp of (pointer_width:int -> string -> basic_proc)

  type op_template = { name : string; generator : generator }
end

module type OpTemplates = sig
  val operations : Template.op_template list
end

module MonomorphizerCLI (OpT : OpTemplates) = struct
  open Cmdliner
  open Cmdliner.Term.Syntax

  let op_map =
    let open Template in
    Hashtbl.of_seq
      (List.to_seq (OpT.operations |> List.map (fun x -> (x.name, x))))

  type op_decl = {
    name : string;
    output_name : string;
    spec : Template.op_spec;
  }
  [@@deriving yojson]

  type runtime_spec = { pointer_width : int; op_requests : op_decl list }
  [@@deriving yojson]

  let parse_runtime_spec (value : Yojson.Safe.t) : runtime_spec =
    match runtime_spec_of_yojson value with
    | Ok spec -> spec
    | Error e ->
        Format.eprintf "Error parsing runtime spec: %s@." e;
        failwith "Invalid runtime spec"

  let fl_to_decls (fl : string) : runtime_spec =
    let json = Yojson.Safe.from_file fl in
    parse_runtime_spec json

  let apply_template
      (op : Template.op_template)
      (pointer_width : int)
      (spec : op_decl) : basic_proc =
    let open Template in
    match (op.generator, spec.spec) with
    | ValueOp f, ValueSpec nspec ->
        f ~pointer_width spec.output_name nspec.shape
    | SimpleOp f, SimpleSpec -> f ~pointer_width spec.output_name
    | _ -> failwith "Invalid template or spec"

  let produce_file (name : string) (rtspec : runtime_spec) =
    let open Proc in
    let procs =
      List.map
        (fun op ->
          apply_template (Hashtbl.find op_map op.name) rtspec.pointer_width op)
        rtspec.op_requests
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
    let rtspec = fl_to_decls target_file in
    produce_file output_file rtspec

  let () = if !Sys.interactive then () else exit (Cmd.eval cmd_generate)
end
