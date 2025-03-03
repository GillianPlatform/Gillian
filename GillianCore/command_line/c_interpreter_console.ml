open Cmdliner
open Command_line_utils
open Syntaxes.Result

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (CState : CState.S with type init_data = ID.t)
    (C_interpreter : G_interpreter.S
                       with type annot = PC.Annot.t
                        and type state_t = CState.t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t) : Console.S =
struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  let return_to_exit (ret_ok : bool) : unit =
    match ret_ok with
    | false -> exit 1
    | true -> ()

  let valid_concrete_result (ret : C_interpreter.result_t list) : bool =
    assert (List.length ret = 1);
    let ret = List.hd ret in
    match ret with
    | Exec_res.RSucc { flag = Flag.Normal; _ } -> true
    | _ -> false

  let run debug (prog : ('a, int) Prog.t) init_data : unit =
    let prog =
      match MP.init_prog prog with
      | Ok prog -> prog
      | _ -> failwith "Program could not be initialised"
    in
    let ret =
      C_interpreter.evaluate_proc
        (fun x -> x)
        prog !Config.entry_point [] (CState.init init_data)
    in
    let () =
      if debug then
        Format.printf "Final state: @\n%a@\n" C_interpreter.Logging.pp_result
          ret
    in
    return_to_exit (valid_concrete_result ret)

  let parse_eprog files already_compiled =
    if not already_compiled then (
      let+ progs = PC.parse_and_compile_files files in
      let e_progs, init_data = (progs.gil_progs, progs.init_data) in
      Gil_parsing.cache_labelled_progs (List.tl e_progs);
      (snd (List.hd e_progs), init_data))
    else
      let+ Gil_parsing.{ labeled_prog; init_data } =
        Gil_parsing.parse_eprog_from_file (List.hd files)
      in
      let init_data =
        match ID.of_yojson init_data with
        | Ok d -> d
        | Error e -> failwith e
      in
      (labeled_prog, init_data)

  let process_files files already_compiled debug outfile_opt =
    let* e_prog, init_data = parse_eprog files already_compiled in
    let () =
      burn_gil ~init_data:(ID.to_yojson init_data)
        ~pp_prog:(Prog.pp_labeled ?pp_annot:None)
        e_prog outfile_opt
    in
    let+ prog =
      Gil_parsing.eprog_to_prog ~other_imports:PC.other_imports e_prog
    in
    run debug prog init_data

  let exec files already_compiled debug outfile_opt no_heap entry_point () =
    let () = Config.current_exec_mode := Concrete in
    let () = Config.no_heap := no_heap in
    let () = Config.entry_point := entry_point in
    let () = PC.initialize Concrete in
    let r = process_files files already_compiled debug outfile_opt in
    let () = Common_args.exit_on_error r in
    exit 0

  let debug =
    let doc =
      "If you want the final state of concrete execution to be printed at the \
       end"
    in
    Arg.(value & flag & info [ "debug"; "print-final-state" ] ~doc)

  let exec_t =
    Term.(
      const exec $ files $ already_compiled $ debug $ output_gil $ no_heap
      $ entry_point)

  let exec_info =
    let doc = "Concretely executes a file of the target language" in
    let man =
      [
        `S Manpage.s_description;
        `P "Concretely executes a given file, after compiling it to GIL";
      ]
    in
    Cmd.info ~exits:Common_args.exit_code_info "exec" ~doc ~man

  let exec_cmd = Console.Normal (Cmd.v exec_info (Common_args.use exec_t))
  let cmds = [ exec_cmd ]
end
