open Compcert
open Config_compcert

let burn_csm = ref false

module TargetLangOptions = struct
  open Cmdliner

  type t = { burn_csm : bool; hide_genv : bool }

  let term =
    let docs = Manpage.s_common_options in
    let doc = "If you want to write the intermediate C#m program into a file" in
    let bcsm = Arg.(value & flag & info [ "burn-csm" ] ~docs ~doc) in
    let doc =
      "If you want to hide the global environment from the reporting of heap"
    in
    let hgenv = Arg.(value & flag & info [ "hide-genv" ] ~docs ~doc) in
    let f burn_csm hide_genv = { burn_csm; hide_genv; } in
    Term.(const f $ bcsm $ hgenv)

  let apply { burn_csm = bcsm; hide_genv } =
    burn_csm := bcsm;
    Config.hide_genv := hide_genv;
end

type err = Errors.errmsg

let pp_err fmt e = Driveraux.print_error fmt e

let get_or_print_and_die = function
  | Errors.OK e    -> e
  | Errors.Error e ->
      Format.printf "%a\n@?" Driveraux.print_error e;
      exit 1

let parse_annots file =
  let parse_with_error token lexbuf =
    try token Annot_lexer.read lexbuf with
    | Annot_lexer.SyntaxError message -> failwith ("SYNTAX ERROR : " ^ message)
    | Annot_parser.Error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let message =
          Printf.sprintf "unexpected token : %s at loc (%i, %i)"
            (Lexing.lexeme lexbuf) curr.pos_lnum
            (curr.pos_cnum - curr.pos_bol + 1)
        in
        failwith ("PARSER ERROR : " ^ message)
  in
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let wprog = parse_with_error Annot_parser.prog lexbuf in
  let () = close_in inx in
  wprog

let parse_and_compile_file path =
  let () = Frontend.init () in
  let () = Warnings.as_error () in
  let () = Optim.disable_all () in
  (* Disable all optims *)
  let pathi = Filename.chop_extension path ^ ".i" in
  let () = Frontend.preprocess path pathi in
  let s = Frontend.parse_c_file path pathi in
  let s = get_or_print_and_die (SimplExpr.transl_program s) in
  let last_clight = get_or_print_and_die (SimplLocals.transf_program s) in
  let csm = get_or_print_and_die (Cshmgen.transl_program last_clight) in
  let () =
    if !burn_csm then
      let pathcsm = Filename.chop_extension path ^ ".csm" in
      let oc = open_out pathcsm in
      let fmt = Format.formatter_of_out_channel oc in
      let () = Format.fprintf fmt "%a" PrintCsharpminor.print_program csm in
      close_out oc
  in
  let annots = parse_annots path in
  Ok
    (Gilgen.trans_program_with_annots
       !Gillian.Utils.Config.current_exec_mode
       last_clight csm annots)

let other_imports = []

let env_var_import_path = Some CConstants.Imports.env_path_var

let initialize = function
  | Gillian.Utils.ExecMode.BiAbduction ->
      let () = Gillian.Utils.Config.bi_unfold_depth := 2 in
      Gillian.Utils.Config.delay_entailment := true
  | Verification -> Gillian.Utils.Config.delay_entailment := false
  | _ -> ()
