open Compcert
open CConstants
open Gillian.Gil_syntax

type symbol

module Symbol_set = Gillian.Utils.Containers.SS

val is_def_sym : symbol -> bool

val sym_name : symbol -> string

(** Data exported during compilation and used during linking. *)
type compilation_data = {
  genv_pred_asrts : Asrt.t list;
  genv_init_cmds : string Cmd.t list;
  symbols : symbol list;
}

val make_init_proc : string Cmd.t list -> (Annot.t, string) Proc.t

val trans_program :
  ?exec_mode:ExecMode.t ->
  ?gil_annot:Gil_logic_gen.gil_annots ->
  clight_prog:Clight.program ->
  filepath:string ->
  mangled_syms:(string, string) Hashtbl.t ->
  Csharpminor.program ->
  (Annot.t, string) Prog.t * compilation_data

val trans_program_with_annots :
  ExecMode.t ->
  Clight.program ->
  Csharpminor.program ->
  filepath:string ->
  mangled_syms:(string, string) Hashtbl.t ->
  CLogic.CProg.t ->
  (Annot.t, string) Prog.t * compilation_data
