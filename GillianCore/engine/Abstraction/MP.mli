open Containers

type outs = (Expr.t * Expr.t) list

val outs_pp : outs Fmt.t

(** The [step] type represents a matching plan step,
    consisting of an assertion together with the possible
    learned outs *)
type step = Asrt.simple * outs [@@deriving yojson]

type label = string * SS.t [@@deriving yojson]
type post = Flag.t * Asrt.t list [@@deriving yojson]

val pp_step : step Fmt.t

type t =
  | Choice of t * t
  | ConsumeStep of step * t
  | LabelStep of label * t
      (** Labels provide additional existentials to be bound manually by the user *)
  | Finished of post option
      (** The optional assertion corresponds to some post-condition that may be produced after successfuly matching.
          For example, a matching plan corresponding to a set of specifications will contain leaves that are respectively anntated with the corresponding post. *)
[@@deriving yojson]

type pred = { pred : Pred.t; def_mp : t; guard_mp : t option }
type 'a with_mp = { mp : t; data : 'a }
type spec = Spec.t with_mp
type lemma = Lemma.t with_mp

type 'annot prog = {
  preds : (string, pred) Hashtbl.t;
  specs : (string, spec) Hashtbl.t;
  lemmas : (string, lemma) Hashtbl.t;
  coverage : (string * int, int) Hashtbl.t;
  prog : ('annot, int) Prog.t;
}

type preds_tbl_t = (string, pred) Hashtbl.t

type err =
  | MPSpec of string * Asrt.t list
  | MPPred of string * Asrt.t list
  | MPLemma of string * Asrt.t list
  | MPAssert of Asrt.t * Asrt.t list
  | MPInvariant of Asrt.t * Asrt.t list
[@@deriving show]

module KB = Expr.Set

val learn_expr :
  ?top_level:bool -> KB.t -> Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> outs

val ins_outs_expr : KB.t -> Expr.t -> Expr.t -> (KB.t * outs) list
val simplify_asrts : Asrt.t -> Asrt.t

val s_init_atoms :
  preds:(string, int list) Hashtbl.t ->
  KB.t ->
  Asrt.t ->
  (step list, Asrt.t) result

val of_step_list : ?post:post -> ?label:label -> step list -> t

val init :
  ?use_params:bool ->
  KB.t ->
  KB.t ->
  (string, int list) Hashtbl.t ->
  (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option)) list ->
  (t, Asrt.t list) result

val init_prog :
  ?preds_tbl:(string, pred) Hashtbl.t ->
  ('a, int) Prog.t ->
  ('a prog, err) result

val init_preds :
  (string, Pred.t) Hashtbl.t -> ((string, pred) Hashtbl.t, err) result

val pp : Format.formatter -> t -> unit
val get_pred_def : preds_tbl_t -> string -> pred
val init_pred_defs : unit -> preds_tbl_t
val pp_pred_defs : Format.formatter -> preds_tbl_t -> unit
val get_procs : 'a prog -> ('a, int) Proc.t list
val get_bispecs : 'a prog -> BiSpec.t list

val pp_asrt :
  ?preds_printer:(Format.formatter -> string * Expr.t list -> unit) ->
  preds:preds_tbl_t ->
  Format.formatter ->
  Asrt.t ->
  unit

val pp_spec :
  ?preds_printer:(Format.formatter -> string * Expr.t list -> unit) ->
  preds:preds_tbl_t ->
  Format.formatter ->
  Spec.t ->
  unit

val pp_normal_spec :
  ?preds_printer:(Format.formatter -> string * Expr.t list -> unit) ->
  preds:preds_tbl_t ->
  Format.formatter ->
  Spec.t ->
  unit

val add_spec : 'a prog -> Spec.t -> unit
val remove_spec : 'a prog -> string -> unit
val get_lemma : 'a prog -> string -> (lemma, unit) result
val update_coverage : 'a prog -> string -> int -> unit
val first_time_running : 'a prog -> string -> int -> bool
