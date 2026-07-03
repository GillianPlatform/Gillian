(** @canonical Gillian.General.StateErr *)

(** @canonical Gillian.General.StateErr.err_t *)
type ('mem_err, 'value) t =
  | EMem of 'mem_err  (** Memory error, depends on instantiation *)
  | EPure of Expr.t (* Missing formula that should be true *)
  | EVar of Var.t (* Undefined variable *)
  | EAsrt of ('value list * Expr.t)
  (* Assertion that failed, with the relevant values for unfolding. *)
  | EOther of string
    (* We want all errors to be proper errors - this is a temporary placeholder *)
[@@deriving yojson, show]

let get_recovery_tactic
    (errs : ('a, 'b) t list)
    (mem_recovery_tactics : 'a -> 'b Recovery_tactic.t) : 'b Recovery_tactic.t =
  let f err =
    match err with
    | EMem err -> mem_recovery_tactics err
    | EAsrt (xs, _) -> Recovery_tactic.try_unfold xs
    | _ -> Recovery_tactic.none
  in
  List.fold_left
    (fun acc x -> Recovery_tactic.merge (f x) acc)
    Recovery_tactic.none errs

let pp_err
    (pp_m_err : Format.formatter -> 'a -> unit)
    (pp_v : Format.formatter -> 'b -> unit)
    (fmt : Format.formatter)
    (err : ('a, 'b) t) : unit =
  match err with
  | EMem m_err -> pp_m_err fmt m_err
  | EPure f -> Fmt.pf fmt "Pure assertion failed: %a" Expr.pp f
  | EVar x -> Fmt.pf fmt "Undefined variable: %s" x
  | EAsrt ([], f) -> Fmt.pf fmt "Assertion failed: %a" Expr.pp f
  | EAsrt (vs, f) ->
      Fmt.pf fmt "Assertion failed for %a: %a"
        (Fmt.list ~sep:(Fmt.any ", ") pp_v)
        vs Expr.pp f
  | EOther msg -> Fmt.pf fmt "%s" msg

let can_fix (can_fix_mem : 'a -> bool) (err : ('a, 'b) t) : bool =
  match err with
  | EMem mem_err -> can_fix_mem mem_err
  | EPure pf -> Reduction.reduce_lexpr pf <> Expr.false_
  | EAsrt (_, pf) ->
      let result = Reduction.reduce_lexpr pf <> Expr.false_ in
      Logging.verbose (fun fmt -> fmt "Can fix: %a: %b" Expr.pp pf result);
      result
  | _ -> false

let get_failing_constraint (err : ('a, 'b) t) (mem_fc : 'a -> Expr.t) : Expr.t =
  match err with
  | EMem m_err -> mem_fc m_err
  | EPure f -> Expr.Infix.not f
  | _ -> Expr.true_
