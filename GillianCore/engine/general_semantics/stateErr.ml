(** @canonical Gillian.General.StateErr *)

(** @canonical Gillian.General.StateErr.err_t *)
type ('mem_err, 'value) t =
  | EMem of 'mem_err  (** Memory error, depends on instantiation *)
  | EType of 'value * Type.t option * Type.t
      (** Incorrect type, depends on value *)
  | EPure of Formula.t (* Missing formula that should be true *)
  | EVar of Var.t (* Undefined variable *)
  | EAsrt of ('value list * Formula.t * Asrt.t list list)
  | EOther of string
    (* We want all errors to be proper errors - this is a temporary placeholder *)
[@@deriving yojson, show]

let get_recovery_tactic
    (errs : ('a, 'b) t list)
    (mem_recovery_tactics : 'a -> 'b Recovery_tactic.t) : 'b Recovery_tactic.t =
  let f err =
    match err with
    | EMem err -> mem_recovery_tactics err
    | EAsrt (xs, _, _) -> Recovery_tactic.try_unfold xs
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
  | EType (v, t1, t2) ->
      Fmt.pf fmt "EType(%a, %a, %s)" pp_v v
        (Fmt.option ~none:(Fmt.any "None") (Fmt.of_to_string Type.str))
        t1 (Type.str t2)
  | EPure f -> Fmt.pf fmt "EPure(%a)" Formula.pp f
  | EVar x -> Fmt.pf fmt "EVar(%s)" x
  | EAsrt (vs, f, asrtss) ->
      let pp_asrts fmt asrts =
        Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") Asrt.pp) asrts
      in
      Fmt.pf fmt "EAsrt(%a; %a; %a)"
        (Fmt.list ~sep:(Fmt.any ", ") pp_v)
        vs Formula.pp f
        (Fmt.list ~sep:(Fmt.any ", ") pp_asrts)
        asrtss
  | EOther msg -> Fmt.pf fmt "%s" msg

let can_fix (can_fix_mem : 'a -> bool) (err : ('a, 'b) t) : bool =
  match err with
  | EMem mem_err -> can_fix_mem mem_err
  | EPure pf -> Reduction.reduce_formula pf <> False
  | EAsrt (_, pf, _) ->
      let result = Reduction.reduce_formula pf <> True in
      Logging.verbose (fun fmt -> fmt "Can fix: %a: %b" Formula.pp pf result);
      result
  | _ -> false

let get_failing_constraint (err : ('a, 'b) t) (mem_fc : 'a -> Formula.t) :
    Formula.t =
  match err with
  | EMem m_err -> mem_fc m_err
  | EPure f -> Not f
  | _ -> True
