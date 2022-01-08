type ('mem_err, 'value) err_t =
  | EMem  of 'mem_err  (** Memory error, depends on instantiation *)
  | EType of 'value * Type.t option * Type.t
      (** Incorrect type, depends on value *)
  | EPure of Formula.t (* Missing formula that should be true *)
  | EVar  of Var.t (* Undefined variable *)
  | EAsrt of ('value list * Formula.t * Asrt.t list list)
[@@deriving yojson]

let get_recovery_vals
    (errs : ('a, 'b) err_t list) (mem_recovery_vals : 'a -> 'b list) : 'b list =
  let f err =
    match err with
    | EMem err         -> mem_recovery_vals err
    | EAsrt (xs, _, _) -> xs
    | _                -> []
  in
  List.concat (List.map f errs)

let pp_err
    (pp_m_err : Format.formatter -> 'a -> unit)
    (pp_v : Format.formatter -> 'b -> unit)
    (fmt : Format.formatter)
    (err : ('a, 'b) err_t) : unit =
  match err with
  | EMem m_err            -> pp_m_err fmt m_err
  | EType (v, t1, t2)     ->
      Fmt.pf fmt "EType(%a, %a, %s)" pp_v v
        (Fmt.option ~none:(Fmt.any "None") (Fmt.of_to_string Type.str))
        t1 (Type.str t2)
  | EPure f               -> Fmt.pf fmt "EPure(%a)" Formula.pp f
  | EVar x                -> Fmt.pf fmt "EVar(%s)" x
  | EAsrt (vs, f, asrtss) ->
      let pp_asrts fmt asrts =
        Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") Asrt.pp) asrts
      in
      Fmt.pf fmt "EAsrt(%a; %a; %a)"
        (Fmt.list ~sep:(Fmt.any ", ") pp_v)
        vs Formula.pp f
        (Fmt.list ~sep:(Fmt.any ", ") pp_asrts)
        asrtss

let can_fix (errs : ('a, 'b) err_t list) : bool =
  let result =
    List.exists
      (fun e ->
        match e with
        | EMem _           -> true
        | EPure pf         -> Reduction.reduce_formula pf <> False
        | EAsrt (_, pf, _) ->
            let result = Reduction.reduce_formula pf <> True in
            Logging.verbose (fun fmt ->
                fmt "Can fix: intermediate: %a: %b" Formula.pp pf result);
            result
        | _                -> false)
      errs
  in
  Logging.verbose (fun fmt -> fmt "Can fix: overall %b" result);
  result

let get_failing_constraint (err : ('a, 'b) err_t) (mem_fc : 'a -> Formula.t) :
    Formula.t =
  match err with
  | EMem m_err -> mem_fc m_err
  | EPure f    -> Not f
  | _          -> True
