(***************************************************************)
(** Separation Logic Commmands                                **)

(***************************************************************)

module SS = Containers.SS

type folding_info = string * (string * Expr.t) list [@@deriving yojson]
type unfold_info = (string * string) list [@@deriving yojson]

(** {b GIL Separation Logic commands}. *)
type t = TypeDef__.slcmd =
  | Fold of string * Expr.t list * folding_info option  (** Fold             *)
  | Unfold of string * Expr.t list * unfold_info option * bool
      (** Unfold           *)
  | Package of { lhs : string * Expr.t list; rhs : string * Expr.t list }
      (** Magic wand packaging *)
  | GUnfold of string  (** Global Unfold    *)
  | ApplyLem of string * Expr.t list * string list  (** Apply lemma      *)
  | SepAssert of Asrt.t * string list  (** Assert           *)
  | Invariant of Asrt.t * string list  (** Invariant        *)
  | Consume of
      Asrt.t
      * string list (* Consumes an assertion. Warning, not frame-preserving *)
  | Produce of Asrt.t (* Produces an assertion. Warning, not frame-preserving *)
  | SymbExec
[@@deriving yojson]

let map (f_a : Asrt.t -> Asrt.t) (f_e : Expr.t -> Expr.t) : t -> t = function
  | Fold (name, les, None) -> Fold (name, List.map f_e les, None)
  | Fold (name, les, Some (s, l)) ->
      Fold
        (name, List.map f_e les, Some (s, List.map (fun (x, e) -> (x, f_e e)) l))
  | Unfold (name, les, unfold_info, b) ->
      Unfold (name, List.map f_e les, unfold_info, b)
  | GUnfold name -> GUnfold name
  | ApplyLem (s, l, existentials) -> ApplyLem (s, List.map f_e l, existentials)
  | SepAssert (a, binders) -> SepAssert (f_a a, binders)
  | Invariant (a, existentials) -> Invariant (f_a a, existentials)
  | Consume (a, binders) -> Consume (f_a a, binders)
  | Produce a -> Produce (f_a a)
  | SymbExec -> SymbExec
  | Package { lhs = lname, largs; rhs = rname, rargs } ->
      Package
        { lhs = (lname, List.map f_e largs); rhs = (rname, List.map f_e rargs) }

let fold = List.fold_left SS.union SS.empty

let pvars (slcmd : t) : SS.t =
  let pvars_es es = fold (List.map Expr.pvars es) in
  match slcmd with
  | Fold (_, es, _) | Unfold (_, es, _, _) | ApplyLem (_, es, _) -> pvars_es es
  | GUnfold _ -> SS.empty
  | Package { lhs = _, les1; rhs = _, les2 } ->
      SS.union (pvars_es les1) (pvars_es les2)
  | SepAssert (a, _) | Invariant (a, _) | Consume (a, _) | Produce a ->
      Asrt.pvars a
  | SymbExec -> SS.empty

let lvars (slcmd : t) : SS.t =
  let lvars_es es = fold (List.map Expr.lvars es) in
  match slcmd with
  | Fold (_, es, finfo) ->
      let lvars_finfo =
        match finfo with
        | None -> SS.empty
        | Some (_, les) ->
            let _, es = List.split les in
            fold (List.map Expr.lvars es)
      in
      SS.union lvars_finfo (lvars_es es)
  | Unfold (_, es, _, _) -> lvars_es es
  | Package { lhs = _, les1; rhs = _, les2 } ->
      SS.union (lvars_es les1) (lvars_es les2)
  | ApplyLem (_, es, _) -> lvars_es es
  | GUnfold _ -> SS.empty
  | SepAssert (a, binders) | Consume (a, binders) ->
      SS.union (Asrt.lvars a) (SS.of_list binders)
  | Invariant (a, _) | Produce a -> Asrt.lvars a
  | SymbExec -> SS.empty

let locs (slcmd : t) : SS.t =
  let locs_es es = fold (List.map Expr.locs es) in
  match slcmd with
  | Fold (_, es, finfo) ->
      let lvars_finfo =
        match finfo with
        | None -> SS.empty
        | Some (_, les) ->
            let _, es = List.split les in
            fold (List.map Expr.locs es)
      in
      SS.union lvars_finfo (locs_es es)
  | Unfold (_, es, _, _) -> locs_es es
  | Package { lhs = _, les1; rhs = _, les2 } ->
      SS.union (locs_es les1) (locs_es les2)
  | ApplyLem (_, es, _) -> locs_es es
  | GUnfold _ -> SS.empty
  | SepAssert (a, _) | Invariant (a, _) | Consume (a, _) | Produce a ->
      Asrt.locs a
  | SymbExec -> SS.empty

let pp_folding_info =
  let pp_ui f (v, le) = Fmt.pf f "(%s := %a)" v Expr.pp le in
  let pp_non_opt f (id, uil) =
    Fmt.pf f " [ %s with %a ]" id (Fmt.list ~sep:(Fmt.any " and ") pp_ui) uil
  in
  Fmt.option pp_non_opt

let pp_unfold_info =
  let pp_ui f (v1, v2) = Fmt.pf f "(%s := %s)" v1 v2 in
  let pp_non_opt f uil =
    Fmt.pf f " [bind: %a]" (Fmt.list ~sep:(Fmt.any " and ") pp_ui) uil
  in
  Fmt.option pp_non_opt

let pp fmt lcmd =
  let pp_args = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_binders f b =
    match b with
    | [] -> ()
    | _ -> Fmt.pf f "[bind: %a]" (Fmt.list ~sep:Fmt.comma Fmt.string) b
  in
  match lcmd with
  | Fold (name, les, fold_info) ->
      Fmt.pf fmt "fold %s(%a)%a" name pp_args les pp_folding_info fold_info
  | Unfold (name, les, unfold_info, b) ->
      let keyword = if b then "unfold*" else "unfold" in
      Fmt.pf fmt "@[%s %s%a %a@]" keyword name (Fmt.parens pp_args) les
        pp_unfold_info unfold_info
  | Package { lhs = lname, largs; rhs = rname, rargs } ->
      let lname = Pp_utils.maybe_quote_ident lname in
      let rname = Pp_utils.maybe_quote_ident rname in
      Fmt.pf fmt "@[package (%s(%a) -* %s(%a)) @]" lname pp_args largs rname
        pp_args rargs
  | GUnfold name -> Fmt.pf fmt "unfold_all %s" name
  | ApplyLem (lem_name, lparams, binders) ->
      let lem_name = Pp_utils.maybe_quote_ident lem_name in
      Fmt.pf fmt "@[apply %s%a %a@]" lem_name (Fmt.parens pp_args) lparams
        pp_binders binders
  | SepAssert (a, binders) ->
      Fmt.pf fmt "@[sep_assert %a %a@]" (Fmt.parens Asrt.pp) a pp_binders
        binders
  | Consume (a, binders) ->
      Fmt.pf fmt "@[consume %a %a@]" (Fmt.parens Asrt.pp) a pp_binders binders
  | Produce a -> Fmt.pf fmt "@[produce %a@]" (Fmt.parens Asrt.pp) a
  | Invariant (a, existentials) ->
      let pp_exs f exs =
        match exs with
        | [] -> ()
        | _ ->
            Fmt.pf f "[existentials: %a]"
              (Fmt.list ~sep:Fmt.comma Fmt.string)
              exs
      in
      Fmt.pf fmt "invariant %a %a" (Fmt.parens Asrt.pp) a pp_exs existentials
  | SymbExec -> Fmt.pf fmt "symb_exec"
