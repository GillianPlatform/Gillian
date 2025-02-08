(** {b GIL logic commands}. *)

open Id

type t = TypeDef__.lcmd =
  | If of Expr.t * t list * t list  (** If-then-else     *)
  | Branch of Expr.t  (** branching on a FO formual *)
  | Macro of string * Expr.t list  (** Macro            *)
  | Assert of Expr.t  (** Assert           *)
  | Assume of Expr.t  (** Assume           *)
  | AssumeType of Expr.t * Type.t  (** Assume Type      *)
  | FreshSVar of string  (** x := fresh_svar() *)
  | SL of SLCmd.t
[@@deriving yojson]

let rec map (f_e : Expr.t -> Expr.t) (f_sl : SLCmd.t -> SLCmd.t) (lcmd : t) =
  let f = map f_e f_sl in
  match lcmd with
  | Branch a -> Branch (f_e a)
  | If (e, l1, l2) -> If (f_e e, List.map f l1, List.map f l2)
  | Macro (s, l) -> Macro (s, List.map f_e l)
  | Assume a -> Assume (f_e a)
  | Assert a -> Assert (f_e a)
  | AssumeType (e, t) -> AssumeType (f_e e, t)
  | FreshSVar _ as lcmd -> lcmd
  | SL sl_cmd -> SL (f_sl sl_cmd)

let rec pvars (lcmd : t) : Var.Set.t =
  let fold = List.fold_left Var.Set.union Var.Set.empty in
  let pvars_es es = fold (List.map Expr.pvars es) in
  let pvars_lcmds es = fold (List.map pvars es) in
  match lcmd with
  | If (e, lthen, lelse) ->
      Var.Set.union (Expr.pvars e)
        (Var.Set.union (pvars_lcmds lthen) (pvars_lcmds lelse))
  | Macro (_, es) -> pvars_es es
  | Branch e | Assert e | Assume e | AssumeType (e, _) -> Expr.pvars e
  | FreshSVar name -> Var.Set.singleton name
  | SL slcmd -> SLCmd.pvars slcmd

let rec lvars (lcmd : t) : LVar.Set.t =
  let fold = List.fold_left LVar.Set.union LVar.Set.empty in
  let lvars_es es = fold (List.map Expr.lvars es) in
  let lvars_lcmds es = fold (List.map lvars es) in
  match lcmd with
  | If (e, lthen, lelse) ->
      LVar.Set.union (Expr.lvars e)
        (LVar.Set.union (lvars_lcmds lthen) (lvars_lcmds lelse))
  | Macro (_, es) -> lvars_es es
  | Branch e | Assert e | Assume e | AssumeType (e, _) -> Expr.lvars e
  | SL slcmd -> SLCmd.lvars slcmd
  | FreshSVar _ -> LVar.Set.empty

let rec locs (lcmd : t) : Sets.LocSet.t =
  let fold = List.fold_left Sets.LocSet.union Sets.LocSet.empty in
  let locs_es es = fold (List.map Expr.locs es) in
  let locs_lcmds es = fold (List.map locs es) in
  match lcmd with
  | If (e, lthen, lelse) ->
      Sets.LocSet.union (Expr.locs e)
        (Sets.LocSet.union (locs_lcmds lthen) (locs_lcmds lelse))
  | Macro (_, es) -> locs_es es
  | Branch e | Assert e | Assume e | AssumeType (e, _) -> Expr.locs e
  | SL slcmd -> SLCmd.locs slcmd
  | FreshSVar _ -> Sets.LocSet.empty

let rec pp fmt lcmd =
  let pp_list = Fmt.list ~sep:Fmt.semi pp in
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  match lcmd with
  | If (le, then_lcmds, else_lcmds) ->
      if List.length else_lcmds > 0 then
        Fmt.pf fmt "@[<hov 2>if (%a) then {@ %a@]@ @[<hov 2>} else {@ %a@]@ }"
          (Fmt.hbox Expr.pp) le pp_list then_lcmds pp_list else_lcmds
      else
        Fmt.pf fmt "if (@[<h>%a@]) @[<hov 2>then {@\n%a@]@\n}" Expr.pp le
          pp_list then_lcmds
  | Branch fo -> Fmt.pf fmt "branch (%a)" Expr.pp fo
  | Macro (name, lparams) -> Fmt.pf fmt "%s(@[%a@])" name pp_params lparams
  | Assert a -> Fmt.pf fmt "assert (@[%a@])" Expr.pp a
  | Assume a -> Fmt.pf fmt "assume (@[%a@])" Expr.pp a
  | AssumeType (e, t) ->
      Fmt.pf fmt "assume_type (%a, %s)" Expr.pp e (Type.str t)
  | SL sl_cmd -> SLCmd.pp fmt sl_cmd
  | FreshSVar x -> Fmt.pf fmt "%s := fresh_svar()" x
