(** {b GIL logic commands}. *)

type t = TypeDef__.lcmd =
  | If         of Expr.t * t list * t list  (** If-then-else     *)
  | Branch     of Formula.t  (** branching on a FO formual *)
  | Macro      of string * Expr.t list  (** Macro            *)
  | Assert     of Formula.t  (** Assert           *)
  | Assume     of Formula.t  (** Assume           *)
  | AssumeType of string * Type.t  (** Assume Type      *)
  | SpecVar    of string list  (** Spec Var         *)
  | SL         of SLCmd.t

let rec map
    (f_l : (t -> t) option)
    (f_e : (Expr.t -> Expr.t) option)
    (f_p : (Formula.t -> Formula.t) option)
    (f_sl : (SLCmd.t -> SLCmd.t) option)
    (lcmd : t) : t =
  (* Functions to map over formulas, expressions, and sl-commands *)
  let f = map f_l f_e f_p f_sl in
  let map_e = Option.value ~default:(fun x -> x) f_e in
  let map_p = Option.value ~default:(fun x -> x) f_p in
  let map_sl = Option.value ~default:(fun x -> x) f_sl in

  (* Apply the given function to the logical command *)
  let mapped_lcmd = Option.fold ~some:(fun f -> f lcmd) ~none:lcmd f_l in

  (* Map over the elements of the command *)
  match mapped_lcmd with
  | Branch a       -> Branch (map_p a)
  | If (e, l1, l2) -> If (map_e e, List.map f l1, List.map f l2)
  | Macro (s, l)   -> Macro (s, List.map map_e l)
  | Assume a       -> Assume (map_p a)
  | Assert a       -> Assert (map_p a)
  | AssumeType _   -> mapped_lcmd
  | SpecVar _      -> mapped_lcmd
  | SL sl_cmd      -> SL (map_sl sl_cmd)

let rec pp fmt lcmd =
  let pp_list = Fmt.list ~sep:Fmt.semi pp in
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  match lcmd with
  | If (le, then_lcmds, else_lcmds) ->
      if List.length else_lcmds > 0 then
        Fmt.pf fmt
          "@[<hov 2>if (@[<h>%a@]) then {@\n%a@]@\n@[<hov 2>} else {@\n%a@]@\n}"
          Expr.pp le pp_list then_lcmds pp_list else_lcmds
      else
        Fmt.pf fmt "if (@[<h>%a@]) @[<hov 2>then {@\n%a@]@\n}" Expr.pp le
          pp_list then_lcmds
  | Branch fo -> Fmt.pf fmt "branch (%a)" Formula.pp fo
  | Macro (name, lparams) -> Fmt.pf fmt "%s(@[%a@])" name pp_params lparams
  | Assert a -> Fmt.pf fmt "assert (@[%a@])" Formula.pp a
  | Assume a -> Fmt.pf fmt "assume (@[%a@])" Formula.pp a
  | AssumeType (x, t) -> Fmt.pf fmt "assume_type (%s, %s)" x (Type.str t)
  | SpecVar xs ->
      Fmt.pf fmt "spec_var (@[%a@])" (Fmt.list ~sep:Fmt.comma Fmt.string) xs
  | SL sl_cmd -> SLCmd.pp fmt sl_cmd
