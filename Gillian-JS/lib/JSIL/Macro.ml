module SSubst = Gillian.Symbolic.Subst

(***************************************************************)

(***************************************************************)

(** Macros                                                    **)
module Expr = Gillian.Gil_syntax.Expr

type t = {
  name : string;  (** Name of the macro *)
  params : string list;  (** Actual parameters *)
  definition : LCmd.t list;  (** Macro definition *)
}

type t_tbl = (string, t) Hashtbl.t

let init_tbl () : t_tbl = Hashtbl.create Config.small_tbl_size

let get (macros : t_tbl) (name : string) : t option =
  Hashtbl.find_opt macros name

let pp fmt macro =
  Fmt.pf fmt "@[<v 2>macro %s(%a)@ %a@]" macro.name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    macro.params
    (Fmt.list ~sep:(Fmt.any "@ ") LCmd.pp)
    macro.definition

let pp_tbl : t_tbl Fmt.t =
  Fmt.hashtbl ~sep:(Fmt.any "@\n@\n") (fun f (_, x) -> pp f x)
