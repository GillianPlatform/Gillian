(** {3 Macros} **)

type t = TypeDef__.macro = {
  macro_name : string;  (** Name of the macro *)
  macro_params : string list;  (** Actual parameters *)
  macro_definition : LCmd.t list;  (** Macro definition *)
}

type t_tbl = (string, t) Hashtbl.t

let init_tbl () : t_tbl = Hashtbl.create Config.small_tbl_size

let get (macros : t_tbl) (name : string) : t option =
  Hashtbl.find_opt macros name

let pp fmt macro =
  Fmt.pf fmt "@[<v 2>macro %s(%a)@\n%a@]" macro.macro_name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    macro.macro_params
    (Fmt.list ~sep:(Fmt.any "@\n") LCmd.pp)
    macro.macro_definition

let pp_tbl : t_tbl Fmt.t =
  Fmt.hashtbl ~sep:(Fmt.any "@\n@\n") (fun f (_, x) -> pp f x)
