(** {b Single JSIL specifications}. *)
type t = {
  name : string;  (** Procedure/spec name               *)
  params : Gil_syntax.Var.t list;  (** Procedure/spec parameters         *)
  pre : Asrt.t;  (** Precondition                      *)
  normalised : bool;  (** If the spec is already normalised *)
}

type t_tbl = (string, t) Hashtbl.t

let init
    (name : string)
    (params : Gil_syntax.Var.t list)
    (pre : Asrt.t)
    (normalised : bool) : t =
  { name; params; pre; normalised }

let init_tbl () : t_tbl = Hashtbl.create Config.medium_tbl_size

let pp fmt bi_spec =
  Fmt.pf fmt "@[<v 2>bispec %s (%a) :@\n[[ @[%a@] ]]@]" bi_spec.name
    (Fmt.list ~sep:(Fmt.any ", ") Gil_syntax.Var.pp)
    bi_spec.params Asrt.pp bi_spec.pre
