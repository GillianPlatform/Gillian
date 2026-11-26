(** {b Single JSIL specifications}. *)
type t = {
  name : string;  (** Procedure/spec name *)
  params : Gillian.Gil_syntax.Var.t list;  (** Procedure/spec parameters *)
  pre : Asrt.t;  (** Precondition *)
}

type t_tbl = (string, t) Hashtbl.t

let init (name : string) (params : Gillian.Gil_syntax.Var.t list) (pre : Asrt.t)
    : t =
  { name; params; pre }

let init_tbl () : t_tbl = Hashtbl.create Config.medium_tbl_size

let pp fmt bi_spec =
  Fmt.pf fmt "@[<v 2>bispec %s (%a) :@\n[[ @[%a@] ]]@]" bi_spec.name
    (Fmt.list ~sep:(Fmt.any ", ") Gillian.Gil_syntax.Id.pp)
    bi_spec.params Asrt.pp bi_spec.pre
