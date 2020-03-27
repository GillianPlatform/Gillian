(** {b Single JSIL specifications}. *)
type t = {
  name : string;  (** Procedure/spec name               *)
  params : string list;  (** Procedure/spec parameters         *)
  pre : Asrt.t;  (** Precondition                      *)
  normalised : bool;  (** If the spec is already normalised *)
}

type t_tbl = (string, t) Hashtbl.t

let init
    (name : string) (params : string list) (pre : Asrt.t) (normalised : bool) :
    t =
  { name; params; pre; normalised }

let init_tbl () : t_tbl = Hashtbl.create Config.medium_tbl_size

let pp fmt bi_spec =
  Fmt.pf fmt "@[<v 2>bispec %s (%a) :@\n[[ @[%a@] ]]@]" bi_spec.name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    bi_spec.params Asrt.pp bi_spec.pre
