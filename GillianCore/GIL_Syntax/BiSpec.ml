open Location

(** {b Single GIL specifications}. *)
type t = TypeDef__.bispec = {
  bispec_name : string;  (** Procedure/spec name               *)
  bispec_params : string list;  (** Procedure/spec parameters         *)
  bispec_pres : Asrt.t located list;  (** Possible preconditions            *)
  bispec_normalised : bool;  (** If the spec is already normalised *)
}

type t_tbl = (string, t) Hashtbl.t

let init
    (bispec_name : string)
    (bispec_params : string list)
    (bispec_pres : Asrt.t located list)
    (bispec_normalised : bool) : t =
  { bispec_name; bispec_params; bispec_pres; bispec_normalised }

let init_tbl () : t_tbl = Hashtbl.create Config.medium_tbl_size

let pp fmt bi_spec =
  Fmt.pf fmt "bispec %s (%a) : [[@[<hov 2>  %a  @]]]" bi_spec.bispec_name
    (Fmt.list ~sep:Fmt.comma Fmt.string)
    bi_spec.bispec_params
    (Fmt.list ~sep:Fmt.semi Asrt.pp)
    (List.map fst bi_spec.bispec_pres)
