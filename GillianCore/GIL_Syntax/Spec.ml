module SS = Containers.SS

(** {b Single GIL specifications}. *)
type st = TypeDef__.single_spec = {
  ss_pre : Asrt.t Location.located;  (** Precondition *)
  ss_posts : Asrt.t Location.located list;  (** Postcondition *)
  ss_variant : Expr.t option;  (** Variant *)
  ss_flag : Flag.t;  (** Return flag *)
  ss_to_verify : bool;  (** Should the spec be verified? *)
  ss_label : (string * string list) option;
}

(** {b Full GIL specifications}. *)
type t = TypeDef__.spec = {
  spec_name : string;  (** Procedure/spec name *)
  spec_params : string list;  (** Procedure/spec parameters *)
  spec_sspecs : st list;  (** List of single specifications *)
  spec_normalised : bool;  (** If the spec is already normalised *)
  spec_incomplete : bool;  (** If the spec is incomplete *)
  spec_to_verify : bool;  (** Should the spec be verified? *)
}

(** Creates a GIL specification given its components *)
let s_init
    ?(ss_label : (string * string list) option)
    (ss_pre : Asrt.t Location.located)
    (ss_posts : Asrt.t Location.located list)
    (ss_variant : Expr.t option)
    (ss_flag : Flag.t)
    (ss_to_verify : bool) : st =
  { ss_pre; ss_posts; ss_variant; ss_flag; ss_to_verify; ss_label }

let init
    (spec_name : string)
    (spec_params : string list)
    (spec_sspecs : st list)
    (spec_normalised : bool)
    (spec_incomplete : bool)
    (spec_to_verify : bool) : t =
  {
    spec_name;
    spec_params;
    spec_sspecs;
    spec_normalised;
    spec_incomplete;
    spec_to_verify;
  }

let extend (spec : t) (sspecs : st list) : t =
  { spec with spec_sspecs = sspecs @ spec.spec_sspecs }

let get_params (spec : t) : string list = spec.spec_params

let pp_sspec fmt sspec =
  let pp_lab fmt' (lab, exs) =
    match exs with
    | [] -> Fmt.pf fmt' "<%s>@\n" lab
    | exs -> Fmt.pf fmt' "<%s: %a>@\n" lab Fmt.(list ~sep:comma string) exs
  in
  Fmt.pf fmt "%a[[  @[<hv 0>%a@]  ]]@\n[[  @[<hv 0>%a@]  ]]@\n%s"
    (Fmt.option pp_lab) sspec.ss_label Asrt.pp (fst sspec.ss_pre)
    (Fmt.list ~sep:(Fmt.any ";@\n") Asrt.pp)
    (List.map fst sspec.ss_posts)
    (Flag.str sspec.ss_flag)

let pp fmt spec =
  let pp_incomplete fmt = function
    | true -> Fmt.string fmt "incomplete "
    | false -> ()
  in
  let name = Pp_utils.maybe_quote_ident spec.spec_name in
  Fmt.pf fmt "@[<v 2>@[<h>%aspec %s(%a)@]@\n%a@]" pp_incomplete
    spec.spec_incomplete name
    Fmt.(list ~sep:comma string)
    spec.spec_params
    Fmt.(list ~sep:semi pp_sspec)
    spec.spec_sspecs

let parameter_types (preds : (string, Pred.t) Hashtbl.t) (spec : t) : t =
  let map_asrts (pred, loc) =
    match Pred.extend_asrt_pred_types preds pred with
    | Ok pred -> (pred, loc)
    | Error msg -> raise (Gillian_result.Exc.verification_failure ?loc msg)
  in
  let pt_sspec (sspec : st) : st =
    {
      sspec with
      ss_pre = map_asrts sspec.ss_pre;
      ss_posts = List.map map_asrts sspec.ss_posts;
    }
  in
  { spec with spec_sspecs = List.map pt_sspec spec.spec_sspecs }

let label_vars_to_set lab =
  Option.map (fun (l, vl) -> (l, Containers.SS.of_list vl)) lab

let to_yojson = TypeDef__.spec_to_yojson
let of_yojson = TypeDef__.spec_of_yojson

let hash_of_t (spec : t) =
  Fmt.str "%a" pp spec |> Digest.string |> Digest.to_hex
