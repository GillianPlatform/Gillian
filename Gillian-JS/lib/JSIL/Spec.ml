module SSubst = Gillian.Symbolic.Subst
module SVal = Gillian.Symbolic.Values
module Flag = Gillian.Gil_syntax.Flag
module Expr = Gillian.Gil_syntax.Expr
module SS = Containers.SS

(** {b Single JSIL specifications}. *)
type st = {
  pre : Asrt.t;  (** Precondition *)
  posts : Asrt.t list;  (** Postcondition *)
  flag : Flag.t;  (** Return flag ({!type:jsil_return_flag}) *)
  to_verify : bool;  (** Should the spec be verified? *)
  label : (string * SS.t) option;
}

(** {b Full JSIL specifications}. *)
type t = {
  name : string;  (** Procedure/spec name *)
  params : string list;  (** Procedure/spec parameters *)
  sspecs : st list;  (** List of single specifications *)
  normalised : bool;  (** If the spec is already normalised *)
  to_verify : bool;  (** Should the spec be verified? *)
}

(** Creates a JSIL specification given its components *)
let s_init
    ?(label : (string * SS.t) option)
    (pre : Asrt.t)
    (posts : Asrt.t list)
    (flag : Flag.t)
    (to_verify : bool) : st =
  { pre; posts; flag; to_verify; label }

let init
    (name : string)
    (params : string list)
    (sspecs : st list)
    (normalised : bool)
    (to_verify : bool) : t =
  { name; params; sspecs; normalised; to_verify }

let extend (spec : t) (sspecs : st list) : t =
  { spec with sspecs = sspecs @ spec.sspecs }

let get_params (spec : t) : string list = spec.params

let pp_sspec fmt sspec =
  let pp_lab fmt' (lab, exs) =
    if SS.is_empty exs then Fmt.pf fmt' "<%s>" lab
    else
      Fmt.pf fmt' "<%s: %a>" lab
        (Fmt.iter ~sep:(Fmt.any ", ") SS.iter Fmt.string)
        exs
  in
  Fmt.pf fmt "%a[[  @[<hov 0>%a@]  ]]@\n[[  @[<hov 0>%a@]  ]]@\n%s"
    (Fmt.option pp_lab) sspec.label Asrt.pp sspec.pre
    (Fmt.list ~sep:(Fmt.any ";@\n") Asrt.pp)
    sspec.posts (Flag.str sspec.flag)

let pp fmt spec =
  Fmt.pf fmt "@[<hov 2>@[<h>spec %s(%a)@]@\n%a@]" spec.name
    (Fmt.list ~sep:Fmt.comma Fmt.string)
    spec.params
    (Fmt.list ~sep:(Fmt.any "@\n@\n") pp_sspec)
    spec.sspecs

let parameter_types (preds : (string, Pred.t) Hashtbl.t) (spec : t) : t =
  let pt_asrt (a : Asrt.t) : Asrt.t =
    let f_a_after a : Asrt.t =
      match (a : Asrt.t) with
      | Pred (name, les) ->
          let pred =
            try Hashtbl.find preds name
            with _ ->
              raise
                (Failure
                   ( "DEATH. parameter_types: predicate " ^ name
                   ^ " does not exist." ))
          in
          (* Printf.printf "Pred: %s\n\tParams1: %s\n\tParams2: %s\n" name
             (String.concat ", " (let x, _ = List.split pred.params in x)) (String.concat ", " (List.map (Fmt.to_to_string Expr.pp) les)); *)
          let ac_types =
            List.fold_left
              (fun ac_types ((x, t_x), le) ->
                match t_x with
                | None     -> ac_types
                | Some t_x -> (le, t_x) :: ac_types)
              []
              (List.combine pred.params les)
          in
          Star (Types ac_types, a)
      | _                -> a
    in
    Asrt.map None (Some f_a_after) None None a
  in

  let pt_sspec (sspec : st) : st =
    { sspec with pre = pt_asrt sspec.pre; posts = List.map pt_asrt sspec.posts }
  in

  { spec with sspecs = List.map pt_sspec spec.sspecs }
