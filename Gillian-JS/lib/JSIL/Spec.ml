module SSubst = Gillian.Symbolic.Subst
module SVal = Gillian.Symbolic.Values
module Flag = Gillian.Gil_syntax.Flag
module Expr = Gillian.Gil_syntax.Expr
module Id = Gillian.Gil_syntax.Id
module Var = Gillian.Gil_syntax.Var
module LVar = Gillian.Gil_syntax.LVar

(** {b Single JSIL specifications}. *)
type st = {
  pre : Asrt.t;  (** Precondition *)
  posts : Asrt.t list;  (** Postcondition *)
  flag : Flag.t;  (** Return flag ({!type:jsil_return_flag}) *)
  to_verify : bool;  (** Should the spec be verified? *)
  label : (string * LVar.Set.t) option;
}

(** {b Full JSIL specifications}. *)
type t = {
  name : string;  (** Procedure/spec name *)
  params : Var.t list;  (** Procedure/spec parameters *)
  sspecs : st list;  (** List of single specifications *)
  incomplete : bool;  (** If the spec is incomplete *)
  to_verify : bool;  (** Should the spec be verified? *)
}

(** Creates a JSIL specification given its components *)
let s_init
    ?(label : (string * LVar.Set.t) option)
    (pre : Asrt.t)
    (posts : Asrt.t list)
    (flag : Flag.t)
    (to_verify : bool) : st =
  { pre; posts; flag; to_verify; label }

let init
    (name : string)
    (params : Var.t list)
    (sspecs : st list)
    (incomplete : bool)
    (to_verify : bool) : t =
  { name; params; sspecs; incomplete; to_verify }

let extend (spec : t) (sspecs : st list) : t =
  { spec with sspecs = sspecs @ spec.sspecs }

let get_params (spec : t) : Var.t list = spec.params

let pp_sspec fmt sspec =
  let pp_lab fmt' (lab, exs) =
    if LVar.Set.is_empty exs then Fmt.pf fmt' "<%s>" lab
    else
      Fmt.pf fmt' "<%s: %a>" lab
        (Fmt.iter ~sep:(Fmt.any ", ") LVar.Set.iter Id.pp)
        exs
  in
  Fmt.pf fmt "%a[[  @[<hov 0>%a@]  ]]@\n[[  @[<hov 0>%a@]  ]]@\n%s"
    (Fmt.option pp_lab) sspec.label Asrt.pp sspec.pre
    (Fmt.list ~sep:(Fmt.any ";@\n") Asrt.pp)
    sspec.posts (Flag.str sspec.flag)

let pp fmt spec =
  let pp_incomplete fmt = function
    | true -> Fmt.string fmt "incomplete "
    | false -> ()
  in
  Fmt.pf fmt "@[<hov 2>@[<h>%a spec %s(%a)@]@\n%a@]" pp_incomplete
    spec.incomplete spec.name
    (Fmt.list ~sep:Fmt.comma Id.pp)
    spec.params
    (Fmt.list ~sep:(Fmt.any "@\n@\n") pp_sspec)
    spec.sspecs
