module Expr = Gillian.Gil_syntax.Expr

type lemma_spec = { pre : Asrt.t; posts : Asrt.t list }

type t = {
  name : string;
  (* Name of the lemma *)
  params : string list;
  (* Params *)
  pre : Asrt.t;
  (* Pre *)
  posts : Asrt.t list;
  (* Post *)
  proof : LCmd.t list option;
  (* (Optional) Proof body *)
  variant : Expr.t option;
  (* The paramater to treat as the variant. Will trigger termination checks *)
  existentials : string list;
}

let init_tbl () : (string, t) Hashtbl.t = Hashtbl.create Config.small_tbl_size

let pp fmt lemma =
  let pp_proof fmt' proof =
    Fmt.pf fmt' "[*  @[<hov 0>%a@]  *]"
      (Fmt.list ~sep:(Fmt.any "@\n") LCmd.pp)
      proof
  in
  Fmt.pf fmt
    "@[<hov 2>lemma %s(%a)@\n[[  @[<hov 0>%a@] ]]@\n[[  @[<hov 0>%a@] ]]@\n%a@]"
    lemma.name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    lemma.params Asrt.pp lemma.pre
    (Fmt.list ~sep:(Fmt.any "@\n") Asrt.pp)
    lemma.posts (Fmt.option pp_proof) lemma.proof
