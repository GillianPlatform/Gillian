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

let parameter_types (preds : (string, Pred.t) Hashtbl.t) (lemma : t) : t =
  let pt_asrt (a : Asrt.t) : Asrt.t =
    let f_a_after a : Asrt.t =
      match (a : Asrt.t) with
      | Pred (name, les) ->
          let pred =
            try Hashtbl.find preds name
            with _ ->
              raise
                (Failure
                   ("DEATH. parameter_types: predicate " ^ name
                  ^ " does not exist."))
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

  { lemma with pre = pt_asrt lemma.pre; posts = List.map pt_asrt lemma.posts }
