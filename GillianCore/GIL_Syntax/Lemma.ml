open Location

type spec = TypeDef__.lemma_spec = {
  lemma_hyp : Asrt.t located;
  lemma_concs : Asrt.t located list;
  lemma_spec_variant : Expr.t option;
}

type t = TypeDef__.lemma = {
  lemma_name : string;
  (* Name of the lemma *)
  lemma_source_path : string option;
  lemma_internal : bool;
  lemma_params : string list;
  (* Params *)
  lemma_specs : spec list;
  lemma_proof : LCmd.t list option;
  (* (Optional) Proof body *)
  lemma_variant : Expr.t option;
  (* The paramater to treat as the variant. Will trigger termination checks *)
  lemma_existentials : string list;
}

let init_tbl () : (string, t) Hashtbl.t = Hashtbl.create Config.small_tbl_size

let pp fmt lemma =
  let pp_spec fmt spec =
    Fmt.pf fmt "[[  @[<hov 0>%a@] ]]@ [[  @[<hov 0>%a@] ]]" Asrt.pp
      (fst spec.lemma_hyp)
      (Fmt.list ~sep:Fmt.semi Asrt.pp)
      (List.map fst spec.lemma_concs)
  in
  let pp_proof fmt' proof =
    Fmt.pf fmt' "[*  @[<hov 0>%a@]  *]" (Fmt.list ~sep:Fmt.semi LCmd.pp) proof
  in
  let pp_path_opt fmt = function
    | None -> Fmt.pf fmt "@nopath@\n"
    | Some _ -> ()
  in
  let pp_internal fmt = function
    | true -> Fmt.pf fmt "@internal@\n"
    | false -> ()
  in
  Fmt.pf fmt "%a%a@[<v 2>lemma %s(%a)@ %a@ %a@]" pp_path_opt
    lemma.lemma_source_path pp_internal lemma.lemma_internal
    (Pp_utils.maybe_quote_ident lemma.lemma_name)
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    lemma.lemma_params
    (Fmt.list ~sep:Fmt.sp pp_spec)
    lemma.lemma_specs (Fmt.option pp_proof) lemma.lemma_proof

let parameter_types (preds : (string, Pred.t) Hashtbl.t) (lemma : t) : t =
  let map_asrts (pred, loc) =
    match Pred.extend_asrt_pred_types preds pred with
    | Ok pred -> (pred, loc)
    | Error msg -> raise (Gillian_result.Exc.verification_failure ?loc msg)
  in
  let pt_spec { lemma_hyp; lemma_concs; lemma_spec_variant } =
    {
      lemma_hyp = map_asrts lemma_hyp;
      lemma_concs = List.map map_asrts lemma_concs;
      lemma_spec_variant;
    }
  in

  { lemma with lemma_specs = List.map pt_spec lemma.lemma_specs }

let add_param_bindings (lemma : t) =
  let params = lemma.lemma_params in
  let lvar_params = List.map (fun x -> "#" ^ x) params in
  let param_eqs =
    List.map2
      (fun pv lv -> Asrt.Pure (Expr.BinOp (PVar pv, Equal, LVar lv)))
      params lvar_params
  in
  let add_to_spec spec =
    let lemma_hyp = (param_eqs @ fst spec.lemma_hyp, snd spec.lemma_hyp) in
    { spec with lemma_hyp }
  in
  { lemma with lemma_specs = List.map add_to_spec lemma.lemma_specs }
