type spec = TypeDef__.lemma_spec = {
  lemma_hyp : Asrt.t;
  lemma_concs : Asrt.t list;
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
      spec.lemma_hyp
      (Fmt.list ~sep:Fmt.semi Asrt.pp)
      spec.lemma_concs
  in
  let pp_proof fmt' proof =
    Fmt.pf fmt' "[*  @[<hov 0>%a@]  *]"
      (Fmt.list ~sep:(Fmt.any "@\n") LCmd.pp)
      proof
  in
  let pp_path_opt fmt = function
    | None   -> Fmt.pf fmt "@nopath@\n"
    | Some _ -> ()
  in
  let pp_internal fmt = function
    | true  -> Fmt.pf fmt "@internal@\n"
    | false -> ()
  in
  Fmt.pf fmt "%a%a@[<v 2>lemma %s(%a)@ %a %a@]" pp_path_opt
    lemma.lemma_source_path pp_internal lemma.lemma_internal lemma.lemma_name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    lemma.lemma_params
    (Fmt.list ~sep:Fmt.sp pp_spec)
    lemma.lemma_specs (Fmt.option pp_proof) lemma.lemma_proof

let parameter_types (preds : (string, Pred.t) Hashtbl.t) (lemma : t) : t =
  (* copied from spec - needs refactoring *)
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
              (fun ac_types ((_, t_x), le) ->
                match t_x with
                | None     -> ac_types
                | Some t_x -> (le, t_x) :: ac_types)
              []
              (List.combine pred.pred_params les)
          in
          Star (Types ac_types, a)
      | _                -> a
    in
    Asrt.map None (Some f_a_after) None None a
  in
  let pt_spec { lemma_hyp; lemma_concs } =
    {
      lemma_hyp = pt_asrt lemma_hyp;
      lemma_concs = List.map pt_asrt lemma_concs;
    }
  in

  { lemma with lemma_specs = List.map pt_spec lemma.lemma_specs }

let add_param_bindings (lemma : t) =
  let params = lemma.lemma_params in
  let lvar_params = List.map (fun x -> "#" ^ x) params in
  let param_eqs =
    List.map2
      (fun pv lv -> Asrt.Pure (Eq (PVar pv, LVar lv)))
      params lvar_params
  in
  let param_eqs = Asrt.star param_eqs in
  let add_to_spec spec =
    { spec with lemma_hyp = Asrt.Star (param_eqs, spec.lemma_hyp) }
  in
  { lemma with lemma_specs = List.map add_to_spec lemma.lemma_specs }
