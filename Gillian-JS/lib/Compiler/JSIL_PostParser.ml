open Containers
open Gillian.Gil_syntax
open Jsil_syntax

let heap_asrt_name = "initialHeapPostWeak"
let pre_scope_prefix = "PreScope_"
let post_scope_prefix = "PostScope_"
let reserved_methods = Hashtbl.create Config.small_tbl_size
let reserved_properties = Hashtbl.create Config.small_tbl_size
let counter = ref 0
let pvar_counter = ref 0

let fresh_bi_lvar () =
  let v = "#bi_var_" ^ string_of_int !counter in
  counter := !counter + 1;
  v

let populate () : unit =
  Hashtbl.replace reserved_methods "hasOwnProperty" "$lobj_proto";
  Hashtbl.replace reserved_properties "length"
    ("$larr_proto", "ArrayObj", "ArrayProto")

let post_parse_lcmd (cmd : Annot.t * string option * LabCmd.t) :
    (Annot.t * string option * LabCmd.t) list =
  let annot, lab, cmd = cmd in
  match !Javert_utils.Js_config.cosette with
  | true -> [ (annot, lab, cmd) ]
  | false -> (
      match cmd with
      | LCall (_, Lit (String p_name), [ r_arg ], _, _)
        when p_name = JS2JSIL_Helpers.getValueName ->
          let unfold_macro =
            LCmd.Macro (JS2JSIL_Helpers.macro_GPVU_name, [ r_arg ])
          in
          [ (annot, lab, cmd); (annot, None, LLogic unfold_macro) ]
      | LCall (_, Lit (String p_name), [ r_arg; _ ], _, _)
        when p_name = JS2JSIL_Helpers.putValueName ->
          let unfold_macro =
            LCmd.Macro (JS2JSIL_Helpers.macro_GPVU_name, [ r_arg ])
          in
          [ (annot, lab, cmd); (annot, None, LLogic unfold_macro) ]
      | LCall (_, Lit (String p_name), [ _; _ ], _, _)
        when p_name = JS2JSIL_Helpers.hasPropertyName ->
          [
            (annot, lab, cmd);
            ( annot,
              None,
              LLogic (SL (GUnfold JS2JSIL_Helpers.pi_predicate_name)) );
          ]
      | _ -> [ (annot, lab, cmd) ])

let post_parse_eproc (eproc : EProc.t) : EProc.t =
  let new_body =
    if not !Javert_utils.Js_config.cosette then
      Array.of_list
        (List.concat (List.map post_parse_lcmd (Array.to_list eproc.body)))
    else eproc.body
  in
  {
    name = eproc.name;
    body = new_body;
    params = eproc.params;
    spec = eproc.spec;
  }

let post_parse_eprog (eprog : EProg.t) : EProg.t =
  let copy_procs = Hashtbl.create Config.big_tbl_size in
  Hashtbl.iter
    (fun name (proc : EProc.t) ->
      let proc' = post_parse_eproc proc in
      Hashtbl.replace copy_procs name proc')
    eprog.procs;
  { eprog with procs = copy_procs }

let expr_from_fid (fid : string) : Expr.t =
  if fid = JS2JSIL_Helpers.main_fid then
    Expr.Lit (Loc JS2JSIL_Helpers.locGlobName)
  else Expr.LVar (Names.lvar_from_str fid)

let make_sc (vis_list : string list) : Expr.t list =
  let chopped_vis_list =
    match List.rev vis_list with
    | _ :: xs -> List.rev xs
    | _ -> []
  in
  List.map expr_from_fid chopped_vis_list

let asrts_js_val (x_val : Expr.t) : Asrt.t list =
  let asrt_empty : Asrt.t = Pure (Not (Eq (x_val, Lit Empty))) in
  let asrt_none : Asrt.t = Pure (Not (Eq (x_val, Lit Nono))) in
  let asrt_list : Asrt.t =
    Pure (Not (Eq (UnOp (TypeOf, x_val), Lit (Type ListType))))
  in
  [ asrt_empty; asrt_none; asrt_list ]

let var_assertion (fid : string) (x : string) (x_val : Expr.t) : Asrt.t =
  let asrt_val : Asrt.t =
    if fid <> !Config.entry_point then
      PointsTo (expr_from_fid fid, Lit (String x), x_val)
    else
      PointsTo
        ( expr_from_fid fid,
          Lit (String x),
          EList
            [
              Lit (String "d");
              x_val;
              Lit (Bool true);
              Lit (Bool true);
              Lit (Bool true);
            ] )
  in
  asrt_val

let make_this_assertion () : Asrt.t =
  let var_this = JS2JSIL_Helpers.var_this in
  let f1 : Formula.t =
    Not (Eq (UnOp (TypeOf, LVar "#this"), Lit (Type ListType)))
  in
  let f2 : Formula.t =
    Not (Eq (UnOp (TypeOf, LVar "#this"), Lit (Type NumberType)))
  in
  let f3 : Formula.t =
    Not (Eq (UnOp (TypeOf, LVar "#this"), Lit (Type StringType)))
  in
  let f4 : Formula.t =
    Not (Eq (UnOp (TypeOf, LVar "#this"), Lit (Type BooleanType)))
  in
  let f5 : Formula.t = Not (Eq (LVar "#this", Lit Empty)) in
  let f6 : Formula.t = Eq (LVar "#this", PVar var_this) in
  Asrt.Pure (Formula.conjunct [ f1; f2; f3; f4; f5; f6 ])

let scope_info_to_assertion
    (eprog : EProg.t)
    (cc_tbl : Jslogic.JSLogicCommon.cc_tbl_type)
    (vis_tbl : Jslogic.JSLogicCommon.vis_tbl_type)
    (fid : string)
    (args : SS.t) : Asrt.t =
  let vis_list = Jslogic.JSLogicCommon.get_vis_list vis_tbl fid in
  let sc_bindings =
    match List.rev (List.map expr_from_fid vis_list) with
    | _ :: les -> List.rev les
    | _ -> raise (Failure "DEATH. EMPTY VIS LIST. scope_info_to_assertion")
  in

  let a_schain =
    Asrt.Pure (Eq (PVar JS2JSIL_Helpers.var_scope, EList sc_bindings))
  in

  let glob_constraints =
    match sc_bindings with
    | _ :: les ->
        List.map
          (fun le ->
            Asrt.Pure (Not (Eq (le, Lit (Loc JS2JSIL_Helpers.locGlobName)))))
          les
    | _ -> []
  in

  let fid_vis_tbl = Jslogic.JSLogicCommon.get_scope_table cc_tbl fid in
  let a_vars =
    Hashtbl.fold
      (fun x fid' asrts ->
        let new_asrts =
          if fid' <> fid then
            let x_val = Expr.LVar (fresh_bi_lvar ()) in
            let a_xval = var_assertion fid' x x_val in
            let proc_x = EProg.get_proc eprog x in
            match proc_x with
            | None -> [ a_xval ]
            | Some proc_x ->
                let proc_x_prototype = Expr.LVar (fresh_bi_lvar ()) in
                let proc_x_vis_list =
                  Jslogic.JSLogicCommon.get_vis_list vis_tbl proc_x.name
                in
                let proc_x_sc = Expr.EList (make_sc proc_x_vis_list) in
                let fun_obj_asrt =
                  Asrt.Pred
                    ( "JSFunctionObject",
                      [
                        x_val;
                        Lit (String proc_x.name);
                        proc_x_sc;
                        Lit (Num (float_of_int (List.length proc_x.params)));
                        proc_x_prototype;
                      ] )
                in
                let proto_asrt = Asrt.Pred ("JSObject", [ proc_x_prototype ]) in
                [ fun_obj_asrt; proto_asrt; a_xval ]
          else if SS.mem x args then
            let x_val : Expr.t = LVar (Names.svar_from_str x) in
            let asrts_x = asrts_js_val x_val in
            Pure (Eq (PVar x, x_val)) :: asrts_x
          else []
        in
        asrts @ new_asrts)
      fid_vis_tbl []
  in

  let this_asrt = make_this_assertion () in

  let init_heap_asrt : Asrt.t = Pred (heap_asrt_name, []) in
  if fid <> JS2JSIL_Helpers.main_fid then
    Asrt.star
      (glob_constraints @ (this_asrt :: init_heap_asrt :: a_schain :: a_vars))
  else Asrt.star (glob_constraints @ (this_asrt :: a_schain :: a_vars))

let create_pre_scope_pred
    (eprog : EProg.t)
    (cc_tbl : Jslogic.JSLogicCommon.cc_tbl_type)
    (vis_tbl : Jslogic.JSLogicCommon.vis_tbl_type)
    (fid : string)
    (_ : SS.t) : Pred.t =
  let vis_list = Jslogic.JSLogicCommon.get_vis_list vis_tbl fid in
  let sc_bindings =
    match List.rev (List.map expr_from_fid vis_list) with
    | _ :: les -> List.rev les
    | _ -> raise (Failure "DEATH. EMPTY VIS LIST. scope_info_to_assertion")
  in

  let a_schain =
    Asrt.Pure (Eq (PVar JS2JSIL_Helpers.var_scope, EList sc_bindings))
  in

  let fid_vis_tbl = Jslogic.JSLogicCommon.get_scope_table cc_tbl fid in
  let p_args, a_vars =
    Hashtbl.fold
      (fun x fid' (p_args, asrts) ->
        let x, new_asrts =
          if fid' <> fid then
            let a_x = var_assertion fid' x (PVar x) in
            let proc_x = EProg.get_proc eprog x in
            match proc_x with
            | None -> ([ x ], [ a_x ])
            | Some proc_x ->
                let proc_x_prototype =
                  Expr.LVar (Javert_utils.Js_generators.fresh_lvar ())
                in
                let proc_x_vis_list =
                  Jslogic.JSLogicCommon.get_vis_list vis_tbl proc_x.name
                in
                let proc_x_sc = Expr.EList (make_sc proc_x_vis_list) in
                let fun_obj_asrt =
                  Asrt.Pred
                    ( "JSFunctionObject",
                      [
                        PVar x;
                        Lit (String proc_x.name);
                        proc_x_sc;
                        Lit (Num (float_of_int (List.length proc_x.params)));
                        proc_x_prototype;
                      ] )
                in
                let proto_asrt = Asrt.Pred ("JSObject", [ proc_x_prototype ]) in
                ([ x ], [ fun_obj_asrt; proto_asrt; a_x ])
          else ([], [])
        in
        (x @ p_args, new_asrts @ asrts))
      fid_vis_tbl ([], [])
  in

  let params =
    List.map (fun x -> (x, None)) (JS2JSIL_Helpers.var_scope :: p_args)
  in

  {
    name = pre_scope_prefix ^ fid;
    num_params = List.length p_args + 1;
    params;
    ins = [ 0 ];
    definitions = [ (None, Asrt.star (a_schain :: a_vars)) ];
    facts = [];
    pure = false;
    abstract = false;
    nounfold = false;
    normalised = false;
  }

let create_function_predicate
    (_ : Jslogic.JSLogicCommon.cc_tbl_type)
    (vis_tbl : Jslogic.JSLogicCommon.vis_tbl_type)
    (fid : string)
    (fparams : string list) : Pred.t =
  let pred_name = fid ^ "_FO_BI" in

  let x = "x" in
  let fid_vis_list = Jslogic.JSLogicCommon.get_vis_list vis_tbl fid in
  let fid_x_sc = Expr.EList (make_sc fid_vis_list) in
  let fid_prototype = Expr.LVar ("#" ^ fid ^ "_prototype") in

  let fo_asrt =
    Asrt.Pred
      ( "JSFunctionObject",
        [
          PVar x;
          Lit (String fid);
          fid_x_sc;
          Lit (Num (float_of_int (List.length fparams)));
          fid_prototype;
        ] )
  in
  let proto_asrt = Asrt.Pred ("JSObject", [ fid_prototype ]) in

  {
    name = pred_name;
    num_params = 1;
    params = [ ("x", None) ];
    ins = [ 0 ];
    definitions = [ (None, Asrt.star [ fo_asrt; proto_asrt ]) ];
    facts = [];
    pure = false;
    abstract = false;
    nounfold = false;
    normalised = false;
  }

(*
  type t = {
  name        : string;                                            (** Name of the predicate  *)
  num_params  : int;                                               (** Number of parameters   *)
  params      : (string * Type.t option) list;                     (** Actual parameters      *)
  ins         : int list;                                          (** Ins                    *)
  definitions : (((string * (string list)) option) * Asrt.t) list; (** Predicate definitions  *)
  pure        : bool;                                              (** Is the predicate pure  *)
  normalised  : bool;                                              (** If the predicate has been previously normalised *)
}
*)

let create_post_scope_pred
    (_ : EProg.t)
    (cc_tbl : Jslogic.JSLogicCommon.cc_tbl_type)
    (_ : Jslogic.JSLogicCommon.vis_tbl_type)
    (fid : string)
    (_ : SS.t) : Pred.t =
  (* let args = SS.diff args (SS.of_list [ JS2JSIL_Helpers.var_this; JS2JSIL_Helpers.var_scope ]) in  *)
  let vis_tbl = Jslogic.JSLogicCommon.get_scope_table cc_tbl fid in
  let all_params, out_params, in_params =
    Hashtbl.fold
      (fun x fid' (params, out_params, in_params) ->
        if fid' <> fid then (x :: params, x :: out_params, in_params)
        else (x :: params, out_params, x :: in_params))
      vis_tbl ([], [], [])
  in
  let params =
    JS2JSIL_Helpers.var_scope :: JS2JSIL_Helpers.var_er :: all_params
  in
  let params = List.map (fun x -> (x, None)) params in

  let args_asrts =
    List.map
      (fun x ->
        Asrt.PointsTo (PVar JS2JSIL_Helpers.var_er, Lit (String x), PVar x))
      in_params
  in

  (* MetaData (er, _lvar_md_er) *)
  let arg_er : Expr.t = PVar JS2JSIL_Helpers.var_er in
  let lv_er_md : Expr.t = LVar (Javert_utils.Js_generators.fresh_lvar ()) in
  let mtdt_er_a : Asrt.t = MetaData (arg_er, lv_er_md) in
  (* empty_fields(er : -{ "arguments", in_params }-) *)
  let args_strs : Expr.t list =
    List.map (fun x -> Expr.Lit (String x)) in_params
  in
  let ef_er_a : Asrt.t = EmptyFields (arg_er, ESet args_strs) in
  (* ((er, "arguments") -> arguments) *)
  (* let args_a    : Asrt.t      = PointsTo (arg_er, Lit (String "arguments"), PVar "arguments") in *)
  (* MetaData (_lvar_md_er, null) *)
  let md_md_a : Asrt.t = MetaData (lv_er_md, Lit Null) in
  (* empty_fields(_lvar_md_er : -{ "@er" }-) *)
  let ef_md_a : Asrt.t =
    EmptyFields (lv_er_md, ESet [ Lit (String JS2JSIL_Helpers._erFlagPropName) ])
  in
  (* ((lv_er_md, "@er") -> true) *)
  let er_fl_a : Asrt.t =
    PointsTo
      (lv_er_md, Lit (String JS2JSIL_Helpers._erFlagPropName), Lit (Bool true))
  in
  (* all together *)
  let er_asrts =
    [ mtdt_er_a; ef_er_a; (* args_a; *) md_md_a; ef_md_a; er_fl_a ]
  in

  let pred_params =
    List.map (fun x -> Expr.PVar x) (JS2JSIL_Helpers.var_scope :: out_params)
  in
  let pre_scope_asrt = Asrt.Pred (pre_scope_prefix ^ fid, pred_params) in

  {
    name = post_scope_prefix ^ fid;
    num_params = List.length params;
    params;
    ins = [ 0; 1 ];
    definitions =
      [ (None, Asrt.star (pre_scope_asrt :: (args_asrts @ er_asrts))) ];
    facts = [];
    pure = false;
    abstract = false;
    nounfold = false;
    normalised = false;
  }

let bi_post_parse_cmd (cmd : Annot.t * string option * LabCmd.t) :
    (Annot.t * string option * LabCmd.t) list =
  let annot, lab, cmd = cmd in

  match cmd with
  | LReturnError ->
      let x_r = JS2JSIL_Helpers.fresh_var () in
      let lab_t = JS2JSIL_Helpers.fresh_then_label () in
      let lab_f = JS2JSIL_Helpers.fresh_else_label () in

      (*
                    x_r := "isNativeErrorName"(ret)
                    goto [x_r] then1 else1
           then1 :  assert (false)
           else1 :  throw1

        *)
      let call =
        LabCmd.LCall
          ( x_r,
            Lit (String JS2JSIL_Helpers.isNativeErrorName),
            [ PVar "ret" ],
            None,
            None )
      in
      let test = LabCmd.LGuardedGoto (PVar x_r, lab_t, lab_f) in
      let t_cmd = LabCmd.LLogic (Assert False) in
      let f_cmd = LabCmd.LReturnError in

      [
        (annot, lab, call);
        (annot, None, test);
        (annot, Some lab_t, t_cmd);
        (annot, Some lab_f, f_cmd);
      ]
  | _ -> [ (annot, lab, cmd) ]

let bi_post_parse_eproc
    (_ : EProg.t)
    (_ : Jslogic.JSLogicCommon.cc_tbl_type)
    (_ : Jslogic.JSLogicCommon.vis_tbl_type)
    (eproc : EProc.t) : EProc.t =
  let new_body =
    Array.of_list
      (List.concat (List.map bi_post_parse_cmd (Array.to_list eproc.body)))
  in

  {
    name = eproc.name;
    body = new_body;
    params = eproc.params;
    spec = eproc.spec;
  }

let create_new_bispec
    (eprog : EProg.t)
    (cc_tbl : Jslogic.JSLogicCommon.cc_tbl_type)
    (vis_tbl : Jslogic.JSLogicCommon.vis_tbl_type)
    (eproc : EProc.t) : unit =
  match eproc.spec with
  | Some _ -> ()
  | None ->
      if eproc.name = !Config.entry_point then ()
      else
        let pre =
          scope_info_to_assertion eprog cc_tbl vis_tbl eproc.name
            (SS.of_list eproc.params)
        in
        let bispec : BiSpec.t =
          { name = eproc.name; params = eproc.params; pre; normalised = false }
        in
        Hashtbl.replace eprog.bi_specs eproc.name bispec;
        ()

let bi_post_parse_eprog
    (eprog : EProg.t)
    (cc_tbl : Jslogic.JSLogicCommon.cc_tbl_type)
    (vis_tbl : Jslogic.JSLogicCommon.vis_tbl_type) : EProg.t =
  let copy_procs = Hashtbl.create Config.big_tbl_size in
  Hashtbl.iter
    (fun name (proc : EProc.t) ->
      let proc' = bi_post_parse_eproc eprog cc_tbl vis_tbl proc in
      let pre_scope_pred =
        create_pre_scope_pred eprog cc_tbl vis_tbl proc.name
          (SS.of_list proc.params)
      in
      let post_scope_pred =
        create_post_scope_pred eprog cc_tbl vis_tbl proc.name
          (SS.of_list proc.params)
      in
      let fun_pred =
        create_function_predicate cc_tbl vis_tbl proc.name proc.params
      in
      create_new_bispec eprog cc_tbl vis_tbl proc';
      Hashtbl.replace eprog.preds pre_scope_pred.name pre_scope_pred;
      Hashtbl.replace eprog.preds post_scope_pred.name post_scope_pred;
      Hashtbl.replace eprog.preds fun_pred.name fun_pred;
      Hashtbl.replace copy_procs name proc')
    eprog.procs;
  { eprog with procs = copy_procs }
