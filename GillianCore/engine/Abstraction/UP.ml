open Containers
open Names
open SVal
module L = Logging

exception CompatFound of Asrt.t

type st = Asrt.t list

type t =
  | Leaf            of Asrt.t option * (Flag.t * Asrt.t list) option (* Final node and associated post-condition *)
  | Inner           of Asrt.t * t list
  | LabInner        of Asrt.t * (t * (string * SS.t) option) list
  | PhantomInner    of t list
  | LabPhantomInner of (t * (string * SS.t) option) list

type pred = { pred : Pred.t; pure : bool; up : t }

type spec = { spec : Spec.t; up : t }

type lemma = { lemma : Lemma.t; up : t }

type prog = {
  preds : (string, pred) Hashtbl.t;
  specs : (string, spec) Hashtbl.t;
  lemmas : (string, lemma) Hashtbl.t;
  coverage : (string * int, int) Hashtbl.t;
  prog : (Annot.t, int) Prog.t;
}

type up_search_state = st * SI.t * SS.t

type preds_tbl_t = (string, pred) Hashtbl.t

type up_err_t =
  | UPSpec      of string * Asrt.t list list
  | UPPred      of string * Asrt.t list list
  | UPLemma     of string * Asrt.t list list
  | UPAssert    of Asrt.t * Asrt.t list list
  | UPInvariant of Asrt.t * Asrt.t list list

exception UPError of up_err_t

(* I need an ins_expr *)

let rec outs_expr (le : Expr.t) : SS.t =
  let f = outs_expr in
  match le with
  | PVar x | LVar x | ALoc x -> SS.singleton x
  | UnOp (LstRev, le) -> f le
  (* LstCat is tricky, because we have to consider the single-element case that doesn't normally exist *)
  | NOp (LstCat, [ x ]) -> f x
  | NOp (LstCat, Lit (LList les) :: le2) ->
      SS.union (f (Lit (LList les))) (f (NOp (LstCat, le2)))
  | NOp (LstCat, EList les :: le2) ->
      SS.union (f (EList les)) (f (NOp (LstCat, le2)))
  (* TODO: For the moment, don't use this next one
     | BinOp (le1, LstCat, EList les) -> SS.union (f le1) (f (EList les)) *)
  | EList les -> List.fold_left (fun ac le -> SS.union (f le) ac) SS.empty les
  | _ -> SS.empty

let rec ins_outs_formula (preds : (string, Pred.t) Hashtbl.t) (form : Formula.t)
    : (SS.t * SS.t * Formula.t) list =
  let f = ins_outs_formula preds in
  let ins_e e =
    SS.union (SS.union (Expr.lvars e) (Expr.alocs e)) (Expr.pvars e)
  in
  let ins_f f =
    SS.union (SS.union (Formula.lvars f) (Formula.alocs f)) (Formula.pvars f)
  in

  L.verbose (fun m -> m "Formula: @[<h>%a@]" Formula.pp form);

  match form with
  | Not e        -> [ (ins_f e, SS.empty, form) ]
  | And (f1, f2) ->
      List_utils.cross_product (f f1) (f f2)
        (fun (ins1, outs1, f1) (ins2, outs2, f2) ->
          (SS.union ins1 ins2, SS.union outs1 outs2, Formula.And (f1, f2)))
  | Eq (e1, e2)  ->
      let ins1 = ins_e e1 in
      let outs1 = outs_expr e1 in
      let ins2 = ins_e e2 in
      let outs2 = outs_expr e2 in

      L.(
        verbose (fun m ->
            m "ins: %s: %s\nouts: %s: %s\nins: %s: %s\nouts: %s: %s"
              ((Fmt.to_to_string Expr.pp) e1)
              (String.concat ", " (SS.elements ins1))
              ((Fmt.to_to_string Expr.pp) e1)
              (String.concat ", " (SS.elements outs1))
              ((Fmt.to_to_string Expr.pp) e2)
              (String.concat ", " (SS.elements ins2))
              ((Fmt.to_to_string Expr.pp) e2)
              (String.concat ", " (SS.elements outs2))));

      let io_l2r =
        if SS.subset ins2 outs2 then [ (ins1, outs2, Formula.Eq (e2, e1)) ]
        else []
      in
      let io_r2l =
        if SS.subset ins1 outs1 then [ (ins2, outs1, Formula.Eq (e1, e2)) ]
        else []
      in
      let ios = io_l2r @ io_r2l in
      L.(verbose (fun m -> m "ios: %d" (List.length ios)));
      if ios <> [] then ios
      else [ (SS.union ins1 ins2, SS.union outs1 outs2, Formula.Eq (e1, e2)) ]
  | _            -> [ (ins_f form, SS.empty, form) ]

let rec ins_outs_assertion (preds : (string, Pred.t) Hashtbl.t) (asrt : Asrt.t)
    : (SS.t * SS.t * Asrt.t) list =
  let ins_expr le =
    SS.union (SS.union (Expr.lvars le) (Expr.alocs le)) (Expr.pvars le)
  in

  let get_pred_ins name =
    match Hashtbl.find_opt preds name with
    | None      -> raise
                     (Failure ("ins_outs_assertion. Unknown Predicate: " ^ name))
    | Some pred -> pred.pred_ins
  in

  match (asrt : Asrt.t) with
  | Pure form           ->
      List.map
        (fun (ins, outs, form) -> (ins, outs, Asrt.Pure form))
        (ins_outs_formula preds form)
  | GA (x, es1, es2)    ->
      let ins =
        List.fold_left (fun ac e -> SS.union ac (ins_expr e)) SS.empty es1
      in
      let outs =
        List.fold_left (fun ac e -> SS.union ac (outs_expr e)) SS.empty es2
      in
      [ (ins, outs, asrt) ]
  | Pred (p_name, args) ->
      let p_ins = get_pred_ins p_name in
      let _, ins, outs =
        List.fold_left
          (fun (i, ins, outs) arg ->
            if List.mem i p_ins then (i + 1, SS.union ins (ins_expr arg), outs)
            else (i + 1, ins, SS.union (outs_expr arg) outs))
          (0, SS.empty, SS.empty) args
      in
      [ (ins, outs, asrt) ]
  | Types les           ->
      let ins =
        List.fold_left
          (fun ins (le, _) -> SS.union ins (ins_expr le))
          SS.empty les
      in
      [ (ins, SS.empty, asrt) ]
  | _                   -> raise
                             (Failure
                                "DEATH. ins_outs_assertion. non-simple assertion")

let rec collect_simple_asrts (a : Asrt.t) : Asrt.t list =
  let f = collect_simple_asrts in
  match a with
  | Pure True | Emp -> []
  | Pure _ | Pred _ | Types _ | GA _ -> [ a ]
  | Star (a1, a2) -> f a1 @ f a2

let s_init
    (known_lvars : SS.t) (preds : (string, Pred.t) Hashtbl.t) (a : Asrt.t) :
    (st, st) result =
  let prioritise (la : Asrt.t list) = List.sort Asrt.prioritise la in

  L.verbose (fun m -> m "@[<v 2>s_init on:@ @[%a]@ @]" Asrt.pp a);

  let simple_asrts = collect_simple_asrts a in
  let simple_asrts =
    List.map
      (fun a ->
        match (a : Asrt.t) with
        | Types _ -> Reduction.reduce_assertion a
        | _       -> a)
      simple_asrts
  in
  let simple_asrts =
    List.concat
      (List.map
         (fun a ->
           match (a : Asrt.t) with
           | Types le -> List.map (fun e -> Asrt.Types [ e ]) le
           | _        -> [ a ])
         simple_asrts)
  in
  let simple_asrts =
    if List.mem (Asrt.Pure False) simple_asrts then [ Asrt.Pure False ]
    else simple_asrts
  in
  let simple_asrts = List.filter (fun a -> a <> Asrt.Pure True) simple_asrts in
  let stay, rest =
    List.partition
      (function
        | Asrt.Pred _ | Asrt.GA _ -> true
        | _                       -> false)
      simple_asrts
  in
  let rest = Asrt.Set.elements (Asrt.Set.of_list rest) in
  let simple_asrts = prioritise (stay @ rest) in
  let simple_asrts_io =
    List.map (fun a -> (a, ins_outs_assertion preds a)) simple_asrts
  in
  let simple_asrts_io = Array.of_list simple_asrts_io in

  (* check if the assertion at index i can be added to the unification
     plan - its ins need to be contained in the current known lvars *)
  let visit_asrt (known_lvars : SS.t) (i : int) : (Asrt.t * SS.t) list =
    let _, ios = simple_asrts_io.(i) in
    let act_ios =
      List.filter (fun (ins, _, _) -> SS.subset ins known_lvars) ios
    in
    List.map (fun (_, outs, a) -> (a, SS.union known_lvars outs)) act_ios
  in

  let rec visit_asrt_lst
      (known_lvars : SS.t) (indexes : SI.t) (visited_indexes : int list) :
      (SI.t * (Asrt.t * SS.t) list) option =
    if indexes = SI.empty then None
    else
      let i = SI.min_elt indexes in
      let rest_indexes = SI.remove i indexes in
      match visit_asrt known_lvars i with
      | []  -> visit_asrt_lst known_lvars rest_indexes (i :: visited_indexes)
      | ret -> Some (SI.union (SI.of_list visited_indexes) rest_indexes, ret)
  in

  let rec search (up_search_states : up_search_state list) : (st, st) result =
    match up_search_states with
    | [] ->
        raise
          (Failure
             "UP: Should not happen: unification plan creation called with no \
              starting state.")
    | (up, unchecked, _) :: _ when unchecked = SI.empty ->
        L.verbose (fun m -> m "Successfully created UP.");
        Ok (List.rev up)
    | (up, unchecked, known_lvars) :: rest -> (
        L.verbose (fun m ->
            m
              "KNOWN VARS: @[%a@].@\n\
               @[<v 2>CUR UP:@\n\
               %a@]@\n\
               TO VISIT: @[%a@]@\n\
               @[%a@]"
              Fmt.(iter ~sep:comma SS.iter string)
              known_lvars
              Fmt.(list ~sep:(any "@\n") Asrt.pp)
              up
              Fmt.(iter ~sep:comma SI.iter int)
              unchecked
              Fmt.(
                iter ~sep:(any "@\n") SI.iter (fun f i ->
                    Asrt.pp f (fst simple_asrts_io.(i))))
              unchecked);

        match visit_asrt_lst known_lvars unchecked [] with
        | None                      ->
            L.verbose (fun m -> m "No assertions left to visit.");
            if rest = [] then (
              L.verbose (fun m -> m "Detecting spec-var existentials.");
              let unchckd =
                List.map
                  (fun i ->
                    match simple_asrts_io.(i) with
                    | _, (ins, _, _) :: _ -> ins
                    | _                   ->
                        raise
                          (Exceptions.Impossible
                             "s_init: guaranteed by construction"))
                  (SI.elements unchecked)
              in
              let unchckd =
                List.map
                  (fun u ->
                    SS.diff
                      (SS.filter (fun x -> is_spec_var_name x) u)
                      known_lvars)
                  unchckd
              in
              let unchckd = List.filter (fun u -> u <> SS.empty) unchckd in
              L.(
                verbose (fun m ->
                    m "\t%s"
                      (String.concat "\n\t"
                         (List.map
                            (fun u -> String.concat ", " (SS.elements u))
                            unchckd))));
              (* if (List.length unchckd > 0) then (
                   let heuristic_var : string = SS.min_elt (List.hd unchckd) in
                     L.log L.verbose (lazy ("Heuristically adding existential: " ^ heuristic_var));
                     search [ (up, unchecked, SS.add heuristic_var known_lvars) ]
                 ) else (
                   (* This is where it really ends - couldn't continue naturally, couldn't heuristically extend *) *)
              L.verbose (fun m -> m "Unification plan creation failure.");
              let unchecked, _ =
                List.split
                  (List.map
                     (fun i -> simple_asrts_io.(i))
                     (SI.elements unchecked))
              in
              Error unchecked )
            else search rest
        | Some (new_unchecked, ret) ->
            (* L.log L.verbose (lazy "Successfully added more assertions to the UP.");
               L.(verbose (fun m -> m "States to examine: %d" (List.length ret)));
               L.(verbose (fun m -> m "Unchecked remaining: %d" (SI.cardinal new_unchecked))); *)
            let new_search_states =
              List.map
                (fun (a, new_known_vars) ->
                  (a :: up, new_unchecked, new_known_vars))
                ret
            in
            search (new_search_states @ rest) )
  in

  let initial_indexes =
    SI.of_list
      (Array.to_list (Array.init (List.length simple_asrts) (fun i -> i)))
  in
  let initial_search_state = ([], initial_indexes, known_lvars) in
  search [ initial_search_state ]

let rec lift_up (up : st) (posts : (Flag.t * Asrt.t list) option) : t =
  match up with
  | []       -> Leaf (None, posts)
  | [ p ]    -> Leaf (Some p, posts)
  | p :: up' -> Inner (p, [ lift_up up' posts ])

let add_up (g_up : t) (up_post : st * (Flag.t * Asrt.t list) option) : t =
  match (g_up, up_post) with
  | PhantomInner ups, (up, posts) -> PhantomInner (ups @ [ lift_up up posts ])
  | _, (up, posts) -> PhantomInner [ g_up; lift_up up posts ]

let lift_ups
    (ups : (st * ((string * SS.t) option * (Flag.t * Asrt.t list) option)) list)
    : t =
  let b =
    List.exists
      (fun (_, (lab, _)) ->
        match lab with
        | Some _ -> true
        | _      -> false)
      ups
  in
  let ups' = List.map (fun (up, (_, posts)) -> (up, posts)) ups in
  if b then
    (* Printf.printf "BUILDING GUP FOR SPEC WITH EXISTENTIALS\n"; *)
    let gups =
      List.map (fun (up, (lab, posts)) -> (lift_up up posts, lab)) ups
    in
    LabPhantomInner gups
  else List.fold_left add_up (PhantomInner []) ups'

let init
    ?(use_params : bool option)
    (known_vars : SS.t)
    (params : SS.t)
    (preds : (string, Pred.t) Hashtbl.t)
    (asrts_posts :
      (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option)) list)
    : (t, st list) result =
  let known_vars =
    match use_params with
    | None   -> known_vars
    | Some _ -> SS.union known_vars params
  in

  let ups =
    List.map
      (fun (asrt, (lab, posts)) ->
        let existentials =
          Option.fold
            ~some:(fun (_, existentials) -> existentials)
            ~none:SS.empty lab
        in
        let known_vars = SS.union known_vars existentials in
        (s_init known_vars preds asrt, (lab, posts)))
      asrts_posts
  in
  let errors, _ =
    List.partition
      (fun (up, _) ->
        match up with
        | Error _ -> true
        | Ok _    -> false)
      ups
  in
  let errors, _ = List.split errors in
  let errors =
    List.map
      (fun x ->
        match x with
        | Error e -> e
        | _       -> raise (Failure "UP: init: Impossible: non-error error"))
      errors
  in

  if errors <> [] then Error errors
  else
    Ok
      (lift_ups
         (List.map
            (fun (up, posts) ->
              ( ( match up with
                | Ok up   -> up
                | Error _ ->
                    raise (Failure "UP: init: Impossible: ok, but error") ),
                posts ))
            ups))

let next ?(lab : string option) (up : t) :
    (t * (string * SS.t) option) list option =
  match up with
  | Leaf _ -> None
  | Inner (_, ups) -> Some (List.map (fun x -> (x, None)) ups)
  | LabInner (_, lab_ups) when List.length lab_ups > 0 -> Some lab_ups
  | PhantomInner ups when List.length ups > 0 ->
      Some (List.map (fun x -> (x, None)) ups)
  | LabPhantomInner lab_ups when List.length lab_ups > 0 -> Some lab_ups
  | _ -> None

let head (up : t) : Asrt.t option =
  match up with
  | Leaf (Some p, _) | Inner (p, _) | LabInner (p, _) -> Some p
  | _ -> None

let posts (up : t) : (Flag.t * Asrt.t list) option =
  match up with
  | Leaf (_, posts) -> posts
  | _               -> None

let rec pp ft up =
  let open Fmt in
  let pp_lab ft lab =
    let lab, vars = lab in
    pf ft " [%s: @[<h>%a@]]" lab (iter ~sep:comma SS.iter string) vars
  in
  match up with
  | Leaf (a, None) ->
      pf ft "Leaf: @[%a@] with Posts = NONE"
        (option ~none:(any "NONE") Asrt.pp)
        a
  | Leaf (a, Some (flag, posts)) ->
      pf ft "Leaf: @[%a@] with Flag %a and Posts:@\n  @[%a@]"
        (option ~none:(any "NONE") Asrt.pp)
        a Flag.pp flag
        (list ~sep:(any "@\n") (hovbox Asrt.pp))
        posts
  | Inner (a, next_ups) ->
      let pp_children ft ch =
        if List.length ch = 1 then pp ft (List.hd ch)
        else
          let pp_one_child ftp (i, up) = pf ft "Children %d@\n%a" i pp up in
          pf ft "@[<v 2>  %a@]"
            (iter_bindings ~sep:(any "@\n") List.iteri pp_one_child)
            ch
      in
      pf ft "Inner Node: @[%a@] with %d children@\n%a" Asrt.pp a
        (List.length next_ups) pp_children next_ups
  | LabInner (a, next_ups) ->
      let pp_children ft ch =
        if List.length ch = 1 then
          let up, lab = List.hd ch in
          (pair (option pp_lab) pp) ft (lab, up)
        else
          let pp_one_child ftp (i, (up, lab)) =
            pf ft "Children %d%a@\n%a" i (option pp_lab) lab pp up
          in
          pf ft "@[<v 2>  %a@]"
            (iter_bindings ~sep:(any "@\n") List.iteri pp_one_child)
            ch
      in
      pf ft "Inner Node: @[<h>%a@] with %d children@\n%a" Asrt.pp a
        (List.length next_ups) pp_children next_ups
  | PhantomInner next_ups ->
      let pp_child ft (i, ch) = pf ft "Children %d@\n%a" i pp ch in
      pf ft "@[<v 2>Phantom Node %d children@\n%a@]" (List.length next_ups)
        (iter_bindings ~sep:(any "@\n") List.iteri pp_child)
        next_ups
  | LabPhantomInner next_ups ->
      let pp_child ft (i, (up, lab)) =
        pf ft "Children %d%a@\n%a" i (option pp_lab) lab pp up
      in
      pf ft "@[<v 2>LabPhantom Node %d children@\n%a@]" (List.length next_ups)
        (iter_bindings ~sep:(any "@\n") List.iteri pp_child)
        next_ups

let init_specs (preds : (string, Pred.t) Hashtbl.t) (specs : Spec.t list) :
    ((string, spec) Hashtbl.t, up_err_t) result =
  let u_specs = Hashtbl.create Config.medium_tbl_size in
  try
    List.iter
      (fun (spec : Spec.t) ->
        L.(
          verbose (fun m ->
              m "Attempting to create UP for a spec of %s : %d specs"
                spec.spec_name
                (List.length spec.spec_sspecs)));
        let params = SS.of_list spec.spec_params in
        let sspecs :
            (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option))
            list =
          List.mapi
            (fun i (sspec : Spec.st) ->
              L.verbose (fun m ->
                  m "lab of sspec %d: @[<h>%a@]" i
                    Fmt.(
                      option
                        (brackets
                           (pair ~sep:(any ": ") string
                              (list ~sep:comma string))))
                    sspec.ss_label);
              ( sspec.ss_pre,
                ( Spec.label_vars_to_set sspec.ss_label,
                  Some (sspec.ss_flag, sspec.ss_posts) ) ))
            spec.spec_sspecs
        in

        let up = init ~use_params:true SS.empty params preds sspecs in
        match up with
        | Error err ->
            raise (UPError (UPSpec (spec.spec_name, err)))
            (* let msg = Printf.sprintf "Specification of %s cannot be turned into UP. %s"
                 spec.name (Spec.str spec) in
               L.fail msg *)
        | Ok up     ->
            L.(
              verbose (fun m ->
                  m "Successfully created UP of specification of %s"
                    spec.spec_name));
            Hashtbl.replace u_specs spec.spec_name { spec; up })
      specs;
    Ok u_specs
  with UPError e -> Error e

let init_lemmas (preds : (string, Pred.t) Hashtbl.t) (lemmas : Lemma.t list) :
    ((string, lemma) Hashtbl.t, up_err_t) result =
  let u_lemmas = Hashtbl.create Config.medium_tbl_size in
  try
    List.iter
      (fun (lemma : Lemma.t) ->
        let params = SS.of_list lemma.lemma_params in
        let sspecs :
            (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option))
            list =
          [ (lemma.lemma_hyp, (None, Some (Flag.Normal, lemma.lemma_concs))) ]
        in
        let up = init ~use_params:true SS.empty params preds sspecs in
        match up with
        | Error err ->
            raise (UPError (UPLemma (lemma.lemma_name, err)))
            (* let msg = Printf.sprintf "Lemma %s cannot be turned into UP" lemma.name in
               L.fail msg *)
        | Ok up     ->
            L.(
              verbose (fun m ->
                  m "Successfully created UP of Lemma %s" lemma.lemma_name));
            Hashtbl.replace u_lemmas lemma.lemma_name { lemma; up })
      lemmas;
    Ok u_lemmas
  with UPError e -> Error e

let init_preds (preds : (string, Pred.t) Hashtbl.t) :
    ((string, pred) Hashtbl.t, up_err_t) result =
  let u_preds = Hashtbl.create Config.medium_tbl_size in
  try
    Hashtbl.iter
      (fun name (pred : Pred.t) ->
        L.(verbose (fun m -> m "Attempting to create UP of predicate %s" name));
        let known_params =
          SS.of_list
            (List.map
               (fun i ->
                 let param, _ = List.nth pred.pred_params i in
                 param)
               pred.pred_ins)
        in

        let defs =
          List.map
            (fun (lab, def) ->
              let lab' =
                Option.map (fun (s, vars) -> (s, SS.of_list vars)) lab
              in
              (def, (lab', None)))
            pred.pred_definitions
        in

        match init known_params SS.empty preds defs with
        | Error err -> raise (UPError (UPPred (pred.pred_name, err)))
        (* let msg = Printf.sprintf "Predicate definition of %s cannot be turned into UP" pred.name in
           L.fail msg *)
        | Ok up ->
            L.verbose (fun m ->
                m "Successfully created UP of predicate %s:\n%a" name pp up);
            Hashtbl.replace u_preds name { pred; pure = pred.pred_pure; up })
      preds;
    Ok u_preds
  with UPError e -> Error e

let init_prog (prog : ('a, int) Prog.t) : (prog, up_err_t) result =
  let all_specs : Spec.t list = Prog.get_specs prog in

  let lemmas : Lemma.t list = Prog.get_lemmas prog in
  let preds_tbl : ((string, pred) Hashtbl.t, up_err_t) result =
    init_preds prog.preds
  in
  match preds_tbl with
  | Error e      -> Error e
  | Ok preds_tbl -> (
      let lemmas_tbl : ((string, lemma) Hashtbl.t, up_err_t) result =
        init_lemmas prog.preds lemmas
      in
      match lemmas_tbl with
      | Error e       -> Error e
      | Ok lemmas_tbl -> (
          let specs_tbl : ((string, spec) Hashtbl.t, up_err_t) result =
            init_specs prog.preds all_specs
          in
          match specs_tbl with
          | Error e      -> Error e
          | Ok specs_tbl ->
              let coverage : (string * int, int) Hashtbl.t =
                Hashtbl.create Config.big_tbl_size
              in
              Ok
                {
                  prog;
                  specs = specs_tbl;
                  preds = preds_tbl;
                  lemmas = lemmas_tbl;
                  coverage;
                } ) )

let rec expr_compatible e e' subst : bool =
  let result =
    match ((e : Expr.t), (e' : Expr.t)) with
    | Lit l1, Lit l2 when l1 = l2 -> true
    | PVar p1, PVar p2 when p1 = p2 -> true
    | LVar v1, LVar v2 -> (
        match SSubst.get subst v1 with
        | None     ->
            SSubst.extend subst [ (v1, Expr.LVar v2) ];
            true
        | Some v2' -> Expr.LVar v2 = v2' )
    | ALoc a1, ALoc a2 -> (
        match SSubst.get subst a1 with
        | None     ->
            SSubst.extend subst [ (a1, Expr.ALoc a2) ];
            true
        | Some a2' -> Expr.ALoc a2 = a2' )
    | UnOp (op1, e), UnOp (op2, e') when op1 = op2 -> expr_compatible e e' subst
    | BinOp (e1, op1, e2), BinOp (e1', op2, e2') when op1 = op2 ->
        expr_compatible e1 e1' subst && expr_compatible e2 e2' subst
    | NOp (op1, les), NOp (op2, les') when op1 = op2 ->
        expr_list_compatible (List.combine les les') subst
    | EList les, EList les' | ESet les, ESet les' ->
        expr_list_compatible (List.combine les les') subst
    | _, _ -> false
  in
  (* L.(verbose (fun m -> m "Compat expr: %s %s : %b with %s" ((Fmt.to_to_string Expr.pp) e) ((Fmt.to_to_string Expr.pp) e') result (SSubst.str subst))); *)
  result

and expr_list_compatible (esses : (Expr.t * Expr.t) list) (subst : SSubst.t) :
    bool =
  let temp_subst : SSubst.t = SSubst.init [] in

  let rec loop esses =
    match esses with
    | []              ->
        if SSubst.compatible subst temp_subst then (
          SSubst.merge_left subst temp_subst;
          true )
        else false
    | (e, e') :: rest ->
        if expr_compatible e e' temp_subst then loop rest else false
  in
  loop esses

let asrt_compatible p q subst =
  match ((p : Asrt.t), (q : Asrt.t)) with
  | GA (name, es1, es2), GA (name', es1', es2') when name = name' ->
      expr_list_compatible (List.combine es1 es1') subst
      && expr_list_compatible (List.combine es2 es2') subst
  | Pred (name, es), Pred (name', es') when name = name' ->
      expr_list_compatible (List.combine es es') subst
  | Pure (Eq (PVar x, le)), Pure (Eq (PVar y, le')) ->
      x = y && expr_list_compatible [ (le, le') ] subst
  | _ -> false

let check_compatibility (ps : Asrt.t list) (qs : Asrt.t list) : SSubst.t option
    =
  let subst = SSubst.init [] in

  let rec loop (ps : Asrt.Set.t) (qs : Asrt.Set.t) : bool =
    match ps = Asrt.Set.empty with
    | true  -> true
    | false -> (
        let p = Asrt.Set.min_elt ps in
        let ps = Asrt.Set.remove p ps in
        try
          Asrt.Set.iter
            (fun q ->
              let cassrts = asrt_compatible p q subst in
              (* L.(verbose (fun m -> m "Compat asrt: %s %s : %b with %s" (Asrt.str p) (Asrt.str q) cassrts (SSubst.str subst))); *)
              if cassrts then raise (CompatFound q))
            qs;
          false
        with CompatFound q ->
          let qs = Asrt.Set.remove q qs in
          loop ps qs )
  in
  if loop (Asrt.Set.of_list ps) (Asrt.Set.of_list qs) then Some subst else None

(** Substitution inverse *)
let inverse (subst : SSubst.t) : SSubst.t =
  let inv_subst = SSubst.init [] in
  SSubst.iter subst (fun v le ->
      (* Convert v to le *)
      let v_le : Expr.t =
        if is_spec_var_name v || is_lvar_name v then LVar v
        else if is_aloc_name v then ALoc v
        else raise (Failure ("Bizarre variable in subst: " ^ v))
      in

      (* Convert le to v *)
      let le_v =
        match (le : Expr.t) with
        | LVar x | ALoc x -> Some x
        | _               -> None
      in

      match le_v with
      | None   -> ()
      | Some v -> SSubst.add inv_subst v v_le);
  inv_subst

let get_pred_def (pred_defs : preds_tbl_t) (name : string) : pred =
  try
    let up_pred = Hashtbl.find pred_defs name in
    up_pred
  with _ -> raise (Failure (Printf.sprintf "DEATH. PRED %s NOT DEFINED" name))

let init_pred_defs () : preds_tbl_t = Hashtbl.create Config.medium_tbl_size

let get_procs (prog : prog) : ('a, int) Proc.t list = Prog.get_procs prog.prog

let get_bispecs (prog : prog) : BiSpec.t list = Prog.get_bispecs prog.prog

let get_lemma (prog : prog) (name : string) : (lemma, unit) result =
  match Hashtbl.find_opt prog.lemmas name with
  | Some lemma -> Ok lemma
  | None       -> Error ()

let rec pp_asrt
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (a : Asrt.t) =
  let pp_asrt = pp_asrt ?preds_printer ~preds in
  match a with
  | Star (a1, a2)     -> Fmt.pf fmt "%a *@ %a" pp_asrt a1 pp_asrt a2
  | Pred (name, args) -> (
      match preds_printer with
      | Some pp_pred -> (Fmt.hbox pp_pred) fmt (name, args)
      | None         -> (
          try
            let pred = get_pred_def preds name in
            let out_params = Pred.out_params pred.pred in
            let out_args = Pred.out_args pred.pred args in
            let in_args = Pred.in_args pred.pred args in
            let out_params_args = List.combine out_params out_args in
            let pp_out_params_args fmt (x, e) =
              Fmt.pf fmt "@[<h>%s: %a@]" x Expr.pp e
            in
            Fmt.pf fmt "%s(@[<h>%a@])" name
              (Pred.pp_ins_outs pred.pred Expr.pp pp_out_params_args)
              (in_args, out_params_args)
          with _ -> Asrt.pp fmt a ) )
  | a                 -> Asrt.pp fmt a

let pp_sspec
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (sspec : Spec.st) =
  let pp_a = pp_asrt ?preds_printer ~preds in
  Fmt.pf fmt "[[ @[<hv>%a@] ]]@\n[[ @[<hv>%a@] ]]@\n%a" pp_a sspec.ss_pre
    Fmt.(list ~sep:semi pp_a)
    sspec.ss_posts Flag.pp sspec.ss_flag

let pp_spec
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (spec : Spec.t) =
  let normal_specs, error_specs =
    List.partition
      (fun (spec : Spec.st) -> spec.ss_flag = Flag.Normal)
      spec.spec_sspecs
  in
  let pp_sspec = pp_sspec ?preds_printer ~preds in
  Fmt.pf fmt "@[<v 2>spec %s (@[<h>%a@])@\n%a;@\n%a@]" spec.spec_name
    Fmt.(list ~sep:comma string)
    spec.spec_params
    Fmt.(list ~sep:(any "@\n") pp_sspec)
    normal_specs
    Fmt.(list ~sep:(any "@\n") pp_sspec)
    error_specs

let pp_normal_spec
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (spec : Spec.t) =
  let normal_specs =
    List.filter
      (fun (spec : Spec.st) -> spec.ss_flag = Flag.Normal)
      spec.spec_sspecs
  in
  let pp_sspec = pp_sspec ?preds_printer ~preds in
  Fmt.pf fmt "@[<v 2>spec %s (@[<h>%a@])@\n%a@]" spec.spec_name
    Fmt.(list ~sep:comma string)
    spec.spec_params
    Fmt.(list ~sep:(any "@\n") pp_sspec)
    normal_specs

let add_spec (prog : prog) (spec : Spec.t) : unit =
  let params = SS.of_list spec.spec_params in
  let proc =
    match Prog.get_proc prog.prog spec.spec_name with
    | None      -> raise (Failure "DEATH. ADDING SPEC TO UNKNOWN PROC!")
    | Some proc -> proc
  in

  let posts_from_sspecs sspecs =
    List.map
      (fun (sspec : Spec.st) ->
        (sspec.ss_pre, Some (sspec.ss_flag, sspec.ss_posts)))
      sspecs
  in

  let new_uspec (spec : Spec.t) : spec =
    let posts =
      List.map
        (fun (x, y) -> (x, (None, y)))
        (posts_from_sspecs spec.spec_sspecs)
    in
    let up = init ~use_params:true SS.empty params prog.prog.preds posts in
    match up with
    | Error _ ->
        let msg =
          Fmt.str
            "Spec addition: specification of %s cannot be turned into UP. %a"
            spec.spec_name Spec.pp spec
        in
        L.fail msg
    | Ok up   ->
        L.(
          verbose (fun m ->
              m "Successfully created UP of specification of %s" spec.spec_name));
        let new_spec : spec = { spec; up } in
        new_spec
  in

  let extend_spec (uspec : spec) (sspecs : Spec.st list) : spec =
    let spec = Spec.extend uspec.spec sspecs in
    let ups =
      List.map
        (fun (asrt, posts) -> (asrt, s_init params prog.prog.preds asrt, posts))
        (posts_from_sspecs sspecs)
    in
    let new_gup =
      List.fold_left
        (fun g_up (pre, pre_up, posts) ->
          match pre_up with
          | Error _   ->
              L.verbose (fun m ->
                  m
                    "WARNING!!! IT IS NOT POSSIBLE TO BUILD UP FOR INFERRED \
                     SPEC of %s!PRE:@\n\
                     @[%a@]@\n"
                    uspec.spec.spec_name Asrt.pp pre);
              (* Printf.printf "%s" msg; *)
              g_up
          | Ok pre_up -> add_up g_up (pre_up, posts))
        uspec.up ups
    in
    let uspec' : spec = { spec; up = new_gup } in
    uspec'
  in

  let new_uspec =
    match Hashtbl.find_opt prog.specs spec.spec_name with
    | None       -> new_uspec spec
    | Some uspec -> extend_spec uspec spec.spec_sspecs
  in

  Hashtbl.replace prog.specs spec.spec_name new_uspec;
  Hashtbl.replace prog.prog.procs spec.spec_name
    { proc with proc_spec = Some new_uspec.spec }

let update_coverage (prog : prog) (proc_name : string) (index : int) : unit =
  try
    let count = Hashtbl.find prog.coverage (proc_name, index) in
    Hashtbl.replace prog.coverage (proc_name, index) (count + 1)
  with Not_found -> Hashtbl.replace prog.coverage (proc_name, index) 0

let first_time_running (prog : prog) (proc_name : string) (index : int) : bool =
  not (Hashtbl.mem prog.coverage (proc_name, index))

let pp_pred_defs fmt pred_defs =
  let pp_binding fmt (_, up_pred) = Pred.pp fmt up_pred.pred in
  Fmt.(hashtbl ~sep:(any "@\n") pp_binding) fmt pred_defs
