open Containers
open Names
module L = Logging

(** The [outs] type represents a list of learned outs, together
    with (optionally) the way of constructing them *)
type outs = (Expr.t * Expr.t) list [@@deriving yojson]

let outs_pp = Fmt.(list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.pp)))

(** The [up_step] type represents a unification plan step,
    consisting of an assertion together with the possible
    learned outs *)
type step = Asrt.t * outs [@@deriving yojson]

let step_pp = Fmt.Dump.pair Asrt.pp outs_pp

(** The [pt] type represents a pre-unification plan,
    consisting of a list of unification plan steps *)
type pt = step list

let pt_pp = Fmt.(brackets (list ~sep:semi step_pp))

type t =
  | Leaf of step option * (Flag.t * Asrt.t list) option * string list
      (** Final node and associated post-condition *)
  | Inner of step * t list
  | PhantomInner of t list
  | LabPhantomInner of (t * (string * SS.t) option) list
[@@deriving yojson]

type pred = { pred : Pred.t; up : t }
type spec = { spec : Spec.t; up : t }
type lemma = { lemma : Lemma.t; up : t }

type prog = {
  preds : (string, pred) Hashtbl.t;
  specs : (string, spec) Hashtbl.t;
  lemmas : (string, lemma) Hashtbl.t;
  coverage : (string * int, int) Hashtbl.t;
  prog : (Annot.t, int) Prog.t;
}

(** Knowledge bases *)
module KB = Expr.Set

let kb_pp = Fmt.(braces (iter ~sep:comma KB.iter Expr.full_pp))

type up_search_state = pt * SI.t * KB.t
type preds_tbl_t = (string, pred) Hashtbl.t

type up_err_t =
  | UPSpec of string * Asrt.t list list
  | UPPred of string * Asrt.t list list
  | UPLemma of string * Asrt.t list list
  | UPAssert of Asrt.t * Asrt.t list list
  | UPInvariant of Asrt.t * Asrt.t list list
[@@deriving show]

exception UPError of up_err_t

let is_var (e : Expr.t) : bool =
  match e with
  | PVar _ | LVar _ -> true
  | _ -> false

(** List lengths are not required if their variables are *)
let minimise_unifiables (kb : KB.t) : KB.t =
  KB.fold
    (fun u ac ->
      match u with
      | UnOp (LstLen, e) -> (
          match KB.mem e kb with
          | true -> ac
          | false -> KB.add u ac)
      | _ -> KB.add u ac)
    kb KB.empty

(** [missing kb e] returns a list of unifiables that are missing
    in order for the expression [e] to be known under knowledge
    base [kb]. The expression is required to have previously been
    fully reduced. *)
let rec missing_expr (kb : KB.t) (e : Expr.t) : KB.t list =
  let f = missing_expr kb in
  let join (le : Expr.t list) =
    let mle = List.map f le in
    let cpmle = List_utils.list_product mle in
    let umle =
      List.map
        (fun le -> List.fold_left (fun m e -> KB.union m e) KB.empty le)
        cpmle
    in
    if umle = [] || List.mem KB.empty umle then [ KB.empty ] else umle
  in
  match KB.mem e kb with
  | true -> [ KB.empty ]
  | false -> (
      match e with
      (* Literals are always known *)
      | Lit _ -> [ KB.empty ]
      (* Program variables, logical variables, and abstract locations
         are known if and only if they are in the knowledge base *)
      | PVar _ | LVar _ | ALoc _ -> [ KB.singleton e ]
      | UnOp (LstLen, e1) -> (
          (* If a LstLen exists, then it must be of a program or a logical variable.
             All other cases (literal list, expression list, list concat, sub-list)
             must have been taken care of by reduction *)
          match e1 with
          | EList _ -> [ KB.empty ]
          | _ -> (
              let () =
                if not (is_var e1) then
                  raise
                    (Failure
                       (Format.asprintf
                          "missing_expr: Should have been reduced: %a" Expr.pp
                          e1))
              in
              match KB.mem e1 kb with
              | true -> [ KB.empty ]
              (* List lengths are unifiables *)
              | false -> [ KB.singleton e1; KB.singleton e ]))
      (* The remaining cases proceed recursively *)
      | UnOp (_, e) -> f e
      | BinOp (e1, _, e2) -> join [ e1; e2 ]
      | NOp (_, le) | EList le | ESet le -> join le
      | LstSub (e1, e2, e3) ->
          let result = join [ e1; e2; e3 ] in
          L.verbose (fun fmt ->
              fmt "Missing for %a: %a" Expr.full_pp e
                Fmt.(brackets (list ~sep:semi kb_pp))
                result);
          result)

(** [is_known kb e] returns true if the expression [e] is known
    under knowledge base [kb], and false otherwise *)
let is_known_expr (kb : KB.t) (e : Expr.t) : bool =
  missing_expr kb e = [ KB.empty ]

(** [learn kb e] tries to learn unifiables in the expression [e]
    not known in the knowledge base [kb]. It returns a list of
    pairs, each of which contains the learned unifiable and the
    method of its construction. *)
let rec learn_expr
    ?(top_level = false)
    (kb : KB.t)
    (base_expr : Expr.t)
    (e : Expr.t) : outs =
  let f = learn_expr kb in
  match e with
  (* Literals, abstract locations, sublists, and sets are never invertible *)
  | Lit _ | LstSub _ | ESet _ -> []
  (* Nothing is learned if the top-level expr is a program or a logical variable *)
  (* Nothing is learned if the program or logical variable is already known *)
  | (PVar _ | LVar _ | ALoc _ | UnOp (LstLen, _)) when top_level || KB.mem e kb
    -> []
  (* Otherwise, we do learn the program or logical variable *)
  | PVar _ | LVar _ | ALoc _ | UnOp (LstLen, _) -> [ (e, base_expr) ]
  (* Unary minuses are invertible *)
  | UnOp (IUnaryMinus, e') -> f (UnOp (IUnaryMinus, e)) e'
  | UnOp (FUnaryMinus, e') -> f (UnOp (FUnaryMinus, e)) e'
  (* TODO: Finish the remaining invertible unary operators *)
  | UnOp _ -> []
  (* EList is iteratively invertible *)
  | EList le ->
      let le_with_base_exprs =
        List.mapi
          (fun i e -> (e, Expr.BinOp (base_expr, LstNth, Expr.int i)))
          le
      in
      L.(
        verbose (fun m ->
            m "List of expressions: %a"
              Fmt.(
                brackets
                  (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.pp))))
              le_with_base_exprs));
      (* Now comes the iteration *)
      learn_expr_list kb le_with_base_exprs
  (* Set n-ary operators are not invertible *)
  | NOp (SetInter, _) | NOp (SetUnion, _) -> []
  (* TODO: LstCat is invertible, but not for now *)
  | NOp (LstCat, []) -> f base_expr (EList [])
  | NOp (LstCat, [ x ]) -> f base_expr x
  | NOp (LstCat, e :: rest) -> (
      let overall_length = Expr.list_length base_expr in
      let e_length = Expr.list_length e in
      match is_known_expr kb e_length with
      | true ->
          let e_base_expr = Expr.LstSub (base_expr, Expr.zero_i, e_length) in
          let e_outs = f e_base_expr e in
          let kb' : KB.t =
            List.fold_left (fun kb (u, _) -> KB.add u kb) kb e_outs
          in
          let rest = Expr.NOp (LstCat, rest) in
          let rest_base_expr =
            Expr.LstSub
              (base_expr, e_length, BinOp (overall_length, IMinus, e_length))
          in
          e_outs @ learn_expr kb' rest_base_expr rest
      | false -> [])
  (* Floating-point plus is invertible *)
  | BinOp (e1, FPlus, e2) -> (
      (* If both operands are known or both are unknown, nothing can be done *)
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      match (ike1, ike2) with
      | true, true | false, false -> []
      | _ ->
          (* Get the known and the unknown operand *)
          let ke, ue =
            match ike1 with
            | true -> (e1, e2)
            | false -> (e2, e1)
          in
          f (BinOp (base_expr, FMinus, ke)) ue)
  | BinOp (e1, IPlus, e2) -> (
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      match (ike1, ike2) with
      | true, true | false, false -> []
      | true, false -> f (BinOp (base_expr, IMinus, e1)) e2
      | false, true -> f (BinOp (base_expr, IMinus, e2)) e1)
  (* Floating-point minus is invertible in two different ways *)
  | BinOp (e1, FMinus, e2) -> (
      (* If both operands are known or both are unknown, nothing can be done *)
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      match (ike1, ike2) with
      | true, true | false, false -> []
      | false, true -> f (BinOp (base_expr, FPlus, e2)) e1
      | true, false -> f (BinOp (e1, FMinus, base_expr)) e2)
  | BinOp (e1, IMinus, e2) -> (
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      match (ike1, ike2) with
      | true, true | false, false -> []
      | false, true -> f (BinOp (base_expr, IPlus, e2)) e1
      | true, false -> f (BinOp (e1, IMinus, base_expr)) e2)
  (* TODO: Finish the remaining invertible binary operators *)
  | BinOp _ -> []

and learn_expr_list (kb : KB.t) (le : (Expr.t * Expr.t) list) =
  (* L.(verbose (fun m -> m "Entering learn_expr_list: \nKB: %a\nList: %a" kb_pp kb Fmt.(brackets (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.pp)))) le)); *)
  (* Learn unifiables per-element *)
  let learned_exprs =
    List.map
      (fun (e, base_expr) -> ((e, base_expr), learn_expr kb base_expr e))
      le
  in
  (* Filter learned unifiables *)
  let learned, not_learned =
    List.partition (fun (_, learned) -> learned <> []) learned_exprs
  in
  match learned with
  (* We have learned nothing, therefore we stop *)
  | [] -> []
  | _ ->
      (* Get all learned unifiables in order from left to right *)
      let learned = List.concat (snd (List.split learned)) in
      (* Add learned unifiables to knowledge base *)
      let new_kb = List.fold_left (fun kb (e, _) -> KB.add e kb) kb learned in
      (* Recover the not-yet-learned bindings *)
      let not_learned = fst (List.split not_learned) in
      (* and try to learn more *)
      learned @ learn_expr_list new_kb not_learned

let simple_ins_expr_collector =
  object
    inherit [_] Visitors.reduce as super
    method zero = (KB.empty, KB.empty)
    method plus (a, c) (b, d) = (KB.union a b, KB.union c d)

    method! visit_expr () e =
      match e with
      | LVar _ | PVar _ | ALoc _ -> (KB.empty, KB.singleton e)
      | UnOp (LstLen, ((PVar _ | LVar _) as v)) -> (KB.singleton v, KB.empty)
      | _ -> super#visit_expr () e
  end

(** [simple_ins_expr e] returns the list of possible ins
    for a given expression [e] *)
let simple_ins_expr (e : Expr.t) : KB.t list =
  let open Expr in
  let llens, others = simple_ins_expr_collector#visit_expr () e in
  (* List lengths whose variables do not appear elsewhere *)
  let llens = Set.elements (Set.diff llens others) in
  (* Those we can learn by knowing the variable or the list length *)
  let llens = List.map (fun le -> [ le; UnOp (LstLen, le) ]) llens in
  let llen_choices = List_utils.list_product llens in
  let simple_ins =
    match llen_choices with
    | [] -> [ others ]
    | _ ->
        List.map
          (fun llen_choice -> KB.add_seq (List.to_seq llen_choice) others)
          llen_choices
  in
  simple_ins

let outs_expr (kb : KB.t) (base_expr : Expr.t) (e : Expr.t) : outs =
  match KB.mem base_expr kb with
  (* If we don't know the expression, there's nothing we can do *)
  | false -> []
  (* Otherwise, there may be scenarios in which not all ins are required *)
  | true -> learn_expr ~top_level:true kb base_expr e

(** [ins_outs_expr kb e] returns the possible ins and outs of
    the expression [e] given a knowledge base [kb]. The outs
    are provided together with the way they are constructed
    given the ins *)
let ins_outs_expr (kb : KB.t) (base_expr : Expr.t) (e : Expr.t) :
    (KB.t * outs) list =
  let ins = simple_ins_expr e in
  let outs = outs_expr kb base_expr e in
  let learned_outs = KB.of_list (fst (List.split outs)) in
  List.map (fun ins -> (KB.diff ins learned_outs, outs)) ins

let ins_and_outs_from_lists (kb : KB.t) (lei : Expr.t list) (leo : Expr.t list)
    =
  L.(
    verbose (fun m ->
        m "Ins_and_outs_from_lists:\nIns: %a\nOuts: %a"
          Fmt.(brackets (list ~sep:semi Expr.pp))
          lei
          Fmt.(brackets (list ~sep:semi Expr.pp))
          leo));
  let ins = List.map simple_ins_expr lei in
  let ins = List_utils.list_product ins in
  let ins = List.map (List.fold_left KB.union KB.empty) ins in
  let ins = List_utils.remove_duplicates ins in
  let ins = List.map minimise_unifiables ins in
  L.(
    verbose (fun m ->
        m "Calculated ins: %a" Fmt.(brackets (list ~sep:semi kb_pp)) ins));
  let outs : outs =
    (* Trick to keep track of parameter order *)
    let leo = List.mapi (fun i u -> (u, Expr.PVar (string_of_int i))) leo in
    (* Outs that are unifiables we learn immediately and add to knowledge base *)
    let kb' = KB.union kb (KB.of_list (snd (List.split leo))) in
    learn_expr_list kb' leo
  in
  match ins with
  | [] -> [ (KB.empty, []) ]
  | _ -> List.map (fun ins -> (ins, outs)) ins

(** [simple_ins_formula pf] returns the list of possible ins
    for a given formula [pf] *)
let rec simple_ins_formula (kb : KB.t) (pf : Formula.t) : KB.t list =
  let f = simple_ins_formula kb in
  match pf with
  | True | False -> []
  | Not pf -> f pf
  (* Conjunction and disjunction are treated the same *)
  | And (pf1, pf2) | Or (pf1, pf2) ->
      let ins_pf1 = f pf1 in
      let ins_pf2 = f pf2 in
      let ins = List_utils.cross_product ins_pf1 ins_pf2 KB.union in
      let ins = List_utils.remove_duplicates ins in
      List.map minimise_unifiables ins
  (* Relational formulae are all treated the same *)
  | Eq (e1, e2)
  | ILess (e1, e2)
  | ILessEq (e1, e2)
  | FLess (e1, e2)
  | FLessEq (e1, e2)
  | StrLess (e1, e2)
  | SetMem (e1, e2)
  | SetSub (e1, e2) ->
      let ins_e1 = simple_ins_expr e1 in
      let ins_e2 = simple_ins_expr e2 in
      let ins = List_utils.list_product [ ins_e1; ins_e2 ] in
      let ins =
        List.map
          (fun ins ->
            List.fold_left (fun kb_ac kb -> KB.union kb_ac kb) KB.empty ins)
          ins
      in
      let ins = List_utils.remove_duplicates ins in
      List.map minimise_unifiables ins
  (* Forall must exclude the binders *)
  | ForAll (binders, pf) ->
      let binders =
        List.fold_left
          (fun acc (b, _) -> KB.add (Expr.LVar b) acc)
          KB.empty binders
      in
      let ins_pf = f pf in
      let ins = List.map (fun ins -> KB.diff ins binders) ins_pf in
      List.map minimise_unifiables ins

(** [ins_outs_formula kb pf] returns a list of possible ins-outs pairs
    for a given formula [pf] under a given knowledge base [kb] *)
let ins_outs_formula (kb : KB.t) (pf : Formula.t) : (KB.t * outs) list =
  let default_ins = simple_ins_formula kb pf in
  let default_result : (KB.t * outs) list =
    List.map (fun ins -> (ins, [])) default_ins
  in
  match pf with
  | Eq (e1, e2) -> (
      L.verbose (fun fmt -> fmt "IO Equality: %a" Formula.pp pf);
      L.verbose (fun fmt ->
          fmt "Ins: %a" Fmt.(brackets (list ~sep:semi kb_pp)) default_ins);
      L.verbose (fun fmt -> fmt "KB: %a" kb_pp kb);
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      L.verbose (fun fmt -> fmt "Known left: %b\tKnown right: %b" ike1 ike2);
      match (ike1, ike2) with
      (* Cannot progress if both sides are known *)
      | false, false | true, true -> default_result
      (* But maybe can if one is not known *)
      | _ ->
          (* Understand which side is known and which is unknown *)
          let ke, ue =
            match ike1 with
            | true -> (e1, e2)
            | false -> (e2, e1)
          in
          (* Try to learn outs from the other side *)
          let learned_outs = learn_expr kb ke ue in
          let outs = KB.of_list (fst (List.split learned_outs)) in
          (* Take away the learnable outs from the ins *)
          let ins = List.map (fun ins -> KB.diff ins outs) default_ins in
          let result = List.map (fun ins -> (ins, learned_outs)) ins in
          L.verbose (fun fmt ->
              fmt "Result: %a"
                Fmt.(
                  brackets
                    (list ~sep:semi (parens (pair ~sep:comma kb_pp outs_pp))))
                result);
          result)
  | And _ ->
      raise
        (Failure
           (Format.asprintf "ins_outs_formula: Should have been reduced: %a"
              Formula.pp pf))
  | _ -> default_result

(** [ins_outs_assertion kb a] returns a list of possible ins-outs pairs
    for a given assertion [a] under a given knowledge base [kb] *)
let ins_outs_assertion
    (pred_ins : (string, int list) Hashtbl.t)
    (kb : KB.t)
    (asrt : Asrt.t) : (KB.t * outs) list =
  let get_pred_ins name =
    match Hashtbl.find_opt pred_ins name with
    | None -> raise (Failure ("ins_outs_assertion. Unknown Predicate: " ^ name))
    | Some ins -> ins
  in
  match (asrt : Asrt.t) with
  | Pure form -> ins_outs_formula kb form
  | GA (_, lie, loe) -> ins_and_outs_from_lists kb lie loe
  | Pred (p_name, args) ->
      let p_ins = get_pred_ins p_name in
      let _, lie, loe =
        List.fold_left
          (fun (i, lie, loe) arg ->
            if List.mem i p_ins then (i + 1, lie @ [ arg ], loe)
            else (i + 1, lie, loe @ [ arg ]))
          (0, [], []) args
      in
      ins_and_outs_from_lists kb lie loe
  (* The types assertion has no outs and requires all ins *)
  | Types [ (e, _) ] ->
      let ins = simple_ins_expr e in
      List.map (fun ins -> (ins, [])) ins
  | _ ->
      raise (Failure "Impossible: non-simple assertion in ins_outs_assertion.")

let rec collect_simple_asrts (a : Asrt.t) : Asrt.t list =
  let f = collect_simple_asrts in
  match a with
  | Pure True | Emp -> []
  | Pure (And (f1, f2)) -> f (Pure f1) @ f (Pure f2)
  | Pure _ | Pred _ | GA _ -> [ a ]
  | Types _ -> (
      let a = Reduction.reduce_assertion a in
      match a with
      | Types les -> List.map (fun e -> Asrt.Types [ e ]) les
      | _ -> f a)
  | Star (a1, a2) -> f a1 @ f a2

let s_init (kb : KB.t) (preds : (string, int list) Hashtbl.t) (a : Asrt.t) :
    (pt, Asrt.t list) result =
  let prioritise (la : Asrt.t list) = List.sort Asrt.prioritise la in

  L.verbose (fun m -> m "Entering s-init on: %a\n\nKB: %a\n" Asrt.pp a kb_pp kb);

  let simple_asrts = collect_simple_asrts a in
  let simple_asrts =
    if List.mem (Asrt.Pure False) simple_asrts then [ Asrt.Pure False ]
    else simple_asrts
  in
  let separating, overlapping =
    List.partition
      (function
        | Asrt.Pred _ | Asrt.GA _ -> true
        | _ -> false)
      simple_asrts
  in
  let overlapping = List.sort_uniq Stdlib.compare overlapping in
  let simple_asrts = prioritise (separating @ overlapping) in
  let simple_asrts_io = Array.of_list simple_asrts in

  (* Check if the assertion at index i can be added to the unification
     plan - its ins need to be contained in the current known lvars *)
  let visit_asrt (kb : KB.t) (i : int) : step list =
    (* Get assertion ins and outs *)
    let a = simple_asrts_io.(i) in
    let ios = ins_outs_assertion preds kb a in
    (* Check if any ins are fully known *)
    let act_ios = List.filter (fun (ins, _) -> KB.subset ins kb) ios in
    (* And produce the appropriate outs *)
    List.map (fun (_, outs) -> (a, outs)) act_ios
  in

  (* Attempt to find an assertion in a given list that can be added
     to the unification plan *)
  let rec visit_asrt_lst
      (kb : KB.t)
      (indexes : SI.t)
      (visited_indexes : int list) : (SI.t * step list) option =
    if indexes = SI.empty then None
    else
      let i = SI.min_elt indexes in
      let rest_indexes = SI.remove i indexes in
      match visit_asrt kb i with
      | [] -> visit_asrt_lst kb rest_indexes (i :: visited_indexes)
      | ret -> Some (SI.union (SI.of_list visited_indexes) rest_indexes, ret)
  in

  let rec search (up_search_states : up_search_state list) :
      (pt, Asrt.t list) result =
    match up_search_states with
    | [] ->
        raise
          (Failure
             "UP: Should not happen: unification plan creation called with no \
              starting state.")
    | (up, unchecked, _) :: _ when unchecked = SI.empty ->
        L.verbose (fun m -> m "Successfully created UP.");
        Ok (List.rev up)
    | (up, unchecked, kb) :: rest -> (
        L.verbose (fun m ->
            m
              "KNOWN: @[%a@].@\n\
               @[<v 2>CUR UP:@\n\
               %a@]@\n\
               TO VISIT: @[%a@]@\n\
               @[%a@]"
              kb_pp kb pt_pp up
              Fmt.(iter ~sep:comma SI.iter int)
              unchecked
              Fmt.(
                iter ~sep:(any "@\n") SI.iter (fun f i ->
                    Asrt.full_pp f simple_asrts_io.(i)))
              unchecked);

        match visit_asrt_lst kb unchecked [] with
        | None ->
            L.verbose (fun m -> m "No assertions left to visit.");
            if rest = [] then (
              L.verbose (fun m ->
                  m "Missing assertions: %a"
                    Fmt.(list ~sep:comma int)
                    (SI.elements unchecked));
              L.verbose (fun m -> m "Missing variables:");
              let unchckd =
                List.map
                  (fun i ->
                    match ins_outs_assertion preds kb simple_asrts_io.(i) with
                    | (ins, _) :: _ -> ins
                    | _ ->
                        let message =
                          Fmt.str
                            "s_init: guaranteed by construction. While \
                             handling :\n\
                             %a"
                            Asrt.pp a
                        in
                        raise (Exceptions.Impossible message))
                  (SI.elements unchecked)
              in
              let unchckd =
                List.map
                  (fun u ->
                    KB.diff
                      (KB.filter
                         (fun x ->
                           match x with
                           | LVar x -> is_spec_var_name x
                           | _ -> false)
                         u)
                      kb)
                  unchckd
              in
              let unchckd = List.filter (fun u -> u <> KB.empty) unchckd in
              L.(
                verbose (fun m ->
                    m "\t%s"
                      (String.concat "\n\t"
                         (List.map
                            (fun u ->
                              Format.asprintf "%a"
                                Fmt.(list ~sep:comma Expr.full_pp)
                                (KB.elements u))
                            unchckd))));
              L.verbose (fun m -> m "Unification plan creation failure.");
              let unchecked =
                List.map (fun i -> simple_asrts_io.(i)) (SI.elements unchecked)
              in
              Error unchecked)
            else search rest
        | Some (new_unchecked, ret) ->
            (* L.log L.verbose (lazy "Successfully added more assertions to the UP.");
               L.(verbose (fun m -> m "States to examine: %d" (List.length ret)));
               L.(verbose (fun m -> m "Unchecked remaining: %d" (SI.cardinal new_unchecked))); *)
            let new_search_states =
              List.map
                (fun (a, outs) ->
                  let new_unifiables = fst (List.split outs) in
                  let kb' = KB.union kb (KB.of_list new_unifiables) in
                  ((a, outs) :: up, new_unchecked, kb'))
                ret
            in
            search (new_search_states @ rest))
  in

  let initial_indexes = SI.of_list (List.mapi (fun i _ -> i) simple_asrts) in
  let initial_search_state = ([], initial_indexes, kb) in
  search [ initial_search_state ]

let rec lift_up
    (up : pt)
    (posts : (Flag.t * Asrt.t list) option)
    (hides : string list) : t =
  match up with
  | [] -> Leaf (None, posts, hides)
  | [ p ] -> Leaf (Some p, posts, hides)
  | p :: up' -> Inner (p, [ lift_up up' posts hides ])

let add_up
    (g_up : t)
    (up_post : pt * (Flag.t * Asrt.t list) option)
    (hides : string list) : t =
  match (g_up, up_post) with
  | PhantomInner ups, (up, posts) ->
      PhantomInner (ups @ [ lift_up up posts hides ])
  | _, (up, posts) -> PhantomInner [ g_up; lift_up up posts hides ]

let lift_ups
    (ups :
      (pt
      * ((string * SS.t) option * (Flag.t * Asrt.t list) option)
      * string list)
      list) : t =
  let b =
    List.exists
      (fun (_, (lab, _), _) ->
        match lab with
        | Some _ -> true
        | _ -> false)
      ups
  in
  let ups' = List.map (fun (up, (_, posts), hides) -> (up, posts, hides)) ups in
  if b then
    (* Printf.printf "BUILDING GUP FOR SPEC WITH EXISTENTIALS\n"; *)
    let gups =
      List.map
        (fun (up, (lab, posts), hides) -> (lift_up up posts hides, lab))
        ups
    in
    LabPhantomInner gups
  else
    List.fold_left
      (fun ac (up, posts, hides) -> add_up ac (up, posts) hides)
      (PhantomInner []) ups'

let empty_up = Leaf (None, None, [])

let init
    ?(use_params : bool option)
    (known_unifiables : KB.t)
    (params : KB.t)
    (preds : (string, int list) Hashtbl.t)
    (asrts_posts :
      (Asrt.t
      * ((string * SS.t) option * (Flag.t * Asrt.t list) option * string list))
      list) : (t, Asrt.t list list) result =
  let known_unifiables =
    match use_params with
    | None -> known_unifiables
    | Some _ -> KB.union known_unifiables params
  in

  let ups =
    List.map
      (fun (asrt, (lab, posts, hides)) ->
        let existentials =
          Option.fold
            ~some:(fun (_, existentials) ->
              let existentials =
                List.map (fun x -> Expr.LVar x) (SS.elements existentials)
              in
              KB.of_list existentials)
            ~none:KB.empty lab
        in
        L.verbose (fun m -> m "Known unifiables: %a\n" kb_pp known_unifiables);
        L.verbose (fun m -> m "Existentials: %a\n" kb_pp existentials);
        let known_unifiables = KB.union known_unifiables existentials in
        (s_init known_unifiables preds asrt, (lab, posts, hides)))
      asrts_posts
  in
  let errors, _ =
    List.partition
      (fun (up, _) ->
        match up with
        | Error _ -> true
        | Ok _ -> false)
      ups
  in
  let errors, _ = List.split errors in
  let errors =
    List.map
      (fun x ->
        match x with
        | Error e -> e
        | _ -> raise (Failure "UP: init: Impossible: non-error error"))
      errors
  in

  if errors <> [] then Error errors
  else
    Ok
      (lift_ups
         (List.map
            (fun (up, (lab, posts, hides)) ->
              ( (match up with
                | Ok up -> up
                | Error _ ->
                    raise (Failure "UP: init: Impossible: ok, but error")),
                (lab, posts),
                hides ))
            ups))

let next (up : t) : (t * (string * SS.t) option) list option =
  match up with
  | Leaf _ -> None
  | Inner (_, ups) -> Some (List.map (fun x -> (x, None)) ups)
  | PhantomInner ups ->
      assert (List.length ups > 0);
      Some (List.map (fun x -> (x, None)) ups)
  | LabPhantomInner lab_ups ->
      assert (List.length lab_ups > 0);
      Some lab_ups

let head (up : t) : step option =
  match up with
  | Leaf (Some p, _, _) | Inner (p, _) -> Some p
  | _ -> None

let posts (up : t) : (Flag.t * Asrt.t list) option =
  match up with
  | Leaf (_, posts, _) -> posts
  | _ -> None

let hides (up : t) : string list option =
  match up with
  | Leaf (_, _, hides) -> Some hides
  | _ -> None

let rec pp ft up =
  let open Fmt in
  let pp_lab ft lab =
    let lab, vars = lab in
    pf ft " [%s: @[<h>%a@]]" lab (iter ~sep:comma SS.iter string) vars
  in
  match up with
  | Leaf (ostep, None, hides) ->
      pf ft "Leaf: @[%a@], Posts = NONE, hiding [%a]"
        (option ~none:(any "none") step_pp)
        ostep
        (Fmt.list ~sep:comma Fmt.string)
        hides
  | Leaf (ostep, Some (flag, posts), hides) ->
      pf ft "Leaf: @[%a@] with Flag %a, Posts:@\n  @[%a@], hiding [%a]"
        (option ~none:(any "none") step_pp)
        ostep Flag.pp flag
        (list ~sep:(any "@\n@\n") (hovbox Asrt.pp))
        posts
        (Fmt.list ~sep:comma Fmt.string)
        hides
  | Inner (step, next_ups) ->
      let pp_children ft ch =
        if List.length ch = 1 then pp ft (List.hd ch)
        else
          let pp_one_child ftp (i, up) = pf ftp "Children %d@\n%a" i pp up in
          pf ft "@[<v 2>  %a@]"
            (iter_bindings ~sep:(any "@\n") List.iteri pp_one_child)
            ch
      in
      pf ft "Inner Node: @[%a@] with %d children@\n%a" step_pp step
        (List.length next_ups) pp_children next_ups
  | PhantomInner next_ups ->
      let pp_child ft (i, ch) = pf ft "Children %d@\n%a" i pp ch in
      pf ft "@[<v 2>Phantom Node %d children@ %a@]" (List.length next_ups)
        (iter_bindings ~sep:(any "@\n") List.iteri pp_child)
        next_ups
  | LabPhantomInner next_ups ->
      let pp_child ft (i, (up, lab)) =
        pf ft "Children %d%a@\n%a" i (option pp_lab) lab pp up
      in
      pf ft "@[<v 2>LabPhantom Node %d children@\n%a@]" (List.length next_ups)
        (iter_bindings ~sep:(any "@\n") List.iteri pp_child)
        next_ups

let init_specs (preds : (string, int list) Hashtbl.t) (specs : Spec.t list) :
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
        let params =
          KB.of_list (List.map (fun x -> Expr.PVar x) spec.spec_params)
        in
        let sspecs :
            (Asrt.t
            * ((string * SS.t) option
              * (Flag.t * Asrt.t list) option
              * string list))
            list =
          List.mapi
            (fun i (sspec : Spec.st) ->
              L.verbose (fun m ->
                  m "lab of sspec %d: @[<h>%a@]" i
                    Fmt.(
                      option
                        (brackets
                           (pair ~sep:(any ": ") string (list ~sep:comma string))))
                    sspec.ss_label);
              ( sspec.ss_pre,
                ( Spec.label_vars_to_set sspec.ss_label,
                  Some (sspec.ss_flag, sspec.ss_posts),
                  [] ) ))
            spec.spec_sspecs
        in

        let up = init ~use_params:true KB.empty params preds sspecs in
        match up with
        | Error err ->
            raise (UPError (UPSpec (spec.spec_name, err)))
            (* let msg = Printf.sprintf "Specification of %s cannot be turned into UP. %s"
                 spec.name (Spec.str spec) in
               L.fail msg *)
        | Ok up ->
            L.(
              verbose (fun m ->
                  m "Successfully created UP of specification of %s"
                    spec.spec_name));
            Hashtbl.replace u_specs spec.spec_name { spec; up })
      specs;
    Ok u_specs
  with UPError e -> Error e

let init_lemmas (preds : (string, int list) Hashtbl.t) (lemmas : Lemma.t list) :
    ((string, lemma) Hashtbl.t, up_err_t) result =
  let u_lemmas = Hashtbl.create Config.medium_tbl_size in
  try
    List.iter
      (fun (lemma : Lemma.t) ->
        let params =
          KB.of_list (List.map (fun x -> Expr.PVar x) lemma.lemma_params)
        in
        let sspecs :
            (Asrt.t
            * ((string * SS.t) option
              * (Flag.t * Asrt.t list) option
              * string list))
            list =
          List.map
            (fun spec ->
              ( spec.Lemma.lemma_hyp,
                (None, Some (Flag.Normal, spec.lemma_concs), []) ))
            lemma.lemma_specs
        in
        let up = init ~use_params:true KB.empty params preds sspecs in
        match up with
        | Error err ->
            raise (UPError (UPLemma (lemma.lemma_name, err)))
            (* let msg = Printf.sprintf "Lemma %s cannot be turned into UP" lemma.name in
               L.fail msg *)
        | Ok up ->
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

  let pred_ins =
    Hashtbl.fold
      (fun name (pred : Pred.t) pred_ins ->
        Hashtbl.add pred_ins name pred.pred_ins;
        pred_ins)
      preds
      (Hashtbl.create Config.medium_tbl_size)
  in

  try
    Hashtbl.iter
      (fun name (pred : Pred.t) ->
        L.(verbose (fun m -> m "Attempting to create UP of predicate %s" name));
        let known_params =
          KB.of_list
            (List.map
               (fun i ->
                 let param, _ = List.nth pred.pred_params i in
                 Expr.PVar param)
               pred.pred_ins)
        in

        let defs =
          List.map
            (fun (lab, def, hides) ->
              let lab' =
                Option.map (fun (s, vars) -> (s, SS.of_list vars)) lab
              in
              (def, (lab', None, hides)))
            pred.pred_definitions
        in

        match init known_params KB.empty pred_ins defs with
        | Error err -> raise (UPError (UPPred (pred.pred_name, err)))
        (* let msg = Printf.sprintf "Predicate definition of %s cannot be turned into UP" pred.name in
           L.fail msg *)
        | Ok up ->
            L.verbose (fun m ->
                m "Successfully created UP of predicate %s:\n%a" name pp up);
            Hashtbl.replace u_preds name { pred; up })
      preds;
    Ok u_preds
  with UPError e -> Error e

let init_prog ?preds_tbl (prog : ('a, int) Prog.t) : (prog, up_err_t) result =
  let all_specs : Spec.t list = Prog.get_specs prog in

  let lemmas : Lemma.t list = Prog.get_lemmas prog in
  let preds_tbl : ((string, pred) Hashtbl.t, up_err_t) result =
    match preds_tbl with
    | Some preds_tbl -> Ok preds_tbl
    | None -> init_preds prog.preds
  in
  match preds_tbl with
  | Error e -> Error e
  | Ok preds_tbl -> (
      let pred_ins =
        Hashtbl.fold
          (fun name (pred : pred) pred_ins ->
            Hashtbl.add pred_ins name pred.pred.pred_ins;
            pred_ins)
          preds_tbl
          (Hashtbl.create Config.medium_tbl_size)
      in

      let lemmas_tbl : ((string, lemma) Hashtbl.t, up_err_t) result =
        L.verbose (fun fmt -> fmt "Calculating UPs for lemmas");
        init_lemmas pred_ins lemmas
      in
      match lemmas_tbl with
      | Error e -> Error e
      | Ok lemmas_tbl -> (
          let specs_tbl : ((string, spec) Hashtbl.t, up_err_t) result =
            init_specs pred_ins all_specs
          in
          match specs_tbl with
          | Error e -> Error e
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
                }))

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
  | None -> Error ()

let rec pp_asrt
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (a : Asrt.t) =
  let pp_asrt = pp_asrt ?preds_printer ~preds in
  match a with
  | Star (a1, a2) -> Fmt.pf fmt "%a *@ %a" pp_asrt a1 pp_asrt a2
  | Pred (name, args) -> (
      match preds_printer with
      | Some pp_pred -> (Fmt.hbox pp_pred) fmt (name, args)
      | None -> (
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
          with _ -> Asrt.pp fmt a))
  | a -> Asrt.pp fmt a

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
  let params = KB.of_list (List.map (fun x -> Expr.PVar x) spec.spec_params) in
  let proc =
    match Prog.get_proc prog.prog spec.spec_name with
    | None -> raise (Failure "DEATH. ADDING SPEC TO UNKNOWN PROC!")
    | Some proc -> proc
  in

  let pred_ins =
    Hashtbl.fold
      (fun name (pred : pred) pred_ins ->
        Hashtbl.add pred_ins name pred.pred.pred_ins;
        pred_ins)
      prog.preds
      (Hashtbl.create Config.medium_tbl_size)
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
        (fun (x, y) -> (x, (None, y, [])))
        (posts_from_sspecs spec.spec_sspecs)
    in
    let up = init ~use_params:true KB.empty params pred_ins posts in
    match up with
    | Error _ ->
        let msg =
          Fmt.str "Spec addition: specification of %s cannot be turned into UP."
            spec.spec_name
        in
        L.fail msg
    | Ok up ->
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
        (fun (asrt, posts) -> (asrt, s_init params pred_ins asrt, posts))
        (posts_from_sspecs sspecs)
    in
    let new_gup =
      List.fold_left
        (fun g_up (pre, pre_up, posts) ->
          match pre_up with
          | Error _ ->
              L.verbose (fun m ->
                  m
                    "WARNING!!! IT IS NOT POSSIBLE TO BUILD UP FOR INFERRED \
                     SPEC of %s!PRE:@\n\
                     @[%a@]@\n"
                    uspec.spec.spec_name Asrt.pp pre);
              (* Printf.printf "%s" msg; *)
              g_up
          | Ok pre_up -> add_up g_up (pre_up, posts) [])
        uspec.up ups
    in
    let uspec' : spec = { spec; up = new_gup } in
    uspec'
  in

  let new_uspec =
    match Hashtbl.find_opt prog.specs spec.spec_name with
    | None -> new_uspec spec
    | Some uspec -> extend_spec uspec spec.spec_sspecs
  in

  Hashtbl.replace prog.specs spec.spec_name new_uspec;
  Hashtbl.replace prog.prog.procs spec.spec_name
    { proc with proc_spec = Some new_uspec.spec }

let remove_spec (prog : prog) spec_name =
  let proc = Prog.get_proc_exn prog.prog spec_name in
  Hashtbl.replace prog.prog.procs spec_name { proc with proc_spec = None };
  Hashtbl.remove prog.specs spec_name

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
