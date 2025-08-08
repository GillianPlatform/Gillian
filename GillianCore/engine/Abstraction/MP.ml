open Containers
module L = Logging

(** The [outs] type represents a list of learned outs, together
    with (optionally) the way of constructing them *)
type outs = (Expr.t * Expr.t) list [@@deriving yojson, eq]

let outs_pp =
  Fmt.(
    list ~sep:(Fmt.any "; ") (parens (pair ~sep:(Fmt.any ", ") Expr.pp Expr.pp)))

(** The [mp_step] type represents a matching plan step,
    consisting of an assertion together with the possible
    learned outs *)
type step = Asrt.atom * outs [@@deriving yojson, eq]

let pp_step = Fmt.pair ~sep:(Fmt.any ", ") Asrt.pp_atom_full outs_pp
let pp_step_list = Fmt.Dump.list pp_step

type label = string * SS.t [@@deriving eq, yojson]

let pp_label ft (lab, ss) =
  Fmt.pf ft "LABEL(%s): %a" lab (Fmt.Dump.iter SS.iter Fmt.nop Fmt.string) ss

type post = Flag.t * Asrt.t list [@@deriving eq, yojson]

let pp_post ft (flag, asrts) =
  Fmt.pf ft "%a: %a" Flag.pp flag Fmt.(list ~sep:comma Asrt.pp) asrts

(** At a high level, a matching plan is a tree of assertions.
     *)
type t =
  | Choice of t * t
  | ConsumeStep of step * t
  | LabelStep of label * t
      (** Labels provide additional existentials to be bound manually by the user *)
  | Finished of post option
      (** The optional assertion corresponds to some post-condition that may be produced after successfuly matching.
          For example, a matching plan corresponding to a set of specifications will contain leaves that are respectively anntated with the corresponding post. *)
[@@deriving yojson]

(* type t =
     | Leaf of step option * (Flag.t * Asrt.t list) option
         (** Final node and associated post-condition *)
     | Inner of step * t list
     | PhantomInner of t list
     | LabPhantomInner of (t * (string * SS.t) option) list
   [@@deriving yojson] *)

type 'a with_mp = { mp : t; data : 'a }
type spec = Spec.t with_mp
type lemma = Lemma.t with_mp
type pred = { pred : Pred.t; def_mp : t; guard_mp : t option }

type 'annot prog = {
  preds : (string, pred) Hashtbl.t;
  specs : (string, spec) Hashtbl.t;
  lemmas : (string, lemma) Hashtbl.t;
  coverage : (string * int, int) Hashtbl.t;
  prog : ('annot, int) Prog.t;
}

(** Knowledge bases *)
module KB = Expr.Set

let kb_pp = Fmt.(braces (iter ~sep:comma KB.iter Expr.full_pp))

type preds_tbl_t = (string, pred) Hashtbl.t

type err_ =
  | MPSpec of string * Asrt.t list
  | MPPred of string * Asrt.t list
  | MPLemma of string * Asrt.t list
  | MPAssert of Asrt.t * Asrt.t list
  | MPInvariant of Asrt.t * Asrt.t list
[@@deriving show]

let pp_err_ fmt = function
  | MPSpec (name, asrts) ->
      Fmt.pf fmt "MP failed for spec %s:@\n@[%a@]" name
        Fmt.(list ~sep:(any ", ") Asrt.pp)
        asrts
  | MPPred (name, asrts) ->
      Fmt.pf fmt "MP failed for predicate %s:@\n@[%a@]" name
        Fmt.(list ~sep:(any ", ") Asrt.pp)
        asrts
  | MPLemma (name, asrts) ->
      Fmt.pf fmt "MP failed for lemma %s:@\n@[%a@]" name
        Fmt.(list ~sep:(any ", ") Asrt.pp)
        asrts
  | MPAssert (asrt, asrts) ->
      Fmt.pf fmt "MP failed for assertion %a:@\n@[%a@]" Asrt.pp asrt
        Fmt.(list ~sep:(any ", ") Asrt.pp)
        asrts
  | MPInvariant (asrt, asrts) ->
      Fmt.pf fmt "MP failed for invariant %a:@\n@[%a@]" Asrt.pp asrt
        Fmt.(list ~sep:(any ", ") Asrt.pp)
        asrts

type err = err_ Location.located

let pp_err fmt (e, _) = pp_err_ fmt e
let show_err (e, _) = show_err_ e

exception MPError of err

let is_var (e : Expr.t) : bool =
  match e with
  | PVar _ | LVar _ -> true
  | _ -> false

(** List lengths are not required if their variables are *)
let minimise_matchables (kb : KB.t) : KB.t =
  KB.fold
    (fun u ac ->
      match u with
      | UnOp (LstLen, e) -> (
          match KB.mem e kb with
          | true -> ac
          | false -> KB.add u ac)
      | _ -> KB.add u ac)
    kb KB.empty

(** [missing kb e] returns a list of matchables that are missing
    in order for the expression [e] to be known under knowledge
    base [kb]. The expression is required to have previously been
    fully reduced. *)
let rec missing_expr (kb : KB.t) (e : Expr.t) : KB.t list =
  let f' = missing_expr in
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
  if KB.mem e kb then [ KB.empty ]
  else
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
                        "missing_expr: Should have been reduced: %a" Expr.pp e1))
            in
            match KB.mem e1 kb with
            | true -> [ KB.empty ]
            (* List lengths are matchables *)
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
        result
    | Exists (bt, e) | ForAll (bt, e) ->
        let kb' =
          KB.add_seq (List.to_seq bt |> Seq.map (fun (x, _) -> Expr.LVar x)) kb
        in
        f' kb' e

(** [is_known kb e] returns true if the expression [e] is known
    under knowledge base [kb], and false otherwise *)
let is_known_expr (kb : KB.t) (e : Expr.t) : bool =
  missing_expr kb e = [ KB.empty ]

(** [learn kb e] tries to learn matchables in the expression [e]
    not known in the knowledge base [kb]. It returns a list of
    pairs, each of which contains the learned matchable and the
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
  (* Converting int to num is invertible *)
  | UnOp (IntToNum, e') -> f (UnOp (NumToInt, base_expr)) e'
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
      if is_known_expr kb e_length then
        let e_base_expr = Expr.LstSub (base_expr, Expr.zero_i, e_length) in
        let e_outs = f e_base_expr e in
        let kb' : KB.t =
          List.fold_left (fun kb (u, _) -> KB.add u kb) kb e_outs
        in
        let rest = Expr.NOp (LstCat, rest) in
        let rest_base_expr =
          Expr.LstSub
            (base_expr, e_length, Expr.Infix.(overall_length - e_length))
        in
        e_outs @ learn_expr kb' rest_base_expr rest
      else
        match rest with
        | [ e' ] ->
            let e'_length = Expr.list_length e' in
            if is_known_expr kb e'_length then
              let e'_base_expr =
                Expr.LstSub
                  (base_expr, Expr.Infix.(overall_length - e'_length), e'_length)
              in
              let e'_outs = f e'_base_expr e' in
              let kb' : KB.t =
                List.fold_left (fun kb (u, _) -> KB.add u kb) kb e'_outs
              in
              let rest_base_expr =
                Expr.LstSub
                  ( base_expr,
                    Expr.zero_i,
                    Expr.Infix.(overall_length - e'_length) )
              in
              e'_outs @ learn_expr kb' rest_base_expr e
            else []
        | _ -> [])
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
  | BinOp (e1, ITimes, e2) -> (
      let ike1, ike2 = (is_known_expr kb e1, is_known_expr kb e2) in
      match (ike1, ike2) with
      | true, true | false, false -> []
      | true, false -> f (BinOp (base_expr, IDiv, e1)) e2
      | false, true -> f (BinOp (base_expr, IDiv, e2)) e1)
  (* TODO: Finish the remaining invertible binary operators *)
  | BinOp _ -> []
  (* Can we learn anything from Exists? *)
  | Exists _ | ForAll _ -> []

and learn_expr_list (kb : KB.t) (le : (Expr.t * Expr.t) list) =
  (* L.(verbose (fun m -> m "Entering learn_expr_list: \nKB: %a\nList: %a" kb_pp kb Fmt.(brackets (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.pp)))) le)); *)
  (* Learn matchables per-element *)
  let learned_exprs =
    List.map
      (fun (e, base_expr) -> ((e, base_expr), learn_expr kb base_expr e))
      le
  in
  (* Filter learned matchables *)
  let learned, not_learned =
    List.partition (fun (_, learned) -> learned <> []) learned_exprs
  in
  match learned with
  (* We have learned nothing, therefore we stop *)
  | [] -> []
  | _ ->
      (* Get all learned matchables in order from left to right *)
      let learned = List.concat (snd (List.split learned)) in
      (* Add learned matchables to knowledge base *)
      let new_kb = List.fold_left (fun kb (e, _) -> KB.add e kb) kb learned in
      (* Recover the not-yet-learned bindings *)
      let not_learned = fst (List.split not_learned) in
      (* and try to learn more *)
      learned @ learn_expr_list new_kb not_learned

let simple_ins_expr_collector =
  object (self)
    inherit [_] Visitors.reduce as super
    method zero = (KB.empty, KB.empty)
    method plus (a, c) (b, d) = (KB.union a b, KB.union c d)

    method! visit_expr exclude e =
      match e with
      | (LVar s | PVar s | ALoc s) when not (SS.mem s exclude) ->
          (KB.empty, KB.singleton e)
      | UnOp (LstLen, ((PVar s | LVar s) as v)) when not (SS.mem s exclude) ->
          (KB.singleton v, KB.empty)
      | Exists (bt, e) | ForAll (bt, e) ->
          let exclude =
            List.fold_left (fun acc (x, _) -> SS.add x acc) exclude bt
          in
          self#visit_expr exclude e
      | _ -> super#visit_expr exclude e
  end

(** [simple_ins_expr e] returns the list of possible ins
    for a given expression [e] *)
let simple_ins_expr (e : Expr.t) : KB.t list =
  let open Expr in
  let llens, others = simple_ins_expr_collector#visit_expr SS.empty e in
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
  let ins = List.map minimise_matchables ins in
  L.(
    verbose (fun m ->
        m "Calculated ins: %a" Fmt.(brackets (list ~sep:semi kb_pp)) ins));
  let outs : outs =
    (* Trick to keep track of parameter order *)
    let leo = List.mapi (fun i u -> (u, Expr.PVar (string_of_int i))) leo in
    (* Outs that are matchables we learn immediately and add to knowledge base *)
    let kb' = KB.union kb (KB.of_list (snd (List.split leo))) in
    learn_expr_list kb' leo
  in
  match ins with
  | [] -> [ (KB.empty, []) ]
  | _ -> List.map (fun ins -> (ins, outs)) ins

(** [simple_ins_formula pf] returns the list of possible ins
    for a given formula [pf] *)
let rec simple_ins_formula (kb : KB.t) (pf : Expr.t) : KB.t list =
  let f = simple_ins_formula kb in
  match pf with
  | UnOp (Not, pf) -> f pf
  (* Conjunction and disjunction are treated the same *)
  | BinOp (pf1, And, pf2) | BinOp (pf1, Or, pf2) ->
      let ins_pf1 = f pf1 in
      let ins_pf2 = f pf2 in
      let ins = List_utils.cross_product ins_pf1 ins_pf2 KB.union in
      let ins = List_utils.remove_duplicates ins in
      List.map minimise_matchables ins
  | BinOp (f1, Impl, f2) ->
      simple_ins_formula kb (BinOp (UnOp (Not, f1), Or, f2))
  (* Relational formulae are all treated the same *)
  | BinOp (e1, _, e2) ->
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
      List.map minimise_matchables ins
  | UnOp (_, e) ->
      e |> simple_ins_expr |> List_utils.remove_duplicates
      |> List.map minimise_matchables
  (* ForAll/Exists must exclude the binders *)
  | Exists (binders, pf) | ForAll (binders, pf) ->
      let binders =
        List.fold_left
          (fun acc (b, _) -> KB.add (Expr.LVar b) acc)
          KB.empty binders
      in
      let ins_pf = f pf in
      let ins = List.map (fun ins -> KB.diff ins binders) ins_pf in
      List.map minimise_matchables ins
  | Lit _ | PVar _ | LVar _ | ALoc _ | LstSub _ | NOp _ | EList _ | ESet _ -> []

(** [ins_outs_formula kb pf] returns a list of possible ins-outs pairs
    for a given formula [pf] under a given knowledge base [kb] *)
let ins_outs_formula (kb : KB.t) (pf : Expr.t) : (KB.t * outs) list =
  let default_ins = simple_ins_formula kb pf in
  let default_result : (KB.t * outs) list =
    List.map (fun ins -> (ins, [])) default_ins
  in
  match pf with
  | BinOp (e1, Equal, e2) -> (
      L.verbose (fun fmt -> fmt "IO Equality: %a" Expr.pp pf);
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
  | BinOp (_, And, _) ->
      raise
        (Failure
           (Format.asprintf "ins_outs_formula: Should have been reduced: %a"
              Expr.pp pf))
  | _ -> default_result

(** [ins_outs_assertion kb a] returns a list of possible ins-outs pairs
    for a given assertion [a] under a given knowledge base [kb] *)
let ins_outs_assertion
    (pred_ins : (string, int list) Hashtbl.t)
    (kb : KB.t)
    (asrt : Asrt.atom) : (KB.t * outs) list =
  let get_pred_ins name =
    match Hashtbl.find_opt pred_ins name with
    | None -> raise (Failure ("ins_outs_assertion. Unknown Predicate: " ^ name))
    | Some ins -> ins
  in
  match (asrt : Asrt.atom) with
  | Emp -> []
  | Pure form -> ins_outs_formula kb form
  | CorePred (_, lie, loe) -> ins_and_outs_from_lists kb lie loe
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
  | Types _ -> failwith "Impossible: non-atomic types assertion in get_pred_ins"
  | Wand { lhs = _, largs; rhs = rname, rargs } ->
      let r_ins = get_pred_ins rname in
      let _, llie, lloe =
        List.fold_left
          (fun (i, lie, loe) arg ->
            if List.mem i r_ins then (i + 1, arg :: lie, loe)
            else (i + 1, lie, arg :: loe))
          (0, [], []) rargs
      in
      ins_and_outs_from_lists kb (largs @ List.rev llie) lloe

let simplify_asrts ?(sorted = true) a =
  let rec aux (a : Asrt.atom) : Asrt.atom list =
    match a with
    | Pure (Lit (Bool true)) | Emp -> []
    | Pure (BinOp (f1, And, f2)) -> aux (Pure f1) @ aux (Pure f2)
    | Pure _ | Pred _ | CorePred _ | Wand _ -> [ a ]
    | Types _ -> (
        let a = Reduction.reduce_assertion [ a ] in
        match a with
        | [ Types les ] -> List.map (fun e -> Asrt.Types [ e ]) les
        | _ -> List.concat_map aux a)
  in
  let atoms = List.concat_map aux a in
  if List.mem (Asrt.Pure (Lit (Bool false))) atoms then
    [ Asrt.Pure (Lit (Bool false)) ]
  else if not sorted then atoms
  else
    let overlapping, separating = List.partition Asrt.is_pure_asrt atoms in
    let overlapping = List.sort_uniq Stdlib.compare overlapping in
    List.sort Asrt.prioritise (separating @ overlapping)

let s_init_atoms ~preds kb atoms =
  let step_of_atom ~kb atom =
    ins_outs_assertion preds kb atom
    |> List.find_map (fun (ins, outs) ->
           if KB.subset ins kb then Some (atom, outs) else None)
  in
  let rec search current kb rest =
    L.verbose (fun m ->
        m "KNOWN: @[%a@].@\n@[<v 2>CUR MP:@\n%a@]@\nTO VISIT: @[%a@]" kb_pp kb
          pp_step_list current
          (Fmt.list ~sep:(Fmt.any "@\n") Asrt.pp_atom_full)
          rest);
    match rest with
    | [] ->
        let result = List.rev current in
        L.verbose (fun m -> m "Successfully created MP.");
        Ok result
    | rest -> (
        match List_utils.pop_map (step_of_atom ~kb) rest with
        | None ->
            L.verbose (fun m -> m "No assertions left to visit.");
            Error rest
        | Some (((_, outs) as step), rest) ->
            let learned = List.to_seq outs |> Seq.map fst |> KB.of_seq in
            let kb = KB.union kb learned in
            search (step :: current) kb rest)
  in
  search [] kb atoms

let s_init ~(preds : (string, int list) Hashtbl.t) (kb : KB.t) (a : Asrt.t) :
    (step list, Asrt.t) result =
  L.verbose (fun m -> m "Entering s-init on: %a\n\nKB: %a\n" Asrt.pp a kb_pp kb);
  let atoms = simplify_asrts a in
  s_init_atoms ~preds kb atoms

let of_step_list ?post ?label (steps : step list) : t =
  let rec consume_steps = function
    | [] -> Finished post
    | p :: steps' -> ConsumeStep (p, consume_steps steps')
  in
  let consume_steps = consume_steps steps in
  match label with
  | None -> consume_steps
  | Some label -> LabelStep (label, consume_steps)

(** Adds a linear matching plan (without choices) to a possibly non-linear one.
    Will be under-optimised if a non-linear matching plan is passed on the lhs.
    We try to preserve order in which the assertions are added, as to maintain priorities set by the user.
    In the future, we could provide an option that automatically prioritizes the shortest MP on the left-hand side. *)
let rec add_linear_mp (current_mp : t) (mp_to_add : t) : t =
  let rec merge_into_left left right =
    match (left, right) with
    | ConsumeStep (stepl, restl), ConsumeStep (stepr, restr)
      when equal_step stepl stepr ->
        Some (ConsumeStep (stepl, add_linear_mp restl restr))
    | LabelStep (labell, restl), LabelStep (labelr, restr)
      when equal_label labell labelr ->
        Some (LabelStep (labell, add_linear_mp restl restr))
    | Finished postl, Finished postr when (Option.equal equal_post) postl postr
      -> Some current_mp
    | Choice (cl, cr), mp_to_add -> (
        (* We try to add the merge MP to the right choice*)
        match merge_into_left cr mp_to_add with
        | Some merged -> Some (Choice (cl, merged))
        | None -> (
            (* If it fails we try to add it to the left*)
            match merge_into_left cl mp_to_add with
            | Some merged -> Some (Choice (merged, cr))
            | None ->
                (* If this also fails, we put a choice between everything *)
                Some (Choice (cl, Choice (cr, mp_to_add)))))
    | _ -> None
  in
  match merge_into_left current_mp mp_to_add with
  | Some x -> x
  | None -> Choice (current_mp, mp_to_add)

(** This function builds a general (slightly optimised by selecting common roots) matching plan
    once the step list for each case has been decided. *)
let build_mp (cases : (step list * label option * post option) list) : t =
  let linear_mps =
    List.map (fun (steps, label, post) -> of_step_list ?label ?post steps) cases
  in
  match linear_mps with
  | [] -> Finished None
  | a :: r -> List.fold_left add_linear_mp a r

let init
    ?(use_params : bool option)
    (known_matchables : KB.t)
    (params : KB.t)
    (preds : (string, int list) Hashtbl.t)
    (asrts_posts :
      (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option)) list)
    : (t, Asrt.atom list list) result =
  let known_matchables =
    match use_params with
    | None -> known_matchables
    | Some _ -> KB.union known_matchables params
  in

  let mps =
    List.map
      (fun (asrt, (lab, posts)) ->
        let existentials =
          Option.fold
            ~some:(fun (_, existentials) ->
              let existentials =
                List.map (fun x -> Expr.LVar x) (SS.elements existentials)
              in
              KB.of_list existentials)
            ~none:KB.empty lab
        in
        L.verbose (fun m -> m "Known matchables: %a\n" kb_pp known_matchables);
        L.verbose (fun m -> m "Existentials: %a\n" kb_pp existentials);
        let known_matchables = KB.union known_matchables existentials in
        (s_init ~preds known_matchables asrt, lab, posts))
      asrts_posts
  in
  let successes, errors =
    List.partition_map
      (fun (mp, lab, post) ->
        match mp with
        | Error x -> Right x
        | Ok mp -> Left (mp, lab, post))
      mps
  in
  match (successes, errors) with
  | _, _ :: _ -> Error errors
  | successes, [] -> Ok (build_mp successes)

let pp ft mp =
  let open Fmt in
  let rec aux ~prefix mp =
    match mp with
    | Choice (left, right) ->
        string ft prefix;
        pf ft "CHOICE@\n";
        string ft prefix;
        string ft "├── ";
        aux ~prefix:(prefix ^ "│   ") left;
        string ft prefix;
        string ft "└──";
        aux ~prefix:(prefix ^ "   ") right
    | ConsumeStep (step, t) ->
        pf ft "@[<h>· %a@]@\n" pp_step step;
        string ft prefix;
        aux ~prefix t
    | LabelStep (label, t) ->
        pf ft "· %a@\n" pp_label label;
        string ft prefix;
        aux ~prefix t
    | Finished post ->
        pf ft "===FINISHED=== POST: %a@\n%s@\n" (Fmt.Dump.option pp_post) post
          prefix
  in
  aux ~prefix:"" mp

let init_specs (preds : (string, int list) Hashtbl.t) (specs : Spec.t list) :
    ((string, spec) Hashtbl.t, err) result =
  let u_specs = Hashtbl.create Config.medium_tbl_size in
  try
    List.iter
      (fun (spec : Spec.t) ->
        L.(
          verbose (fun m ->
              m "Attempting to create MP for a spec of %s : %d specs"
                spec.spec_name
                (List.length spec.spec_sspecs)));
        let params =
          KB.of_list (List.map (fun x -> Expr.PVar x) spec.spec_params)
        in
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
                           (pair ~sep:(any ": ") string (list ~sep:comma string))))
                    sspec.ss_label);
              ( fst sspec.ss_pre,
                ( Spec.label_vars_to_set sspec.ss_label,
                  Some (sspec.ss_flag, List.map fst sspec.ss_posts) ) ))
            spec.spec_sspecs
        in

        let mp = init ~use_params:true KB.empty params preds sspecs in
        match mp with
        | Error err ->
            raise (MPError (MPSpec (spec.spec_name, err), spec.spec_location))
            (* let msg = Printf.sprintf "Specification of %s cannot be turned into MP. %s"
                 spec.name (Spec.str spec) in
               L.fail msg *)
        | Ok mp ->
            L.(
              verbose (fun m ->
                  m "Successfully created MP of specification of %s"
                    spec.spec_name));
            L.tmi (fun m -> m "%a" pp mp);
            Hashtbl.replace u_specs spec.spec_name { data = spec; mp })
      specs;
    Ok u_specs
  with MPError e -> Error e

let init_lemmas (preds : (string, int list) Hashtbl.t) (lemmas : Lemma.t list) :
    ((string, lemma) Hashtbl.t, err) result =
  let u_lemmas = Hashtbl.create Config.medium_tbl_size in
  try
    List.iter
      (fun (lemma : Lemma.t) ->
        let params =
          KB.of_list (List.map (fun x -> Expr.PVar x) lemma.lemma_params)
        in
        let sspecs :
            (Asrt.t * ((string * SS.t) option * (Flag.t * Asrt.t list) option))
            list =
          List.map
            (fun spec ->
              ( fst spec.Lemma.lemma_hyp,
                (None, Some (Flag.Normal, List.map fst spec.lemma_concs)) ))
            lemma.lemma_specs
        in
        let mp = init ~use_params:true KB.empty params preds sspecs in
        match mp with
        | Error err ->
            raise
              (MPError (MPLemma (lemma.lemma_name, err), lemma.lemma_location))
            (* let msg = Printf.sprintf "Lemma %s cannot be turned into MP" lemma.name in
               L.fail msg *)
        | Ok mp ->
            L.(
              verbose (fun m ->
                  m "Successfully created MP of Lemma %s" lemma.lemma_name));
            Hashtbl.replace u_lemmas lemma.lemma_name { data = lemma; mp })
      lemmas;
    Ok u_lemmas
  with MPError e -> Error e

let init_preds (preds : (string, Pred.t) Hashtbl.t) :
    ((string, pred) Hashtbl.t, err) result =
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
        L.(verbose (fun m -> m "Attempting to create MP of predicate %s" name));
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
            (fun (lab, def) ->
              let lab' =
                Option.map (fun (s, vars) -> (s, SS.of_list vars)) lab
              in
              (def, (lab', None)))
            pred.pred_definitions
        in
        let create_or_raise defs =
          match init known_params KB.empty pred_ins defs with
          | Error err ->
              raise (MPError (MPPred (pred.pred_name, err), pred.pred_loc))
          (* let msg = Printf.sprintf "Predicate definition of %s cannot be turned into MP" pred.name in
             L.fail msg *)
          | Ok mp -> mp
        in
        let def_mp = create_or_raise defs in
        L.verbose (fun m ->
            m "Successfully created MP of predicate %s:@\n%a" name pp def_mp);
        let guard_mp =
          Option.map
            (fun guard -> create_or_raise [ (guard, (None, None)) ])
            pred.pred_guard
        in
        Option.iter
          (fun mp ->
            L.verbose (fun m ->
                m "Successfully created MP of predicate's guard %s:@\n%a" name
                  pp mp))
          guard_mp;
        Hashtbl.replace u_preds name { pred; def_mp; guard_mp })
      preds;
    Ok u_preds
  with MPError e -> Error e

let init_prog ?preds_tbl (prog : ('a, int) Prog.t) : 'a prog =
  let res =
    let open Syntaxes.Result in
    let all_specs : Spec.t list = Prog.get_specs prog in
    let lemmas : Lemma.t list = Prog.get_lemmas prog in
    let* preds =
      match preds_tbl with
      | Some preds_tbl -> Ok preds_tbl
      | None -> init_preds prog.preds
    in
    let pred_ins =
      Hashtbl.fold
        (fun name (pred : pred) pred_ins ->
          Hashtbl.add pred_ins name pred.pred.pred_ins;
          pred_ins)
        preds
        (Hashtbl.create Config.medium_tbl_size)
    in
    let* lemmas =
      L.verbose (fun fmt -> fmt "Calculating MPs for lemmas");
      init_lemmas pred_ins lemmas
    in
    let+ specs = init_specs pred_ins all_specs in
    let coverage : (string * int, int) Hashtbl.t =
      Hashtbl.create Config.big_tbl_size
    in
    { prog; specs; preds; lemmas; coverage }
  in
  match res with
  | Ok res -> res
  | Error (e, loc) ->
      let msg =
        Fmt.str "Creation of matching plans failed:@\n %a@\n@?" pp_err_ e
      in
      raise
        (Gillian_result.Exc.analysis_failure ~is_preprocessing:true ?loc msg)

let get_pred_def (pred_defs : preds_tbl_t) (name : string) : pred =
  match Hashtbl.find_opt pred_defs name with
  | Some mp_pred -> mp_pred
  | None -> Fmt.failwith "DEATH. PRED %s NOT DEFINED" name

let init_pred_defs () : preds_tbl_t = Hashtbl.create Config.medium_tbl_size

let get_procs (prog : 'a prog) : ('a, int) Proc.t list =
  Prog.get_procs prog.prog

let get_bispecs (prog : 'a prog) : BiSpec.t list = Prog.get_bispecs prog.prog

let get_lemma (prog : 'a prog) (name : string) : (lemma, unit) result =
  match Hashtbl.find_opt prog.lemmas name with
  | Some lemma -> Ok lemma
  | None -> Error ()

let pp_asrt
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (a : Asrt.t) =
  let pp_atom_asrt fmt = function
    | Asrt.Pred (name, args) -> (
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
    | a -> Asrt.pp_atom fmt a
  in
  Fmt.list ~sep:(Fmt.any " *@ ") pp_atom_asrt fmt a

let pp_sspec
    ?(preds_printer : (Format.formatter -> string * Expr.t list -> unit) option)
    ~(preds : preds_tbl_t)
    (fmt : Format.formatter)
    (sspec : Spec.st) =
  let pp_a = pp_asrt ?preds_printer ~preds in
  Fmt.pf fmt "[[ @[<hv>%a@] ]]@\n[[ @[<hv>%a@] ]]@\n%a" pp_a (fst sspec.ss_pre)
    Fmt.(list ~sep:semi pp_a)
    (List.map fst sspec.ss_posts)
    Flag.pp sspec.ss_flag

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

let add_spec (prog : 'a prog) (spec : Spec.t) : unit =
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
        (fst sspec.ss_pre, Some (sspec.ss_flag, List.map fst sspec.ss_posts)))
      sspecs
  in

  let new_uspec (spec : Spec.t) : spec =
    let posts =
      List.map
        (fun (x, y) -> (x, (None, y)))
        (posts_from_sspecs spec.spec_sspecs)
    in
    let mp = init ~use_params:true KB.empty params pred_ins posts in
    match mp with
    | Error _ ->
        let msg =
          Fmt.str "Spec addition: specification of %s cannot be turned into MP."
            spec.spec_name
        in
        L.fail msg
    | Ok mp ->
        L.(
          verbose (fun m ->
              m "Successfully created MP of specification of %s" spec.spec_name));
        let new_spec : spec = { data = spec; mp } in
        new_spec
  in

  let extend_spec (uspec : spec) (sspecs : Spec.st list) : spec =
    let spec = Spec.extend uspec.data sspecs in
    let new_mp =
      List.fold_left
        (fun current_mp (asrt, post) ->
          match s_init ~preds:pred_ins params asrt with
          | Error _ ->
              if !Config.under_approximation then (
                L.verbose (fun m ->
                    m
                      "WARNING!!! IT IS NOT POSSIBLE TO BUILD MP FOR INFERRED \
                       SPEC of %s!PRE:@\n\
                       @[%a@]@\n"
                      uspec.data.spec_name Asrt.pp asrt);
                current_mp)
              else failwith "Couldn't build MP when extending spec!"
          | Ok step_list ->
              let new_mp = of_step_list ?post step_list in
              add_linear_mp current_mp new_mp)
        uspec.mp (posts_from_sspecs sspecs)
    in
    let uspec' : spec = { data = spec; mp = new_mp } in
    uspec'
  in

  let new_uspec =
    match Hashtbl.find_opt prog.specs spec.spec_name with
    | None -> new_uspec spec
    | Some uspec -> extend_spec uspec spec.spec_sspecs
  in

  Hashtbl.replace prog.specs spec.spec_name new_uspec;
  Hashtbl.replace prog.prog.procs spec.spec_name
    { proc with proc_spec = Some new_uspec.data }

let remove_spec (prog : 'a prog) spec_name =
  let proc = Prog.get_proc_exn prog.prog spec_name in
  Hashtbl.replace prog.prog.procs spec_name { proc with proc_spec = None };
  Hashtbl.remove prog.specs spec_name

let update_coverage (prog : 'a prog) (proc_name : string) (index : int) : unit =
  try
    let count = Hashtbl.find prog.coverage (proc_name, index) in
    Hashtbl.replace prog.coverage (proc_name, index) (count + 1)
  with Not_found -> Hashtbl.replace prog.coverage (proc_name, index) 0

let first_time_running (prog : 'a prog) (proc_name : string) (index : int) :
    bool =
  not (Hashtbl.mem prog.coverage (proc_name, index))

let pp_pred_defs fmt pred_defs =
  let pp_binding fmt (_, mp_pred) = Pred.pp fmt mp_pred.pred in
  Fmt.(hashtbl ~sep:(any "@\n") pp_binding) fmt pred_defs
