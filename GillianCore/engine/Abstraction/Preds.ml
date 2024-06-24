module L = Logging

type vt = Expr.t [@@deriving yojson]
type st = SVal.SESubst.t
type abs_t = string * vt list
type t = (string * vt list) list ref [@@deriving yojson]

(** Returns the number of predicate assertions *)
let length x = List.length !x

(** Returns a new (empty) predicate set *)
let init (pas : abs_t list) : t = ref pas

(** Returns the serialization of --preds-- as a list of abs_ts *)
let to_list (preds : t) : abs_t list = !preds

(** Returns a copy of --preds-- *)
let copy (preds : t) : t = ref !preds

(** Returns true if --preds-- is empty *)
let is_empty (preds : t) : bool =
  match !preds with
  | [] -> true
  | _ -> false

(** Extends --preds-- with --pa-- *)
let extend ?(pure = false) (preds : t) (pa : abs_t) : unit =
  preds :=
    match pure with
    | true when List.mem pa !preds ->
        let name, params = pa in
        L.verbose (fun fmt ->
            fmt "Pure predicate already there, not producing: %s(%a)" name
              Fmt.(list ~sep:comma Expr.pp)
              params);
        !preds
    | _ -> pa :: !preds

let pop preds f =
  let rec val_and_remove passed = function
    | [] -> (List.rev passed, None)
    | a :: r when f a -> (List.rev_append passed r, Some a)
    | a :: r -> val_and_remove (a :: passed) r
  in
  let new_list, value = val_and_remove [] !preds in
  preds := new_list;
  value

let strategic_choice ~consume preds (f : 'a -> int) =
  let rec val_and_remove passed idx lst =
    match (idx, lst) with
    | 0, hd :: tl -> (List.rev passed @ tl, hd)
    | n, hd :: tl -> val_and_remove (hd :: passed) (n - 1) tl
    | _ -> failwith "Impossible: Strategic Choice"
  in
  let results = List.map f !preds in
  let _, (idx, max) =
    List.fold_left
      (fun (c, (i, max)) x ->
        if x > max then (c + 1, (c, x)) else (c + 1, (i, max)))
      (0, (0, 0))
      results
  in
  if max = 0 then None
  else
    let new_list, value = val_and_remove [] idx !preds in
    (* We remove iff consume is true *)
    if consume then preds := new_list;
    Some value

let pop_all preds f =
  let rec vals_and_remove passed acc = function
    | [] -> (List.rev passed, acc)
    | a :: r when f a -> vals_and_remove passed (a :: acc) r
    | a :: r -> vals_and_remove (a :: passed) acc r
  in
  let new_list, values = vals_and_remove [] [] !preds in
  preds := new_list;
  values

(** Removes the first occurrence of a pa with name --p_name-- and returns it *)
let remove_by_name (preds : t) (pname : string) : abs_t option =
  pop preds (fun (n, _) -> String.equal n pname)

(** Find predicate_assertion via pname. Returns a list with all the pabs with name pname *)
let find_pabs_by_name (preds : t) (pname : string) : abs_t list =
  List.filter (fun (pn, _) -> pn = pname) !preds

let get_lvars (preds : t) : SS.t =
  List.fold_left
    (fun ac (_, vs) ->
      List.fold_left (fun ac e -> SS.union ac (Expr.lvars e)) ac vs)
    SS.empty !preds

let get_alocs (preds : t) : SS.t =
  List.fold_left
    (fun ac (_, vs) ->
      List.fold_left (fun ac e -> SS.union ac (Expr.alocs e)) ac vs)
    SS.empty !preds

(** Printing function *)
let pp_pabs fmt pa =
  let pname, vs = pa in
  Fmt.pf fmt "%s(%a)" pname (Fmt.list ~sep:(Fmt.any ", ") Expr.pp) vs

let pp fmt preds =
  let lpreds = to_list preds in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pabs) fmt lpreds

let find (preds : t) (sel : abs_t -> bool) : abs_t option =
  List.find_opt sel !preds

let filter (preds : t) (sel : abs_t -> bool) : abs_t list =
  List.filter sel !preds

let get_all ~maintain f p = if maintain then filter p f else pop_all p f

(** TODO: EFICIENCY ISSUE!!! *)
let consume_pred
    ~(maintain : bool)
    (preds : t)
    (name : string)
    (args : vt option list)
    (ins : Containers.SI.t)
    (f_eq : vt -> vt -> bool) : abs_t option =
  (* Auxiliary printers *)
  let lv_pp = Fmt.Dump.list Expr.pp in
  let lov_pp = Fmt.Dump.(list (option Expr.pp)) in
  (* How many ins do we need to find *)
  let ins_count = Containers.SI.cardinal ins in
  (* How many outs do we know *)
  let _, known_outs_count =
    List.fold_left
      (fun (i, acc) arg ->
        if Containers.SI.mem i ins || Option.is_none arg then (i + 1, acc)
        else (i + 1, acc + 1))
      (0, 0) args
  in
  L.verbose (fun fmt ->
      fmt "Preds.consume_pred: Looking for: %s%a" name lov_pp args);
  (* Evaluate a candidate predicate with respect to the desired ins and outs and an equality function *)
  let eval_cand_with_fun
      (candidate : vt list)
      (targets : vt option list)
      (f_eq : vt -> vt -> bool) : bool * (int * int) =
    let candidate = List.mapi (fun i cv -> (i, cv)) candidate in
    let icount, ocount =
      List.fold_left2
        (fun (ic, oc) (i, cv) tv ->
          match tv with
          | None -> (ic, oc)
          (* First check syntactic equality and only then try f_eq *)
          | Some tv when (not (Expr.equal cv tv)) && not (f_eq cv tv) -> (ic, oc)
          | _ -> if Containers.SI.mem i ins then (ic + 1, oc) else (ic, oc + 1))
        (0, 0) candidate targets
    in
    let result = (icount = ins_count, (icount, ocount)) in
    result
  in
  (* Sort the candidate predicates according to the number of ins matched,
     and then the number of outs matched, in decreasing order of matches *)
  let find_pred
      (candidates : vt list list)
      (targets : vt option list)
      (f_eq : vt -> vt -> bool) =
    List.fold_left
      (fun (b', i', o', result) candidate ->
        let ccurrent = (b', i', o', result) in
        if i' = ins_count && o' = known_outs_count then ccurrent
        else
          let b, (i, o) =
            try
              (* In case something goes wrong with the evaluation, ignore *)
              eval_cand_with_fun candidate targets f_eq
            with _ -> (false, (0, 0))
          in
          let cnew = (b, i, o, candidate) in
          match (b', b) with
          | false, true -> cnew
          | true, false -> ccurrent
          | _ -> (
              match (i > i', i' > i) with
              | true, _ -> cnew
              | _, true -> ccurrent
              | _ -> if o > o' then cnew else ccurrent))
      (false, 0, 0, []) candidates
  in
  (* Frame off found predicate *)
  let frame_off name args =
    match maintain with
    | true -> Some (name, args)
    | false ->
        pop preds (fun pred -> [%eq: string * Expr.t list] pred (name, args))
  in
  let candidates = filter preds (fun (pname, _) -> name = pname) in
  let candidates = List.map (fun (_, args) -> args) candidates in
  L.verbose (fun fmt ->
      fmt "Found %d candidates: \n%a" (List.length candidates)
        Fmt.(list ~sep:(any "@\n") lv_pp)
        candidates);
  let syntactic_result = find_pred candidates args Expr.equal in
  match syntactic_result with
  | true, _, o, syntactic_result when o = known_outs_count ->
      frame_off name syntactic_result
  | true, _, o, syntactic_result -> (
      let semantic_result = find_pred candidates args f_eq in
      match semantic_result with
      | true, _, o', semantic_result ->
          let result = if o >= o' then syntactic_result else semantic_result in
          frame_off name result
      | false, _, _, _ -> frame_off name syntactic_result)
  | false, _, _, _ -> (
      let semantic_result = find_pred candidates args f_eq in
      match semantic_result with
      | true, _, _, semantic_result -> frame_off name semantic_result
      | false, _, _, _ -> None)

let subst_in_val (subst : st) (v : vt) : vt =
  SVal.SESubst.subst_in_expr subst ~partial:true v

(** Updates --preds-- to subst(preds) *)
let substitution_in_place (subst : st) (preds : t) : unit =
  let pred_substitution subst (s, vs) = (s, List.map (subst_in_val subst) vs) in
  preds := List.map (pred_substitution subst) !preds

let to_assertions (preds : t) : Asrt.t list =
  let preds = to_list preds in
  let pred_to_assert (n, args) = Asrt.Pred (n, args) in
  List.sort Asrt.compare (List.map pred_to_assert preds)
