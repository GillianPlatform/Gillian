module Subst = SVal.SESubst
module L = Logging

type wand = { lhs : string * Expr.t list; rhs : string * Expr.t list }
[@@deriving yojson]

type query = {
  lname : string;
  rname : string;
  largs : Expr.t list;
  r_ins : Expr.t list;
  r_outs : Expr.t option list;
}

let query_ins_outs query = (query.largs @ query.r_ins, query.r_outs)

(** The in-parameters of a magic wand of the form [P(_) -* Q(_)] are:
               - all the parameters of P
               - the in parameters of Q *)
let make_query ~pred_defs ~subst wand =
  let { lhs = lname, largs; rhs = rname, rargs } = wand in
  let largs = List.map subst largs in
  let rargs = List.map subst rargs in
  let rpred = MP.get_pred_def pred_defs rname in
  let r_ins = Pred.in_args rpred.pred rargs in
  let r_outs = Pred.out_args rpred.pred rargs in
  match (Option_utils.all largs, Option_utils.all r_ins) with
  | Some largs, Some r_ins -> Some { lname; rname; largs; r_ins; r_outs }
  | _ -> None

let pp_query ft query =
  Fmt.pf ft "%s(%a) -* %s(%a; %a)" query.lname (pp_list Expr.pp) query.largs
    query.rname (pp_list Expr.pp) query.r_ins
    (pp_list @@ Fmt.Dump.option Expr.pp)
    query.r_outs

type t = wand list ref [@@deriving yojson]

let sure_is_nonempty wands =
  match !wands with
  | [] -> false
  | _ -> true

(** Returns the number of wand assertions *)
let length x = List.length !x

let init (wands : wand list) : t = ref wands
let to_list wands = !wands
let copy wands = ref !wands

let is_empty wands =
  match !wands with
  | [] -> true
  | _ -> false

let substitution_in_place subst wands =
  let subst_in_val (v : Expr.t) : Expr.t =
    Subst.subst_in_expr subst ~partial:true v
  in

  let subst_wand { lhs = lname, largs; rhs = rname, rargs } =
    {
      lhs = (lname, List.map subst_in_val largs);
      rhs = (rname, List.map subst_in_val rargs);
    }
  in
  wands := List.map subst_wand !wands

let pp_wand ft t =
  let { lhs = lname, largs; rhs = rname, rargs } = t in
  Fmt.pf ft "%s(%a) -* %s(%a)" lname
    (Fmt.list ~sep:(Fmt.any ", ") Expr.pp)
    largs rname
    (Fmt.list ~sep:(Fmt.any ", ") Expr.pp)
    rargs

let pp ft t = (Fmt.list ~sep:(Fmt.any "@\n") pp_wand) ft !t
let extend wands new_wand = wands := new_wand :: !wands

let get_lvars t =
  let lvars_val_list el =
    List.fold_left (fun acc expr -> SS.union acc (Expr.lvars expr)) SS.empty el
  in
  List.fold_left
    (fun acc { lhs = _, largs; rhs = _, rargs } ->
      acc |> SS.union (lvars_val_list largs) |> SS.union (lvars_val_list rargs))
    SS.empty !t

let get_alocs t =
  let alocs_val_list el =
    List.fold_left (fun acc v -> SS.union acc (Expr.alocs v)) SS.empty el
  in
  List.fold_left
    (fun acc { lhs = _, largs; rhs = _, rargs } ->
      acc |> SS.union (alocs_val_list largs) |> SS.union (alocs_val_list rargs))
    SS.empty !t

let to_assertions (wands : t) =
  let wand_to_asrt { lhs; rhs } = Asrt.Wand { lhs; rhs } in
  List.map wand_to_asrt !wands

let wand_ins_outs ~pred_defs { lhs = _, largs; rhs = rname, rargs } =
  let rpred = MP.get_pred_def pred_defs rname in
  let r_ins = Pred.in_args rpred.pred rargs in
  let r_outs = Pred.out_args rpred.pred rargs in
  let ins = largs @ r_ins in
  let outs = r_outs in
  (ins, outs)

let pop_nth n wands =
  let ith, rest = List_utils.get_and_remove_nth n !wands in
  wands := rest;
  ith

module Consume_wand = struct
  (* [score_candidate] returns [Some i] if:
     - All ins matched the query
     - i outs matched the query
     and returns [None] if some outs did not match *)
  let score_candidate
      (candidate : Expr.t list * Expr.t list)
      (query : Expr.t list * Expr.t option list)
      (f_eq : Expr.t -> Expr.t -> bool) : int option =
    let cand_ins, cand_outs = candidate in
    let target_ins, target_outs = query in
    let all_ins_match = List.for_all2 f_eq cand_ins target_ins in
    if not all_ins_match then None
    else
      (* Compute how many outs match *)
      let matched_outs =
        List.fold_left2
          (fun acc cand_out target_out ->
            match target_out with
            | None -> acc
            | Some target_out ->
                if f_eq cand_out target_out then acc + 1 else acc)
          0 cand_outs target_outs
      in
      Some matched_outs

  (* Given a list of wands, an input query and an argument matching function [f_eq], returns
     - [Some (i, s)] with [i] the index of a wand that matches all ins and [s] outs
     - [None] if no wand matches all ins *)
  let find_highest_scoring_match
      (candidates : (int * Expr.t list * Expr.t list) List.t)
      (query : Expr.t list * Expr.t option list)
      (f_eq : Expr.t -> Expr.t -> bool) : (int * int) option =
    List.fold_left
      (fun current candidate ->
        let index, cand_ins, cand_outs = candidate in
        let cand_score = score_candidate (cand_ins, cand_outs) query f_eq in
        match (current, cand_score) with
        | None, None -> None
        | None, Some cand_score -> Some (index, cand_score)
        | Some _, None -> current
        | Some (_, current_score), Some cand_score ->
            if cand_score > current_score then Some (index, cand_score)
            else current)
      None candidates

  let consume_wand ~pred_defs ~semantic_eq (wands : t) (query : query) =
    L.verbose (fun m -> m "Wands.consume_wand: Looking for %a" pp_query query);
    let rpred = MP.get_pred_def pred_defs query.rname in
    let candidates =
      List_utils.filter_mapi
        (fun i { lhs; rhs } ->
          let lname, largs = lhs in
          let rname, rargs = rhs in
          let name_match =
            [%eq: string * string] (lname, rname) (query.lname, query.rname)
          in
          if not name_match then None
          else
            let r_ins = Pred.in_args rpred.pred rargs in
            let outs = Pred.out_args rpred.pred rargs in
            let ins = largs @ r_ins in
            Some (i, ins, outs))
        !wands
    in
    let query_args = query_ins_outs query in
    let total_outs =
      List.fold_left
        (fun acc x -> if Option.is_some x then acc + 1 else acc)
        0 (snd query_args)
    in
    let best_match =
      let syntactic_match =
        find_highest_scoring_match candidates query_args Expr.equal
      in
      match syntactic_match with
      | Some (_, s) when s = total_outs -> syntactic_match
      | _ -> (
          (* Either we have no syntactic match, or we have one but we may find a better semantic match *)
          let semantic_match =
            find_highest_scoring_match candidates query_args semantic_eq
          in
          match (syntactic_match, semantic_match) with
          | None, None -> None
          | Some _, None -> syntactic_match
          | None, Some _ -> semantic_match
          | Some (_, syn_scode), Some (_, sem_score) ->
              if sem_score > syn_scode then semantic_match else syntactic_match)
    in
    match best_match with
    | None -> None
    | Some (i, _) -> pop_nth i wands
end

let consume_wand :
    pred_defs:MP.preds_tbl_t ->
    semantic_eq:(Expr.t -> Expr.t -> bool) ->
    t ->
    query ->
    wand option =
  Consume_wand.consume_wand
