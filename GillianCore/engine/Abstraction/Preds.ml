module type S = sig
  (* value type *)
  type vt

  (* substitution type for the value type *)
  type st

  (* preds *)
  type t

  type abs_t = string * vt list

  val length : t -> int

  val init : abs_t list -> t

  val to_list : t -> abs_t list

  val copy : t -> t

  val is_empty : t -> bool

  val extend : ?pure:bool -> t -> abs_t -> unit

  val pop : t -> (abs_t -> bool) -> abs_t option

  val remove_by_name : t -> string -> abs_t option

  val find_pabs_by_name : t -> string -> abs_t list

  val pp : Format.formatter -> t -> unit

  val pp_pabs : Format.formatter -> abs_t -> unit

  val get_pred :
    maintain:bool ->
    t ->
    string ->
    vt list ->
    int list ->
    (vt -> vt -> bool) ->
    abs_t option

  val find : t -> (abs_t -> bool) -> abs_t option

  val get_all : maintain:bool -> (abs_t -> bool) -> t -> abs_t list

  val substitution_in_place : st -> t -> unit

  (** Turns a predicate set into a list of assertions *)
  val to_assertions : t -> Asrt.t list
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et) :
  S with type vt = Val.t and type st = ESubst.t = struct
  module L = Logging

  type vt = Val.t

  type st = ESubst.t

  type abs_t = string * vt list

  type t = (string * vt list) list ref

  (** Returns the number of predicate assertions *)
  let length x = List.length !x

  (** Returns a new (empty) predicate set *)
  let init (pas : abs_t list) : t = ref pas

  (** Returns the serialization of --preds-- as a list of abs_ts *)
  let to_list (preds : t) : abs_t list = !preds

  (** Returns a copy of --preds-- *)
  let copy (preds : t) : t = ref !preds

  (** Returns true if --preds-- is empty *)
  let is_empty (preds : t) : bool = List.compare_length_with !preds 0 = 0

  (** Extends --preds-- with --pa-- *)
  let extend ?(pure = false) (preds : t) (pa : abs_t) : unit =
    preds :=
      match pure with
      | true when List.mem pa !preds -> !preds
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

  (** Printing function *)
  let pp_pabs fmt pa =
    let pname, vs = pa in
    Fmt.pf fmt "%s(%a)" pname (Fmt.list ~sep:(Fmt.any ", ") Val.pp) vs

  let pp fmt preds =
    let lpreds = to_list preds in
    if List.length lpreds != 0 then
      (Fmt.list ~sep:(Fmt.any "@\n") pp_pabs) fmt lpreds

  let find (preds : t) (sel : abs_t -> bool) : abs_t option =
    List.find_opt sel !preds

  let find_all (preds : t) (sel : abs_t -> bool) : abs_t list =
    List.find_all sel !preds

  let get_all ~maintain f p = if maintain then find_all p f else pop_all p f

  (** TODO: EFICIENCY ISSUE!!! *)
  let get_pred
      ~(maintain : bool)
      (preds : t)
      (name : string)
      (args : vt list)
      (ins : int list)
      (f_eq : vt -> vt -> bool) : abs_t option =
    let sort (candidates : (vt list * vt list) list) (targets : vt list) =
      let equals (candidates : vt list) (targets : vt list) =
        List.fold_left2
          (fun sum c t ->
            L.tmi (fun fmt ->
                fmt "Candidate equals: %a vs %a" Val.pp c Val.pp t);
            if c = t then sum - 1 else sum)
          0 candidates targets
      in
      let sort_fun (p1 : vt list * vt list) (p2 : vt list * vt list) =
        let i1, o1 = p1 in
        let i2, o2 = p2 in
        Stdlib.compare (equals i1 targets) (equals i2 targets)
      in
      List.sort sort_fun candidates
    in
    let basic_matcher (args', _) =
      List.for_all
        (fun (arg, arg') ->
          L.tmi (fun m ->
              m "Checking if %a = %a syntactically\n" Val.pp arg Val.pp arg');
          arg = arg')
        (List.combine args args')
    in
    let matcher (args', _) =
      List.for_all
        (fun (arg, arg') ->
          L.tmi (fun m -> m "Checking if %a = %a\n" Val.pp arg Val.pp arg');
          f_eq arg arg')
        (List.combine args args')
    in
    let candidates = find_all preds (fun (pname, _) -> name = pname) in
    let candidates = List.map (fun (_, args) -> args) candidates in
    let candidates =
      List.map
        (fun args -> (List.map (fun i -> List.nth args i) ins, args))
        candidates
    in
    let candidates = sort candidates args in
    (* First attempt: syntactic equality *)
    let result =
      List.fold_left
        (fun (res : abs_t option) (candidate : vt list * vt list) ->
          if res <> None then res
          else if basic_matcher candidate then Some (name, snd candidate)
          else None)
        None candidates
    in
    (* Next attempt: provided comparator *)
    let result =
      match result with
      | None        ->
          List.fold_left
            (fun (res : abs_t option) (candidate : vt list * vt list) ->
              if res <> None then res
              else if matcher candidate then Some (name, snd candidate)
              else None)
            None candidates
      | Some result -> Some result
    in
    match result with
    | None        -> None
    | Some result -> (
        match maintain with
        | true  -> Some result
        | false -> pop preds (fun pred -> pred = result) )

  let subst_in_val (subst : st) (v : vt) : vt =
    let le' = ESubst.subst_in_expr subst true (Val.to_expr v) in
    Option.fold ~some:(fun v -> v) ~none:v (Val.from_expr le')

  (** Updates --preds-- to subst(preds) *)
  let substitution_in_place (subst : st) (preds : t) : unit =
    let pred_substitution subst (s, vs) =
      (s, List.map (subst_in_val subst) vs)
    in
    preds := List.map (pred_substitution subst) !preds

  let to_assertions (preds : t) : Asrt.t list =
    let preds = to_list preds in
    let pred_to_assert (n, a) =
      let args = List.map Val.to_expr a in
      Asrt.Pred (n, args)
    in
    List.sort Asrt.compare (List.map pred_to_assert preds)
end

module SPreds = Make (SVal.M) (SVal.SESubst)
