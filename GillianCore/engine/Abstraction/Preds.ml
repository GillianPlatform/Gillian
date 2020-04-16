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

  val extend : t -> abs_t -> unit

  val remove : t -> abs_t -> bool

  val remove_by_name : t -> string -> abs_t option

  val find_pabs_by_name : t -> string -> abs_t list

  val find_index : t -> abs_t -> int option

  val iteri : (int -> abs_t -> unit) -> t -> unit

  val get_index : t -> int -> abs_t

  val remove_index : t -> int -> unit

  val set_index : t -> int -> abs_t -> unit

  val pp : Format.formatter -> t -> unit

  val pp_pabs : Format.formatter -> abs_t -> unit

  val get_pred :
    bool ->
    t ->
    string ->
    vt list ->
    int list ->
    (vt -> vt -> bool) ->
    abs_t option

  val find : bool -> t -> (abs_t -> bool) -> abs_t option

  val find_all : ?keep:bool -> (abs_t -> bool) -> t -> abs_t list

  val substitution : st -> t -> t

  val substitution_in_place : st -> t -> unit

  val to_assertions : t -> Asrt.t list
  (** Turns a predicate set into a list of assertions *)

  val index_of : t -> (abs_t -> bool) -> int option

  val lvars : t -> Containers.SS.t
end

module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st) :
  S with type vt = Val.t and type st = Subst.t = struct
  module L = Logging

  type vt = Val.t

  type st = Subst.t

  type abs_t = string * vt list

  type t = (string * vt list) DynArray.t

  (** Returns the number of predicate assertions *)
  let length (preds : t) : int = DynArray.length preds

  (** Returns a new (empty) predicate set *)
  let init (pas : abs_t list) : t =
    let preds = DynArray.make Config.small_tbl_size in
    List.iter (DynArray.add preds) pas;
    preds

  (** Returns the serialization of --preds-- as a list of abs_ts *)
  let to_list (preds : t) : abs_t list = DynArray.to_list preds

  (** Returns a copy of --preds-- *)
  let copy (preds : t) : t = DynArray.copy preds

  (** Returns true if --preds-- is empty *)
  let is_empty (preds : t) : bool = DynArray.length preds = 0

  (** Extends --preds-- with --pa-- *)
  let extend (preds : t) (pa : abs_t) : unit = DynArray.add preds pa

  (** Finds the index of --pa-- in --preds-- *)
  let find_index (preds : t) (pa : abs_t) : int option =
    try Some (DynArray.index_of (fun pa' -> pa = pa) preds) with _ -> None

  (** Removes the i-th pabs of --preds-- *)
  let remove_index (preds : t) (i : int) : unit = DynArray.delete preds i

  (** Sets the i-th pabs of --preds-- to pabs *)
  let set_index (preds : t) (i : int) (pa : abs_t) : unit =
    DynArray.set preds i pa

  (** Sets the i-th pabs of --preds-- to pabs *)
  let get_index (preds : t) (i : int) : abs_t =
    try DynArray.get preds i
    with _ -> raise (Failure "PREDS. Accesing preds with illegal index")

  (** Removes the binding of --pa-- from --preds-- *)
  let remove (preds : t) (pa : abs_t) : bool =
    match find_index preds pa with
    | Some i ->
        DynArray.delete preds i;
        true
    | _      -> false

  (** Removes the first occurrence of a pa with name --p_name-- and returns it *)
  let remove_by_name (preds : t) (pname : string) : abs_t option =
    try
      let i = DynArray.index_of (fun (pname', _) -> pname' = pname) preds in
      let pa = DynArray.get preds i in
      DynArray.delete preds i;
      Some pa
    with _ -> None

  (** Find predicate_assertion via pname. Returns a list with all the pabs with name pname *)
  let find_pabs_by_name (preds : t) (pname : string) : abs_t list =
    let preds_l = to_list preds in
    List.filter (fun (pn, _) -> pn = pname) preds_l

  (** Printing function *)
  let pp_pabs fmt pa =
    let pname, vs = pa in
    Fmt.pf fmt "%s(%a)" pname (Fmt.list ~sep:(Fmt.any ", ") Val.pp) vs

  let iteri (f : int -> abs_t -> unit) (preds : t) = DynArray.iteri f preds

  let pp fmt preds =
    let lpreds = to_list preds in
    if List.length lpreds != 0 then
      (Fmt.list ~sep:(Fmt.any "@\n") pp_pabs) fmt lpreds

  (** TODO: EFICIENCY ISSUE!!! *)
  let get_pred
      (maintain : bool)
      (preds : t)
      (name : string)
      (args : vt list)
      (ins : int list)
      (f_eq : vt -> vt -> bool) : abs_t option =
    try
      let pred_index =
        DynArray.index_of
          (fun (name', args') ->
            if not (name = name') then false
            else
              let args' = List.map (fun i -> List.nth args' i) ins in
              List.for_all
                (fun (arg, arg') ->
                  L.tmi (fun m ->
                      m "Checking if %a = %a\n" Val.pp arg Val.pp arg');
                  f_eq arg arg')
                (List.combine args args'))
          preds
      in
      let pa = DynArray.get preds pred_index in
      if not maintain then (
        L.verbose (fun m -> m "Removing predicate: %s" name);
        DynArray.delete preds pred_index );
      Some pa
    with Not_found -> None

  let subst_in_val (subst : st) (v : vt) : vt =
    let le' = Subst.subst_in_expr subst true (Val.to_expr v) in
    Option.fold ~some:(fun v -> v) ~none:v (Val.from_expr le')

  (** Returns subst(preds) *)
  let substitution (subst : st) (preds : t) : t =
    let preds = to_list preds in
    let preds' =
      List.map (fun (s, vs) -> (s, List.map (subst_in_val subst) vs)) preds
    in
    init preds'

  (** Updates --preds-- to subst(preds) *)
  let substitution_in_place (subst : st) (preds : t) : unit =
    let pred_substitution subst (s, vs) =
      (s, List.map (subst_in_val subst) vs)
    in
    DynArray.iteri
      (fun i pred -> DynArray.set preds i (pred_substitution subst pred))
      preds

  let to_assertions (preds : t) : Asrt.t list =
    let preds = to_list preds in
    let rec loop preds assertions : Asrt.t list =
      match preds with
      | []                        -> assertions
      | (pred_name, args) :: rest ->
          let args' = List.map Val.to_expr args in
          loop rest (Asrt.Pred (pred_name, args') :: assertions)
    in
    List.sort Asrt.compare (loop preds [])

  let index_of (preds : t) (f : abs_t -> bool) : int option =
    try Some (DynArray.index_of f preds) with Not_found -> None

  let find (maintain : bool) (preds : t) (sel : abs_t -> bool) : abs_t option =
    try
      let pred_index = DynArray.index_of sel preds in
      let name, vs = DynArray.get preds pred_index in
      if not maintain then (
        L.verbose (fun m -> m "Removing predicate: %s" name);
        DynArray.delete preds pred_index );
      Some (name, vs)
    with Not_found -> None

  let find_all ?(keep : bool option) (sel : abs_t -> bool) (preds : t) :
      abs_t list =
    let keep = Option.value ~default:true keep in
    let indexes, fpreds, _ =
      DynArray.fold_left
        (fun (indexes, fpreds, i) (pname, vs) ->
          if sel (pname, vs) then (i :: indexes, (pname, vs) :: fpreds, i + 1)
          else (indexes, fpreds, i + 1))
        ([], [], 0) preds
    in
    if keep then fpreds
    else (
      List.iter (fun i -> DynArray.delete preds i) indexes;
      fpreds )

  let lvars (preds : t) : Containers.SS.t =
    let open Containers in
    List.fold_left
      (fun cur_lvars (pname, pargs) ->
        let lvars =
          List.fold_left
            (fun cur_lvars arg ->
              let e_arg = Val.to_expr arg in
              let lvars = Expr.lvars e_arg in
              SS.union cur_lvars lvars)
            SS.empty pargs
        in
        SS.union cur_lvars lvars)
      SS.empty (to_list preds)
end

module SPreds = Make (SVal.M) (SVal.SSubst)
