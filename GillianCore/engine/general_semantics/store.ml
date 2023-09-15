open SVal

(**
  Interface for GIL Stores.
  GIL stores are mappings from GIL variables to GIL values.
  GIL stores are mutable.
*)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL stores *)
  type t [@@deriving yojson]

  (** Return the set of bindings in a given store *)
  val bindings : t -> (Var.t * vt) list

  (** Store copy *)
  val copy : t -> t

  (** Store domain *)
  val domain : t -> Var.Set.t

  (** Store filtering *)
  val filter_map_inplace : t -> (Var.t -> vt -> vt option) -> unit

  (** Store fold *)
  val fold : t -> (Var.t -> vt -> 'a -> 'a) -> 'a -> 'a

  (** Return value of a given variable, if possible *)
  val get : t -> Var.t -> vt option

  (** Return value of a given variable or throw *)
  val get_unsafe : t -> Var.t -> vt

  (** Store constructor, with a list of bindings of the form (variable, value) *)
  val init : (Var.t * vt) list -> t

  (** Store iterator *)
  val iter : t -> (Var.t -> vt -> unit) -> unit

  (** Store membership *)
  val mem : t -> Var.t -> bool

  (** Partition store domain *)
  val partition : t -> (vt -> bool) -> Var.Set.t * Var.Set.t

  (** Store projection (returns new store) *)
  val projection : t -> Var.t list -> t

  (** Update value of variable in store *)
  val put : t -> Var.t -> vt -> unit

  (** Remove value of variable in store *)
  val remove : t -> Var.t -> unit

  (** Store printer *)
  val pp : Format.formatter -> t -> unit

  (** Store printer by need *)
  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit

  (** Converts the store into an ssubst *)
  val to_ssubst : t -> SESubst.t

  (** Logical variables *)
  val lvars : t -> Var.Set.t
end

(** Implementation of GIL Stores *)
module Make (Val : Val.S) : S with type vt = Val.t = struct
  module L = Logging

  (** Type of GIL values *)
  type vt = Val.t [@@deriving yojson]

  (** Actual type of GIL Stores *)
  type t = (Var.t, vt) Hashtbl.t [@@deriving yojson]

  (**
    Store initialisation

    @param vars_and_vals Variables and values to be put in the store
    @return Newly constructed and initialised store
  *)
  let init (vars_and_vals : (Var.t * vt) list) : t =
    Hashtbl.of_seq (List.to_seq vars_and_vals)

  (**
    Store copy

    @param store Target store
    @return Copy of the given store
  *)
  let copy (store : t) : t = Hashtbl.copy store

  (**
    Store lookup

    @param store Target store
    @param x Target variable
    @return Optional value of the variable in the store
  *)
  let get (store : t) (x : Var.t) : vt option = Hashtbl.find_opt store x

  (**
    Store get with throw

    @param store Target store
    @param x Target variable
    @raise Failure Variable not found in the store
    @return Value of the variable in the store
  *)
  let get_unsafe (store : t) (v : Var.t) : vt =
    match get store v with
    | Some result -> result
    | None -> Fmt.failwith "Store.get_unsafe: variable %s not found in store" v

  (**
    Store update (in-place)

    @param store Target store
    @param x Target variable
    @param v Value to be put
  *)
  let put (store : t) (x : Var.t) (v : vt) : unit = Hashtbl.replace store x v

  (**
    Store removal (in-place)

    @param store Target store
    @param x Target variable
  *)
  let remove (store : t) (x : Var.t) : unit = Hashtbl.remove store x

  (**
    Store membership check

    @param store Target store
    @param x Target variable
    @return true if the variable is in the store, false otherwise
  *)
  let mem (store : t) (x : Var.t) : bool = Hashtbl.mem store x

  (**
    Store iterator

    @param store Target store
    @param f Iterator function
  *)
  let iter (store : t) (f : Var.t -> vt -> unit) : unit = Hashtbl.iter f store

  (**
    Store fold

    @param store Target store
    @param f Fold function
    @param ac Accumulator
  *)
  let fold (store : t) (f : Var.t -> vt -> 'a -> 'a) (ac : 'a) : 'a =
    Hashtbl.fold f store ac

  (**
    Store bindings

    @param store Target store
    @return Bindings of the store formatted as (variable, value)
  *)
  let bindings (store : t) : (Var.t * vt) list =
    Hashtbl.to_seq store |> List.of_seq

  (**
    Store domain

    @param store Target store
    @return Set of variables that are in the domain of the store
  *)
  let domain (store : t) : Var.Set.t =
    Hashtbl.to_seq_keys store |> Var.Set.of_seq

  (**
    Store filtering (in-place)

    @param store Target store
    @param f The filtering function
  *)
  let filter_map_inplace (store : t) (f : Var.t -> vt -> vt option) : unit =
    Hashtbl.filter_map_inplace f store

  (**
    Store partition

    @param store Target store
    @param f The partitioning function
    @return Set of variables satisfying the function, set of variables not satisfying the function
  *)
  let partition (store : t) (f : vt -> bool) : Var.Set.t * Var.Set.t =
    fold store
      (fun store le (pred_xs, npred_xs) ->
        if f le then (Var.Set.add store pred_xs, npred_xs)
        else (pred_xs, Var.Set.add store npred_xs))
      (Var.Set.empty, Var.Set.empty)

  (**
    Store projection

    @param store Target store
    @param xs List of variables to be projected
    @return New store that only contains the projected variables
  *)
  let projection (store : t) (vars : Var.t list) : t =
    let projected = Hashtbl.create (List.length vars) in
    List.iter
      (fun var ->
        get store var |> Option.iter (fun value -> put projected var value))
      vars;
    projected

  (**
    Store printer

    @param store GIL store
    @return String representation of the store
  *)
  let pp fmt (store : t) =
    let sep = Fmt.any "@\n" in
    let pp_pair =
      Fmt.(hbox (parens (pair ~sep:(any ": ") string Val.full_pp)))
    in
    let bindings =
      List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) (bindings store)
    in
    (Fmt.list ~sep pp_pair) fmt bindings

  let pp_by_need (pvars : Containers.SS.t) fmt (store : t) =
    let sep = Fmt.any "@\n" in
    let pp_pair =
      Fmt.(hbox (parens (pair ~sep:(any ": ") string Val.full_pp)))
    in
    let bindings =
      List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) (bindings store)
    in
    (* Filter for the ones needed *)
    let bindings =
      List.filter (fun (v, _) -> Containers.SS.mem v pvars) bindings
    in
    (Fmt.list ~sep pp_pair) fmt bindings

  (**
    Store to substitution

    @param store to turn into an ssubst
    @return ssubst mapping the store variables to lexprs
  *)
  let to_ssubst (store : t) : SESubst.t =
    let subst = SESubst.init [] in
    iter store (fun x v -> SESubst.put subst (Expr.PVar x) (Val.to_expr v));
    subst

  let lvars (store : t) : Var.Set.t =
    Hashtbl.fold
      (fun _ v ac -> Var.Set.union ac (Expr.lvars (Val.to_expr v)))
      store Var.Set.empty
end
