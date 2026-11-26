(** @canonical Gillian.General.Subst

    Interface for GIL Substitutions. GIL substitutions are mappings from GIL
    Variables to GIL Values. GIL substitutions are mutable. *)

(** @canonical Gillian.General.Subst.S *)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL substitutions *)
  type t

  (** Substitution constructor, with a list of bindings of the form (variable,
      value) *)
  val init : ([< Id.substable ] Id.t * vt) list -> t

  (** Is the substitution empty? *)
  val is_empty : t -> bool

  (** Reset substitution *)
  val clear : t -> unit

  (** Domain of the substitution *)
  val domain : t -> (Id.Substable.t -> bool) option -> Id.Substable.Set.t

  (** Range of the substitution *)
  val range : t -> vt list

  (** Substitution lookup *)
  val get : t -> [< Id.substable ] Id.t -> vt option

  (** Substitution incremental update *)
  val add : t -> [< Id.substable ] Id.t -> vt -> unit

  (** Substitution update *)
  val put : t -> [< Id.substable ] Id.t -> vt -> unit

  (** Substitution membership *)
  val mem : t -> [< Id.substable ] Id.t -> bool

  (** Substitution copy *)
  val copy : t -> t

  (** Substitution extension with a list of bindings *)
  val extend : t -> ([< Id.substable ] Id.t * vt) list -> unit

  (** Substution merge into left *)
  val merge_left : t -> t -> unit

  (** Substitution filter *)
  val filter : t -> (Id.Substable.t -> vt -> bool) -> t

  (** Substitution variable filter *)
  val projection : t -> Id.Substable.Set.t -> t

  (** Substitution iterator *)
  val iter : t -> (Id.Substable.t -> vt -> unit) -> unit

  (** Substitution fold *)
  val fold : t -> (Id.Substable.t -> vt -> 'a -> 'a) -> 'a -> 'a

  (** Pretty Printer *)
  val pp : Format.formatter -> t -> unit

  (** Full pretty Printer *)
  val full_pp : Format.formatter -> t -> unit

  val filter_in_place : t -> (Id.Substable.t -> vt -> vt option) -> unit

  (** Convert substitution to list *)
  val to_list : t -> (Id.Substable.t * vt) list

  (** Substitution inside a logical expression *)
  val subst_in_expr : t -> partial:bool -> Expr.t -> Expr.t

  (** Optional substitution inside a logical expression *)
  val subst_in_expr_opt : t -> Expr.t -> Expr.t option

  val substitute_asrt : t -> partial:bool -> Asrt.t -> Asrt.t
  val substitute_slcmd : t -> partial:bool -> SLCmd.t -> SLCmd.t
  val substitute_lcmd : t -> partial:bool -> LCmd.t -> LCmd.t
end

module Make (Val : Val.S) : S with type vt = Val.t = struct
  module L = Logging

  (** Type of GIL values *)
  type vt = Val.t

  (** Type of GIL substitutions, implemented as hashtables *)
  type t = (Id.Substable.t, vt) Hashtbl.t

  let key (x : [< Id.substable ] Id.t) : Id.Substable.t = (x :> Id.Substable.t)

  (** Substitution constructor

      @param vars_les Bindings of the form (variable, value)
      @return Substitution with the given bindings *)
  let init vars_les : t =
    let subst = Hashtbl.create Config.big_tbl_size in
    List.iter (fun (v, v_val) -> Hashtbl.replace subst (key v) v_val) vars_les;
    subst

  let clear (subst : t) : unit = Hashtbl.clear subst

  (** Substitution domain

      @param subst Target substitution
      @param filter_out Optional filtering function
      @return Domain of the (filtered) substitution *)
  let domain (subst : t) (filter_out : (Id.Substable.t -> bool) option) :
      Id.Substable.Set.t =
    let filter =
      match filter_out with
      | Some filter -> filter
      | None -> fun _ -> false
    in
    Hashtbl.fold
      (fun k _ ac -> if filter k then ac else Id.Substable.Set.add k ac)
      subst Id.Substable.Set.empty

  (** Substitution range

      @param subst Target substitution
      @return Range of the substitution *)
  let range (subst : t) : vt list =
    Hashtbl.fold (fun _ v_val ac -> v_val :: ac) subst []

  (** Substitution lookup

      @param subst Target substitution
      @param x Target variable
      @return Resulting (optional) value *)
  let get (subst : t) x : vt option = Hashtbl.find_opt subst (key x)

  (** Substitution incremental update

      @param subst Target substitution
      @param x Target variable
      @param v Target value *)
  let add (subst : t) x (v : vt) : unit = Hashtbl.add subst (key x) v

  (** Substitution update

      @param subst Target substitution
      @param x Target variable
      @param v Target value *)
  let put (subst : t) x (v : vt) : unit = Hashtbl.replace subst (key x) v

  (** Substitution membership

      @param subst Target substitution
      @param x Target variable
      @return
        Returns true if the variable is in the domain of the substitution, and
        false otherwise *)
  let mem (subst : t) x : bool = Hashtbl.mem subst (key x)

  (** Substitution copy

      @param subst Target store
      @return Copy of the given substitution *)
  let copy (subst : t) : t = Hashtbl.copy subst

  (** Substitution extension

      @param store Target substitution
      @param extend *)
  let extend (subst : t) vars_les : unit =
    List.iter (fun (v, v_val) -> Hashtbl.replace subst (key v) v_val) vars_les

  (** Substitution iterator

      @param subst Target substitution
      @param f Iterator function *)
  let iter (subst : t) (f : Id.Substable.t -> vt -> unit) : unit =
    Hashtbl.iter f subst

  (** Substitution fold

      @param subst Target substitution
      @param f Fold function
      @param ac Accumulator *)
  let fold (subst : t) f ac = Hashtbl.fold f subst ac

  (** Substitution merge into left

      @param subst Target substitution
      @param subst_ext Substitution extension *)
  let merge_left (subst : t) (subst_ext : t) : unit =
    Hashtbl.iter (fun v v_val -> Hashtbl.replace subst v v_val) subst_ext

  (** Substitution filter

      @param subst Target substitution
      @param filter Filtering function
      @return The new, filtered substitution *)
  let filter (subst : t) (filter : Id.Substable.t -> vt -> bool) : t =
    let new_subst = copy subst in
    Hashtbl.filter_map_inplace
      (fun v v_val ->
        match filter v v_val with
        | true -> Some v_val
        | false -> None)
      new_subst;
    new_subst

  (** Substitution filter by variables

      @param subst Target substitution
      @param vars Variables to save
      @return The new, filtered substitution *)
  let projection (subst : t) (vars : Id.Substable.Set.t) : t =
    filter subst (fun x _ -> Id.Substable.Set.mem x vars)

  (** Substitution pretty_printer

      @param fmt Formatter
      @param subst Target substitution
      @return unit *)
  let pp fmt (subst : t) =
    let pp_pair fmt (v, v_val) =
      Fmt.pf fmt "@[<h>(%a: %a)@]" Id.pp v Val.pp v_val
    in
    Fmt.pf fmt "[ @[%a@] ]" (Fmt.hashtbl ~sep:Fmt.comma pp_pair) subst

  (** Substitution full pretty_printer

      @param fmt Formatter
      @param subst Target substitution
      @return unit *)
  let full_pp fmt (subst : t) =
    let pp_pair fmt (v, v_val) =
      Fmt.pf fmt "@[<h>(%a: %a)@]" Id.pp v Val.full_pp v_val
    in
    Fmt.pf fmt "[ @[%a@] ]" (Fmt.hashtbl ~sep:Fmt.comma pp_pair) subst

  (** Substitution in-place filter

      @param subst Target substitution
      @param filter Filtering function
      @return Filtered substitution *)
  let filter_in_place (subst : t) (filter : Id.Substable.t -> vt -> vt option) :
      unit =
    Hashtbl.filter_map_inplace filter subst

  (**
    Conversion to a list

    @params subst Target substitution
    @return List of bindings of the form (variable, value)
  *)
  let to_list (subst : t) : (Id.Substable.t * vt) list =
    Hashtbl.fold (fun v v_val ac -> (v, v_val) :: ac) subst []

  (** Substitution inside an expression

      @param subst Target substitution
      @param le Target expression
      @return
        Expression resulting from the substitution, with fresh locations
        created. *)
  let subst_in_expr (subst : t) ~(partial : bool) (le : Expr.t) : Expr.t =
    let find_in_subst x (le_x_old : Expr.t) (make_new_x : unit -> Expr.t) :
        Expr.t =
      match get subst x with
      | Some v -> Val.to_expr v
      | None -> (
          if partial then le_x_old
          else
            let new_le_x = make_new_x () in
            match Val.from_expr new_le_x with
            | Some sv ->
                put subst x sv;
                new_le_x
            | None -> raise (Failure "DEATH. subst_in_expr"))
    in
    let mapper =
      object (self)
        inherit [_] Gil_syntax.Visitors.endo
        val mutable self_subst = init []

        method! visit_LVar () this x =
          find_in_subst x this (fun () -> Expr.LVar (LVar.alloc ()))

        method! visit_ALoc () this x =
          find_in_subst x this (fun () -> Expr.ALoc (ALoc.alloc ()))

        method! visit_PVar () this x =
          find_in_subst x this (fun () ->
              let lvar = LVar.alloc () in
              L.(
                verbose (fun m ->
                    m
                      "General: Subst in lexpr: PVar %a not in subst, \
                       generating fresh: %a"
                      Id.pp x Id.pp lvar));
              Expr.LVar lvar)

        method! visit_Exists () this bt e =
          let binders = List.to_seq bt |> Seq.map fst in
          let binder_substs =
            binders
            |> Seq.filter_map (fun x ->
                   Option.map (fun x_v -> (x, x_v)) (get self_subst x))
          in
          Seq.iter (fun x -> put self_subst x (Val.from_lvar x)) binders;
          let new_expr = self#visit_expr () e in
          Seq.iter (fun (x, le_x) -> put self_subst x le_x) binder_substs;
          if new_expr == e then this else Exists (bt, new_expr)

        method! visit_ForAll () this bt e =
          let binders = List.to_seq bt |> Seq.map fst in
          let binder_substs =
            binders
            |> Seq.filter_map (fun x ->
                   Option.map (fun x_v -> (x, x_v)) (get self_subst x))
          in
          Seq.iter (fun x -> put self_subst x (Val.from_lvar x)) binders;
          let new_expr = self#visit_expr () e in
          Seq.iter (fun (x, le_x) -> put self_subst x le_x) binder_substs;
          if new_expr == e then this else ForAll (bt, new_expr)
      end
    in
    mapper#visit_expr () le

  (** Optional substitution inside an expression

      @param subst Target substitution
      @param le Target expression
      @return
        Expression resulting from the substitution. No fresh locations are
        created. *)
  let subst_in_expr_opt (subst : t) (le : Expr.t) : Expr.t option =
    let f_before : Expr.t -> Expr.t option * bool = function
      | LVar x -> (Option.map Val.to_expr (get subst x), false)
      | ALoc x -> (Option.map Val.to_expr (get subst x), false)
      | PVar x -> (Option.map Val.to_expr (get subst x), false)
      | _ -> (Some le, true)
    in
    Expr.map_opt f_before None le

  let is_empty (subst : t) : bool = Hashtbl.length subst = 0

  let substitute_asrt (subst : t) ~(partial : bool) : Asrt.t -> Asrt.t =
    Asrt.map (subst_in_expr subst ~partial)

  let substitute_slcmd (subst : t) ~(partial : bool) : SLCmd.t -> SLCmd.t =
    SLCmd.map (substitute_asrt subst ~partial) (subst_in_expr subst ~partial)

  let substitute_lcmd (subst : t) ~(partial : bool) : LCmd.t -> LCmd.t =
    LCmd.map (subst_in_expr subst ~partial) (substitute_slcmd subst ~partial)
end
