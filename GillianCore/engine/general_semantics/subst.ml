(** @canonical Gillian.General.Subst

  Interface for GIL Substitutions.
  GIL substitutions are mappings from GIL Variables to GIL Values.
  GIL substitutions are mutable. *)

(** @canonical Gillian.General.Subst.S *)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL substitutions *)
  type t

  (** Substitution constructor, with a list of bindings of the form (variable, value) *)
  val init : (Var.t * vt) list -> t

  (** Is the substitution empty? *)
  val is_empty : t -> bool

  (** Reset substitution *)
  val clear : t -> unit

  (** Domain of the substitution *)
  val domain : t -> (Var.t -> bool) option -> Var.Set.t

  (** Range of the substitution *)
  val range : t -> vt list

  (** Substitution lookup *)
  val get : t -> Var.t -> vt option

  (** Substitution incremental update *)
  val add : t -> Var.t -> vt -> unit

  (** Substitution update *)
  val put : t -> Var.t -> vt -> unit

  (** Substitution membership *)
  val mem : t -> Var.t -> bool

  (** Substitution copy *)
  val copy : t -> t

  (** Substitution extension with a list of bindings *)
  val extend : t -> (Var.t * vt) list -> unit

  (** Substution merge into left *)
  val merge_left : t -> t -> unit

  (** Substitution filter *)
  val filter : t -> (Var.t -> vt -> bool) -> t

  (** Substitution variable filter *)
  val projection : t -> Var.Set.t -> t

  (** Substitution iterator *)
  val iter : t -> (Var.t -> vt -> unit) -> unit

  (** Substitution fold *)
  val fold : t -> (Var.t -> vt -> 'a -> 'a) -> 'a -> 'a

  (** Pretty Printer *)
  val pp : Format.formatter -> t -> unit

  (** Full pretty Printer *)
  val full_pp : Format.formatter -> t -> unit

  val filter_in_place : t -> (Var.t -> vt -> vt option) -> unit

  (** Convert substitution to list *)
  val to_list : t -> (Var.t * vt) list

  (** Substitution inside a logical expression *)
  val subst_in_expr : t -> partial:bool -> Expr.t -> Expr.t

  (** Optional substitution inside a logical expression *)
  val subst_in_expr_opt : t -> Expr.t -> Expr.t option

  val substitute_formula : t -> partial:bool -> Formula.t -> Formula.t
  val substitute_asrt : t -> partial:bool -> Asrt.t -> Asrt.t
  val substitute_slcmd : t -> partial:bool -> SLCmd.t -> SLCmd.t
  val substitute_lcmd : t -> partial:bool -> LCmd.t -> LCmd.t
end

module Make (Val : Val.S) : S with type vt = Val.t = struct
  module L = Logging

  (** Type of GIL values *)
  type vt = Val.t

  (** Type of GIL substitutions, implemented as hashtables *)
  type t = (Var.t, vt) Hashtbl.t

  (**
    Substitution constructor

    @param vars_les Bindings of the form (variable, value)
    @return Substitution with the given bindings
  *)
  let init (vars_les : (Var.t * vt) list) : t =
    let subst = Hashtbl.create Config.big_tbl_size in
    List.iter (fun (v, v_val) -> Hashtbl.replace subst v v_val) vars_les;
    subst

  let clear (subst : t) : unit = Hashtbl.clear subst

  (**
    Substitution domain

    @param subst Target substitution
    @param filter_out Optional filtering function
    @return Domain of the (filtered) substitution
  *)
  let domain (subst : t) (filter_out : (Var.t -> bool) option) : Var.Set.t =
    let filter =
      match filter_out with
      | Some filter -> filter
      | None -> fun _ -> false
    in
    Hashtbl.fold
      (fun k _ ac -> if filter k then ac else Var.Set.add k ac)
      subst Var.Set.empty

  (**
    Substitution range

    @param subst Target substitution
    @return Range of the substitution
  *)
  let range (subst : t) : vt list =
    Hashtbl.fold (fun _ v_val ac -> v_val :: ac) subst []

  (**
    Substitution lookup

    @param subst Target substitution
    @param x Target variable
    @return Resulting (optional) value
  *)
  let get (subst : t) (x : Var.t) : vt option = Hashtbl.find_opt subst x

  (**
    Substitution incremental update

    @param subst Target substitution
    @param x Target variable
    @param v Target value
  *)
  let add (subst : t) (x : Var.t) (v : vt) : unit = Hashtbl.add subst x v

  (**
    Substitution update

    @param subst Target substitution
    @param x Target variable
    @param v Target value
  *)
  let put (subst : t) (x : Var.t) (v : vt) : unit = Hashtbl.replace subst x v

  (**
    Substitution membership

    @param subst Target substitution
    @param x Target variable
    @return Returns true if the variable is in the domain of the substitution, and false otherwise
  *)
  let mem (subst : t) (x : Var.t) : bool = Hashtbl.mem subst x

  (**
    Substitution copy

    @param subst Target store
    @return Copy of the given substitution
  *)
  let copy (subst : t) : t = Hashtbl.copy subst

  (**
    Substitution extension

    @param store Target substitution
    @param extend
  *)
  let extend (subst : t) (vars_les : (Var.t * vt) list) : unit =
    List.iter (fun (v, v_val) -> Hashtbl.replace subst v v_val) vars_les

  (**
    Substitution iterator

    @param subst Target substitution
    @param f Iterator function
  *)
  let iter (subst : t) (f : Var.t -> vt -> unit) : unit = Hashtbl.iter f subst

  (**
    Substitution fold

    @param subst Target substitution
    @param f Fold function
    @param ac Accumulator
  *)
  let fold (subst : t) f ac = Hashtbl.fold f subst ac

  (**
    Substitution merge into left

    @param subst Target substitution
    @param subst_ext Substitution extension
  *)
  let merge_left (subst : t) (subst_ext : t) : unit =
    Hashtbl.iter (fun v v_val -> Hashtbl.replace subst v v_val) subst_ext

  (**
    Substitution filter

    @param subst Target substitution
    @param filter Filtering function
    @return The new, filtered substitution
  *)
  let filter (subst : t) (filter : Var.t -> vt -> bool) : t =
    let new_subst = copy subst in
    Hashtbl.filter_map_inplace
      (fun v v_val ->
        match filter v v_val with
        | true -> Some v_val
        | false -> None)
      new_subst;
    new_subst

  (**
    Substitution filter by variables

    @param subst Target substitution
    @param vars Variables to save
    @return The new, filtered substitution
  *)
  let projection (subst : t) (vars : Var.Set.t) : t =
    filter subst (fun x _ -> Var.Set.mem x vars)

  (**
    Substitution pretty_printer

    @param fmt Formatter
    @param subst Target substitution
    @return unit
  *)
  let pp fmt (subst : t) =
    let pp_pair fmt (v, v_val) = Fmt.pf fmt "@[<h>(%s: %a)@]" v Val.pp v_val in
    Fmt.pf fmt "[ @[%a@] ]" (Fmt.hashtbl ~sep:Fmt.comma pp_pair) subst

  (**
    Substitution full pretty_printer

    @param fmt Formatter
    @param subst Target substitution
    @return unit
  *)
  let full_pp fmt (subst : t) =
    let pp_pair fmt (v, v_val) =
      Fmt.pf fmt "@[<h>(%s: %a)@]" v Val.full_pp v_val
    in
    Fmt.pf fmt "[ @[%a@] ]" (Fmt.hashtbl ~sep:Fmt.comma pp_pair) subst

  (**
    Substitution in-place filter

    @param subst Target substitution
    @param filter Filtering function
    @return Filtered substitution
  *)
  let filter_in_place (subst : t) (filter : Var.t -> vt -> vt option) : unit =
    Hashtbl.filter_map_inplace filter subst

  (**
    Conversion to a list

    @params subst Target substitution
    @return List of bindings of the form (variable, value)
  *)
  let to_list (subst : t) : (Var.t * vt) list =
    Hashtbl.fold (fun v v_val ac -> (v, v_val) :: ac) subst []

  (**
    Substitution inside an expression

    @param subst Target substitution
    @param le Target expression
    @return Expression resulting from the substitution, with fresh locations created.
  *)
  let subst_in_expr (subst : t) ~(partial : bool) (le : Expr.t) : Expr.t =
    let find_in_subst
        (x : Var.t)
        (le_x_old : Expr.t)
        (make_new_x : unit -> Expr.t) : Expr.t =
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
      object
        inherit [_] Gil_syntax.Visitors.endo

        method! visit_LVar () this x =
          find_in_subst x this (fun () -> Expr.LVar (LVar.alloc ()))

        method! visit_ALoc () this x =
          find_in_subst x this (fun () -> Expr.ALoc (LVar.alloc ()))

        method! visit_PVar () this x =
          find_in_subst x this (fun () ->
              let lvar = LVar.alloc () in
              L.(
                verbose (fun m ->
                    m
                      "General: Subst in lexpr: PVar %s not in subst, \
                       generating fresh: %s"
                      x lvar));
              Expr.LVar lvar)
      end
    in
    mapper#visit_expr () le

  (**
    Optional substitution inside an expression

    @param subst Target substitution
    @param le Target expression
    @return Expression resulting from the substitution. No fresh locations are created.
  *)
  let subst_in_expr_opt (subst : t) (le : Expr.t) : Expr.t option =
    let f_before (le : Expr.t) =
      match (le : Expr.t) with
      | LVar x | ALoc x | PVar x -> (Option.map Val.to_expr (get subst x), false)
      | _ -> (Some le, true)
    in
    Expr.map_opt f_before None le

  let is_empty (subst : t) : bool = Hashtbl.length subst = 0

  let substitute_formula (subst : t) ~(partial : bool) : Formula.t -> Formula.t
      =
    let open Formula in
    let old_binders_substs = ref [] in
    let f_before a =
      match a with
      | ForAll (bt, _) ->
          let binders, _ = List.split bt in
          let binders_substs =
            List.map
              (fun x -> Option.map (fun x_v -> (x, x_v)) (get subst x))
              binders
          in
          let binders_substs =
            try
              List.map Option.get
                (List.filter (fun x -> not (x = None)) binders_substs)
            with _ -> raise (Failure "DEATH. asrt_substitution")
          in
          old_binders_substs := binders_substs;
          List.iter (fun x -> put subst x (Val.from_lvar_name x)) binders;
          (a, true)
      | _ -> (a, true)
    in
    let f_after a =
      match a with
      | ForAll _ ->
          List.iter (fun (x, le_x) -> put subst x le_x) !old_binders_substs;
          a
      | _ -> a
    in
    map (Some f_before) (Some f_after) (Some (subst_in_expr subst ~partial))

  let substitute_asrt (subst : t) ~(partial : bool) : Asrt.t -> Asrt.t =
    Asrt.map (subst_in_expr subst ~partial) (substitute_formula subst ~partial)

  let substitute_slcmd (subst : t) ~(partial : bool) : SLCmd.t -> SLCmd.t =
    SLCmd.map (substitute_asrt subst ~partial) (subst_in_expr subst ~partial)

  let substitute_lcmd (subst : t) ~(partial : bool) : LCmd.t -> LCmd.t =
    LCmd.map
      (subst_in_expr subst ~partial)
      (substitute_formula subst ~partial)
      (substitute_slcmd subst ~partial)
end
