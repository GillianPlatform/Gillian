(**
    Interface for GIL Extended Substitutions (e-substitutions).
    GIL e-substitutions are mappings from GIL Variables to GIL Values.
    GIL e-substitutions are mutable.
*)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL e-substitutions *)
  type t [@@deriving yojson]

  (** E-substitution constructor, with a list of bindings of the form (variable, value) *)
  val init : (Expr.t * vt) list -> t

  (** Is the e-substitution empty? *)
  val is_empty : t -> bool

  (** Reset e-substitution *)
  val clear : t -> unit

  (** Domain of the e-substitution *)
  val domain : t -> (Expr.t -> bool) option -> Expr.Set.t

  (** Range of the e-substitution *)
  val range : t -> vt list

  (** Substitution lookup *)
  val get : t -> Expr.t -> vt option

  (** Substitution incremental update *)
  val add : t -> Expr.t -> vt -> unit

  (** Substitution update *)
  val put : t -> Expr.t -> vt -> unit

  (** Substitution membership *)
  val mem : t -> Expr.t -> bool

  (** Substitution copy *)
  val copy : t -> t

  (** Substitution extension with a list of bindings *)
  val extend : t -> (Expr.t * vt) list -> unit

  (** Substution merge into left *)
  val merge_left : t -> t -> unit

  (** Substitution filter *)
  val filter : t -> (Expr.t -> vt -> bool) -> t

  (** Substitution variable filter *)
  val projection : t -> Expr.Set.t -> t

  (** Substitution iterator *)
  val iter : t -> (Expr.t -> vt -> unit) -> unit

  (** Substitution fold *)
  val fold : t -> (Expr.t -> vt -> 'a -> 'a) -> 'a -> 'a

  (** Pretty Printer *)
  val pp : Format.formatter -> t -> unit

  (** Full pretty Printer *)
  val full_pp : Format.formatter -> t -> unit

  (** Selective Pretty Printer *)
  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit

  val filter_in_place : t -> (Expr.t -> vt -> vt option) -> unit

  (** Convert substitution to list *)
  val to_list : t -> (Expr.t * vt) list

  val to_list_pp : t -> (string * string) list

  (** Substitution inside a logical expression *)
  val subst_in_expr : t -> partial:bool -> Expr.t -> Expr.t

  (** Optional substitution inside a logical expression *)
  val subst_in_expr_opt : t -> Expr.t -> Expr.t option

  val substitute_formula : t -> partial:bool -> Formula.t -> Formula.t
  val substitute_in_formula_opt : t -> Formula.t -> Formula.t option
  val substitute_asrt : t -> partial:bool -> Asrt.t -> Asrt.t
  val substitute_slcmd : t -> partial:bool -> SLCmd.t -> SLCmd.t
  val substitute_lcmd : t -> partial:bool -> LCmd.t -> LCmd.t
end

module Make (Val : Val.S) : S with type vt = Val.t = struct
  open Containers
  module L = Logging

  (** Type of GIL values *)
  type vt = Val.t [@@deriving yojson]

  (** Type of GIL substitutions, implemented as hashtables *)
  type t = (Expr.t, vt) Hashtbl.t [@@deriving yojson]

  (**
    Substitution constructor

    @param vars_les Bindings of the form (variable, value)
    @return Substitution with the given bindings
  *)
  let init (exprs_les : (Expr.t * vt) list) : t =
    let subst = Hashtbl.create Config.big_tbl_size in
    List.iter
      (fun (e, e_val) ->
        let () = assert (Expr.is_unifiable e) in
        Hashtbl.replace subst e e_val)
      exprs_les;
    subst

  let clear (subst : t) : unit = Hashtbl.clear subst

  (**
    Substitution domain

    @param subst Target substitution
    @param filter_out Optional filtering function
    @return Domain of the (filtered) substitution
  *)
  let domain (subst : t) (filter_out : (Expr.t -> bool) option) : Expr.Set.t =
    let filter =
      match filter_out with
      | Some filter -> filter
      | None -> fun _ -> false
    in
    Hashtbl.fold
      (fun e _ ac -> if filter e then ac else Expr.Set.add e ac)
      subst Expr.Set.empty

  (**
    Substitution range

    @param subst Target substitution
    @return Range of the substitution
  *)
  let range (subst : t) : vt list =
    Hashtbl.fold (fun _ e_val ac -> e_val :: ac) subst []

  (**
    Substitution lookup

    @param subst Target substitution
    @param x Target variable
    @return Resulting (optional) value
  *)
  let get (subst : t) (e : Expr.t) : vt option = Hashtbl.find_opt subst e

  (**
    Substitution incremental update

    @param subst Target substitution
    @param e Target expression
    @param v Target value
  *)
  let add (subst : t) (e : Expr.t) (v : vt) =
    let () = assert (Expr.is_unifiable e) in
    Hashtbl.add subst e v

  (**
    Substitution update

    @param subst Target substitution
    @param e Target variable
    @param v Target value
  *)
  let put (subst : t) (e : Expr.t) (v : vt) =
    let () = assert (Expr.is_unifiable e) in
    Hashtbl.replace subst e v

  (**
    Substitution membership

    @param subst Target substitution
    @param e Target variable
    @return Returns true if the variable is in the domain of the substitution, and false otherwise
  *)
  let mem (subst : t) (e : Expr.t) =
    let () = assert (Expr.is_unifiable e) in
    Hashtbl.mem subst e

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
  let extend (subst : t) (exprs_les : (Expr.t * vt) list) : unit =
    List.iter
      (fun (e, e_val) ->
        let () = assert (Expr.is_unifiable e) in
        Hashtbl.replace subst e e_val)
      exprs_les

  (**
    Substitution iterator

    @param subst Target substitution
    @param f Iterator function
  *)
  let iter (subst : t) (f : Expr.t -> vt -> unit) : unit = Hashtbl.iter f subst

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
    Hashtbl.iter (fun e e_val -> Hashtbl.replace subst e e_val) subst_ext

  (**
    Substitution filter

    @param subst Target substitution
    @param filter Filtering function
    @return The new, filtered substitution
  *)
  let filter (subst : t) (filter : Expr.t -> vt -> bool) : t =
    let new_subst = copy subst in
    Hashtbl.filter_map_inplace
      (fun e e_val ->
        match filter e e_val with
        | true -> Some e_val
        | false -> None)
      new_subst;
    new_subst

  (**
    Substitution filter by variables

    @param subst Target substitution
    @param vars Variables to save
    @return The new, filtered substitution
  *)
  let projection (subst : t) (exprs : Expr.Set.t) : t =
    filter subst (fun e _ -> Expr.Set.mem e exprs)

  (**
    Substitution pretty_printer

    @param fmt Formatter
    @param subst Target substitution
    @return unit
  *)
  let pp fmt (subst : t) =
    let pp_pair fmt (e, e_val) =
      Fmt.pf fmt "@[<h>(%a: %a)@]" Expr.pp e Val.pp e_val
    in
    let bindings = fold subst (fun x t ac -> (x, t) :: ac) [] in
    let bindings =
      List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings
    in
    Fmt.pf fmt "@[<hv 2>[ %a ]@]" (Fmt.list ~sep:Fmt.comma pp_pair) bindings

  let pp_by_need (filter_vars : Containers.SS.t) fmt (subst : t) =
    let pp_pair fmt (e, e_val) =
      Fmt.pf fmt "@[<h>(%a: %a)@]" Expr.pp e Val.pp e_val
    in
    let bindings = fold subst (fun x t ac -> (x, t) :: ac) [] in
    let bindings =
      List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings
    in
    let bindings =
      List.filter
        (fun (v, _) ->
          let pvars, lvars, alocs =
            (Expr.pvars v, Expr.lvars v, Expr.alocs v)
          in
          Containers.SS.inter
            (SS.union pvars (SS.union lvars alocs))
            filter_vars
          <> Containers.SS.empty)
        bindings
    in
    Fmt.pf fmt "@[<hv 2>[ %a ]@]" (Fmt.list ~sep:Fmt.comma pp_pair) bindings

  (**
    Substitution full pretty_printer

    @param fmt Formatter
    @param subst Target substitution
    @return unit
  *)
  let full_pp fmt (subst : t) =
    let pp_pair fmt (e, e_val) =
      Fmt.pf fmt "@[<h>(%a: %a)@]" Expr.pp e Val.full_pp e_val
    in
    Fmt.pf fmt "[ @[%a@] ]" (Fmt.hashtbl ~sep:Fmt.comma pp_pair) subst

  (**
    Substitution in-place filter

    @param subst Target substitution
    @param filter Filtering function
    @return Filtered substitution
  *)
  let filter_in_place (subst : t) (filter : Expr.t -> vt -> vt option) : unit =
    Hashtbl.filter_map_inplace filter subst

  (**
    Conversion to a list

    @params subst Target substitution
    @return List of bindings of the form (variable, value)
  *)
  let to_list (subst : t) : (Expr.t * vt) list =
    Hashtbl.fold (fun e e_val ac -> (e, e_val) :: ac) subst []

  let to_list_pp (subst : t) : (string * string) list =
    subst |> to_list
    |> List.map (fun (e, e_val) ->
           (Fmt.str "%a" Expr.pp e, Fmt.str "%a" Val.pp e_val))

  let substitutor =
    object (self)
      inherit [_] Visitors.endo as super
      val empty_subst = init []
      val mutable self_subst = init []
      val mutable self_partial = true

      method init ~partial ~subst =
        self_subst <- subst;
        self_partial <- partial

      method clear () = self_subst <- empty_subst

      method find_in_subst ~make_new_x e =
        match get self_subst e with
        | Some v -> Val.to_expr v
        | None -> (
            if self_partial then e
            else
              let new_le_x = make_new_x () in
              match Val.from_expr new_le_x with
              | Some sv ->
                  put self_subst e sv;
                  new_le_x
              | None ->
                  raise
                    (Failure
                       "DEATH: subst_in_expr: Cannot convert fresh expression \
                        to a value"))

      method! visit_'annot _ (this : Annot.t) = this
      method! visit_'label _ (this : int) = this

      method! visit_LVar () this _ =
        self#find_in_subst
          ~make_new_x:(fun () -> Expr.LVar (LVar.alloc ()))
          this

      method! visit_ALoc () this _ =
        self#find_in_subst
          ~make_new_x:(fun () -> Expr.ALoc (ALoc.alloc ()))
          this

      method! visit_PVar () this x =
        self#find_in_subst
          ~make_new_x:(fun () ->
            let lvar = LVar.alloc () in
            L.(
              verbose (fun m ->
                  m
                    "General: Subst in lexpr: PVar %s not in subst, generating \
                     fresh: %s"
                    x lvar));
            Expr.LVar lvar)
          this

      method! visit_UnOp () this unop e =
        match (unop, e) with
        | (LstLen, PVar _ | LstLen, LVar _) when mem self_subst this ->
            Val.to_expr (Option.get (get self_subst this))
        | _ -> super#visit_UnOp () this unop e

      method! visit_ForAll () this bt form =
        let binders, _ = List.split bt in
        let binders_substs =
          List.filter_map
            (fun x ->
              Option.map (fun x_v -> (x, x_v)) (get self_subst (LVar x)))
            binders
        in
        List.iter
          (fun x -> put self_subst (LVar x) (Val.from_lvar_name x))
          binders;
        let new_formula = self#visit_formula () form in
        List.iter (fun (x, le_x) -> put self_subst (LVar x) le_x) binders_substs;
        if new_formula == form then this else ForAll (bt, new_formula)
    end

  (**
    Substitution inside an expression

    @param subst Target substitution
    @param le Target expression
    @return Expression resulting from the substitution, with fresh locations created.
  *)
  let subst_in_expr (subst : t) ~(partial : bool) (le : Expr.t) : Expr.t =
    substitutor#init ~partial ~subst;
    let res = substitutor#visit_expr () le in
    substitutor#clear ();
    res

  (**
    Optional substitution inside an expression

    @param subst Target substitution
    @param le Target expression
    @return Expression resulting from the substitution. No fresh locations are created.
  *)
  let subst_in_expr_opt (subst : t) (le : Expr.t) : Expr.t option =
    let f_before (le : Expr.t) =
      match (le : Expr.t) with
      | LVar _ | ALoc _ | PVar _ ->
          (Option.map Val.to_expr (get subst le), false)
      | (UnOp (LstLen, PVar _) | UnOp (LstLen, LVar _)) when mem subst le ->
          (Option.map Val.to_expr (get subst le), false)
      | _ -> (Some le, true)
    in
    Expr.map_opt f_before None le

  let is_empty (subst : t) : bool = Hashtbl.length subst = 0

  let substitute_formula (subst : t) ~(partial : bool) (a : Formula.t) :
      Formula.t =
    substitutor#init ~partial ~subst;
    let res = substitutor#visit_formula () a in
    substitutor#clear ();
    res

  let substitute_in_formula_opt (subst : t) (a : Formula.t) : Formula.t option =
    let open Formula in
    let old_binders_substs = ref [] in
    let f_before a =
      match a with
      | ForAll (bt, _) ->
          let binders, _ = List.split bt in
          let binders_substs =
            List.map
              (fun x -> Option.map (fun x_v -> (x, x_v)) (get subst (LVar x)))
              binders
          in
          let binders_substs =
            try List.filter_map (fun x -> x) binders_substs
            with _ -> raise (Failure "DEATH. asrt_substitution")
          in
          old_binders_substs := binders_substs;
          List.iter (fun x -> put subst (LVar x) (Val.from_lvar_name x)) binders;
          (Some a, true)
      | _ -> (Some a, true)
    in
    let f_after a =
      match a with
      | ForAll _ ->
          List.iter
            (fun (x, le_x) -> put subst (LVar x) le_x)
            !old_binders_substs;
          a
      | _ -> a
    in
    map_opt (Some f_before) (Some f_after) (Some (subst_in_expr_opt subst)) a

  let substitute_asrt (subst : t) ~(partial : bool) (a : Asrt.t) : Asrt.t =
    substitutor#init ~partial ~subst;
    let res = substitutor#visit_assertion () a in
    substitutor#clear ();
    res

  let substitute_slcmd (subst : t) ~(partial : bool) (lcmd : SLCmd.t) : SLCmd.t
      =
    substitutor#init ~partial ~subst;
    let res = substitutor#visit_slcmd () lcmd in
    substitutor#clear ();
    res

  let substitute_lcmd (subst : t) ~(partial : bool) (lcmd : LCmd.t) : LCmd.t =
    substitutor#init ~partial ~subst;
    let res = substitutor#visit_lcmd () lcmd in
    substitutor#clear ();
    res
end
