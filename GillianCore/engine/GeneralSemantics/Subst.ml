(**
    Interface for GIL Substitutions.
    GIL substitutions are mappings from GIL Variables to GIL Values.
    GIL substitutions are mutable.
*)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL substitutions *)
  type t

  val init : (Var.t * vt) list -> t
  (** Substitution constructor, with a list of bindings of the form (variable, value) *)

  val is_empty : t -> bool
  (** Is the substitution empty? *)

  val clear : t -> unit
  (** Reset substitution *)

  val domain : t -> (Var.t -> bool) option -> Var.Set.t
  (** Domain of the substitution *)

  val range : t -> vt list
  (** Range of the substitution *)

  val get : t -> Var.t -> vt option
  (** Substitution lookup *)

  val add : t -> Var.t -> vt -> unit
  (** Substitution incremental update *)

  val put : t -> Var.t -> vt -> unit
  (** Substitution update *)

  val mem : t -> Var.t -> bool
  (** Substitution membership *)

  val copy : t -> t
  (** Substitution copy *)

  val extend : t -> (Var.t * vt) list -> unit
  (** Substitution extension with a list of bindings *)

  val merge_left : t -> t -> unit
  (** Substution merge into left *)

  val compatible : t -> t -> bool
  (** Compatible substitutions *)

  val filter : t -> (Var.t -> vt -> bool) -> t
  (** Substitution filter *)

  val projection : t -> Var.Set.t -> t
  (** Substitution variable filter *)

  val iter : t -> (Var.t -> vt -> unit) -> unit
  (** Substitution iterator *)

  val fold : t -> (Var.t -> vt -> 'a -> 'a) -> 'a -> 'a
  (** Substitution fold *)

  val pp : Format.formatter -> t -> unit
  (** Pretty Printer *)

  val filter_in_place : t -> (Var.t -> vt -> vt option) -> unit

  val to_list : t -> (Var.t * vt) list
  (** Convert substitution to list *)

  val subst_in_expr : t -> partial:bool -> Expr.t -> Expr.t
  (** Substitution inside a logical expression *)

  val subst_in_expr_opt : t -> Expr.t -> Expr.t option
  (** Optional substitution inside a logical expression *)

  val to_ssubst : t -> (Var.t * Expr.t) list
  (** Convert to a symbolic substitution *)

  val to_formulae : t -> Formula.t list
  (** creates a list of equalities from the substitution table
    before substitution_to_list *)

  val substitute_formula : t -> partial:bool -> Formula.t -> Formula.t

  val substitute_asrt : t -> partial:bool -> Asrt.t -> Asrt.t

  val substitute_slcmd : t -> partial:bool -> SLCmd.t -> SLCmd.t

  val substitute_lcmd : t -> partial:bool -> LCmd.t -> LCmd.t
end

module Make (Val : Val.S) : S with type vt = Val.t = struct
  open Containers
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
      | None        -> fun x -> false
    in
    Hashtbl.fold
      (fun k v ac -> if filter k then ac else Var.Set.add k ac)
      subst Var.Set.empty

  (**
    Substitution range

    @param subst Target substitution
    @return Range of the substitution
  *)
  let range (subst : t) : vt list =
    Hashtbl.fold (fun v v_val ac -> v_val :: ac) subst []

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
        | true  -> Some v_val
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
        (x : Var.t) (le_x_old : Expr.t) (make_new_x : unit -> Expr.t) : Expr.t =
      match get subst x with
      | Some v -> Val.to_expr v
      | None   -> (
          if partial then le_x_old
          else
            let new_le_x = make_new_x () in
            match Val.from_expr new_le_x with
            | Some sv ->
                put subst x sv;
                new_le_x
            | None    -> raise (Failure "DEATH. subst_in_expr") )
    in

    let f_before (le : Expr.t) =
      let open Generators in
      match (le : Expr.t) with
      | LVar x ->
          (find_in_subst x le (fun () -> Expr.LVar (LVar.alloc ())), false)
      | ALoc x ->
          (find_in_subst x le (fun () -> Expr.ALoc (LVar.alloc ())), false)
      | PVar x ->
          ( find_in_subst x le (fun () ->
                let lvar = LVar.alloc () in
                L.(
                  verboser (fun m ->
                      m
                        "General: Subst in lexpr: PVar %s not in subst, \
                         generating fresh: %s"
                        x lvar));
                Expr.LVar lvar),
            false )
      | _      -> (le, true)
    in
    Expr.map f_before None le

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
      | _                        -> (Some le, true)
    in
    Expr.map_opt f_before None le

  (**
    Conversion to a symbolic substitution

    @params subst Target substitution
    @return List of bindings of the form (variable, logical expression)
  *)
  let to_ssubst (subst : t) : (Var.t * Expr.t) list =
    List.map (fun (x, v_x) -> (x, Val.to_expr v_x)) (to_list subst)

  let compatible (subst : t) (new_subst : t) : bool =
    Hashtbl.fold
      (fun x v ac ->
        if not ac then false
        else if Hashtbl.mem new_subst x then v = Hashtbl.find new_subst x
        else true)
      subst true

  let is_empty (subst : t) : bool = Hashtbl.length subst = 0

  let substitute_formula (subst : t) ~(partial : bool) (a : Formula.t) :
      Formula.t =
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
      | _              -> (a, true)
    in
    let f_after a =
      match a with
      | ForAll _ ->
          List.iter (fun (x, le_x) -> put subst x le_x) !old_binders_substs;
          a
      | _        -> a
    in
    map (Some f_before) (Some f_after) (Some (subst_in_expr subst ~partial)) a

  let substitute_asrt (subst : t) ~(partial : bool) (a : Asrt.t) : Asrt.t =
    Asrt.map None None
      (Some (subst_in_expr subst ~partial))
      (Some (substitute_formula subst ~partial))
      a

  let substitute_slcmd (subst : t) ~(partial : bool) (lcmd : SLCmd.t) : SLCmd.t
      =
    SLCmd.map None
      (Some (substitute_asrt subst ~partial))
      (Some (subst_in_expr subst ~partial))
      lcmd

  let substitute_lcmd (subst : t) ~(partial : bool) (lcmd : LCmd.t) : LCmd.t =
    LCmd.map None
      (Some (subst_in_expr subst ~partial))
      (Some (substitute_formula subst ~partial))
      (Some (substitute_slcmd subst ~partial))
      lcmd

  (** creates a list of equalities from the substitution table
    before substitution_to_list *)
  let to_formulae (subst : t) : Formula.t list =
    List.map
      (fun (x, x_val) ->
        let open Val in
        Formula.Eq (to_expr (from_lvar_name x), to_expr x_val))
      (to_list subst)
end
