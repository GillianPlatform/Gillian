type err_t = Symbol_not_found of string [@@deriving show]

module Make (Def_value : sig
  type t
  type vt
  type lt

  val pp : Format.formatter -> t -> unit
  val to_expr : t -> Gil_syntax.Expr.t
  val of_expr : Gil_syntax.Expr.t -> t
  val expr_to_vt : Gil_syntax.Expr.t -> vt
  val vt_to_expr : vt -> Gil_syntax.Expr.t
  val of_lt : lt -> t
end) (Delayed_hack : sig
  type 'a t

  val ( #== ) : Def_value.t -> Def_value.t -> Gil_syntax.Formula.t list

  val return :
    ?learned:Gil_syntax.Formula.t list ->
    ?learned_types:(string * Gil_syntax.Type.t) list ->
    'a ->
    'a t

  val resolve_or_create_lt : Def_value.lt -> string t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end) =
struct
  module GUtils = Gillian.Utils

  type nonrec err_t = err_t

  let pp_err_t = pp_err_t
  let show_err_t = show_err_t

  type def = FunDef of Def_value.t | GlobVar of Def_value.t

  type t = {
    symb : (string, string) PMap.t;  (** maps symbols to loc names *)
    defs : (string, def) PMap.t;  (** maps loc names to definitions *)
  }

  let find_opt x s = try Some (PMap.find x s) with Not_found -> None

  let find_symbol genv sym =
    try Ok (PMap.find sym genv.symb)
    with Not_found -> Error (Symbol_not_found sym)

  let set_symbol genv sym block =
    let open Delayed_hack in
    try
      let cur_symb = PMap.find sym genv.symb in
      let cur_symb_e = Gil_syntax.Expr.loc_from_loc_name cur_symb in
      let learned =
        (Def_value.of_lt block) #== (Def_value.of_expr cur_symb_e)
      in
      return ~learned genv
    with Not_found ->
      let+ block = Delayed_hack.resolve_or_create_lt block in
      let symb = PMap.add sym block genv.symb in
      { genv with symb }

  let find_def genv block = PMap.find block genv.defs

  let set_def genv block def =
    try
      let cur_def = PMap.find block genv.defs in
      match (def, cur_def) with
      | GlobVar a, GlobVar b | FunDef a, FunDef b ->
          let open Delayed_hack in
          return ~learned:a #== b genv
      | _ ->
          failwith
            "Equality between a global variable and a function definition"
    with Not_found ->
      let defs = PMap.add block def genv.defs in
      Delayed_hack.return { genv with defs }

  let empty = { symb = PMap.empty; defs = PMap.empty }

  (** Serialization of definitions *)
  let serialize_def def =
    let open Gil_syntax in
    let expr =
      match def with
      | FunDef fname ->
          Expr.EList [ Lit (String "function"); Def_value.to_expr fname ]
      | GlobVar vname ->
          EList [ Lit (String "variable"); Def_value.to_expr vname ]
    in
    Def_value.expr_to_vt expr

  let deserialize_def sdef =
    let open Gillian.Gil_syntax.Literal in
    let sdef_expr = Def_value.vt_to_expr sdef in
    match sdef_expr with
    | EList [ Lit (String "function"); fname ] ->
        FunDef (Def_value.of_expr fname)
    | EList [ Lit (String "variable"); vname ] ->
        GlobVar (Def_value.of_expr vname)
    | _ ->
        failwith
          (Format.asprintf "Invalid global definition : %a"
             Gil_syntax.Expr.full_pp sdef_expr)

  (* Pretty printing *)

  let pp_def fmt def =
    match def with
    | FunDef f -> Format.fprintf fmt "(Function %a)" Def_value.pp f
    | GlobVar v -> Format.fprintf fmt "(Variable %a)" Def_value.pp v

  let pp fmt genv =
    let not_printed = ref 0 in
    let pp_one ft s l =
      try
        let d = find_def genv l in
        Format.fprintf ft "'%s' -> %s -> %a@\n" s l pp_def d
      with Not_found -> Format.fprintf ft "'%s' -> %s -> UNKNOWN@\n" s l
    in
    if !Kconfig.hide_genv then Format.fprintf fmt "{@[<v 2>@\nHIDDEN@]@\n}"
    else
      let () = Format.fprintf fmt "{@[<v 2>@\n" in
      PMap.iter (fun s l -> pp_one fmt s l) genv.symb;
      Format.fprintf fmt "There are %i unimplemented external functions@]@\n"
        !not_printed;
      Format.fprintf fmt "}"

  let substitution subst genv =
    let open Gillian.Gil_syntax in
    let open Gillian.Symbolic in
    let substitute_in_def def =
      match def with
      | FunDef f ->
          let f_e = Def_value.to_expr f in
          let substituted = Subst.subst_in_expr subst ~partial:true f_e in
          let substituted = Def_value.of_expr substituted in
          FunDef substituted
      | GlobVar f ->
          let f_e = Def_value.to_expr f in
          let substituted = Subst.subst_in_expr subst ~partial:true f_e in
          let substituted = Def_value.of_expr substituted in
          GlobVar substituted
    in
    let with_substituted_defs =
      { genv with defs = PMap.map substitute_in_def genv.defs }
    in
    let aloc_subst =
      Subst.filter subst (fun var _ ->
          match var with
          | ALoc _ -> true
          | _ -> false)
    in
    let rename_val old_loc new_loc pmap =
      PMap.map (fun k -> if String.equal old_loc k then new_loc else k) pmap
    in
    let rename_key old_loc new_loc pmap =
      match find_opt old_loc pmap with
      | None -> pmap
      | Some d -> PMap.add new_loc d (PMap.remove old_loc pmap)
    in
    (* Then we substitute the locations *)
    Subst.fold aloc_subst
      (fun old_loc new_loc cgenv ->
        let old_loc =
          match old_loc with
          | ALoc loc -> loc
          | _ -> raise (Failure "Impossible by construction")
        in
        let new_loc =
          match new_loc with
          | Lit (Loc loc) | ALoc loc -> loc
          | _ ->
              failwith
                (Format.asprintf "Heap substitution failed for loc : %a" Expr.pp
                   new_loc)
        in
        {
          symb = rename_val old_loc new_loc cgenv.symb;
          defs = rename_key old_loc new_loc cgenv.defs;
        })
      with_substituted_defs

  (** This function returns the assertions as well as a list of
      locations corresponding to functions declaration, so that memory knows not
      to duplicate that ressource. *)
  let assertions genv =
    let build_asrt s loc def =
      match def with
      | FunDef fname ->
          let f_ser = Def_value.to_expr fname in
          (true, Predicates.Others.glob_fun ~symb:s ~fname:f_ser)
      | GlobVar vname ->
          let v_ser = Def_value.to_expr vname in
          let loc = Gil_syntax.Expr.loc_from_loc_name loc in
          ( false,
            Predicates.Others.glob_var_unallocated_loc ~symb:s ~loc ~vname:v_ser
          )
    in
    let assert_symb symb loc =
      let def = find_def genv loc in
      build_asrt symb loc def
    in
    PMap.foldi
      (fun sym loc (locs, asrts) ->
        let is_fun, asrt = assert_symb sym loc in
        let new_locs = if is_fun then loc :: locs else locs in
        (new_locs, asrt :: asrts))
      genv.symb ([], [])
end

module Concrete =
  Make
    (struct
      open Gil_syntax

      type t = string
      type vt = Literal.t
      type lt = string

      let pp = Fmt.string
      let to_expr s = Expr.Lit (String s)

      let of_expr = function
        | Expr.Lit (String s) -> s
        | e -> Fmt.failwith "Invalid function name: %a" Expr.pp e

      let rec expr_to_vt = function
        | Expr.EList l -> Literal.LList (List.map expr_to_vt l)
        | Lit l -> l
        | e -> Fmt.failwith "The following should be concrete : %a" Expr.pp e

      let rec vt_to_expr = function
        | Literal.LList ll -> Expr.EList (List.map vt_to_expr ll)
        | l -> Lit l

      let of_lt x = x
    end)
    (struct
      type 'a t = 'a

      let ( let+ ) a f = f a
      let resolve_or_create_lt x = x
      let return ?learned:_ ?learned_types:_ x = x
      let ( #== ) _ _ = []
    end)

module Symbolic =
  Make
    (struct
      open Gil_syntax

      type t = Expr.t
      type vt = Expr.t
      type lt = Expr.t

      let pp = Expr.pp
      let to_expr e = e
      let of_expr e = e
      let expr_to_vt e = e

      let vt_to_expr =
        let rec lift_lit = function
          | Literal.LList ll -> Expr.EList (List.map lift_lit ll)
          | l -> Lit l
        in
        function
        | Expr.Lit l -> lift_lit l
        | e -> e

      let of_lt x = x
    end)
    (struct
      include Gillian.Monadic.Delayed

      let ( let+ ) = map

      let ( #== ) a b =
        let open Gil_syntax.Formula.Infix in
        [ a #== b ]

      let resolve_or_create_lt lvar_loc : string t =
        let open Syntax in
        let* loc_name = resolve_loc lvar_loc in
        match loc_name with
        | None ->
            let new_loc_name = Gil_syntax.ALoc.alloc () in
            let learned = lvar_loc #== (ALoc new_loc_name) in
            Logging.verbose (fun fmt ->
                fmt "Couldn't resolve loc %a, created %s" Gil_syntax.Expr.pp
                  lvar_loc new_loc_name);
            return ~learned new_loc_name
        | Some l ->
            Logging.verbose (fun fmt ->
                fmt "Resolved %a as %s" Gil_syntax.Expr.pp lvar_loc l);
            return l
    end)
