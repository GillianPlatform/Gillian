include SVal
open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

exception NotACompCertValue of Expr.t

module Patterns = struct
  open Formula.Infix

  let int_typ, float_typ, single_typ, long_typ =
    let open Expr in
    let open CConstants.VTypes in
    let num_typ typ_str x =
      (typeof x) #== (type_ ListType)
      #&& ((list_length x) #== (num 2.))
      #&& ((list_nth x 0) #== (string typ_str))
      #&& ((typeof (list_nth x 1)) #== (type_ NumberType))
    in
    ( num_typ int_type,
      num_typ float_type,
      num_typ single_type,
      num_typ long_type )

  let undefined x = x #== (Expr.Lit Undefined)

  let obj x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (num 2.))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ NumberType))
end

let of_gil_expr sval_e =
  let open Formula.Infix in
  let open Patterns in
  Logging.verbose (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
  match%ent sval_e with
  | undefined  -> DO.some SUndefined
  | obj        ->
      let loc_expr = Expr.list_nth sval_e 0 in
      let ofs = Expr.list_nth sval_e 1 in
      let* loc_opt = Delayed.resolve_loc loc_expr in
      let loc, learned =
        match loc_opt with
        | Some l -> (l, [])
        | None   ->
            let aloc = ALoc.alloc () in
            let learned = [ loc_expr #== (ALoc aloc) ] in
            (aloc, learned)
      in
      DO.some ~learned (Sptr (loc, ofs))
  | int_typ    -> DO.some (SVint (Expr.list_nth sval_e 1))
  | float_typ  -> DO.some (SVfloat (Expr.list_nth sval_e 1))
  | long_typ   -> DO.some (SVlong (Expr.list_nth sval_e 1))
  | single_typ -> DO.some (SVsingle (Expr.list_nth sval_e 1))
  | _          -> DO.none ()

let of_gil_expr_exn sval_e =
  let* value_opt = of_gil_expr sval_e in
  match value_opt with
  | None       -> raise (NotACompCertValue sval_e)
  | Some value -> Delayed.return value

let to_gil_expr_undelayed = to_gil_expr

let to_gil_expr sval =
  let exp, typings = to_gil_expr_undelayed sval in
  let typing_pfs =
    List.map
      (fun (e, t) ->
        let open Expr in
        let open Formula.Infix in
        (typeof e) #== (type_ t))
      typings
  in
  Delayed.return ~learned:typing_pfs exp

let sure_is_zero = function
  | SVint (Lit (Num 0.))
  | SVlong (Lit (Num 0.))
  | SVfloat (Lit (Num 0.))
  | SVsingle (Lit (Num 0.)) -> true
  | _ -> false

module SVArray = struct
  type nonrec t = Conc of t list | Abst of Expr.t | AllUndef | AllZeros

  let empty = Conc []

  let is_empty =
    let open Formula.Infix in
    function
    | Conc [] -> Formula.True
    | Abst e  -> (Expr.list_length e) #== (Expr.num 0.)
    | _       -> False

  let sure_is_all_zeros = function
    | Conc l   -> List.for_all sure_is_zero l
    | AllZeros -> true
    | _        -> false

  let equal arr_a arr_b =
    match (arr_a, arr_b) with
    | Conc la, Conc lb -> List.for_all2 (fun a b -> SVal.equal a b) la lb
    | Abst a, Abst b -> Expr.equal a b
    | AllUndef, AllUndef | AllZeros, AllZeros -> true
    | _ -> false

  let conc_to_abst_undelayed conc =
    let rev_l, gamma =
      List.fold_left
        (fun (acc, gamma) sval ->
          let new_el, new_gamma = SVal.to_gil_expr sval in
          (new_el :: acc, new_gamma @ gamma))
        ([], []) conc
    in
    let learned =
      List.map
        (let open Formula.Infix in
        fun (e, t) -> (Expr.typeof e) #== (Expr.type_ t))
        gamma
    in
    (Expr.EList (List.rev rev_l), learned)

  let conc_to_abst conc =
    let e, learned = conc_to_abst_undelayed conc in
    Delayed.return ~learned e

  let rec array_sub arr o len : t Delayed.t =
    let open Delayed.Syntax in
    match arr with
    | AllZeros -> Delayed.return AllZeros
    | AllUndef -> Delayed.return AllUndef
    | Abst e   -> Delayed.return (Abst (Expr.list_sub ~lst:e ~start:o ~size:len))
    | Conc l   -> (
        match (o, len) with
        | Lit (Num o), Lit (Num len) ->
            Delayed.return
              (Conc
                 (Option.get (* This error should probably be handled better *)
                    (Gillian.Utils.List_utils.list_sub l (int_of_float o)
                       (int_of_float len))))
        | _                          ->
            let* abst = conc_to_abst l in
            array_sub (Abst abst) o len)

  let array_cat (arr_a : t) (arr_b : t) =
    let open Delayed.Syntax in
    match (arr_a, arr_b) with
    | arr_a, arr_b when sure_is_all_zeros arr_a && sure_is_all_zeros arr_b ->
        Delayed.return AllZeros
    | Abst a, Abst b -> Delayed.return (Abst (Expr.list_cat a b))
    | Conc a, Conc b -> Delayed.return (Conc (a @ b))
    | Abst a, Conc b ->
        let+ b = conc_to_abst b in
        Abst (Expr.list_cat a b)
    | Conc a, Abst b ->
        let+ a = conc_to_abst a in
        Abst (Expr.list_cat a b)
    | AllZeros, _ | _, AllZeros | AllUndef, _ | _, AllUndef ->
        Delayed.return AllUndef

  let array_cons (el : SVal.t) arr = array_cat (Conc [ el ]) arr

  let array_append arr el = array_cat arr (Conc [ el ])

  let pp fmt = function
    | Conc svl -> (Fmt.Dump.list SVal.pp) fmt svl
    | Abst e   -> Expr.pp fmt e
    | AllUndef -> Fmt.string fmt "AllUndef"
    | AllZeros -> Fmt.string fmt "AllZeros"

  let undefined_pf arr_exp =
    let open Formula.Infix in
    let i = LVar.alloc () in
    let i_e = Expr.LVar i in
    let zero = Expr.num 0. in
    forall [ (i, Some NumberType) ]
      zero #<= i_e
      #&& (i_e #< (Expr.list_length arr_exp))
      #=> ((Expr.list_nth_e arr_exp i_e) #== (Lit Undefined))

  let zeros_pf ~chunk arr_exp =
    let open Formula.Infix in
    let is_zero e =
      let typ = Chunk.type_of chunk in
      let typ_str =
        match typ with
        | Tlong   -> CConstants.VTypes.long_type
        | Tfloat  -> CConstants.VTypes.float_type
        | Tsingle -> CConstants.VTypes.single_type
        | Tint    -> CConstants.VTypes.int_type
        | _       -> failwith "invalid chunk"
      in
      (Expr.typeof e) #== (Expr.type_ ListType)
      #&& ((Expr.list_nth e 0) #== (Expr.string typ_str))
      #&& ((Expr.list_nth e 1) #== (Expr.num 0.))
    in
    let i = LVar.alloc () in
    let i_e = Expr.LVar i in
    let zero = Expr.num 0. in
    forall [ (i, Some NumberType) ]
      zero #<= i_e
      #&& (i_e #< (Expr.list_length arr_exp))
      #=> (is_zero (Expr.list_nth_e arr_exp i_e))

  let sval_to_gil_expr_undelayed = to_gil_expr_undelayed

  let rec to_gil_expr_undelayed ~chunk ~range svarr =
    let chunk_size = Chunk.size_expr chunk in
    let size =
      let open Expr.Infix in
      let low, high = range in
      (high -. low) /. chunk_size
    in
    let f_of_all_same ~describing_pf ~concrete_single =
      match size with
      | Lit (Num n) ->
          let arr =
            Conc (Utils.List_utils.make (int_of_float n) concrete_single)
          in
          to_gil_expr_undelayed ~chunk ~range arr
      | _           ->
          let open Formula.Infix in
          let arr = LVar.alloc () in
          let arr_e = Expr.LVar arr in
          let learned =
            let open Expr in
            [
              (typeof arr_e) #== (type_ ListType);
              (list_length arr_e) #== size;
              describing_pf arr_e;
            ]
          in
          (arr_e, learned)
    in
    match svarr with
    | Abst e   ->
        let open Formula.Infix in
        let learned =
          [
            (Expr.typeof e) #== (Expr.type_ ListType);
            (Expr.list_length e) #== size;
          ]
        in
        (e, learned)
    | Conc tl  ->
        let exps, typings =
          List.split (List.map sval_to_gil_expr_undelayed tl)
        in
        let exp = Expr.list exps in
        let learned =
          List.map
            (fun (e, t) ->
              let open Expr in
              let open Formula.Infix in
              (typeof e) #== (type_ t))
            (List.concat typings)
        in
        (exp, learned)
    | AllZeros ->
        f_of_all_same ~concrete_single:(zero_of_chunk chunk)
          ~describing_pf:(zeros_pf ~chunk)
    | AllUndef ->
        f_of_all_same ~concrete_single:SUndefined ~describing_pf:undefined_pf

  let to_gil_expr ~chunk ~range (svarr : t) : Expr.t Delayed.t =
    let e, learned = to_gil_expr_undelayed ~chunk ~range svarr in
    Delayed.return ~learned e

  let of_gil_expr_exn expr = Abst expr

  (* type nonrec t = Conc of t list | Abst of Expr.t | AllUndef | AllZeros *)

  let subst ~le_subst ~sval_subst t =
    match t with
    | Conc l      -> Conc (List.map sval_subst l)
    | Abst e as a -> if le_subst e == e then a else Abst (le_subst e)
    | AllUndef    -> AllUndef
    | AllZeros    -> AllZeros
end
