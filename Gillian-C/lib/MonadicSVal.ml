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

module SVArray = struct
  type nonrec t = Conc of t list | Abst of Expr.t | AllUndef

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
            array_sub (Abst abst) o len )

  let array_cat (arr_a : t) (arr_b : t) =
    let open Delayed.Syntax in
    match (arr_a, arr_b) with
    | Abst a, Abst b            -> Delayed.return (Abst (Expr.list_cat a b))
    | Conc a, Conc b            -> Delayed.return (Conc (a @ b))
    | Abst a, Conc b            ->
        let+ b = conc_to_abst b in
        Abst (Expr.list_cat a b)
    | Conc a, Abst b            ->
        let+ a = conc_to_abst a in
        Abst (Expr.list_cat a b)
    | AllUndef, _ | _, AllUndef -> Delayed.return AllUndef

  let array_cons (el : SVal.t) arr = array_cat (Conc [ el ]) arr

  let array_append arr el = array_cat arr (Conc [ el ])

  let pp fmt = function
    | Conc svl -> (Fmt.Dump.list SVal.pp) fmt svl
    | Abst e   -> Expr.pp fmt e
    | AllUndef -> Fmt.string fmt "AllUndef"

  let undefined_pf arr_exp =
    let open Formula.Infix in
    let i = LVar.alloc () in
    let i_e = Expr.LVar i in
    let zero = Expr.num 0. in
    forall [ (i, Some NumberType) ]
      zero #<= i_e
      #&& (i_e #< (Expr.list_length arr_exp))
      #=> ((Expr.list_nth_e arr_exp i_e) #== (Lit Undefined))

  let sval_to_gil_expr_undelayed = to_gil_expr_undelayed

  let rec to_gil_expr_undelayed ~chunk ~range svarr =
    let chunk_size = Chunk.size_expr chunk in
    let size =
      let open Expr.Infix in
      let high, low = range in
      (high -. low) /. chunk_size
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
    | AllUndef -> (
        match size with
        | Lit (Num n) ->
            let arr =
              Conc (Utils.List_utils.make (int_of_float n) SVal.SUndefined)
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
                undefined_pf arr_e;
              ]
            in
            (arr_e, learned) )

  let to_gil_expr ~chunk ~range (svarr : t) : Expr.t Delayed.t =
    let e, learned = to_gil_expr_undelayed ~chunk ~range svarr in
    Delayed.return ~learned e

  let of_gil_expr_exn expr = Abst expr
end
