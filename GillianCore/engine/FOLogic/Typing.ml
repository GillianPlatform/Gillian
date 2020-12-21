module L = Logging
module SSubst = SVal.SESubst

exception Break

(* ******************** *)
(* ** TYPE INFERENCE ** *)
(* ******************** *)

let rec infer_types_to_gamma
    (flag : bool)
    (gamma : TypEnv.t)
    (new_gamma : TypEnv.t)
    (le : Expr.t)
    (tt : Type.t) : bool =
  let f = infer_types_to_gamma flag gamma new_gamma in

  match le with
  (* Literals are always typable *)
  | Lit lit -> Literal.type_of lit = tt
  (* Variables are reverse-typable if they are already typable *)
  (* with the target type or if they are not typable           *)
  | LVar var | PVar var -> (
      match (TypEnv.get gamma var, TypEnv.get new_gamma var) with
      | Some t, None | None, Some t -> t = tt
      | None, None ->
          TypEnv.update new_gamma var tt;
          true
      | Some t1, Some t2 -> t1 = t2)
  (* Abstract locations are reverse-typable if the target type is ObjectType *)
  | ALoc _ -> tt = ObjectType
  (* EList and ESet are not reverse typable because we lose type information *)
  | EList _ -> if flag then tt = ListType else false
  | ESet _ -> if flag then tt = SetType else false
  (* Members of unions and intersections must all be sets *)
  | NOp (SetUnion, les) | NOp (SetInter, les) ->
      tt = SetType && List.for_all (fun x -> f x SetType) les
  (* Members of LstCat must be all lists *)
  | NOp (LstCat, les) ->
      tt = ListType && List.for_all (fun x -> f x ListType) les
  | LstSub (le1, le2, le3) ->
      tt = ListType && f le1 ListType && f le2 NumberType && f le3 NumberType
  | UnOp (unop, le) -> (
      match unop with
      | UNot -> tt = BooleanType && f le BooleanType
      | M_isNaN -> tt = BooleanType && f le NumberType
      | IUnaryMinus -> tt = IntType && f le IntType
      | FUnaryMinus
      | BitwiseNot
      | M_sgn
      | M_abs
      | M_acos
      | M_asin
      | M_atan
      | M_ceil
      | M_cos
      | M_exp
      | M_floor
      | M_log
      | M_round
      | M_sin
      | M_sqrt
      | M_tan
      | ToIntOp
      | ToUint16Op
      | ToInt32Op
      | ToUint32Op -> tt = NumberType && f le NumberType
      | ToStringOp -> tt = StringType && f le NumberType
      | ToNumberOp -> tt = NumberType && f le StringType
      | TypeOf -> tt = TypeType
      | Cdr -> tt = ListType && f le ListType
      | Car -> f le ListType
      | LstLen -> tt = NumberType && f le ListType
      | LstRev -> tt = ListType && f le ListType
      | StrLen -> tt = NumberType && f le StringType
      | SetToList -> tt = ListType && f le SetType)
  | BinOp (le1, op, le2) -> (
      let (rqt1 : Type.t option), (rqt2 : Type.t option), (rt : Type.t option) =
        match op with
        | Equal -> (None, None, Some BooleanType)
        | ILessThan | ILessThanEqual ->
            (Some IntType, Some IntType, Some BooleanType)
        | FLessThan | FLessThanEqual ->
            (Some NumberType, Some NumberType, Some BooleanType)
        | SLessThan -> (Some StringType, Some StringType, Some BooleanType)
        | BAnd -> (Some BooleanType, Some BooleanType, Some BooleanType)
        | BOr -> (Some BooleanType, Some BooleanType, Some BooleanType)
        | StrCat -> (Some StringType, Some StringType, Some StringType)
        | BSetMem -> (None, Some SetType, Some BooleanType)
        | SetDiff -> (Some SetType, Some SetType, Some SetType)
        | BSetSub -> (Some SetType, Some SetType, Some BooleanType)
        | LstNth -> (Some ListType, Some NumberType, None)
        | StrNth -> (Some ListType, Some NumberType, None)
        | IPlus | IMinus | ITimes | IMod | IDiv ->
            (Some IntType, Some IntType, Some IntType)
        | FPlus | FMinus | FTimes | FMod | FDiv ->
            (Some NumberType, Some NumberType, Some NumberType)
        (* FIXME: Specify cases *)
        | _ -> (Some NumberType, Some NumberType, Some NumberType)
      in
      Option.fold ~some:(fun t -> f le1 t) ~none:true rqt1
      && Option.fold ~some:(fun t -> f le2 t) ~none:true rqt2
      &&
      match rt with
      | None    -> true
      | Some rt -> tt = rt)

let reverse_type_lexpr
    (flag : bool) (gamma : TypEnv.t) (e_types : (Expr.t * Type.t) list) :
    TypEnv.t option =
  let new_gamma = TypEnv.init () in
  let ret =
    List.fold_left
      (fun ac (e, t) -> ac && infer_types_to_gamma flag gamma new_gamma e t)
      true e_types
  in
  if ret then Some new_gamma else None

let safe_extend_gamma (gamma : TypEnv.t) (le : Expr.t) (t : Type.t) : unit =
  let new_gamma = reverse_type_lexpr true gamma [ (le, t) ] in
  match new_gamma with
  | Some new_gamma -> TypEnv.extend gamma new_gamma
  | None           ->
      let msg =
        Fmt.str "ERROR: Safe Extend Gamma: Untypable expression: %a in @[%a@]"
          Expr.pp le TypEnv.pp gamma
      in
      L.fail msg

(* Destructively extend gamma with typing information from logic expressions *)
let rec infer_types_expr gamma le : unit =
  let f = infer_types_expr gamma in
  let e = safe_extend_gamma gamma in

  match (le : Expr.t) with
  (* Do nothing, these cases do not provide additional information *)
  | Lit _ | ALoc _ | LVar _ | PVar _ -> ()
  (* Set union and intersection - all members must be sets, plus any additional information from the members themselves *)
  | NOp (SetUnion, lle) | NOp (SetInter, lle) ->
      e le SetType;
      List.iter (fun le -> f le) lle
  | NOp (LstCat, lle) ->
      e le ListType;
      List.iter (fun le -> f le) lle
  | EList lle | ESet lle -> List.iter (fun le -> f le) lle
  | BinOp (le1, op, le2) -> (
      match op with
      | FPlus | FMinus | FTimes | FDiv | FMod ->
          e le1 NumberType;
          e le2 NumberType
      | LstNth ->
          e le1 ListType;
          e le2 NumberType
      | StrNth ->
          e le1 StringType;
          e le2 NumberType
      (* FIXME: Specify cases *)
      | _ -> ())
  (* FIXME: Specify cases *)
  | _ -> ()

let rec infer_types_formula (gamma : TypEnv.t) (a : Formula.t) : unit =
  let f = infer_types_formula gamma in
  let e = safe_extend_gamma gamma in

  match a with
  (* LForAll can be more precise *)
  | True | False | ForAll _ -> ()
  | Not a -> f a
  | And (a1, a2) | Or (a1, a2) ->
      f a1;
      f a2
  | Less (e1, e2) | LessEq (e1, e2) ->
      e e1 NumberType;
      e e2 NumberType
  | StrLess (e1, e2) ->
      e e1 StringType;
      e e2 StringType
  | SetMem (_, e2) -> e e2 SetType
  | SetSub (e1, e2) ->
      e e1 SetType;
      e e2 SetType
  (* FIXME: Specify cases *)
  | _ -> ()

(*****************)
(* Type checking *)
(*****************)

let rec type_lexpr (gamma : TypEnv.t) (le : Expr.t) :
    Type.t option * bool * Formula.t list =
  let f = type_lexpr gamma in
  let def_pos (ot : Type.t option) = (ot, true, []) in
  let def_neg = (None, false, []) in

  let infer_type le tt constraints =
    let outcome = reverse_type_lexpr true gamma [ (le, tt) ] in
    Option.fold
      ~some:(fun new_gamma ->
        TypEnv.extend gamma new_gamma;
        (Some tt, true, constraints))
      ~none:def_neg outcome
  in

  let typable_list ?(target_type : Type.t option) les =
    List.fold_left
      (fun (ac, ac_constraints) elem ->
        if not ac then (false, [])
        else
          let t, ite, constraints =
            let t, ite, constraints = f elem in
            match t with
            | Some _ -> (t, ite, constraints)
            | None   -> (
                match target_type with
                | None    -> (t, ite, constraints)
                | Some tt -> infer_type elem tt constraints)
          in
          let correct_type = target_type = None || t = target_type in
          (ac && correct_type && ite, constraints @ ac_constraints))
      (true, []) les
  in

  let result =
    match le with
    (* Literals are always typable *)
    | Lit lit -> def_pos (Some (Literal.type_of lit))
    (* Variables are typable if in gamma, otherwise no, but typing continues *)
    | LVar var | PVar var -> def_pos (TypEnv.get gamma var)
    (* Abstract locations are always typable, by construction *)
    | ALoc _ -> def_pos (Some ObjectType)
    (* Lists are always typable *)
    | EList _ -> (Some ListType, true, [])
    (* Sets are always typable *)
    | ESet _ -> (Some SetType, true, [])
    | UnOp (unop, e) -> (
        let _, ite, constraints = f e in

        match ite with
        | false -> def_neg
        | true  ->
            let (tt : Type.t), new_constraints =
              match unop with
              | TypeOf         -> (TypeType, [])
              | UNot | M_isNaN -> (BooleanType, [])
              | ToStringOp     -> (StringType, [])
              | Car | Cdr      ->
                  (ListType, [ Formula.LessEq (Lit (Num 1.), UnOp (LstLen, e)) ])
              | LstRev         -> (ListType, [])
              | _              -> (NumberType, [])
            in
            infer_type le tt (new_constraints @ constraints))
    | BinOp (e1, op, e2) -> (
        let _, ite1, constraints1 = f e1 in
        let _, ite2, constraints2 = f e2 in
        let constraints = constraints1 @ constraints2 in

        (* Both expressions must be typable *)
        match (ite1, ite2) with
        | true, true -> (
            match op with
            (* List length is typable with constraints *)
            | LstNth -> (
                let _, success, _ = infer_type e1 ListType constraints in
                match success with
                | false -> def_neg
                | true  -> (
                    let _, success, _ = infer_type e2 NumberType constraints in
                    match success with
                    | false -> def_neg
                    | true  ->
                        let new_constraint1 : Formula.t =
                          LessEq (Lit (Num 0.), e2)
                        in
                        let new_constraint2 : Formula.t =
                          Less (e2, UnOp (LstLen, e1))
                        in
                        ( None,
                          true,
                          new_constraint1 :: new_constraint2 :: constraints )))
            (* String length is typable with constraints *)
            | StrNth -> (
                let _, success, _ = infer_type e1 StringType constraints in
                match success with
                | false -> def_neg
                | true  -> (
                    let _, success, _ = infer_type e2 NumberType constraints in
                    match success with
                    | false -> def_neg
                    | true  ->
                        let new_constraint1 : Formula.t =
                          LessEq (Lit (Num 0.), e2)
                        in
                        let new_constraint2 : Formula.t =
                          Less (e2, UnOp (StrLen, e1))
                        in
                        ( None,
                          true,
                          new_constraint1 :: new_constraint2 :: constraints )))
            | _ ->
                let tt : Type.t =
                  match op with
                  | Equal
                  | ILessThan
                  | ILessThanEqual
                  | FLessThan
                  | FLessThanEqual
                  | SLessThan
                  | BAnd
                  | BOr
                  | BSetMem
                  | BSetSub -> BooleanType
                  | SetDiff -> SetType
                  | StrCat -> StringType
                  | IPlus | IMinus | ITimes | IDiv | IMod -> IntType
                  | LstNth | StrNth -> raise (Failure "Impossible match case")
                  (* FIXME: Specify cases *)
                  | _ -> NumberType
                in
                infer_type le tt constraints)
        | _, _       -> def_neg)
    | NOp (SetUnion, les) | NOp (SetInter, les) ->
        let all_typable, constraints =
          typable_list ?target_type:(Some SetType) les
        in
        if all_typable then (Some SetType, true, constraints) else def_neg
    | NOp (LstCat, les) ->
        let all_typable, constraints =
          typable_list ?target_type:(Some ListType) les
        in
        if all_typable then (Some ListType, true, constraints) else def_neg
    | LstSub (le1, le2, le3) ->
        let _, ite1, constraints1 = f le1 in
        let _, ite2, constraints2 = f le2 in
        let _, ite3, constraints3 = f le3 in
        let constraints = constraints1 @ constraints2 @ constraints3 in
        if ite1 && ite2 && ite3 then
          let _, success1, _ = infer_type le1 ListType constraints in
          let _, success2, _ = infer_type le2 NumberType constraints in
          let _, success3, _ = infer_type le3 NumberType constraints in
          if success1 && success2 && success3 (* TODO: there are constraints *)
          then (Some ListType, true, constraints)
          else def_neg
        else def_neg
    (* let _, success, _ = infer_type le1 ListType constraints in
       (match success with
       | false -> def_neg
       | true ->
         let _, success, _ = infer_type e2 NumberType constraints in
         (match success with
         | false -> def_neg
         | true ->
           let new_constraint1 : Formula.t = (LessEq (Lit (Num 0.), e2)) in
           let new_constraint2 : Formula.t = (Less (e2, UnOp (LstLen, e1))) in
           (None, true, (new_constraint1 :: (new_constraint2 :: constraints)) ) *)
  in

  result

let te_of_list (vt : (Expr.t * Type.t) list) : TypEnv.t option =
  let result = TypEnv.init () in
  try
    List.iter
      (fun (e, t) ->
        match (e : Expr.t) with
        | Lit l           ->
            let t' = Literal.type_of l in
            if t <> t' then raise Break
        | LVar x | PVar x ->
            if TypEnv.mem result x then (
              let t' = TypEnv.get_unsafe result x in
              if t <> t' then raise Break)
            else TypEnv.update result x t
        | _               -> (
            let t', _, _ = type_lexpr result e in
            match t' with
            | Some t' when t = t' -> ()
            | _ -> raise Break))
      vt;
    Some result
  with Break -> None

let naively_infer_type_information (pfs : PFS.t) (gamma : TypEnv.t) : unit =
  PFS.iter
    (fun a ->
      match (a : Formula.t) with
      | Eq (LVar x, le) | Eq (le, LVar x) ->
          if not (TypEnv.mem gamma x) then
            let le_type, _, _ = type_lexpr gamma le in
            Option.fold
              ~some:(fun x_type -> TypEnv.update gamma x x_type)
              ~none:() le_type
      | Eq (UnOp (TypeOf, LVar x), Lit (Type t))
      | Eq (Lit (Type t), UnOp (TypeOf, LVar x)) -> TypEnv.update gamma x t
      | _ -> ())
    pfs

let substitution_in_place (subst : SSubst.t) (gamma : TypEnv.t) : unit =
  let ve_pairs : (Expr.t * Expr.t) list = SSubst.to_list subst in
  let et_pairs : (Expr.t * Type.t) list =
    List.fold_left
      (fun ac (x, e) ->
        match x with
        | Expr.LVar x | PVar x ->
            Option.fold
              ~some:(fun x_type ->
                TypEnv.remove gamma x;
                (e, x_type) :: ac)
              ~none:ac (TypEnv.get gamma x)
        | _                    -> ac)
      [] ve_pairs
  in
  let gamma' = reverse_type_lexpr true gamma et_pairs in
  Option.fold ~some:(fun gamma' -> TypEnv.extend gamma gamma') ~none:() gamma'
