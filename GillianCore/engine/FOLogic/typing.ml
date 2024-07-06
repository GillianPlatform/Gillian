module L = Logging
module SSubst = SVal.SESubst

exception Break

(* ******************** *)
(* ** TYPE INFERENCE ** *)
(* ******************** *)

module Infer_types_to_gamma = struct
  open Type

  let rec infer_unop
      (flag : bool)
      (gamma : Type_env.t)
      (new_gamma : Type_env.t)
      (op : UnOp.t)
      (le : Expr.t)
      (tt : Type.t) =
    let f = f flag gamma new_gamma in
    match op with
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
    | LstLen -> tt = IntType && f le ListType
    | LstRev -> tt = ListType && f le ListType
    | IntToNum -> tt = NumberType && f le IntType
    | NumToInt -> tt = IntType && f le NumberType
    | StrLen -> tt = NumberType && f le StringType
    | SetToList -> tt = ListType && f le SetType

  and infer_binop
      (flag : bool)
      (gamma : Type_env.t)
      (new_gamma : Type_env.t)
      (op : BinOp.t)
      (le1 : Expr.t)
      (le2 : Expr.t)
      (tt : Type.t) =
    let f = f flag gamma new_gamma in
    let (rqt1 : Type.t option), (rqt2 : Type.t option), (rt : Type.t option) =
      match op with
      | Equal -> (None, None, Some BooleanType)
      | ILessThan | ILessThanEqual ->
          (Some IntType, Some IntType, Some BooleanType)
      | FLessThan | FLessThanEqual ->
          (Some NumberType, Some NumberType, Some BooleanType)
      | SLessThan -> (Some StringType, Some StringType, Some BooleanType)
      | BAnd | BOr | BImpl ->
          (Some BooleanType, Some BooleanType, Some BooleanType)
      | StrCat -> (Some StringType, Some StringType, Some StringType)
      | BSetMem -> (None, Some SetType, Some BooleanType)
      | SetDiff -> (Some SetType, Some SetType, Some SetType)
      | BSetSub -> (Some SetType, Some SetType, Some BooleanType)
      | LstNth -> (Some ListType, Some IntType, None)
      | LstRepeat -> (None, Some IntType, Some ListType)
      | StrNth -> (Some ListType, Some NumberType, None)
      | IPlus
      | IMinus
      | ITimes
      | IMod
      | IDiv
      | LeftShiftL
      | UnsignedRightShiftL
      | BitwiseOrL
      | BitwiseAndL
      | BitwiseAnd
      | BitwiseOr
      | BitwiseXor
      | LeftShift
      | SignedRightShift
      | UnsignedRightShift
      | SignedRightShiftL
      | BitwiseXorL -> (Some IntType, Some IntType, Some IntType)
      | FPlus
      | FMinus
      | FTimes
      | FMod
      | FDiv
      | BitwiseAndF
      | BitwiseOrF
      | BitwiseXorF
      | LeftShiftF
      | SignedRightShiftF
      | UnsignedRightShiftF
      | M_atan2
      | M_pow -> (Some NumberType, Some NumberType, Some NumberType)
    in
    Option.fold ~some:(fun t -> f le1 t) ~none:true rqt1
    && Option.fold ~some:(fun t -> f le2 t) ~none:true rqt2
    &&
    match rt with
    | None -> true
    | Some rt -> tt = rt

  and f
      (flag : bool)
      (gamma : Type_env.t)
      (new_gamma : Type_env.t)
      (le : Expr.t)
      (tt : Type.t) : bool =
    let f' = f flag in
    let f = f flag gamma new_gamma in
    let ( = ) = Type.equal in
    match le with
    (* Literals are always typable *)
    | Lit lit -> Literal.type_of lit = tt
    (* Variables are reverse-typable if they are already typable *)
    (* with the target type or if they are not typable           *)
    | LVar var | PVar var -> (
        match (Type_env.get gamma var, Type_env.get new_gamma var) with
        | Some t, None | None, Some t -> t = tt
        | None, None ->
            Type_env.update new_gamma var tt;
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
        tt = ListType && f le1 ListType && f le2 IntType && f le3 IntType
    | UnOp (op, le) -> infer_unop flag gamma new_gamma op le tt
    | BinOp (le1, op, le2) -> infer_binop flag gamma new_gamma op le1 le2 tt
    | Exists (bt, le) ->
        if not (tt = BooleanType) then false
        else
          let gamma_copy = Type_env.copy gamma in
          let new_gamma_copy = Type_env.copy new_gamma in
          let () =
            List.iter
              (fun (x, t) ->
                let () =
                  match t with
                  | Some t -> Type_env.update gamma_copy x t
                  | None -> Type_env.remove gamma_copy x
                in
                Type_env.remove new_gamma_copy x)
              bt
          in
          let ret = f' gamma_copy new_gamma_copy le BooleanType in
          (* We've updated our new_gamma_copy with a bunch of things.
             We need to import everything except the quantified variables to the new_gamma *)
          Type_env.iter new_gamma_copy (fun x t ->
              if not (List.exists (fun (y, _) -> String.equal x y) bt) then
                Type_env.update new_gamma x t);
          ret
    | EForall (bt, le) ->
        if not (tt = BooleanType) then false
        else
          let gamma_copy = Type_env.copy gamma in
          let new_gamma_copy = Type_env.copy new_gamma in
          let () =
            List.iter
              (fun (x, t) ->
                let () =
                  match t with
                  | Some t -> Type_env.update gamma_copy x t
                  | None -> Type_env.remove gamma_copy x
                in
                Type_env.remove new_gamma_copy x)
              bt
          in
          let ret = f' gamma_copy new_gamma_copy le BooleanType in
          (* We've updated our new_gamma_copy with a bunch of things.
             We need to import everything except the quantified variables to the new_gamma *)
          Type_env.iter new_gamma_copy (fun x t ->
              if not (List.exists (fun (y, _) -> String.equal x y) bt) then
                Type_env.update new_gamma x t);
          ret
end

let infer_types_to_gamma = Infer_types_to_gamma.f

let reverse_type_lexpr
    (flag : bool)
    (gamma : Type_env.t)
    (e_types : (Expr.t * Type.t) list) : Type_env.t option =
  let new_gamma = Type_env.init () in
  let ret =
    List.fold_left
      (fun ac (e, t) -> ac && infer_types_to_gamma flag gamma new_gamma e t)
      true e_types
  in
  if ret then Some new_gamma else None

let safe_extend_gamma (gamma : Type_env.t) (le : Expr.t) (t : Type.t) : unit =
  let new_gamma = reverse_type_lexpr true gamma [ (le, t) ] in
  match new_gamma with
  | Some new_gamma -> Type_env.extend gamma new_gamma
  | None ->
      let msg =
        Fmt.str "ERROR: Safe Extend Gamma: Untypable expression: %a in @[%a@]"
          Expr.pp le Type_env.pp gamma
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
      | IPlus | IMinus | ITimes | IDiv | IMod ->
          e le1 IntType;
          e le2 IntType
      | LstNth ->
          e le1 ListType;
          e le2 IntType
      | LstRepeat ->
          e le2 IntType;
          e le ListType
      | StrNth ->
          e le1 StringType;
          e le2 NumberType
      (* FIXME: Specify cases *)
      | _ -> ())
  (* FIXME: Specify cases *)
  | _ -> ()

let rec infer_types_formula (gamma : Type_env.t) (a : Formula.t) : unit =
  let f = infer_types_formula gamma in
  let e = safe_extend_gamma gamma in

  match a with
  (* LForAll can be more precise *)
  | True | False | ForAll _ -> ()
  | Not a -> f a
  | And (a1, a2) | Or (a1, a2) ->
      f a1;
      f a2
  | FLess (e1, e2) | FLessEq (e1, e2) ->
      e e1 NumberType;
      e e2 NumberType
  | ILess (e1, e2) | ILessEq (e1, e2) ->
      e e1 IntType;
      e e2 IntType
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

module Type_lexpr = struct
  let def_pos (ot : Type.t option) = (ot, true)
  let def_neg = (None, false)

  let infer_type gamma le tt =
    let outcome = reverse_type_lexpr true gamma [ (le, tt) ] in
    Option.fold
      ~some:(fun new_gamma ->
        Type_env.extend gamma new_gamma;
        (Some tt, true))
      ~none:def_neg outcome

  (* List-nth is typable with constraints *)
  let type_lstnth gamma e1 e2 =
    let infer_type = infer_type gamma in
    let _, success = infer_type e1 ListType in
    if not success then def_neg
    else
      let _, success = infer_type e2 IntType in
      if not success then def_neg else (None, true)

  (* String-nth is typable with constraints *)
  let type_strnth gamma e1 e2 =
    let infer_type = infer_type gamma in
    let _, success = infer_type e1 StringType in
    match success with
    | false -> def_neg
    | true -> (
        let _, success = infer_type e2 NumberType in
        match success with
        | false -> def_neg
        | true -> (None, true))

  let rec typable_list gamma ?(target_type : Type.t option) les =
    let f = f gamma in
    List.for_all
      (fun elem ->
        let t, ite =
          let t, ite = f elem in
          match t with
          | Some _ -> (t, ite)
          | None -> (
              match target_type with
              | None -> (t, ite)
              | Some tt -> infer_type gamma elem tt)
        in
        let correct_type =
          let ( = ) = Option.equal Type.equal in
          target_type = None || t = target_type
        in
        correct_type && ite)
      les

  and type_unop gamma le (op : UnOp.t) e =
    let f = f gamma in
    let _, ite = f e in
    if not ite then def_neg
    else
      let (tt : Type.t) =
        match op with
        | TypeOf -> TypeType
        | UNot | M_isNaN -> BooleanType
        | ToStringOp -> StringType
        | Car | Cdr -> ListType
        | LstRev | SetToList -> ListType
        | IUnaryMinus | LstLen | NumToInt -> IntType
        | BitwiseNot
        | FUnaryMinus
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
        | M_sgn
        | M_sin
        | M_sqrt
        | M_tan
        | ToIntOp
        | ToUint16Op
        | ToUint32Op
        | ToInt32Op
        | ToNumberOp
        | IntToNum
        | StrLen -> NumberType
      in
      infer_type gamma le tt

  and type_binop gamma le (op : BinOp.t) e1 e2 =
    let f = f gamma in
    let infer_type = infer_type gamma in
    let _, ite1 = f e1 in
    let _, ite2 = f e2 in

    (* Both expressions must be typable *)
    match (ite1, ite2) with
    | true, true -> (
        match op with
        | LstNth -> type_lstnth gamma e1 e2
        | LstRepeat -> infer_type le ListType
        | StrNth -> type_strnth gamma e1 e2
        | Equal
        | ILessThan
        | ILessThanEqual
        | FLessThan
        | FLessThanEqual
        | SLessThan
        | BAnd
        | BOr
        | BImpl
        | BSetMem
        | BSetSub -> infer_type le BooleanType
        | SetDiff -> infer_type le SetType
        | StrCat -> infer_type le StringType
        | IPlus
        | IMinus
        | ITimes
        | IDiv
        | IMod
        | UnsignedRightShiftL
        | BitwiseAnd
        | BitwiseOr
        | BitwiseXor
        | LeftShift
        | SignedRightShift
        | UnsignedRightShift
        | BitwiseAndL
        | BitwiseOrL
        | BitwiseXorL
        | LeftShiftL
        | SignedRightShiftL -> infer_type le IntType
        | FPlus
        | FMinus
        | FTimes
        | FDiv
        | FMod
        | BitwiseAndF
        | BitwiseOrF
        | BitwiseXorF
        | LeftShiftF
        | SignedRightShiftF
        | UnsignedRightShiftF
        | M_atan2
        | M_pow ->
            infer_type le NumberType
            (* FIXME: Specify cases *)
            (* | _ -> infer_type le NumberType constraints *))
    | _, _ -> def_neg

  and type_lstsub gamma le1 le2 le3 =
    let f = f gamma in
    let infer_type = infer_type gamma in
    let _, ite1 = f le1 in
    let _, ite2 = f le2 in
    let _, ite3 = f le3 in
    if ite1 && ite2 && ite3 then
      let _, success1 = infer_type le1 ListType in
      let _, success2 = infer_type le2 IntType in
      let _, success3 = infer_type le3 IntType in
      if success1 && success2 && success3 then (Some Type.ListType, true)
      else def_neg
    else def_neg

  (** Typing quantified expr is independant on the kind of quantifier *)
  and type_quantified_expr gamma le bt e =
    let gamma_copy = Type_env.copy gamma in
    let () =
      List.iter
        (fun (x, t) ->
          match t with
          | Some ty -> Type_env.update gamma_copy x ty
          | None -> Type_env.remove gamma_copy x)
        bt
    in
    let _, ite = f gamma_copy e in
    if not ite then def_neg else infer_type gamma le BooleanType

  (** This function returns a triple [(t_opt, b, fs)] where
      - [t_opt] is the type of [le] if we can find one
      - [b] indicates if the thing is typable
      - [fs] indicates the constraints that must be satisfied for [le] to be typable
  *)
  and f (gamma : Type_env.t) (le : Expr.t) : Type.t option * bool =
    let typable_list = typable_list gamma in

    let result =
      match le with
      (* Literals are always typable *)
      | Lit lit -> def_pos (Some (Literal.type_of lit))
      (* Variables are typable if in gamma, otherwise no, but typing continues *)
      | LVar var | PVar var -> def_pos (Type_env.get gamma var)
      (* Abstract locations are always typable, by construction *)
      | ALoc _ -> def_pos (Some ObjectType)
      (* Lists are always typable *)
      | EList _ -> def_pos (Some ListType)
      (* Sets are always typable *)
      | ESet _ -> def_pos (Some SetType)
      | Exists (bt, e) | EForall (bt, e) -> type_quantified_expr gamma le bt e
      | UnOp (op, e) -> type_unop gamma le op e
      | BinOp (e1, op, e2) -> type_binop gamma le op e1 e2
      | NOp (SetUnion, les) | NOp (SetInter, les) ->
          let all_typable = typable_list ?target_type:(Some SetType) les in
          if all_typable then (Some SetType, true) else def_neg
      | NOp (LstCat, les) ->
          let all_typable = typable_list ?target_type:(Some ListType) les in
          if all_typable then (Some ListType, true) else def_neg
      | LstSub (le1, le2, le3) -> type_lstsub gamma le1 le2 le3
    in

    result
end

let type_lexpr = Type_lexpr.f

let te_of_list (vt : (Expr.t * Type.t) list) : Type_env.t option =
  let result = Type_env.init () in
  try
    List.iter
      (fun (e, t) ->
        match (e : Expr.t) with
        | Lit l ->
            let t' = Literal.type_of l in
            if t <> t' then raise Break
        | LVar x | PVar x ->
            if Type_env.mem result x then (
              let t' = Type_env.get_unsafe result x in
              if t <> t' then raise Break)
            else Type_env.update result x t
        | _ -> (
            let t', _ = type_lexpr result e in
            match t' with
            | Some t' when t = t' -> ()
            | _ -> raise Break))
      vt;
    Some result
  with Break -> None

let naively_infer_type_information (pfs : PFS.t) (gamma : Type_env.t) : unit =
  PFS.iter
    (fun a ->
      match (a : Formula.t) with
      | Eq (LVar x, le) | Eq (le, LVar x) ->
          if not (Type_env.mem gamma x) then
            let le_type, _ = type_lexpr gamma le in
            Option.fold
              ~some:(fun x_type -> Type_env.update gamma x x_type)
              ~none:() le_type
      | Eq (UnOp (TypeOf, LVar x), Lit (Type t))
      | Eq (Lit (Type t), UnOp (TypeOf, LVar x)) -> Type_env.update gamma x t
      | _ -> ())
    pfs

let substitution_in_place (subst : SSubst.t) (gamma : Type_env.t) : unit =
  let ve_pairs : (Expr.t * Expr.t) list = SSubst.to_list subst in
  let et_pairs : (Expr.t * Type.t) list =
    List.fold_left
      (fun ac (x, e) ->
        match x with
        | Expr.LVar x | PVar x ->
            Option.fold
              ~some:(fun x_type ->
                Type_env.remove gamma x;
                (e, x_type) :: ac)
              ~none:ac (Type_env.get gamma x)
        | _ -> ac)
      [] ve_pairs
  in
  let gamma' = reverse_type_lexpr true gamma et_pairs in
  Option.fold ~some:(fun gamma' -> Type_env.extend gamma gamma') ~none:() gamma'
