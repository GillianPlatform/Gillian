include SVal
open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

exception NotACompCertValue of Expr.t

module Patterns = struct
  open Formula.Infix

  let number e =
    let open Expr in
    (typeof e) #== (type_ NumberType)

  let integer e =
    let open Expr in
    (typeof e) #== (type_ IntType)

  let int_typ, float_typ, single_typ, long_typ =
    let open Expr in
    let open CConstants.VTypes in
    let num_typ int_t typ_str x =
      (typeof x) #== (type_ ListType)
      #&& ((list_length x) #== (int 2))
      #&& ((list_nth x 0) #== (string typ_str))
      #&& ((typeof (list_nth x 1)) #== (type_ int_t))
    in
    ( num_typ IntType int_type,
      num_typ NumberType float_type,
      num_typ NumberType single_type,
      num_typ IntType long_type )

  let undefined x = x #== (Expr.Lit Undefined)

  let obj x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (int 2))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ IntType))
end

let of_chunk_and_expr chunk e =
  let return = Delayed.return in
  let open Patterns in
  let* e = Delayed.reduce e in
  match e with
  | Expr.Lit Undefined -> return SUndefined
  | _ -> (
      match Chunk.type_of chunk with
      | Tlong when Compcert.Archi.ptr64 -> (
          match%ent e with
          | integer -> return (SVlong e)
          | obj -> (
              match e with
              | EList [ ALoc l; o ] -> return (Sptr (l, o))
              | _ ->
                  Fmt.failwith
                    "of_chunk_and_expr: Not a location, but should be: %a"
                    Expr.pp e))
      | Tint when not Compcert.Archi.ptr64 -> (
          match%ent e with
          | integer -> return (SVint e)
          | obj -> (
              match e with
              | EList [ ALoc l; o ] -> return (Sptr (l, o))
              | _ ->
                  Fmt.failwith
                    "of_chunk_and_expr: Not a location, but should be: %a"
                    Expr.pp e))
      | Tlong -> return (SVlong e)
      | Tint ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let learned =
            match chunk with
            | Mint8unsigned -> [ (i 0) #<= e; e #<= (i 255) ]
            | _ -> []
          in
          return ~learned (SVint e)
      | Tfloat -> return (SVfloat e)
      | Tsingle -> return (SVsingle e)
      | Tany32 | Tany64 -> Fmt.failwith "Unhandled chunk: %a" Chunk.pp chunk)

let of_gil_expr sval_e =
  let open Formula.Infix in
  let open Patterns in
  Logging.verbose (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
  let* sval_e = Delayed.reduce sval_e in
  match%ent sval_e with
  | undefined -> DO.some SUndefined
  | obj ->
      let loc_expr = Expr.list_nth sval_e 0 in
      let ofs = Expr.list_nth sval_e 1 in
      let* ofs = Delayed.reduce ofs in
      let* loc_opt = Delayed.resolve_loc loc_expr in
      let loc, learned =
        match loc_opt with
        | Some l -> (l, [])
        | None ->
            let aloc = ALoc.alloc () in
            let learned = [ loc_expr #== (ALoc aloc) ] in
            (aloc, learned)
      in
      DO.some ~learned (Sptr (loc, ofs))
  | int_typ -> DO.some (SVint (Expr.list_nth sval_e 1))
  | float_typ -> DO.some (SVfloat (Expr.list_nth sval_e 1))
  | long_typ -> DO.some (SVlong (Expr.list_nth sval_e 1))
  | single_typ -> DO.some (SVsingle (Expr.list_nth sval_e 1))
  | _ -> DO.none ()

let of_gil_expr_exn sval_e =
  let* value_opt = of_gil_expr sval_e in
  match value_opt with
  | None -> raise (NotACompCertValue sval_e)
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
  | SVint (Lit (Int z)) when Z.equal z Z.zero -> true
  | SVlong (Lit (Int z)) when Z.equal z Z.zero -> true
  | SVfloat (Lit (Num 0.)) | SVsingle (Lit (Num 0.)) -> true
  | _ -> false

module SVArray = struct
  type t =
    | Arr of Expr.t
        (** the parameter should be a list representing a *NON-EMPTY* list *)
    | AllUndef
    | AllZeros
  [@@deriving yojson]

  let reduce t =
    let open Delayed.Syntax in
    match t with
    | Arr e ->
        let+ reduced = Delayed.reduce e in
        Arr reduced
    | _ -> Delayed.return t

  let pp fmt = function
    | Arr e -> Expr.pp fmt e
    | AllUndef -> Fmt.string fmt "AllUndef"
    | AllZeros -> Fmt.string fmt "AllZeros"

  let empty = Arr (EList [])

  let is_empty =
    let open Formula.Infix in
    function
    | Arr e -> (Expr.list_length e) #== (Expr.int 0)
    | _ -> False

  let sure_is_all_zeros = function
    | Arr (EList l) ->
        List.for_all
          (function
            | Expr.Lit (Int z) when Z.equal z Z.zero -> true
            | _ -> false)
          l
    | AllZeros -> true
    | _ -> false

  let equal arr_a arr_b =
    match (arr_a, arr_b) with
    | Arr a, Arr b -> Expr.equal a b
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

  let undefined_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let zero = Expr.int 0 in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: Concrete: %a" Expr.pp size);
        let undefs =
          Expr.Lit (LList (List.init (Z.to_int x) (fun _ -> Literal.Undefined)))
        in
        arr_exp #== undefs
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: not as concrete: %a" Expr.pp size);
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> ((Expr.list_nth_e arr_exp i_e) #== (Lit Undefined))

  let zeros_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt -> fmt "Zeros pf: Concrete: %a" Expr.pp size);
        let zeros =
          Expr.Lit
            (LList (List.init (Z.to_int x) (fun _ -> Literal.Int Z.zero)))
        in
        arr_exp #== zeros
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Zeros pf: not as concrete: %a" Expr.pp size);
        let is_zero e = e #== (Expr.int 0) in
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        let zero = Expr.int 0 in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> (is_zero (Expr.list_nth_e arr_exp i_e))

  let to_arr_with_size arr s =
    let open Formula.Infix in
    let allocate_array_lvar (descr : ?size:Expr.t -> Expr.t -> Formula.t) =
      let x = LVar.alloc () in
      let learned_types = [ (x, Gil_syntax.Type.ListType) ] in
      let x = Expr.LVar x in
      let learned = [ (Expr.list_length x) #== s; descr ~size:s x ] in
      Delayed.return ~learned ~learned_types x
    in
    match arr with
    | Arr e -> Delayed.return e
    | AllUndef -> allocate_array_lvar undefined_pf
    | AllZeros -> allocate_array_lvar zeros_pf

  let concat_knowing_size (left, left_size) (right, right_size) =
    let open Delayed in
    let open Delayed.Syntax in
    match (left, right) with
    | Arr a, Arr b -> return (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> return AllUndef
    | AllZeros, AllZeros -> return AllZeros
    | left, right ->
        let* left = to_arr_with_size left left_size in
        let+ right = to_arr_with_size right right_size in
        Arr (Expr.list_cat left right)

  let concat left right =
    match (left, right) with
    | Arr a, Arr b -> Some (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> Some AllUndef
    | AllZeros, AllZeros -> Some AllZeros
    | _ -> None

  (** This already assumes the value is a number and not a pointer *)
  let to_single_value ~chunk = function
    | Arr (EList [ a ]) ->
        let+ v = of_chunk_and_expr chunk a in
        Some v
    | AllZeros -> DO.some (zero_of_chunk chunk)
    | AllUndef -> DO.some SUndefined
    | _ -> DO.none ()

  let singleton = function
    (* Assuming that the chunk is correct already *)
    | SVfloat e | SVint e | SVlong e | SVsingle e -> Arr (Expr.EList [ e ])
    | Sptr _ as ptr ->
        let e_ptr, _ = to_gil_expr_undelayed ptr in
        Arr (Expr.EList [ e_ptr ])
    | SUndefined -> AllUndef

  let array_sub arr o len : t =
    match arr with
    | AllZeros -> AllZeros
    | AllUndef -> AllUndef
    | Arr e -> Arr (Expr.list_sub ~lst:e ~start:o ~size:len)

  (** This assumes chunks are properly respected outside of the call of this function *)
  let array_cons (el : SVal.t) arr = concat (singleton el) arr

  let array_append arr el = concat arr (singleton el)

  let to_gil_expr_undelayed ~chunk ~range svarr =
    let chunk_size = Chunk.size_expr chunk in
    let size =
      let open Expr.Infix in
      let low, high = range in
      (high - low) / chunk_size
    in
    let f_of_all_same ~describing_pf ~concrete_single =
      match size with
      | Lit (Int n) ->
          (Expr.EList (Utils.List_utils.make (Z.to_int n) concrete_single), [])
      | _ ->
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
    | Arr e ->
        let open Formula.Infix in
        let learned =
          [
            (Expr.typeof e) #== (Expr.type_ ListType);
            (Expr.list_length e) #== size;
          ]
        in
        (e, learned)
    | AllZeros ->
        f_of_all_same ~concrete_single:Expr.zero_i ~describing_pf:zeros_pf
    | AllUndef ->
        f_of_all_same ~concrete_single:(Expr.Lit Undefined)
          ~describing_pf:undefined_pf

  let to_gil_expr ~chunk ~range (svarr : t) : Expr.t Delayed.t =
    let e, learned = to_gil_expr_undelayed ~chunk ~range svarr in
    Delayed.return ~learned e

  let of_gil_expr_exn expr = Arr expr

  (** Only call on Mint8Unsigned arrays *)
  let learn_chunk ~chunk ~size arr =
    let bounds =
      match chunk with
      | Chunk.Mint8unsigned -> Some (0, 255)
      | _ -> None
      (* Should be completed later *)
    in
    let* size = Delayed.reduce size in
    match bounds with
    | None -> Delayed.return ()
    | Some (low, high) -> (
        match arr with
        | Arr (EList e) ->
            let i k = Expr.int k in
            let learned =
              List.concat_map
                (function
                  | Expr.Lit Undefined -> []
                  | x ->
                      let open Formula.Infix in
                      [ (i low) #<= x; x #<= (i high) ])
                e
            in
            Delayed.return ~learned ()
        | Arr e -> (
            match size with
            | Expr.Lit (Int n) ->
                let i k = Expr.int k in
                let learned =
                  List.concat
                    (List.init (Z.to_int n) (fun k ->
                         let x = Expr.list_nth e k in
                         let open Formula.Infix in
                         [ (i low) #<= x; x #<= (i high) ]))
                in
                Delayed.return ~learned ()
            | _ -> Delayed.return ())
        | _ -> Delayed.return ())

  (* type nonrec t = Conc of t list | Abst of Expr.t | AllUndef | AllZeros *)

  let subst ~le_subst t =
    match t with
    | Arr e ->
        let s = le_subst e in
        if s == e then t else Arr s
    | AllUndef -> AllUndef
    | AllZeros -> AllZeros
end

module Infix = struct
  let ( @: ) = SVArray.concat
  let ( ^: ) = SVArray.array_cons
  let ( ^:? ) a b = Option.bind b (fun b -> a ^: b)
end
