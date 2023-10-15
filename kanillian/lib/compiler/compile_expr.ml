open Helpers
open Gil_syntax
module GType = Goto_lib.Type
module GExpr = Goto_lib.Expr

type access =
  | InMemoryFunction of { ptr : Expr.t; symbol : string option }
  | InMemoryScalar of { ptr : Expr.t; loaded : Expr.t option }
  | InMemoryComposit of { ptr : Expr.t; type_ : GType.t }
      (** For copy, we just need the size, not the type.
          However, in the future, we might copy fields
          one-by-one to preserve the correct semantics of C *)
  | Direct of string
  | ListMember of { list : string; index : int; total_size : int }
      (** When a value is represented as a list in the store,
          one can access or override one of the members by.
          For now this only used for overflow result.
          However, we could also model fat pointers like this. *)
  | DirectFunction of string
  | ZST
[@@deriving show { with_path = false }]

let write_list_member ~list ~index ~total_size e =
  let list_e = Expr.PVar list in
  let values =
    List.init total_size (fun i ->
        if index == i then e else Expr.list_nth list_e i)
  in
  let new_list = Expr.EList values in
  Cmd.Assignment (list, new_list)

let dummy_access ~ctx type_ =
  if Ctx.is_zst_access ctx type_ then ZST
  else if Ctx.representable_in_store ctx type_ then
    InMemoryScalar { ptr = Lit Nono; loaded = None }
  else InMemoryComposit { ptr = Lit Nono; type_ }

(* This file is a mess of mutually-recursive functions *)

type binop_comp =
  | GilBinop of BinOp.t
  | Proc of string
  | App of (Expr.t -> Expr.t -> Expr.t)
  | Pre of (Expr.t -> Expr.t -> Expr.t * Expr.t) * binop_comp
  | Then of binop_comp * (Expr.t -> Expr.t Cs.with_cmds)
  | Unhandled of [ `With_type | `Without_type ]

let compile_binop
    ~(ctx : Ctx.t)
    ~(lty : GType.t)
    ~(rty : GType.t)
    (binop : Ops.Binary.t)
    (e1 : Val_repr.t)
    (e2 : Val_repr.t) : Expr.t Cs.with_cmds =
  (* For now, we assume we're on archi64 exactly,
     then we'll figure out a bit more.
     This is for size_t and pointer operations. *)
  assert (Machine_model.(equal archi64 ctx.machine));
  let ( |||> ) comp f = Then (comp, f) in
  let ( ||> ) comp f = Then (comp, fun e -> Cs.return (f e)) in
  let int_in_bounds ~ty e =
    let error () =
      Error.code_error (Fmt.str "int_in_bounds for non-int: %a" GType.pp ty)
    in
    let low, high =
      match Memory.chunk_for_type ~ctx ty with
      | Some (F32 | F64) | None -> error ()
      | Some c -> (
          match Chunk.bounds c with
          | None -> error ()
          | Some (low, high) -> (low, high))
    in
    let ( <= ) a b = Expr.BinOp (a, ILessThanEqual, b) in
    let ( && ) a b = Expr.BinOp (a, BAnd, b) in
    Expr.int_z low <= e && e <= Expr.int_z high
  in
  let assert_int_in_bounds ~ty e =
    let expr_cond = int_in_bounds ~ty e in
    let formula =
      match Formula.lift_logic_expr expr_cond with
      | Some (f, _) -> f
      | _ ->
          Error.code_error
            "created bound condition that cannot be lifted to formula"
    in
    let cmd = Cmd.Logic (LCmd.Assert formula) in
    Cs.return ~app:[ cmd ] e
  in
  let high ~ty =
    let error () =
      Error.code_error (Fmt.str "high for non-int: %a" GType.pp ty)
    in
    match Memory.chunk_for_type ~ctx ty with
    | Some (F32 | F64) | None -> error ()
    | Some c -> (
        match Chunk.bounds c with
        | Some (zero, high) when Z.equal zero Z.zero -> Z.succ high
        | _ -> error ())
  in
  let modulo_max ~ty e =
    let high = high ~ty in
    Expr.BinOp (e, IMod, Expr.int_z high)
  in
  let plus_max_modulo_max ~ty e =
    let high = Expr.int_z (high ~ty) in
    let plus_max = Expr.Infix.( + ) e high in
    Expr.imod plus_max high
  in
  let compile_with =
    (* let open Cgil_lib.CConstants.BinOp_Functions in *)
    let open Constants.Binop_functions in
    match binop with
    | Equal -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ -> GilBinop BinOp.Equal
        | CInteger I_size_t | Pointer _ -> Proc eq_maybe_ptr
        | _ -> Unhandled `With_type)
    | Notequal -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ -> GilBinop BinOp.Equal ||> Expr.Infix.not
        | CInteger I_size_t | Pointer _ -> Proc neq_maybe_ptr
        | _ -> Unhandled `With_type)
    | IeeeFloatEqual -> (
        match lty with
        | Float | Double -> GilBinop Equal
        | _ -> Unhandled `With_type)
    | IeeeFloatNotequal -> (
        match lty with
        | Float | Double -> GilBinop Equal ||> Expr.Infix.not
        | _ -> Unhandled `With_type)
    | Le -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ -> GilBinop ILessThanEqual
        | CInteger I_size_t | Pointer _ -> Proc leq_maybe_ptr
        | _ -> Unhandled `With_type)
    | Lt -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ -> GilBinop ILessThan
        | CInteger I_size_t | Pointer _ -> Proc lt_maybe_ptr
        | _ -> Unhandled `With_type)
    | Gt -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ ->
            GilBinop ILessThanEqual ||> fun e -> Expr.Infix.not e
        | CInteger I_size_t | Pointer _ -> Proc gt_maybe_ptr
        | _ -> Unhandled `With_type)
    | Ge -> (
        match lty with
        | CInteger (I_int | I_char | I_bool | I_ssize_t)
        | Unsignedbv _ | Signedbv _ ->
            GilBinop ILessThan ||> fun e -> Expr.Infix.not e
        | CInteger I_size_t | Pointer _ -> Proc geq_maybe_ptr
        | _ -> Unhandled `With_type)
    | Plus -> (
        match (lty, rty) with
        | Pointer pty, CInteger (I_size_t | I_ssize_t) ->
            let factor = Ctx.size_of ctx pty in
            let pre e1 e2 = (e1, Expr.Infix.( * ) e2 (Expr.int factor)) in
            Pre (pre, Proc add_maybe_ptr)
        | CInteger (I_size_t | I_ssize_t), Pointer pty ->
            let factor = Ctx.size_of ctx pty in
            let pre e1 e2 = (Expr.Infix.( * ) e1 (Expr.int factor), e2) in
            Pre (pre, Proc add_maybe_ptr)
        | CInteger (I_size_t | I_ssize_t), CInteger (I_size_t | I_ssize_t) ->
            Proc add_maybe_ptr
        | CInteger I_int, CInteger I_int | CInteger I_char, CInteger I_char ->
            GilBinop IPlus |||> assert_int_in_bounds ~ty:lty
        | Unsignedbv { width = widtha }, Unsignedbv { width = widthb }
          when widtha == widthb -> GilBinop IPlus ||> modulo_max ~ty:lty
        | Signedbv { width = widtha }, Signedbv { width = widthb }
          when widtha == widthb ->
            GilBinop IPlus |||> assert_int_in_bounds ~ty:lty
        | _ -> Unhandled `With_type)
    | Minus -> (
        match (lty, rty) with
        | (CInteger I_size_t | Pointer _), (CInteger I_size_t | Pointer _) ->
            Proc sub_maybe_ptr
        | CInteger I_int, CInteger I_int
        | CInteger I_char, CInteger I_char
        | CInteger I_ssize_t, CInteger I_ssize_t ->
            GilBinop IMinus |||> assert_int_in_bounds ~ty:lty
        | Unsignedbv { width = widtha }, Unsignedbv { width = widthb }
          when widtha == widthb ->
            GilBinop IMinus ||> plus_max_modulo_max ~ty:lty
        | Signedbv { width = widtha }, Signedbv { width = widthb }
          when widtha == widthb ->
            GilBinop IMinus |||> assert_int_in_bounds ~ty:lty
        | _ -> Unhandled `With_type)
    | Mult -> (
        match lty with
        | Unsignedbv _ -> GilBinop ITimes ||> modulo_max ~ty:lty
        | CInteger _ | Signedbv _ ->
            GilBinop ITimes |||> assert_int_in_bounds ~ty:lty
        | _ -> Unhandled `With_type)
    | Div -> (
        match lty with
        | CInteger _ | Unsignedbv _ | Signedbv _ ->
            GilBinop IDiv |||> assert_int_in_bounds ~ty:lty
        | _ -> Unhandled `With_type)
    | Mod -> (
        match lty with
        | CInteger I_size_t -> Proc mod_maybe_ptr
        | CInteger _ | Unsignedbv _ | Signedbv _ -> GilBinop IMod
        | _ -> Unhandled `With_type)
    | Or -> GilBinop BinOp.BOr
    | And -> GilBinop BinOp.BAnd
    | OverflowPlus -> (
        let int_check =
          GilBinop IPlus ||> int_in_bounds ~ty:lty ||> Expr.Infix.not
        in
        match (lty, rty) with
        | CInteger (I_size_t | I_ssize_t), CInteger (I_size_t | I_ssize_t) ->
            Proc overflow_plus_maybe_ptr
        | CInteger I_int, CInteger I_int | CInteger I_char, CInteger I_char ->
            int_check
        | Unsignedbv { width = widtha }, Unsignedbv { width = widthb }
          when widtha == widthb -> int_check
        | Signedbv { width = widtha }, Signedbv { width = widthb }
          when widtha == widthb -> int_check
        | _ -> Unhandled `With_type)
    | OverflowMult ->
        GilBinop ITimes ||> int_in_bounds ~ty:lty ||> Expr.Infix.not
    | OverflowMinus ->
        GilBinop IMinus ||> int_in_bounds ~ty:lty ||> Expr.Infix.not
    | OverflowResultMinus ->
        App
          (fun left right ->
            let result = Expr.Infix.( - ) left right in
            let overflowed = Expr.Infix.not (int_in_bounds ~ty:lty result) in
            Expr.EList [ result; overflowed ])
    | OverflowResultPlus ->
        App
          (fun left right ->
            let result = Expr.Infix.( + ) left right in
            let overflowed = Expr.Infix.not (int_in_bounds ~ty:lty result) in
            Expr.EList [ result; overflowed ])
    | OverflowResultMult ->
        App
          (fun left right ->
            let result = Expr.Infix.( * ) left right in
            let overflowed = Expr.Infix.not (int_in_bounds ~ty:lty result) in
            Expr.EList [ result; overflowed ])
    | _ -> Unhandled `Without_type
  in
  let e1 = Val_repr.as_value ~msg:"Binary operand" e1 in
  let e2 = Val_repr.as_value ~msg:"Binary operand" e2 in
  let rec compute e1 e2 = function
    | Then (b, f) -> Cs.bind (compute e1 e2 b) f
    | Pre (f, b) ->
        let e1, e2 = f e1 e2 in
        compute e1 e2 b
    | Proc internal_function ->
        let gvar = Ctx.fresh_v ctx in
        let call =
          Cmd.Call (gvar, Lit (String internal_function), [ e1; e2 ], None, None)
        in
        Cs.return ~app:[ call ] (Expr.PVar gvar)
    | GilBinop b -> Cs.return (Expr.BinOp (e1, b, e2))
    | App f -> Cs.return (f e1 e2)
    | Unhandled wt ->
        let type_info =
          match wt with
          | `With_type -> Some (lty, rty)
          | `Without_type -> None
        in
        let cmd =
          Helpers.assert_unhandled ~feature:(BinOp (binop, type_info)) []
        in
        Cs.return ~app:[ cmd ] (Expr.Lit Nono)
  in
  compute e1 e2 compile_with

let fresh_sv ctx =
  let v = Ctx.fresh_v ctx in
  let cmd = Cmd.Logic (FreshSVar v) in
  Cs.return ~app:[ cmd ] v

let rec assume_type ~ctx (type_ : GType.t) (expr : Expr.t) : unit Cs.with_cmds =
  let open Cs.Syntax in
  match type_ with
  | CInteger I_bool ->
      (* Special case, the bounds are different *)
      let assume_int = Cmd.Logic (AssumeType (expr, IntType)) in
      let condition =
        let open Formula.Infix in
        expr #== Expr.one_i #|| (expr #== Expr.zero_i)
      in
      let assume_range = Cmd.Logic (Assume condition) in
      Cs.return ~app:[ assume_int; assume_range ] ()
  | CInteger _ | Signedbv _ | Unsignedbv _ ->
      let assume_int = Cmd.Logic (AssumeType (expr, IntType)) in
      let bounds =
        Option.bind (Memory.chunk_for_type ~ctx type_) Chunk.bounds
      in
      let assume_range =
        match bounds with
        | None -> []
        | Some (low, high) ->
            let open Formula.Infix in
            let condition =
              (Expr.int_z low) #<= expr #&& (expr #<= (Expr.int_z high))
            in
            [ Cmd.Logic (Assume condition) ]
      in
      Cs.unit (assume_int :: assume_range)
  | Double | Float ->
      let assume_num = Cmd.Logic (AssumeType (expr, NumberType)) in
      Cs.unit [ assume_num ]
  | Pointer _ ->
      let* loc = fresh_sv ctx in
      let* ofs = fresh_sv ctx in
      let e_loc = Expr.PVar loc in
      let e_ofs = Expr.PVar ofs in
      let assume_list =
        let f = Formula.Eq (expr, EList [ e_loc; e_ofs ]) in
        Cmd.Logic (Assume f)
      in
      let assume_obj = Cmd.Logic (AssumeType (e_loc, ObjectType)) in
      let assume_int = Cmd.Logic (AssumeType (e_ofs, IntType)) in
      Cs.unit [ assume_list; assume_obj; assume_int ]
  | Bool ->
      let assume_bool = Cmd.Logic (AssumeType (expr, BooleanType)) in
      Cs.unit [ assume_bool ]
  | StructTag _ | Struct _ ->
      let ty =
        let fields = Ctx.resolve_struct_components ctx type_ in
        match Ctx.one_representable_field ctx fields with
        | Some (_, ty) -> ty
        | None ->
            Error.code_error "No representable type for assume_type struct"
      in
      assume_type ~ctx ty expr
  | _ -> Error.code_error "Unreachable: assume_type for non-scalar"

let rec nondet_expr ~ctx ~loc ~type_ () : Val_repr.t Cs.with_body =
  let b = Body_item.make_hloc ~loc in
  let open Cs.Syntax in
  let is_symbolic_exec =
    let open Gillian.Utils in
    Exec_mode.is_symbolic_exec !Config.current_exec_mode
  in
  if not is_symbolic_exec then
    Error.user_error
      "Looks like you're compiling some nondet variables, but you're not in \
       symbolic execution mode"
  else if Ctx.is_zst_access ctx type_ then
    Cs.return (Val_repr.ByValue (Lit Null))
  else if Ctx.representable_in_store ctx type_ then
    let* fresh = fresh_sv ctx |> Cs.map_l b in
    let fresh = Expr.PVar fresh in
    let+ () = assume_type ~ctx type_ fresh |> Cs.map_l b in
    Val_repr.ByValue fresh
  else
    match type_ with
    | StructTag tag | UnionTag tag ->
        nondet_expr ~ctx ~loc ~type_:(Ctx.tag_lookup ctx tag) ()
    | Struct { components; _ } -> (
        match Ctx.one_representable_field ctx components with
        | None ->
            let rec writes curr components () =
              match components with
              | [] -> Seq.Nil
              | Datatype_component.Padding { bits; _ } :: fields ->
                  (* Ignoring what's written in the padding, probably a nondet, in any case it should be poison. *)
                  let byte_width = bits / 8 in
                  Cons
                    ( (curr, Val_repr.Poison { byte_width }),
                      writes (curr + byte_width) fields )
              | Field { type_; _ } :: fields ->
                  let rest = writes (curr + Ctx.size_of ctx type_) fields in
                  if Ctx.is_zst_access ctx type_ then rest ()
                  else
                    Cons
                      ( ( curr,
                          Val_repr.V
                            { type_; value = nondet_expr ~ctx ~loc ~type_ () }
                        ),
                        rest )
            in
            let writes = writes 0 components in
            Cs.return (Val_repr.ByCompositValue { type_; writes })
        | Some (_, type_) -> nondet_expr ~ctx ~loc ~type_ ())
    | Array (ty, sz) ->
        let rec writes cur () =
          if cur == sz then Seq.Nil
          else
            Cons
              ( ( cur,
                  Val_repr.V
                    { type_ = ty; value = nondet_expr ~ctx ~loc ~type_:ty () }
                ),
                writes (cur + 1) )
        in
        let writes = writes 0 in
        Cs.return (Val_repr.ByCompositValue { type_; writes })
    | Union { components; _ } ->
        (* Ok this one is tricky, because each variant will have
           a different layout, and that doesn't work well with my memory.
           So we'll have to branch *)
        let variant_amount = List.length components in
        let* variant =
          Cs.map_l b
          @@ let* variant = fresh_sv ctx in
             let variant = Expr.PVar variant in
             let variant_number = Expr.int variant_amount in
             let variant_int = LCmd.AssumeType (variant, IntType) in
             let variant_constraint =
               let open Formula.Infix in
               Expr.zero_i #<= variant #&& (variant #< variant_number)
             in
             let variant_value = LCmd.Assume variant_constraint in
             Cs.return
               ~app:[ Cmd.Logic variant_int; Logic variant_value ]
               variant
        in
        let* ret_ptr =
          Memory.alloc_temp ~ctx ~location:loc type_ |> Cs.map_l b
        in
        let end_lab = Ctx.fresh_lab ctx in
        let goto_end = Cmd.Goto end_lab in
        let next_lab = ref None in
        let rec gen i components =
          match components with
          | [] -> []
          | Datatype_component.Padding _ :: _ ->
              Error.unexpected "Padding in union fields"
          | Field { type_; _ } :: rest ->
              let curr_lab = !next_lab in
              let nlab = Ctx.fresh_lab ctx in
              let () = next_lab := Some nlab in
              let block_lab = Ctx.fresh_lab ctx in
              let goto =
                b ?label:curr_lab
                  (Cmd.GuardedGoto
                     (Expr.BinOp (variant, Equal, Expr.int i), block_lab, nlab))
              in
              let v, cmds = nondet_expr ~ctx ~loc ~type_ () in
              let write =
                Memory.write ~ctx ~type_ ~annot:b ~dst:ret_ptr ~src:v
              in
              let block =
                set_first_label ~annot:(b ~loop:[]) block_lab
                  (cmds @ write @ [ b goto_end ])
              in
              (goto :: block) @ gen (i + 1) rest
        in
        let* () = Cs.unit (gen 0 components) in
        let* () =
          Cs.unit
            [
              b ?label:!next_lab (Fail ("Unreachable", []));
              b ~label:end_lab Skip;
            ]
        in
        Cs.return (Val_repr.ByCopy { type_; ptr = ret_ptr })
    | _ ->
        let fail_cmd =
          assert_unhandled ~feature:(CompositNondet type_)
            [ Expr.string (GType.show type_) ]
        in
        let+ () = Cs.unit [ b fail_cmd ] in
        Val_repr.ByCopy { ptr = Lit Nono; type_ }

type cast_with =
  | App of (Expr.t -> Expr.t)
  | Nop
  | Unhandled
  | Proc of string * Expr.t list
(* Proc(fname, [a, b]) means to cast e using fname(e, a, b) *)

let compile_cast ~(ctx : Ctx.t) ~(from : GType.t) ~(into : GType.t) e :
    Val_repr.t Cs.with_cmds =
  let from_chunk = Memory.chunk_for_type ~ctx from in
  let into_chunk = Memory.chunk_for_type ~ctx into in
  let cast_with =
    match (from_chunk, into_chunk) with
    | None, Some _ -> (
        (* Special case of casting a boolean into an int.
           Handled separately because there's no chunk involved there,
           it's not a real C value. *)
        match (from, into) with
        | Bool, (CInteger _ | Unsignedbv _ | Signedbv _) ->
            Proc (Constants.Internal_functions.val_of_bool, [])
        | _ -> Unhandled)
    | _, None -> Unhandled
    | Some from_chunk, Some into_chunk -> (
        match (from_chunk, into_chunk) with
        | x, y when Chunk.equal x y -> Nop
        (* Casting a value into a bigger chunk doesn't affect the mathematical
           value, and can always be done *)
        | (U8 | I8), (U16 | U32 | U64 | U128 | I16 | I32 | I64 | I128)
        | (U16 | I16), (U32 | U64 | U128 | I32 | I64 | I128)
        | (U32 | I32), (U64 | U128 | I64 | I128)
        | (U64 | I64), (U128 | I128) -> Nop
        | U128, (U64 | U32 | U16 | U8)
        | U64, (U32 | U16 | U8)
        | U32, (U16 | U8)
        | U16, U8 ->
            let chunk_size_bits = Chunk.size into_chunk * 8 in
            let to_mod_z = Expr.int_z Z.(one lsl chunk_size_bits) in
            App (fun e -> Expr.BinOp (e, IMod, to_mod_z))
        | I128, U128 | I64, U64 | I32, U32 | I16, U16 | I8, U8 ->
            let chunk_size_bits = Chunk.size into_chunk * 8 in
            let two_power_size = Z.(one lsl chunk_size_bits) in
            let imax = Expr.int_z Z.((two_power_size asr 1) - one) in
            let two_power_size = Expr.int_z two_power_size in
            Proc
              ( Constants.Cast_functions.unsign_int_same_size,
                [ imax; two_power_size ] )
        | U8, I8 | U16, I16 | U32, I32 | U64, I64 | U128, I128 -> Nop
        | _ -> Unhandled)
  in
  match cast_with with
  | Proc (function_name, additional_params) ->
      let function_name = Expr.Lit (String function_name) in
      let e = Val_repr.as_value ~msg:"TypeCast operand" e in
      let temp = Ctx.fresh_v ctx in
      let call =
        Cmd.Call (temp, function_name, e :: additional_params, None, None)
      in
      Cs.return ~app:[ call ] (Val_repr.ByValue (PVar temp))
  | App f ->
      let e = Val_repr.as_value ~msg:"TypeCast operand" e in
      Cs.return (Val_repr.ByValue (f e))
  | Nop -> Cs.return e
  | Unhandled ->
      let cmd = Helpers.assert_unhandled ~feature:(Cast (from, into)) [] in
      Cs.return ~app:[ cmd ] (Val_repr.dummy ~ctx into)

let rec lvalue_as_access ~ctx ~read (lvalue : GExpr.t) : access Cs.with_body =
  if Ctx.is_zst_access ctx lvalue.type_ then Cs.return ZST
  else
    let open Cs.Syntax in
    let b = Body_item.make ~loc:(Body_item.compile_location lvalue.location) in
    let as_access = lvalue_as_access ~ctx ~read in
    match lvalue.value with
    | Struct _ | Array _ | StringConstant _ ->
        let* composit_value = compile_expr ~ctx lvalue in
        let* ptr =
          Memory.alloc_temp ~ctx ~location:lvalue.location lvalue.type_
          |> Cs.map_l b
        in
        let* () =
          ( (),
            Memory.write ~ctx ~type_:lvalue.type_ ~dst:ptr ~src:composit_value
              ~annot:(b ~loop:[]) )
        in
        Cs.return (InMemoryComposit { ptr; type_ = lvalue.type_ })
    | Symbol x ->
        if Ctx.is_local ctx x then
          if not (Ctx.representable_in_store ctx lvalue.type_) then
            Cs.return (InMemoryComposit { ptr = PVar x; type_ = lvalue.type_ })
          else if Ctx.in_memory ctx x then
            Cs.return (InMemoryScalar { ptr = PVar x; loaded = None })
          else (Direct x, [])
        else if Ctx.is_function_symbol ctx x then Cs.return (DirectFunction x)
        else
          let+ ptr = Genv.lookup_symbol ~ctx x |> Cs.map_l b in
          if GType.is_function lvalue.type_ then
            InMemoryFunction { ptr; symbol = Some x }
          else if Ctx.representable_in_store ctx lvalue.type_ then
            InMemoryScalar { ptr; loaded = None }
          else InMemoryComposit { ptr; type_ = lvalue.type_ }
    | Dereference e -> (
        let* ge = compile_expr ~ctx e in
        match ge with
        | ByValue ge ->
            (* We do read the memory, but it might be a "fake read".
               We don't necessarily need the value if we're going get its address
                (i.e &*p). Therefore, we keep both the value and the pointer around. *)
            if GType.is_function lvalue.type_ then
              Cs.return (InMemoryFunction { ptr = ge; symbol = None })
            else if not (Ctx.representable_in_store ctx lvalue.type_) then
              (* In the read case, some validity should be checked *)
              Cs.return (InMemoryComposit { ptr = ge; type_ = lvalue.type_ })
            else if read then
              let+ v = Memory.load_scalar ~ctx ge lvalue.type_ |> Cs.map_l b in
              InMemoryScalar { ptr = ge; loaded = Some (PVar v) }
            else Cs.return (InMemoryScalar { ptr = ge; loaded = None })
        | ByCopy _ | ByCompositValue _ ->
            Error.unexpected "Pointers should be scalars passed by value"
        | Procedure _ -> Error.unexpected "Dereferencing a procedure")
    | Index { array; index } ->
        let* index = compile_expr ~ctx index in
        let index =
          match index with
          | ByValue index -> index
          | _ -> Error.unexpected "Indexing with non-scalar value"
        in
        let* lhs_access = as_access array in
        let* ptr =
          match lhs_access with
          | InMemoryComposit { ptr; _ } -> Cs.return ptr
          | ZST ->
              let cmd = b (assert_unhandled ~feature:FlexibleArrayMember []) in
              Cs.return ~app:[ cmd ] (Expr.Lit Nono)
          | _ -> Error.code_error "Array access is not in-memory-composit"
        in
        let sz = Expr.int (Ctx.size_of ctx lvalue.type_) in
        let offset =
          let open Expr.Infix in
          index * sz
        in
        let ptr = Memory.ptr_add_e ptr offset in
        if Ctx.representable_in_store ctx lvalue.type_ then
          Cs.return (InMemoryScalar { ptr; loaded = None })
        else Cs.return (InMemoryComposit { ptr; type_ = lvalue.type_ })
    | Member { lhs; field } -> (
        let* lhs_access = as_access lhs in
        match lhs_access with
        | InMemoryComposit { ptr; type_ = lhs_ty } ->
            let field_offset = Ctx.offset_struct_field ctx lhs_ty field in
            let ptr = Memory.ptr_add ptr field_offset in
            if Ctx.representable_in_store ctx lvalue.type_ then
              (* I might have to perform the read here,
                 in case fake-read is necessary *)
              Cs.return (InMemoryScalar { ptr; loaded = None })
            else Cs.return (InMemoryComposit { ptr; type_ = lvalue.type_ })
        | Direct ovflr when Ctx.is_overflow_result ctx lhs.type_ -> (
            (* an overflow result, which we represent as a pair *)
            match GType.Overflow_result.field field with
            | Result ->
                Cs.return
                  (ListMember { list = ovflr; index = 0; total_size = 2 })
            | Overflowed ->
                Cs.return
                  (ListMember { list = ovflr; index = 1; total_size = 2 }))
        | Direct _ | InMemoryScalar _ ->
            (* This the case when the structure has one non-zst type that
                 is also representable in store, and can therefore be†
                 represented transparently in the store. *)
            let () =
              (* Bunch of sanity checks *)
              let () =
                match lhs.type_ with
                | Struct _ | StructTag _ -> ()
                | _ ->
                    Error.code_error
                      (Fmt.str
                         "Member direct access, expected a structure type but \
                          got %a"
                         GType.pp lhs.type_)
              in
              let components = Ctx.resolve_struct_components ctx lhs.type_ in
              match Ctx.one_representable_field ctx components with
              | None ->
                  Error.code_error
                    "Expected one representable field in structure direct \
                     access"
              | Some (or_field :: _, _) when not (String.equal or_field field)
                ->
                  Error.code_error
                    "Mismatch in one representable field in structure direct \
                     access"
              | _ -> ()
            in
            Cs.return lhs_access
        | _ ->
            Error.code_error
              (Fmt.str "Structure access is not in-memory-composit but %a"
                 pp_access lhs_access))
    | _ ->
        Error.code_error
          (Fmt.str "lvalue_as_access for something that isn't an lvalue: %a"
             GExpr.pp_full lvalue)

and compile_call ~ctx ~add_annot:b (func : GExpr.t) (args : GExpr.t list) =
  let open Cs.Syntax in
  let return_type =
    match func.type_ with
    | Code { return_type; _ } -> return_type
    | _ ->
        Error.unexpected
          "function call where the function doesn't have type code"
  in
  let location = func.location in
  let by_value ?app e = Cs.return ?app (Val_repr.ByValue e) in
  let by_copy ?app ptr type_ =
    Cs.return ?app (Val_repr.ByCopy { ptr; type_ })
  in
  match func.value with
  | Symbol "__CPROVER_assume" ->
      let to_assume =
        match args with
        | [ to_assume ] -> to_assume
        | _ -> Error.user_error "__CPROVER_assume not given 1 params"
      in
      (* I'm not sure it's always an int, I'll need to check, but there's a strong chance *)
      let cast_to_bool =
        match to_assume.type_ with
        | Bool -> false
        | _ -> true
      in
      let* to_assume = compile_expr ~ctx to_assume in
      let to_assume =
        Val_repr.as_value ~msg:"__CPROVER_assume operand" to_assume
      in
      let* to_assume =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Constants.Internal_functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assume ], None, None)
          in
          Cs.return ~app:[ b call ] (Expr.PVar temp)
        else Cs.return to_assume
      in
      let f =
        match Formula.lift_logic_expr to_assume with
        | None ->
            Logging.normal ~severity:Warning (fun m ->
                m "Cannot assume %a, assuming False instead" Expr.pp to_assume);
            Formula.False
        | Some (f, _) -> f
      in
      by_value ~app:[ b (Logic (Assume f)) ] (Lit Null)
  | Symbol "__CPROVER_assert" ->
      (* The second argument of assert is a string that we may to keep
         alive for error messages. For now we're discarding it.
         In the future, we could change Gillian's assume to also have an error message *)
      (* I should still find a way to factor out the call to assume and assert *)
      let to_assert =
        match args with
        | [ to_assert; _msg ] -> to_assert
        | _ -> Error.user_error "__CPROVER_assert not given 2 params"
      in
      (* I'm not sure it's always an int, I'll need to check, but there's a strong chance *)
      let cast_to_bool =
        match to_assert.type_ with
        | Bool -> false
        | _ -> true
      in
      let* to_assert = compile_expr ~ctx to_assert in
      let to_assert =
        Val_repr.as_value ~msg:"__CPROVER_assert operand" to_assert
      in
      let* to_assert =
        if cast_to_bool then
          let temp = Ctx.fresh_v ctx in
          let bool_of_value =
            Expr.Lit (String Constants.Internal_functions.bool_of_val)
          in
          let call =
            Cmd.Call (temp, bool_of_value, [ to_assert ], None, None)
          in
          Cs.return ~app:[ b call ] (Expr.PVar temp)
        else Cs.return to_assert
      in
      let f =
        match Formula.lift_logic_expr to_assert with
        | None ->
            Logging.normal ~severity:Warning (fun m ->
                m "Cannot assert %a, asserting False instead" Expr.pp to_assert);
            Formula.False
        | Some (f, _) -> f
      in
      by_value ~app:[ b (Logic (Assert f)) ] (Expr.Lit Null)
  | _ ->
      let* e = compile_expr ~ctx func in
      let fname =
        match e with
        | Procedure (Lit (String f) as fname) -> (
            match Constants.Internal_functions.hook f with
            | Some fname -> Expr.string fname
            | _ -> fname)
        | Procedure e -> e
        | _ ->
            Error.code_error "function call of something that isn't a procedure"
      in
      let* args =
        Cs.many
          (fun arg ->
            let* c_arg = compile_expr ~ctx arg in
            match c_arg with
            | Val_repr.ByValue e -> Cs.return e
            | ByCopy { ptr; _ } ->
                (* When passing by copy, we trust the receiving function to copy the value when it receives it. *)
                Cs.return ptr
            | ByCompositValue { writes; type_ } ->
                let* temp =
                  Memory.alloc_temp ~ctx ~location:arg.location type_
                  |> Cs.map_l b
                in
                let+ () =
                  Cs.unit (Memory.write_composit ~ctx ~annot:b ~dst:temp writes)
                in
                temp
            | Procedure _ -> Error.unexpected "passing a procedure as argument")
          args
      in
      (* If the return value has to be passed by copy,
         we add an argument before every other in which the return value will be stored.
         It's a trick of CompCert.*)
      if Ctx.representable_in_store ctx return_type then
        let ret_var = Ctx.fresh_v ctx in
        let gil_call = Cmd.Call (ret_var, fname, args, None, None) in
        by_value ~app:[ b gil_call ] (Expr.PVar ret_var)
      else
        (* If the function returns by copy, we add first parameter
           that will contain the result. *)
        let* temp_arg =
          Memory.alloc_temp ~ctx ~location return_type |> Cs.map_l b
        in
        let unused_temp = Ctx.fresh_v ctx in
        let gil_call =
          Cmd.Call (unused_temp, fname, temp_arg :: args, None, None)
        in
        by_copy ~app:[ b gil_call ] temp_arg return_type

and poison ~ctx ~annot (lhs : GExpr.t) =
  let type_ = lhs.type_ in
  let access, pre = lvalue_as_access ~ctx ~read:false lhs in
  let write =
    match access with
    | ZST -> Cmd.Skip
    | Direct x -> Assignment (x, Lit Undefined)
    | InMemoryScalar { ptr; _ } | InMemoryComposit { ptr; _ } ->
        Memory.poison ~ctx ~dst:ptr (Ctx.size_of ctx type_)
    | ListMember { list; index; total_size } ->
        write_list_member ~list ~index ~total_size (Lit Undefined)
    | InMemoryFunction _ | DirectFunction _ ->
        Error.unexpected "poisoning a function"
  in
  pre @ [ annot write ]

and compile_assign ~ctx ~annot ~lhs ~rhs =
  let v, pre1 = compile_expr ~ctx rhs in
  let access, pre2 = lvalue_as_access ~ctx ~read:false lhs in
  let write =
    match (access, v) with
    | ZST, _ ->
        [ annot Cmd.Skip ]
        (* We need a command in case we try want to add a label *)
    | Direct x, ByValue v -> [ annot (Assignment (x, v)) ]
    | InMemoryScalar { ptr; _ }, ByValue v ->
        [ annot (Memory.store_scalar ~ctx ptr v lhs.type_) ]
    | ( InMemoryComposit { ptr = ptr_access; type_ = type_access },
        ByCopy { ptr = ptr_v; type_ = type_v } ) ->
        if not (Ctx.type_equal ctx type_v type_access) then
          Error.unexpected "ByCopy assignment with different types on each side"
        else
          let copy_cmd =
            Memory.memcpy ~ctx ~type_:type_access ~dst:ptr_access ~src:ptr_v
          in
          [ annot copy_cmd ]
    | InMemoryComposit { ptr = dst; _ }, ByCompositValue { writes; _ } ->
        Memory.write_composit ~ctx ~annot ~dst writes
    | _ ->
        Error.code_error
          (Fmt.str
             "Invalid assignement, wrong mix of ByCopy and Direct assignments:\n\
              %a = %a.\n\
              Originally: %a = %a\n\n\
             \              Left type is %a and right type is %a" pp_access
             access Val_repr.pp v GExpr.pp lhs GExpr.pp rhs GType.pp lhs.type_
             GType.pp rhs.type_)
  in
  (v, pre1 @ pre2 @ write)

and compile_expr ~(ctx : Ctx.t) (expr : GExpr.t) : Val_repr.t Cs.with_body =
  let open Cs.Syntax in
  let by_value ?app t = Cs.return ?app (Val_repr.ByValue t) in
  let by_copy ?app ptr type_ =
    Cs.return ?app (Val_repr.ByCopy { ptr; type_ })
  in
  let compile_expr = compile_expr ~ctx in
  let loc = Body_item.compile_location expr.location in
  let b = Body_item.make ~loc in
  let unhandled feature =
    let cmd = assert_unhandled ~feature [] in
    let v = Val_repr.dummy ~ctx expr.type_ in
    Cs.return ~app:[ b cmd ] v
  in
  match expr.value with
  | Symbol _ | Dereference _ | Index _ | Member _ -> (
      if Ctx.is_zst_access ctx expr.type_ then by_value (Lit Null)
      else
        let* access = lvalue_as_access ~ctx ~read:true expr in
        match access with
        | ZST -> by_value (Lit Null)
        | Direct x -> by_value (Expr.PVar x)
        | ListMember { list; index; _ } ->
            by_value (Expr.list_nth (PVar list) index)
        | InMemoryScalar { loaded = Some e; _ } -> by_value e
        | InMemoryScalar { loaded = None; ptr } ->
            let* var = Memory.load_scalar ~ctx ptr expr.type_ |> Cs.map_l b in
            by_value (PVar var)
        | InMemoryComposit { ptr; type_ } -> by_copy ptr type_
        | InMemoryFunction { symbol = Some sym; _ } ->
            Cs.return (Val_repr.Procedure (Expr.string sym))
        | InMemoryFunction { ptr; symbol = None } ->
            let symbol = Ctx.fresh_v ctx in
            let get_name = Constants.Internal_functions.get_function_name in
            let call =
              Cmd.Call (symbol, Lit (String get_name), [ ptr ], None, None)
            in
            Cs.return ~app:[ b call ] (Val_repr.Procedure (Expr.PVar symbol))
        | DirectFunction symbol ->
            Cs.return (Val_repr.Procedure (Lit (String symbol))))
  | AddressOf x -> (
      let* access = lvalue_as_access ~ctx ~read:true x in
      match access with
      | ZST ->
          (* FIXME: we can't model alignment yet *)
          let* ptr =
            nondet_expr ~ctx ~loc:expr.location ~type_:(CInteger I_size_t) ()
          in
          let ptr =
            Val_repr.as_value ~msg:"Nondet I_size_t for ZST pointer" ptr
          in
          (* We need to assert that the ZST pointer is not null.
             If the null pointer is not zero, I don't know what to do.
          *)
          assert ctx.machine.null_is_zero;
          let assume_not_null =
            let open Formula.Infix in
            b (Cmd.Logic (Assume (fnot ptr #== Expr.zero_i)))
          in
          let assume_align_8 =
            let open Formula.Infix in
            let mod_8 = Expr.BinOp (ptr, IMod, Expr.int 8) in
            b (Cmd.Logic (Assume mod_8 #== Expr.zero_i))
          in
          Cs.return
            ~app:[ assume_not_null; assume_align_8 ]
            (Val_repr.ByValue ptr)
          (* Should probably just return a long, with a nondet value that has the right offset *)
      | InMemoryScalar { ptr; _ }
      | InMemoryComposit { ptr; _ }
      | InMemoryFunction { ptr; _ } -> by_value ptr
      | DirectFunction symbol ->
          let+ ptr = Genv.lookup_symbol ~ctx symbol |> Cs.map_l b in
          Val_repr.ByValue ptr
      | Direct x -> Error.code_error ("address of direct access to " ^ x)
      | ListMember _ -> Error.code_error "address of list member access")
  | BoolConstant b -> by_value (Lit (Bool b))
  | CBoolConstant b ->
      let z = if b then Z.one else Z.zero in
      by_value (Lit (Int z))
  | PointerConstant b ->
      let z = Z.of_int b in
      by_value (Lit (Int z))
  | IntConstant z -> by_value (Lit (Int z))
  | DoubleConstant f | FloatConstant f -> by_value (Lit (Num f))
  | BinOp { op; lhs; rhs } ->
      let* e1 = compile_expr lhs in
      let* e2 = compile_expr rhs in
      let lty = lhs.type_ in
      let rty = rhs.type_ in
      let+ e = compile_binop ~ctx ~lty ~rty op e1 e2 |> Cs.map_l b in
      Val_repr.ByValue e
  | UnOp { op; e } -> (
      let* comp_e = compile_expr e in
      let comp_e = Val_repr.as_value ~msg:"Unary operand" comp_e in
      match op with
      | Not -> by_value (Expr.Infix.not comp_e)
      | ObjectSize ->
          let* size =
            Memory.object_size ~ctx ~ptr_ty:e.type_ comp_e |> Cs.map_l b
          in
          by_value size
      | UnaryMinus -> (
          match e.type_ with
          | CInteger _ | Unsignedbv _ | Signedbv _ ->
              by_value (Expr.UnOp (IUnaryMinus, comp_e))
          | Float | Double -> by_value (Expr.UnOp (FUnaryMinus, comp_e))
          | _ ->
              let cmd = b (Helpers.assert_unhandled ~feature:(UnOp op) []) in
              Cs.return ~app:[ cmd ] (Val_repr.ByValue (Lit Nono)))
      | op ->
          let cmd = b (Helpers.assert_unhandled ~feature:(UnOp op) []) in
          Cs.return ~app:[ cmd ] (Val_repr.ByValue (Lit Nono)))
  | Nondet -> nondet_expr ~ctx ~loc:expr.location ~type_:expr.type_ ()
  | TypeCast to_cast ->
      let* to_cast_e = compile_expr to_cast in
      compile_cast ~ctx ~from:to_cast.type_ ~into:expr.type_ to_cast_e
      |> Cs.map_l b
  | EAssign { lhs; rhs } -> compile_assign ~ctx ~lhs ~rhs ~annot:b
  | EFunctionCall { func; args } -> compile_call ~ctx ~add_annot:b func args
  | If { cond; then_; else_ } ->
      let* cond_e = compile_expr cond in
      let then_lab = Ctx.fresh_lab ctx in
      let else_lab = Ctx.fresh_lab ctx in
      let cond_e = Val_repr.as_value ~msg:"Expr If condition" cond_e in
      let* () = Cs.unit [ b (GuardedGoto (cond_e, then_lab, else_lab)) ] in
      let res = Ctx.fresh_v ctx in
      let end_lab = Ctx.fresh_lab ctx in
      (* Ok the following is going to be an interesting trick.
         We assign the expr, even if it's by-copy.
         We don't copy! So we're kinda hacking inside
         the type abstraction. *)
      let* res_then =
        let res_then =
          let* t = compile_expr then_ in
          let* res = Val_repr.copy_into t res |> Cs.map_l b in
          let cmd = b (Cmd.Goto end_lab) in
          Cs.return ~app:[ cmd ] res
        in
        res_then |> Cs.with_label ~annot:(b ~loop:[]) then_lab
      in
      let* res_else =
        let res_else =
          let* t = compile_expr else_ in
          Val_repr.copy_into t res |> Cs.map_l b
        in
        res_else |> Cs.with_label ~annot:(b ~loop:[]) else_lab
      in
      Error.assert_
        (Val_repr.equal res_then res_else)
        "if branche exprs must be equal";
      Cs.return ~app:[ b ~label:end_lab Skip ] res_then
  | ByteExtract { e; offset } ->
      if Ctx.is_zst_access ctx e.type_ then
        if Ctx.is_zst_access ctx expr.type_ && offset == 0 then
          Cs.return Val_repr.null
        else
          Error.unexpected
            (Fmt.str "Byte Extract of ZST that doesn't result in a ZST: %a"
               GExpr.pp expr)
      else
        let* e_repr = compile_expr e in
        let* ptr_to_read =
          match e_repr with
          | ByCopy { ptr; _ } -> Cs.return ptr
          | ByValue _ | ByCompositValue _ ->
              let* ptr =
                Memory.alloc_temp ~ctx ~location:expr.location e.type_
                |> Cs.map_l b
              in
              let write =
                Memory.write ~ctx ~annot:(b ~loop:[]) ~type_:e.type_ ~dst:ptr
                  ~src:e_repr
              in
              Cs.return ~app:write ptr
          | Procedure _ -> Error.unexpected "Byte extracting a function"
        in
        let new_ptr = Memory.ptr_add ptr_to_read offset in
        if Ctx.representable_in_store ctx expr.type_ then
          let* var = Memory.load_scalar ~ctx new_ptr expr.type_ |> Cs.map_l b in
          by_value (PVar var)
        else by_copy new_ptr expr.type_
  | Struct elems -> (
      let fields = Ctx.resolve_struct_components ctx expr.type_ in
      match Ctx.one_representable_field ctx fields with
      | Some (accesses, _) ->
          let select_field accesses fields elems =
            let rec select_field ac f e =
              match (ac, f, e) with
              | a :: ar, Datatype_component.Field { name; type_ } :: af, e :: er
                ->
                  if String.equal a name then on_field ar type_ e
                  else select_field ac af er
              | _ :: _, Datatype_component.Padding _ :: af, _ :: er ->
                  select_field ac af er
              | _ -> Error.code_error "Wrong field mapping"
            and on_field a t e =
              match (a, t, e) with
              (* If we reached the leaf, or if we reached a symbol, we can stop *)
              | ( _ :: _,
                  (StructTag _ | Struct _),
                  (GExpr.{ value = Symbol _; _ } as e) )
              | [], _, e -> e
              | ( _ :: _,
                  (StructTag _ | Struct _),
                  GExpr.{ value = GExpr.Struct elems; _ } ) ->
                  let fields = Ctx.resolve_struct_components ctx t in
                  select_field a fields elems
              | _ ->
                  Error.code_error
                    Fmt.(
                      str
                        "Wrong field mapping:\naccesses: %a\ntype: %a\nelem: %a"
                        (Dump.list string) a GType.pp t GExpr.pp e)
            in
            select_field accesses fields elems
          in
          compile_expr (select_field accesses fields elems)
      | None ->
          (* We start by getting the offsets where we need to write,
             we do that on the type. *)
          let rec writes curr fields elems () =
            match (fields, elems) with
            | [], [] -> Seq.Nil
            | Datatype_component.Padding { bits; _ } :: fields, _ :: elems
            (* Ignoring what's written in the padding, probably a nondet, in any case it should be poison. *)
              ->
                let byte_width = bits / 8 in
                Cons
                  ( (curr, Val_repr.Poison { byte_width }),
                    writes (curr + byte_width) fields elems )
            | Field { type_; _ } :: fields, _ :: elems
              when Ctx.is_zst_access ctx type_ ->
                (* If the field is a ZST, we simply ignore it *)
                writes (curr + Ctx.size_of ctx type_) fields elems ()
            | Field { type_; _ } :: fields, v :: elems ->
                Cons
                  ( (curr, Val_repr.V { type_; value = compile_expr v }),
                    writes (curr + Ctx.size_of ctx type_) fields elems )
            | _ ->
                Error.unexpected
                  "Struct type fields not maching struct constant fields"
          in
          let writes = writes 0 fields elems in
          Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes }))
  | Array elems ->
      let elem_type =
        match expr.type_ with
        | Array (elem_type, _) -> elem_type
        | _ -> Error.unexpected "Array is not of Array type"
      in
      let elem_size = Ctx.size_of ctx elem_type in
      let rec writes count elems () =
        match elems with
        | [] -> Seq.Nil
        | v :: elems ->
            Cons
              ( ( count * elem_size,
                  Val_repr.V { type_ = elem_type; value = compile_expr v } ),
                writes (count + 1) elems )
      in
      let writes = writes 0 elems in
      Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes })
  | StringConstant str ->
      let char_type = GType.CInteger I_char in
      (* Size could be different from 1 n some architecture,
         might as well anticipate here *)
      let char_size = Ctx.size_of ctx char_type in
      let writes =
        String.to_seq str
        |> Seq.mapi (fun i b ->
               ( i * char_size,
                 Val_repr.V
                   {
                     type_ = char_type;
                     value = (Val_repr.ByValue (Expr.int (Char.code b)), []);
                   } ))
      in
      Cs.return (Val_repr.ByCompositValue { type_ = expr.type_; writes })
  | StatementExpression l -> compile_statement_list ~ctx l
  | EUnhandled (id, msg) -> unhandled (ExprIrep (id, msg))

and compile_statement ~ctx (stmt : Stmt.t) : Val_repr.t Cs.with_body =
  let compile_statement_c = compile_statement ~ctx in
  let compile_expr_c = compile_expr ~ctx in
  let loc = Body_item.compile_location stmt.stmt_location in
  let b = Body_item.make ~loc in
  let add_annot x = List.map b x in
  let set_first_label_opt label stmts =
    Helpers.set_first_label_opt ~annot:(b ~loop:[]) label stmts
  in
  let set_first_label label stmts = set_first_label_opt (Some label) stmts in
  let void app = Cs.return ~app (Val_repr.ByValue (Lit Nono)) in
  match stmt.body with
  | Skip -> void [ b Skip ]
  | Block ss -> compile_statement_list ~ctx ss
  | Label (s, ss) ->
      let v, cmds = compile_statement_list ~ctx ss in
      (v, set_first_label s cmds)
  | Goto lab -> [ b (Goto lab) ] |> void
  | Assume { cond } ->
      let e, pre = compile_expr_c cond in
      let e = Val_repr.as_value ~msg:"Assume operand" e in
      (* FIXME: hack to avoid wrong compilation to be in the way.
         Remove that later. *)
      let e =
        Expr.subst_expr_for_expr ~to_subst:(Expr.Lit Nono)
          ~subst_with:(Expr.Lit (Bool true)) e
      in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assume f)) ] |> void
  (* We can't output nothing, as a label might have to get attached *)
  | Assert { property_class = Some "cover"; _ } -> [ b Skip ] |> void
  | Assert { property_class = Some "missing_function"; _ } ->
      [ b (Fail ("unimplemented_function", [])) ] |> void
  | Assert { cond; property_class = _ } ->
      let e, pre = compile_expr_c cond in
      let e = Val_repr.as_value ~msg:"Assert operand" e in
      let e =
        Expr.subst_expr_for_expr ~to_subst:(Expr.Lit Nono)
          ~subst_with:(Expr.Lit (Bool false)) e
      in
      let f =
        match Formula.lift_logic_expr e with
        | None -> Error.code_error (Fmt.str "Unable to lift: %a" Expr.pp e)
        | Some (f, _) -> f
      in
      pre @ [ b (Logic (Assert f)) ] |> void
  | Return e ->
      let e, s =
        match e with
        | Some e -> (
            let open Cs.Syntax in
            let* e = compile_expr_c e in
            match e with
            | ByValue e -> Cs.return e
            | ByCopy { ptr; type_ } ->
                let dst =
                  Expr.PVar Constants.Kanillian_names.return_by_copy_name
                in
                let copy_cmd = Memory.memcpy ~ctx ~type_ ~src:ptr ~dst in
                Cs.return ~app:[ b copy_cmd ] (Expr.Lit Undefined)
            | ByCompositValue { writes; _ } ->
                let dst =
                  Expr.PVar Constants.Kanillian_names.return_by_copy_name
                in
                let cmds = Memory.write_composit ~ctx ~annot:b ~dst writes in
                Cs.return ~app:cmds (Expr.Lit Undefined)
            | Procedure _ -> Error.code_error "Return value is a procedure")
        | None -> Cs.return ~app:[] (Expr.Lit Undefined)
      in
      let variable = Utils.Names.return_variable in
      s
      @ add_annot
          [ Assignment (variable, e); Goto Constants.Kanillian_names.ret_label ]
      |> void
  | Decl { lhs = glhs; value } ->
      (* TODO:
         I have too many if/elses for deciding how things should be done,
         and that's all over the compiler. I should probably have a variant
         and a unique decision procedure for that. *)
      let ty = glhs.type_ in
      let lhs = GExpr.as_symbol glhs in
      (* ZSTs are just (GIL) Null values *)
      if Ctx.is_zst_access ctx ty then
        let cmd = Cmd.Assignment (lhs, Lit Null) in
        [ b cmd ] |> void
      else if not (Ctx.representable_in_store ctx ty) then
        let ptr, alloc_cmd = Memory.alloc_ptr ~ctx ty in
        let assign = Cmd.Assignment (lhs, ptr) in
        let write =
          match value with
          | None -> []
          | Some gv -> (
              let v, cmds = compile_expr_c gv in
              match v with
              | Val_repr.ByCompositValue { writes; _ } ->
                  cmds @ Memory.write_composit ~ctx ~annot:b ~dst:ptr writes
              | Val_repr.ByCopy { type_; ptr = src } ->
                  cmds @ [ b (Memory.memcpy ~ctx ~type_ ~dst:ptr ~src) ]
              | _ ->
                  Error.code_error
                    "Declaring composit value, not writing a composit")
        in
        [ b alloc_cmd; b assign ] @ write |> void
      else if Ctx.in_memory ctx lhs then
        let ptr, action_cmd = Memory.alloc_ptr ~ctx ty in
        let assign = Cmd.Assignment (lhs, ptr) in
        let write =
          match value with
          | None -> []
          | Some e ->
              let v, pre = compile_expr_c e in
              let v =
                Val_repr.as_value ~error:Error.code_error
                  ~msg:"declaration initial value for in-memory scalar access" v
              in
              let write = Memory.store_scalar ~ctx (Expr.PVar lhs) v ty in
              pre @ [ b write ]
        in
        [ b action_cmd; b assign ] @ write |> void
      else
        let v, s =
          match value with
          | Some e ->
              let e, s = compile_expr_c e in
              let e =
                Val_repr.as_value ~error:Error.code_error
                  ~msg:"in memory scalar" e
              in
              (e, s)
          | None -> (Lit Undefined, [])
        in
        s @ [ b (Assignment (lhs, v)) ] |> void
  | SAssign { lhs; rhs } ->
      (* Special case: my patched Kani will comment "deinit" if this assignment
         correspond to a deinit that CBMC doesn't handle. *)
      let body =
        match stmt.comment with
        | Some "deinit" -> poison ~ctx ~annot:b lhs
        | _ ->
            let _, body = compile_assign ~ctx ~annot:b ~lhs ~rhs in
            body
      in
      body |> void
  | Expression e -> compile_expr_c e
  | SFunctionCall { lhs; func; args } -> (
      let v, pre1 = compile_call ~ctx ~add_annot:b func args in
      match lhs with
      | None -> (v, pre1)
      | Some lvalue ->
          let access, pre2 = lvalue_as_access ~ctx ~read:false lvalue in
          let write =
            match (access, v) with
            | ZST, _ -> []
            | Direct x, ByValue v -> [ b (Cmd.Assignment (x, v)) ]
            | InMemoryScalar { ptr; _ }, ByValue v ->
                [ b (Memory.store_scalar ~ctx ptr v lvalue.type_) ]
            | InMemoryComposit { ptr = dst; type_ }, ByCopy { ptr = src; _ } ->
                [ b (Memory.memcpy ~ctx ~type_ ~src ~dst) ]
            | _ ->
                Error.code_error
                  (Fmt.str
                     "Wrong mix of access and value kind for function call:\n\
                      %a = %a" pp_access access Val_repr.pp v)
          in
          pre1 @ pre2 @ write |> void)
  | Switch { control; cases; default } ->
      let end_lab = Ctx.fresh_lab ctx in
      let next_lab = ref None in
      let control_ty = control.type_ in
      let control, control_s = compile_expr_c control in
      let rec compile_cases ~ctx ?(acc = []) = function
        | [] -> acc
        | case :: rest ->
            let cur_lab = !next_lab in
            let nlab = Ctx.fresh_lab ctx in
            next_lab := Some nlab;
            let guard = case.Stmt.case in
            let guard_e, guard_s = compile_expr ~ctx guard in
            let equal_v, comparison_calls =
              compile_binop ~ctx ~lty:control_ty ~rty:guard.type_ Equal control
                guard_e
            in
            let comparison_calls =
              List.map
                (Body_item.make_hloc ~loc:guard.location)
                comparison_calls
            in
            let _, block = compile_statement ~ctx case.sw_body in
            let block_lab, block = Body_item.get_or_set_fresh_lab ~ctx block in
            let goto_block = Cmd.GuardedGoto (equal_v, block_lab, nlab) in
            let total_block =
              set_first_label_opt cur_lab
                (guard_s @ comparison_calls @ [ b goto_block ] @ block)
            in
            let acc = acc @ total_block in
            compile_cases ~ctx ~acc rest
      in
      let compiled_cases =
        Ctx.with_break ctx end_lab (fun ctx -> compile_cases ~ctx cases)
      in
      let default_block =
        match default with
        | None -> [ b ?label:!next_lab Skip ]
        | Some default ->
            Ctx.with_break ctx end_lab (fun ctx ->
                let _, block = compile_statement ~ctx default in
                set_first_label_opt !next_lab block)
      in
      let end_ = [ b ~label:end_lab Skip ] in
      control_s @ compiled_cases @ default_block @ end_ |> void
  | Ifthenelse { guard; then_; else_ } ->
      let comp_guard, cmd_guard = compile_expr_c guard in
      let comp_guard = Val_repr.as_value ~msg:"ifthenelse guard" comp_guard in
      let _, comp_then_ = compile_statement_c then_ in
      let comp_else = Option.map (fun x -> snd (compile_statement_c x)) else_ in
      let end_lab = Ctx.fresh_lab ctx in
      let then_lab, comp_then_ =
        Body_item.get_or_set_fresh_lab ~ctx comp_then_
      in
      let else_lab, comp_else =
        match comp_else with
        | None -> (end_lab, [])
        | Some else_ -> Body_item.get_or_set_fresh_lab ~ctx else_
      in
      let goto_guard = b (Cmd.GuardedGoto (comp_guard, then_lab, else_lab)) in
      let end_ = [ b ~label:end_lab Skip ] in
      cmd_guard @ [ goto_guard ] @ comp_then_ @ comp_else @ end_ |> void
  | Break ->
      (match ctx.break_lab with
      | None -> Error.unexpected "Break call outside of loop of switch"
      | Some break_lab -> [ b (Cmd.Goto break_lab) ])
      |> void
  | SUnhandled id -> [ b (assert_unhandled ~feature:(StmtIrep id) []) ] |> void
  | Output _ ->
      let () = Stats.Unhandled.signal OutputStmt in
      [ b Skip ] |> void

and compile_statement_list ~ctx stmts : Val_repr.t Cs.with_body =
  let rec aux acc last_v = function
    | [] -> (last_v, List.rev acc)
    | stmt :: stmts ->
        let last_v, cstmt = compile_statement ~ctx stmt in
        aux (List.rev_append cstmt acc) last_v stmts
  in
  aux [] (Val_repr.ByValue (Lit Nono)) stmts
