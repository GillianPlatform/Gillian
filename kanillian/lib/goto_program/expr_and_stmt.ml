module rec Expr : sig
  type value =
    | Array of t list
    | IntConstant of Z.t
    | CBoolConstant of bool
    | BoolConstant of bool
    | PointerConstant of int
    | DoubleConstant of float
    | FloatConstant of float
    | Symbol of string
    | EFunctionCall of { func : t; args : t list }
    | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
    | ByteExtract of { e : t; offset : int }
    | Dereference of t
    | EAssign of { lhs : t; rhs : t }
    | UnOp of { op : Ops.Unary.t; e : t }
    | Struct of t list
    | Member of { lhs : t; field : string }
    | AddressOf of t
    | Index of { array : t; index : t }
    | StringConstant of string
    | TypeCast of t
    | If of { cond : t; then_ : t; else_ : t }
    | StatementExpression of Stmt.t list
    | Nondet
    | EUnhandled of Id.t * string

  and t = { value : value; type_ : Type.t; location : Location.t }
  [@@deriving show]

  val pp_full : Format.formatter -> t -> unit
  val as_symbol : t -> string
  val value_of_irep : machine:Machine_model.t -> type_:Type.t -> Irep.t -> value
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end = struct
  type value =
    | Array of t list
    | IntConstant of Z.t [@printer Z.pp_print]
    | CBoolConstant of bool
    | BoolConstant of bool
    | PointerConstant of int
    | DoubleConstant of float
    | FloatConstant of
        float (* FIXME: OCaml doesn't have 32-bit floats, how to handle that? *)
    | Symbol of string
    | EFunctionCall of { func : t; args : t list }
    | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
    | ByteExtract of { e : t; offset : int }
    | Dereference of t
    | EAssign of { lhs : t; rhs : t }
    | UnOp of { op : Ops.Unary.t; e : t }
    | Struct of t list
    | Member of { lhs : t; field : string }
    | AddressOf of t
    | Index of { array : t; index : t }
    | StringConstant of string
    | TypeCast of t
    | If of { cond : t; then_ : t; else_ : t }
    | StatementExpression of Stmt.t list
    | Nondet
    | EUnhandled of Id.t * string

  and t = { value : value; type_ : Type.t; location : Location.t }
  [@@deriving show { with_path = false }]

  let pp_full = pp

  let pp ft t =
    let rec pp ft t =
      let open Fmt in
      match t.value with
      | Array x -> pf ft "%a" (list ~sep:comma pp) x
      | EAssign { lhs; rhs } -> pf ft "%a = %a" pp lhs pp rhs
      | IntConstant z -> pf ft "%a" Z.pp_print z
      | CBoolConstant b -> pf ft "%d" (if b then 1 else 0)
      | DoubleConstant f -> pf ft "%f" f
      | FloatConstant f -> pf ft "%fF" f
      | PointerConstant 0 -> pf ft "NULL"
      | PointerConstant k -> pf ft "POINTER(%d)" k
      | Symbol s -> pf ft "%s" s
      | EFunctionCall { func; args } ->
          pf ft "%a(%a)" pp func (list ~sep:comma pp) args
      | BinOp { op; lhs; rhs } ->
          pf ft "(%a %a %a)" pp lhs Ops.Binary.pp op pp rhs
      | UnOp { op; e } -> pf ft "(%a %a)" Ops.Unary.pp op pp e
      | ByteExtract { e; offset } ->
          pf ft "EXTRACT(%a, %a, %d)" pp e Type.pp t.type_ offset
      | Struct xs -> pf ft "{ %a }" (list ~sep:semi pp) xs
      | Member { lhs; field } -> pf ft "%a.%s" pp lhs field
      | Index { array; index } -> pf ft "%a[%a]" pp array pp index
      | StringConstant s -> pf ft "\"%s\"" s
      | TypeCast value -> pf ft "((%a) %a)" Type.pp t.type_ pp value
      | Nondet -> pf ft "NONDET"
      | BoolConstant b -> pf ft "%b" b
      | AddressOf e -> pf ft "&%a" pp e
      | Dereference e -> pf ft "*%a" pp e
      | If { cond; then_; else_ } ->
          pf ft "%a ? %a : %a" pp cond pp then_ pp else_
      | StatementExpression _ -> pf ft "STMTEXPR"
      | EUnhandled (id, msg) -> (
          match msg with
          | "" -> pf ft "UNHANDLED_EXPR(%s)" (Id.to_string id)
          | _ -> pf ft "UNHANDLED_EXPR(%s::%s)" (Id.to_string id) msg)
    in

    (Fmt.hbox pp) ft t

  let show = Fmt.to_to_string pp

  let unhandled ~irep:_ id msg =
    (* TODO: hide the next line behind a config flag *)
    (* Fmt.pr "UNHANDLED_IREP: %a\n@?"
       (Yojson.Safe.pretty_print ~std:true)
       (Irep.to_yojson irep); *)
    EUnhandled (id, msg)

  let as_symbol e =
    match e.value with
    | Symbol s -> s
    | _ -> Gerror.unexpected "Expected a symbol, got something else!"

  open Irep.Infix
  open Lift_utils

  (** Lifting from Irep *)
  let rec byte_extract_of_irep ~(machine : Machine_model.t) irep =
    let e, offset = exactly_two irep in
    let offset =
      offset $ Value
      |> Irep.as_just_bitpattern ~width:machine.pointer_width ~signed:true
      |> Z.to_int
    in
    ByteExtract { e = of_irep ~machine e; offset }

  and side_effecting_of_irep ~(machine : Machine_model.t) (irep : Irep.t) =
    let of_irep = of_irep ~machine in
    match (irep $ Statement).id with
    | FunctionCall ->
        let func, args =
          match irep.sub with
          | [ fsym; args ] -> (of_irep fsym, List.map of_irep args.sub)
          | _ -> Gerror.unexpected ~irep "function call with not exactly 2 subs"
        in
        EFunctionCall { func; args }
    | Nondet -> Nondet
    | Assign ->
        let lhs, rhs = exactly_two irep in
        EAssign { lhs = of_irep lhs; rhs = of_irep rhs }
    | StatementExpression ->
        let stmts = List.map (Stmt.of_irep ~machine) irep.sub in
        StatementExpression stmts
    | id -> unhandled ~irep id "SideEffect"

  and lift_binop
      ~(machine : Machine_model.t)
      (irep : Irep.t)
      (op : Ops.Binary.t) =
    let of_irep = of_irep ~machine in
    match irep.sub with
    | [ a; b ] -> BinOp { op; lhs = of_irep a; rhs = of_irep b }
    | _ ->
        Gerror.unexpected ~irep
          "Binary operator doesn't have exactly two operands"

  and lift_unop ~(machine : Machine_model.t) (irep : Irep.t) (op : Ops.Unary.t)
      =
    let of_irep = of_irep ~machine in
    match irep.sub with
    | [ a ] -> UnOp { op; e = of_irep a }
    | _ ->
        Gerror.unexpected ~irep
          "Unary operator doesn't have exactly one operand"

  and value_of_irep
      ~(machine : Machine_model.t)
      ~(type_ : Type.t)
      (irep : Irep.t) =
    let unexpected = Gerror.unexpected ~irep in
    let of_irep = of_irep ~machine in
    let lift_binop = lift_binop ~machine irep in
    let lift_unop = lift_unop ~machine irep in
    match irep.id with
    | Array -> Array (List.map of_irep irep.sub)
    | Constant -> (
        match type_ with
        | CInteger I_bool -> (
            let v =
              irep $ Value
              |> Irep.as_just_bitpattern ~width:machine.bool_width ~signed:false
            in
            match Z.to_int v with
            | 1 -> CBoolConstant true
            | 0 -> CBoolConstant false
            | _ -> unexpected "Invalid bool constant")
        | CInteger int_ty ->
            (* Importantly, int_ty cannot be bool *)
            let enc = IntType.Bv_encoding.encode ~machine int_ty in
            let v =
              irep $ Value
              |> Irep.as_just_bitpattern ~width:enc.width ~signed:enc.signed
            in
            IntConstant v
        | Bool -> (
            match (irep $ Value).id with
            | True -> BoolConstant true
            | False -> BoolConstant false
            | _ -> unexpected "invalid boolean value")
        | Unsignedbv { width } ->
            let v =
              irep $ Value |> Irep.as_just_bitpattern ~width ~signed:false
            in
            IntConstant v
        | Signedbv { width } ->
            let v =
              irep $ Value |> Irep.as_just_bitpattern ~width ~signed:true
            in
            IntConstant v
        | Pointer _ -> (
            match (irep $ Value).id with
            | NULL -> PointerConstant 0
            | _ -> unhandled ~irep Constant "Non0PointerConstant")
        | Double ->
            let v =
              irep $ Value |> Irep.as_just_bitpattern ~width:64 ~signed:false
            in
            (* We have to use this dirty hack because Z.to_int64 can overflow.
               That is because int64 are signed and we need them unsigned.
               However, Int.of_string "0u....". will have the right behaviour *)
            let i64 = "0u" ^ Z.to_string v |> Int64.of_string in
            DoubleConstant (Int64.float_of_bits i64)
        | Float ->
            let v =
              irep $ Value |> Irep.as_just_bitpattern ~width:32 ~signed:false
            in
            let i32 = "0u" ^ Z.to_string v |> Int32.of_string in
            FloatConstant (Int32.float_of_bits i32)
        | ty -> unhandled ~irep Constant ("WithType::" ^ Type.show ty))
    | StringConstant -> StringConstant (irep $ Value |> Irep.as_just_string)
    | ByteExtractBigEndian when machine.is_big_endian ->
        byte_extract_of_irep ~machine irep
    | ByteExtractLittleEndian when not machine.is_big_endian ->
        byte_extract_of_irep ~machine irep
    | Symbol ->
        let name = irep $ Identifier |> Irep.as_just_string in
        Symbol name
    | Dereference -> Dereference (of_irep (exactly_one irep))
    | SideEffect -> side_effecting_of_irep ~machine irep
    | AddressOf ->
        let pointee = exactly_one ~msg:"AddressOf" irep in
        AddressOf (of_irep pointee)
    | Struct ->
        let fields = List.map of_irep irep.sub in
        Struct fields
    | Member ->
        let lhs = exactly_one ~msg:"Member" irep |> of_irep in
        let field = irep $ ComponentName |> Irep.as_just_string in
        Member { lhs; field }
    | Index ->
        let array, index = exactly_two ~msg:"Array Indexing" irep in
        Index { array = of_irep array; index = of_irep index }
    | Typecast ->
        let value = exactly_one ~msg:"Type cast" irep |> of_irep in
        TypeCast value
    | If ->
        let cond, then_, else_ = exactly_three ~msg:"If Expr" irep in
        If { cond = of_irep cond; then_ = of_irep then_; else_ = of_irep else_ }
    | Nondet -> Nondet
    (* A bunch of binary operators now*)
    | And -> lift_binop And
    | Ashr -> lift_binop Ashr
    | Bitand -> lift_binop Bitand
    | Bitor -> lift_binop Bitor
    | Bitnand -> lift_binop Bitnand
    | Bitxor -> lift_binop Bitxor
    | Div -> lift_binop Div
    | Equal -> lift_binop Equal
    | Ge -> lift_binop Ge
    | Gt -> lift_binop Gt
    | IeeeFloatEqual -> lift_binop IeeeFloatEqual
    | IeeeFloatNotequal -> lift_binop IeeeFloatNotequal
    | Implies -> lift_binop Implies
    | Le -> lift_binop Le
    | Lshr -> lift_binop Lshr
    | Lt -> lift_binop Lt
    | Minus -> lift_binop Minus
    | Mod -> lift_binop Mod
    | Mult -> lift_binop Mult
    | Notequal -> lift_binop Notequal
    | Or -> lift_binop Or
    | OverflowMinus -> lift_binop OverflowMinus
    | OverflowMult -> lift_binop OverflowMult
    | OverflowPlus -> lift_binop OverflowPlus
    | OverflowResultMinus -> lift_binop OverflowResultMinus
    | OverflowResultMult -> lift_binop OverflowResultMult
    | OverflowResultPlus -> lift_binop OverflowResultPlus
    | Plus -> lift_binop Plus
    | ROk -> lift_binop ROk
    | Rol -> lift_binop Rol
    | Ror -> lift_binop Ror
    | Shl -> lift_binop Shl
    | Xor -> lift_binop Xor
    (* And a bunch of unary operators *)
    | Bitnot -> lift_unop Bitnot
    | BitReverse -> lift_unop BitReverse
    | Bswap -> lift_unop Bswap
    | IsDynamicObject -> lift_unop IsDynamicObject
    | IsFinite -> lift_unop IsFinite
    | Not -> lift_unop Not
    | ObjectSize -> lift_unop ObjectSize
    | PointerObject -> lift_unop PointerObject
    | PointerOffset -> lift_unop PointerOffset
    | Popcount -> lift_unop Popcount
    | UnaryMinus -> lift_unop UnaryMinus
    (* Catch-all *)
    | id -> unhandled ~irep id ""

  and of_irep ~machine irep =
    let location = Location.sloc_in_irep irep in
    let type_ = Type.type_in_irep ~machine irep in
    let value = value_of_irep ~machine ~type_ irep in
    { value; type_; location }
end

and Stmt : sig
  type body =
    | Decl of { lhs : Expr.t; value : Expr.t option }
    | SAssign of { lhs : Expr.t; rhs : Expr.t }
    | Assume of { cond : Expr.t }
    | Assert of { cond : Expr.t; property_class : string option }
    | Block of t list
    | Label of string * t list
    | Goto of string
    | SFunctionCall of {
        lhs : Expr.t option;
        func : Expr.t;
        args : Expr.t list;
      }
    | Switch of {
        control : Expr.t;
        cases : switch_case list;
        default : t option;
      }
    | Ifthenelse of { guard : Expr.t; then_ : t; else_ : t option }
    | Break
    | Skip
    | Expression of Expr.t
    | Output of { msg : Expr.t; value : Expr.t }
    | Return of Expr.t option
    | SUnhandled of Id.t

  and switch_case = { case : Expr.t; sw_body : t }
  and t = { stmt_location : Location.t; body : body; comment : string option }

  val pp : Format.formatter -> t -> unit
  val body_of_irep : machine:Machine_model.t -> Irep.t -> body
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end = struct
  type body =
    | Decl of { lhs : Expr.t; value : Expr.t option }
    | SAssign of { lhs : Expr.t; rhs : Expr.t }
    | Assume of { cond : Expr.t }
    | Assert of { cond : Expr.t; property_class : string option }
    | Block of t list
    | Label of string * t list
    | Goto of string
    | SFunctionCall of {
        lhs : Expr.t option;
        func : Expr.t;
        args : Expr.t list;
      }
    | Switch of {
        control : Expr.t;
        cases : switch_case list;
        default : t option;
      }
    | Ifthenelse of { guard : Expr.t; then_ : t; else_ : t option }
    | Break
    | Skip
    | Expression of Expr.t
    | Output of { msg : Expr.t; value : Expr.t }
    | Return of Expr.t option
    | SUnhandled of Id.t

  and switch_case = { case : Expr.t; sw_body : t }
  and t = { stmt_location : Location.t; body : body; comment : string option }

  let unhandled ~irep:_ id =
    (* TODO: hide the following line under a config flag. *)
    (* Fmt.pr "%a\n@?" Yojson.Safe.pretty_print (Irep.to_yojson irep); *)
    SUnhandled id

  let rec pp ft (t : t) =
    let open Fmt in
    match t.body with
    | Decl { lhs; value } ->
        pf ft "@[<h>%a %a%a;@]" Type.pp lhs.type_ Expr.pp lhs
          (fun ft -> function
            | None -> ()
            | Some e -> pf ft " = %a" Expr.pp e)
          value
    | SAssign { lhs; rhs } -> pf ft "@[<h>%a = %a;@]" Expr.pp lhs Expr.pp rhs
    | SFunctionCall { lhs; func; args } ->
        let pp_lhs ft lhs =
          match lhs with
          | None -> nop ft ()
          | Some lhs -> pf ft "%a = " Expr.pp lhs
        in
        pf ft "@[<h>%a%a(%a);@]" pp_lhs lhs Expr.pp func
          (list ~sep:comma Expr.pp) args
    | Assume { cond } -> pf ft "@[<h>assume(%a);@]" Expr.pp cond
    | Assert { cond; property_class } ->
        let pp_pc ft = function
          | None -> pf ft ""
          | Some s -> pf ft " #%s" s
        in
        pf ft "@[<h>assert(%a);%a@]" Expr.pp cond pp_pc property_class
    | Block body -> pf ft "@[<v 3>{ %a };@]" (Fmt.list ~sep:cut pp) body
    | Label (label, body) ->
        pf ft "@[<v 3>%s: {@.%a};@]" label (Fmt.list ~sep:cut pp) body
    | Skip -> pf ft "skip;"
    | Expression e -> pf ft "@[<v 3>{ %a };@]" Expr.pp e
    | Return e -> pf ft "@[<v 3>return %a;@]" (option Expr.pp) e
    | Goto label -> pf ft "@[<v 3>goto %s;@]" label
    | Output { msg; value } ->
        pf ft "@[<v 3>output (%a, %a);@]" Expr.pp msg Expr.pp value
    | Switch _ -> pf ft "switch"
    | Break -> pf ft "break"
    | Ifthenelse { guard; then_; else_ } ->
        pf ft "@[<v 3>if %a then{@\n %a } else {@\n%a;@]}" Expr.pp guard pp
          then_ (option pp) else_
    | SUnhandled id -> pf ft "UNHANDLED_STMT(%s)" (Id.to_string id)

  (** Lifting from Irep *)
  open Irep.Infix

  open Lift_utils

  let rec body_of_irep ~(machine : Machine_model.t) (irep : Irep.t) : body =
    let of_irep = of_irep ~machine in
    let expr_of_irep = Expr.of_irep ~machine in
    let unexpected = Gerror.unexpected ~irep in
    match (irep $ Statement).id with
    | Skip
    (* Ignoring AtomicBegin and AtomicEnd entirely *)
    | AtomicBegin
    | AtomicEnd -> Skip
    | Goto ->
        let label = irep $ Destination |> Irep.as_just_string in
        Goto label
    | Block ->
        let content = List.map of_irep irep.sub in
        Block content
    | Label ->
        let lab = irep $ Label |> Irep.as_just_string in
        let block = List.map of_irep irep.sub in
        Label (lab, block)
    | Decl ->
        let lhs, value =
          match irep.sub with
          | [ a; b ] -> (expr_of_irep a, Lift_utils.lift_option expr_of_irep b)
          | [ a ] -> (expr_of_irep a, None)
          | _ -> unexpected "Invalid declaration statement!"
        in
        Decl { lhs; value }
    | Assign ->
        let lhs, rhs = exactly_two ~msg:"Assign stmt" irep in
        SAssign { lhs = expr_of_irep lhs; rhs = expr_of_irep rhs }
    | Assume ->
        let to_assume = exactly_one ~msg:"Assume stmt" irep in
        Assume { cond = expr_of_irep to_assume }
    | Assert ->
        (* I might need to extract the property_class/msg here too *)
        let to_assert = exactly_one ~msg:"Assert stmt" irep in
        let property_class =
          let open Kutils.Syntaxes.Option in
          let* sloc = irep $? CSourceLocation in
          let+ pc = sloc $? PropertyClass in
          Irep.as_just_string pc
        in
        Assert { cond = expr_of_irep to_assert; property_class }
    | Return ->
        let ret_value_irep =
          match irep.sub with
          | [] -> Irep.nil
          | [ r ] -> r
          | _ -> unexpected "more than one return value"
        in
        let ret_val = Lift_utils.lift_option expr_of_irep ret_value_irep in
        Return ret_val
    | Expression ->
        let irep_expr = exactly_one ~msg:"Expression stmt" irep in
        Expression (expr_of_irep irep_expr)
    | Switch ->
        let control, content = exactly_two ~msg:"Switch" irep in
        let control = expr_of_irep control in
        let () =
          match (content $ Statement).id with
          | Block -> ()
          | _ -> unexpected "Switch body is not a block"
        in
        let cases, k, default = switch_cases_of_irep ~machine content.sub in
        if not (k == []) then unexpected "Switch body doesn't start with a case";
        Switch { control; cases; default }
    | FunctionCall ->
        let lhs, func, args = exactly_three ~msg:"FunctionCall stmt" irep in
        let lhs = Lift_utils.lift_option expr_of_irep lhs in
        let func = expr_of_irep func in
        let args = List.map expr_of_irep args.sub in
        SFunctionCall { lhs; func; args }
    | Output ->
        let msg, value = exactly_two ~msg:"Output stmt" irep in
        let msg = expr_of_irep msg in
        let value = expr_of_irep value in
        Output { msg; value }
    | Break -> Break
    | Ifthenelse ->
        let guard, then_, else_ =
          match irep.sub with
          | [ a; b; c ] ->
              (expr_of_irep a, of_irep b, Lift_utils.lift_option of_irep c)
          | [ a; b ] -> (expr_of_irep a, of_irep b, None)
          | _ -> unexpected "Invalid if-then-else statement"
        in
        Ifthenelse { guard; then_; else_ }
    | id -> unhandled ~irep id

  and switch_cases_of_irep ~machine l =
    let is_switch_case irep =
      match (irep $ Statement).id with
      | SwitchCase -> true
      | _ -> false
    in
    let is_default irep =
      match irep $? Default with
      | Some { id = Id1; _ } -> true
      | _ -> false
    in
    match l with
    | [] -> ([], [], None)
    | irep :: r when is_default irep -> (
        if not (is_switch_case irep) then
          Gerror.unexpected ~irep "Default case is not a SwitchCase";
        let _, stmt = exactly_two ~msg:"default switch_case" irep in
        match switch_cases_of_irep ~machine r with
        | rest, rest_of_case, None ->
            let block =
              let this_body = of_irep ~machine stmt in
              {
                body = Block (this_body :: rest_of_case);
                stmt_location = this_body.stmt_location;
                comment = None;
              }
            in
            (rest, [], Some block)
        | _, _, Some _ -> Gerror.unexpected "two default switch_cases!")
    | irep :: r when is_switch_case irep ->
        let cases, rest_of_case, default = switch_cases_of_irep ~machine r in
        let case, body = exactly_two irep in
        let case = Expr.of_irep ~machine case in
        let this_body = of_irep ~machine body in
        let sw_body =
          {
            body = Block (this_body :: rest_of_case);
            stmt_location = this_body.stmt_location;
            comment = None;
          }
        in
        ({ case; sw_body } :: cases, [], default)
    | irep :: r ->
        let cases, rest_of_case, default = switch_cases_of_irep ~machine r in
        let content = of_irep ~machine irep in
        (cases, content :: rest_of_case, default)

  and of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
    let stmt_location = Location.sloc_in_irep irep in
    let body = body_of_irep ~machine irep in
    let comment = irep $? Comment |> Option.map Irep.as_just_string in
    { body; stmt_location; comment }
end
