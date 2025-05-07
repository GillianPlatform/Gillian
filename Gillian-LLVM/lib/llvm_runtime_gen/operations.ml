open Gil_syntax
open Llvm_memory_model
open Monomorphizer.Template

module Types = struct
  type label = string
  type cmd = label Gil_syntax.Cmd.t
  type labeled_cmd = label option * cmd
  type block = labeled_cmd list
  type state = { curr_block : block; blocks : block list }
  type 'a t = state -> 'a * state
end

module type S = sig
  include module type of Types

  val add_cmd : cmd -> unit t
  val set_state : state -> unit t
  val get_state : state t
  val if_ : Gil_syntax.Expr.t -> true_case:'a t -> 'a t

  val ite :
    Gil_syntax.Expr.t -> true_case:'a t -> false_case:'b t -> ('a * 'b) t

  val switch :
    cases:(unit t * Gil_syntax.Expr.t) list -> default:unit t -> unit t

  val compile : state -> 'a t -> 'a * labeled_cmd list
  val empty_state : unit -> state
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fresh_sym : unit -> string
  val empty_block : unit -> block
  val new_block : label -> unit t
  val get_current_block_label : label t
end

module Codegenerator : S with type label = string = struct
  type label = string
  type cmd = label Gil_syntax.Cmd.t
  type labeled_cmd = label option * cmd
  type block = labeled_cmd list
  type state = { curr_block : block; blocks : block list }
  type 'a t = state -> 'a * state

  let set_state s = fun state -> ((), s)
  let get_state = fun state -> (state, state)
  let ctr = ref 0

  let fresh_sym () =
    let sym = Printf.sprintf "ctr_%d" !ctr in
    ctr := !ctr + 1;
    sym

  let empty_block () = [ (Some (fresh_sym ()), Gil_syntax.Cmd.Skip) ]
  let return x state = (x, state)

  let bind t f state =
    let tval, tstate = t state in
    let fval, fstate = f tval tstate in
    (fval, fstate)

  let ( let* ) = bind
  let empty_state () = { curr_block = empty_block (); blocks = [] }
  let lst_last lst = List.hd (List.rev lst)

  let get_current_block_label =
    let* st = get_state in
    let label = st.curr_block |> lst_last |> fst |> Option.get in
    return label

  let add_cmd c =
    let* cstate = get_state in
    let curr_block = (None, c) :: cstate.curr_block in
    let blocks = cstate.blocks in
    set_state { curr_block; blocks }

  let new_block label =
    let* cstate = get_state in
    let curr_block = (Some label, Gil_syntax.Cmd.Skip) :: cstate.curr_block in
    let blocks = cstate.blocks in
    set_state { curr_block; blocks }

  let if_ expr ~true_case =
    let tlable = fresh_sym () in
    let flabel = fresh_sym () in
    let* _ = add_cmd (Gil_syntax.Cmd.GuardedGoto (expr, tlable, flabel)) in
    let* _ = new_block tlable in
    let* tval = true_case in
    let* _ = new_block flabel in
    return tval

  let ite expr ~true_case ~false_case =
    let tlable = fresh_sym () in
    let flabel = fresh_sym () in
    let* _ = add_cmd (Gil_syntax.Cmd.GuardedGoto (expr, tlable, flabel)) in
    let* _ = new_block tlable in
    let* tval = true_case in
    let* _ = new_block flabel in
    let* fval = false_case in
    return (tval, fval)

  let rec switch
      ~(cases : (unit t * Gil_syntax.Expr.t) list)
      ~(default : unit t) : unit t =
    match cases with
    | [] -> default
    | (case, expr) :: cases ->
        let* _ =
          ite expr ~true_case:case ~false_case:(switch ~cases ~default)
        in
        return ()

  let compile state t =
    let tval, tstate = t state in
    ( tval,
      tstate.curr_block :: tstate.blocks |> List.map List.rev |> List.flatten )
end

let is_type_of_expr
    (expr : Gil_syntax.Expr.t)
    (type_ : Llvm_memory_model.LLVMRuntimeTypes.t) =
  let open Gil_syntax.Expr.Infix in
  Gil_syntax.Expr.type_eq expr Gil_syntax.Type.ListType
  && Gil_syntax.Expr.list_length expr == Gil_syntax.Expr.int 2
  && Gil_syntax.Expr.list_nth expr 0
     == Gil_syntax.Expr.string
          (Llvm_memory_model.LLVMRuntimeTypes.type_to_string type_)

let rec foldM (f : 'b -> 'a -> 'b Codegenerator.t) (acc : 'b) (lst : 'a list) :
    'b Codegenerator.t =
  let open Codegenerator in
  match lst with
  | [] -> return acc
  | x :: xs ->
      let* acc = f acc x in
      foldM f acc xs

let add_return_of_value (value : Gil_syntax.Expr.t) =
  let open Codegenerator in
  let* _ =
    add_cmd
      (Gil_syntax.Cmd.Assignment (Gillian.Utils.Names.return_variable, value))
  in
  let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
  get_current_block_label

let fail_cmd (err_type : string) (args : Gil_syntax.Expr.t list) =
  Gil_syntax.Cmd.Fail (err_type, args)

let type_fail (expr_and_type : (Gil_syntax.Expr.t * LLVMRuntimeTypes.t) list) =
  let exprs =
    List.map
      (fun (expr, rty) ->
        Expr.list [ expr; LLVMRuntimeTypes.rtype_to_gil_type rty |> Expr.type_ ])
      expr_and_type
  in
  fail_cmd "Type_mismatch" exprs

type bv_op_function = Expr.t list -> bv_op_shape -> Expr.t

type generalized_bv_op_function =
  Expr.t list -> bv_op_shape -> Expr.t Codegenerator.t

let add_overflow_check
    (check_ops : bv_op_function list option)
    (inputs : Expr.t list)
    (shape : bv_op_shape) =
  let open Codegenerator in
  let open Gil_syntax.Expr in
  let open Gil_syntax.Expr.Infix in
  match check_ops with
  | Some ops ->
      let check_expr =
        List.fold_right (fun op acc -> op inputs shape || acc) ops Expr.false_
      in
      let* _ =
        ite check_expr
          ~true_case:
            (let* _ = add_cmd (fail_cmd "Detected_forbidden_overflow" inputs) in
             let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
             return ())
          ~false_case:(return ())
      in
      return ()
  | None -> return ()

let generalized_op_bv_scheme
    (inputs : Expr.t list)
    (op : generalized_bv_op_function)
    (check_ops : bv_op_function list option)
    (shape : bv_op_shape) : unit Codegenerator.t =
  let open Codegenerator in
  let open Gil_syntax.Expr in
  let open Gil_syntax.Expr.Infix in
  let expr_with_type =
    List.map2
      (fun expr width -> (expr, Llvm_memory_model.LLVMRuntimeTypes.Int width))
      inputs shape.args
  in
  let check =
    List.map (fun (expr, rty) -> is_type_of_expr expr rty) expr_with_type
    |> fun lst -> List.fold_right (fun x acc -> x && acc) lst Expr.true_
  in
  let* _ =
    ite check
      ~true_case:
        (let extracted_inputs = List.map (fun i -> Expr.list_nth i 1) inputs in
         let* _ = add_overflow_check check_ops extracted_inputs shape in
         let* res = op extracted_inputs shape in
         let* _ =
           add_return_of_value
             (Expr.EList
                [
                  Expr.string
                    (LLVMRuntimeTypes.type_to_string
                       (LLVMRuntimeTypes.Int (Option.get shape.width_of_result)));
                  res;
                ])
         in
         return get_current_block_label)
      ~false_case:
        (let* _ = add_cmd (type_fail expr_with_type) in
         let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
         return get_current_block_label)
  in
  return ()

let op_bv_scheme
    (inputs : Expr.t list)
    (op : bv_op_function)
    (check_ops : bv_op_function list option)
    (shape : bv_op_shape) : unit Codegenerator.t =
  let open Codegenerator in
  let generalized_op = fun inputs shape -> return (op inputs shape) in
  generalized_op_bv_scheme inputs generalized_op check_ops shape

let op_function
    (name : string)
    (arity : int)
    (f : Gil_syntax.Expr.t list -> unit Codegenerator.t) :
    (Annot.Basic.t, string) Gil_syntax.Proc.t =
  let open Codegenerator in
  let open Gil_syntax.Proc in
  let params = List.init arity (fun _ -> Codegenerator.fresh_sym ()) in
  let body = f (List.map (fun p -> Expr.PVar p) params) in
  let _, cmds = compile (empty_state ()) body in
  let annotated =
    List.map (fun c -> (Annot.Basic.make (), fst c, snd c)) cmds
  in
  let p =
    {
      proc_name = name;
      proc_source_path = None;
      proc_internal = false;
      proc_body = Array.of_list annotated;
      proc_params = params;
      proc_spec = None;
      proc_aliases = [];
      proc_calls = [];
    }
  in
  p

module TypePatterns = struct
  type t = {
    exprs : Gil_syntax.Expr.t list;
    types_ : LLVMRuntimeTypes.t list;
    case_stat : unit Codegenerator.t;
  }

  let pattern_check
      (exprs : Gil_syntax.Expr.t list)
      (types_ : LLVMRuntimeTypes.t list) : Expr.t =
    let open Codegenerator in
    let open Gil_syntax.Expr in
    let open Gil_syntax.Expr.Infix in
    let check_expr =
      List.map2 (fun expr type_ -> is_type_of_expr expr type_) exprs types_
      |> fun lst -> List.fold_right (fun x acc -> x && acc) lst Expr.true_
    in
    check_expr

  let rec type_dispatch
      (patterns : t list)
      (default_stat : unit Codegenerator.t) : unit Codegenerator.t =
    let open Codegenerator in
    match patterns with
    | [] -> default_stat
    | pattern :: patterns ->
        let check = pattern_check pattern.exprs pattern.types_ in
        let* _ = if_ check ~true_case:pattern.case_stat in
        let* _ = type_dispatch patterns default_stat in
        return ()
end

let update_pointer (ptr_exp : Expr.t) (offset : Expr.t) =
  let _ = Expr.list_nth (Expr.list_nth ptr_exp 1) 1 in
  let pointer_object = Expr.list_nth (Expr.list_nth ptr_exp 1) 0 in
  let pointer_ty =
    LLVMRuntimeTypes.type_to_string LLVMRuntimeTypes.Ptr |> Expr.string
  in
  Expr.EList [ pointer_ty; Expr.EList [ pointer_object; offset ] ]

let pattern_function_unary
    (expr : Expr.t)
    (shape : bv_op_shape)
    (op : bv_op_function) =
  let open Codegenerator in
  let open TypePatterns in
  let ptr_width =
    match shape.width_of_result with
    | Some width -> width
    | None -> failwith "Pointer operations should have a result"
  in
  let case_statement_for_ptr (pval : Expr.t) =
    let pointer_offset = Expr.list_nth (Expr.list_nth pval 1) 1 in
    let* _ =
      add_return_of_value (update_pointer pval (op [ pointer_offset ] shape))
    in
    return ()
  in
  let case_statement_for_int (regular_val : Expr.t) =
    let int_val = Expr.list_nth regular_val 1 in
    let* _ =
      add_return_of_value
        (Expr.EList [ Expr.list_nth regular_val 0; op [ int_val ] shape ])
    in
    return ()
  in
  let default_statement = add_cmd (fail_cmd "No_type_pattern_matched" []) in
  let patterns =
    [
      {
        exprs = [ expr ];
        types_ = [ LLVMRuntimeTypes.Ptr ];
        case_stat = case_statement_for_ptr expr;
      };
      {
        exprs = [ expr ];
        types_ = [ LLVMRuntimeTypes.Int ptr_width ];
        case_stat = case_statement_for_int expr;
      };
    ]
  in
  let* _ = type_dispatch patterns default_statement in
  return ()

let cmp_patterns
    ~(pointer_width : int)
    (expr1 : Expr.t)
    (expr2 : Expr.t)
    (op : bv_op_function)
    (shape : bv_op_shape) =
  let open Codegenerator in
  let open TypePatterns in
  let open Gil_syntax.Expr.Infix in
  let can_use_pointer = List.for_all (fun x -> x = pointer_width) shape.args in
  let add_return_of_bool_value (bool_val : Expr.t) =
    let type_of_bool =
      LLVMRuntimeTypes.type_to_string (LLVMRuntimeTypes.Int 1) |> Expr.string
    in
    let* _ =
      ite bool_val
        ~true_case:
          (add_return_of_value
             (Expr.EList [ type_of_bool; Expr.int_to_bv ~width:1 1 ]))
        ~false_case:
          (add_return_of_value
             (Expr.EList [ type_of_bool; Expr.int_to_bv ~width:1 0 ]))
    in
    return ()
  in
  let case_statement_for_ptr (pval1 : Expr.t) (pval2 : Expr.t) =
    let pointer_offset1 = Expr.list_nth (Expr.list_nth pval1 1) 1 in
    let pointer_offset2 = Expr.list_nth (Expr.list_nth pval2 1) 1 in
    let abs_obj1 = Expr.list_nth pval1 0 in
    let abs_obj2 = Expr.list_nth pval2 0 in
    let* _ =
      ite (abs_obj1 == abs_obj2)
        ~true_case:
          (let* _ =
             add_return_of_bool_value
               (op [ pointer_offset1; pointer_offset2 ] shape)
           in
           return ())
        ~false_case:
          (let* _ =
             add_cmd (fail_cmd "Incomparable_pointers" [ expr1; expr2 ])
           in
           let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
           return ())
    in
    return ()
  in
  let case_statement_for_int (regular_val1 : Expr.t) (regular_val2 : Expr.t) =
    let int_val1 = Expr.list_nth regular_val1 1 in
    let int_val2 = Expr.list_nth regular_val2 1 in
    let* _ = add_return_of_bool_value (op [ int_val1; int_val2 ] shape) in
    return ()
  in
  let ptr_patterns =
    if can_use_pointer then
      [
        {
          exprs = [ expr1; expr2 ];
          types_ = [ LLVMRuntimeTypes.Ptr; LLVMRuntimeTypes.Ptr ];
          case_stat = case_statement_for_ptr expr1 expr2;
        };
      ]
    else []
  in
  let int_patterns =
    [
      {
        exprs = [ expr1; expr2 ];
        types_ =
          [
            LLVMRuntimeTypes.Int (List.hd shape.args);
            LLVMRuntimeTypes.Int (List.nth shape.args 1);
          ];
        case_stat = case_statement_for_int expr1 expr2;
      };
    ]
  in
  let patterns = int_patterns @ ptr_patterns in
  let default_statement = add_cmd (fail_cmd "No_type_pattern_matched" []) in
  let* _ = type_dispatch patterns default_statement in
  return ()

let pattern_function
    (expr1 : Expr.t)
    (expr2 : Expr.t)
    (shape : bv_op_shape)
    (op : bv_op_function)
    (commutative : bool)
    (flag_checks : bv_op_function list option) =
  let open Codegenerator in
  let open TypePatterns in
  let open Gil_syntax.Expr.Infix in
  let ptr_width =
    match shape.width_of_result with
    | Some width -> width
    | None -> failwith "Pointer operations should have a result"
  in
  let case_statement_for_ptr (pval : Expr.t) (regular_val : Expr.t) =
    let pointer_offset = Expr.list_nth (Expr.list_nth pval 1) 1 in

    let int_val = Expr.list_nth regular_val 1 in
    let* _ =
      add_return_of_value
        (update_pointer pval (op [ pointer_offset; int_val ] shape))
    in
    return ()
  in
  let case_statement_for_int (regular_val0 : Expr.t) (regular_val1 : Expr.t) =
    let int_valx = Expr.list_nth regular_val0 1 in
    let int_valy = Expr.list_nth regular_val1 1 in
    let* _ = add_overflow_check flag_checks [ int_valx; int_valy ] shape in
    let* _ =
      add_return_of_value
        (Expr.EList
           [ Expr.list_nth regular_val0 0; op [ int_valx; int_valy ] shape ])
    in
    return ()
  in
  let default_statement = add_cmd (fail_cmd "No_type_pattern_matched" []) in
  let non_commutative_patterns =
    [
      {
        exprs = [ expr1; expr2 ];
        types_ = [ LLVMRuntimeTypes.Ptr; LLVMRuntimeTypes.Int ptr_width ];
        case_stat = case_statement_for_ptr expr1 expr2;
      };
      {
        exprs = [ expr1; expr2 ];
        types_ =
          [ LLVMRuntimeTypes.Int ptr_width; LLVMRuntimeTypes.Int ptr_width ];
        case_stat = case_statement_for_int expr1 expr2;
      };
    ]
  in
  let patterns =
    if commutative then
      {
        exprs = [ expr1; expr2 ];
        types_ = [ LLVMRuntimeTypes.Int ptr_width; LLVMRuntimeTypes.Ptr ];
        case_stat = case_statement_for_ptr expr2 expr1;
      }
      :: non_commutative_patterns
    else non_commutative_patterns
  in

  let* _ = type_dispatch patterns default_statement in
  return ()

module OpFunctions = struct
  open Gil_syntax

  let zip_args_with_shape (inputs : Expr.t list) (shape : bv_op_shape) :
      Expr.bv_arg list =
    List.map2 (fun expr width -> Expr.BvExpr (expr, width)) inputs shape.args

  let bv_op_function_custom_res
      ?(literals : int list option)
      (op : BVOps.t)
      inputs
      shape
      res =
    let lits = Option.to_list literals |> List.flatten in
    let args = zip_args_with_shape inputs shape in
    Expr.BVExprIntrinsic
      (op, List.map (fun x -> Expr.Literal x) lits @ args, res)

  let bv_op_function ?(literals : int list option) (op : BVOps.t) inputs shape =
    bv_op_function_custom_res ?literals op inputs shape shape.width_of_result

  let bv_check_function
      ?(literals : int list option)
      (op : BVOps.t)
      inputs
      shape =
    bv_op_function_custom_res ?literals op inputs shape None

  let add_op_function = bv_op_function BVOps.BVPlus
  let add_op_nuw = bv_check_function BVOps.BVUAddO
  let add_op_nsw = bv_check_function BVOps.BVSAddO
  let neg_function = bv_op_function BVOps.BVNeg
  let mul_op_function = bv_op_function BVOps.BVMul
  let sdiv_op_function = bv_op_function BVOps.BVSdiv
  let srem_op_function = bv_op_function BVSrem
  let mul_op_nuw = bv_check_function BVOps.BVUMulO
  let mul_op_nsw = bv_check_function BVOps.BVSMulO
  let and_op_function = bv_op_function BVOps.BVAnd
  let or_op_function = bv_op_function BVOps.BVOr
  let xor_op_function = bv_op_function BVOps.BVXor

  let negated_function
      (f : Expr.t list -> bv_op_shape -> Expr.t)
      (inputs : Expr.t list)
      (shape : bv_op_shape) =
    let orig_res = f inputs shape in
    let negated_res = Expr.UnOp (UnOp.Not, orig_res) in
    negated_res

  let icmp_eq (inputs : Expr.t list) (shape : bv_op_shape) =
    let open Gil_syntax in
    Expr.BinOp (List.hd inputs, BinOp.Equal, List.hd (List.tl inputs))

  let icmp_ne = negated_function icmp_eq
  let icmp_ugt = negated_function (bv_op_function BVOps.BVUleq)
  let icmp_uge = negated_function (bv_op_function BVOps.BVUlt)
  let icmp_ult = bv_op_function BVOps.BVUlt
  let icmp_ule = bv_op_function BVOps.BVUleq
  let icmp_sgt = negated_function (bv_op_function BVOps.BVSleq)
  let icmp_sge = negated_function (bv_op_function BVOps.BVSlt)
  let icmp_slt = bv_op_function BVOps.BVSlt
  let icmp_sle = bv_op_function BVOps.BVSleq

  let unop_function
      ?(compute_lits : (input:int -> output:int -> int list) option)
      (op : BVOps.t)
      inputs
      shape =
    match shape.width_of_result with
    | Some width ->
        let input = List.hd shape.args in
        let lits = Option.map (fun f -> f ~input ~output:width) compute_lits in
        bv_op_function ?literals:lits op inputs shape
    | None -> failwith "Unop function requires a result width"

  let zext_function =
    unop_function
      ~compute_lits:(fun ~input ~output ->
        if input > output then
          failwith "Zext requires a larger or equal output width then input"
        else [ output - input ])
      BVOps.BVZeroExtend

  let sext_function =
    unop_function
      ~compute_lits:(fun ~input ~output ->
        if input > output then
          failwith "Sext requires a larger or equal output width then input"
        else [ output - input ])
      BVOps.BVSignExtend

  let sub_function_overflow
      (op : BVOps.t)
      (inputs : Expr.t list)
      (shape : bv_op_shape) =
    let first_shape = { shape with args = [ List.hd shape.args ] } in
    match inputs with
    | [ x; y ] ->
        bv_check_function op [ neg_function [ y ] first_shape; x ] shape
    | _ -> failwith "Invalid number of arguments"

  let sub_function inputs shape =
    let first_shape = { shape with args = [ List.hd shape.args ] } in
    match inputs with
    | [ x; y ] ->
        bv_op_function BVOps.BVPlus [ neg_function [ y ] first_shape; x ] shape
    | _ -> failwith "Invalid number of arguments"
end

let template_from_pattern_unary
    ~(op : bv_op_function)
    ~(pointer_width : int)
    ~(flag_checks : bv_op_function list option)
    (name : string)
    (shape : bv_op_shape) =
  match List.nth_opt shape.args 0 with
  | Some width when width = pointer_width ->
      op_function name 1 (function
        | [ x ] -> pattern_function_unary x shape op
        | _ -> failwith "Invalid number of arguments")
  | _ -> op_function name 1 (fun xs -> op_bv_scheme xs op flag_checks shape)

let template_from_pattern
    ~(op : bv_op_function)
    ~(commutative : bool)
    ~(pointer_width : int)
    ~(flag_checks : bv_op_function list option)
    (name : string)
    (shape : bv_op_shape) =
  match List.nth_opt shape.args 0 with
  | Some width when width = pointer_width ->
      op_function name 2 (function
        | [ x; y ] -> pattern_function x y shape op commutative flag_checks
        | _ -> failwith "Invalid number of arguments")
  | _ -> op_function name 2 (fun xs -> op_bv_scheme xs op flag_checks shape)

let template_from_pattern_cmp
    ~(op : bv_op_function)
    ~(pointer_width : int)
    ~(flag_checks : bv_op_function list option)
    (name : string)
    (shape : bv_op_shape) =
  op_function name 2 (function
    | [ x; y ] -> cmp_patterns ~pointer_width x y op shape
    | _ -> failwith "Invalid number of arguments")

let template_from_integer_op
    ~(op : bv_op_function)
    ~(pointer_width : int)
    ~(flag_checks : bv_op_function list option)
    (name : string)
    (shape : bv_op_shape) =
  op_function name 2 (fun xs -> op_bv_scheme xs op flag_checks shape)

module MemoryLib = struct
  let alloc_name = "alloc"
  let store_name = "store"
  let load_name = "load"

  module M = Memories.LLVM_ALoc.MonadicSMemory

  type ptr = { base : Expr.t; offset : Expr.t }

  let access_ptr (expr : Expr.t) : ptr =
    let vl = Expr.list_nth expr 1 in
    let base = Expr.list_nth vl 0 in
    let offset = Expr.list_nth vl 1 in
    { base; offset }

  let pointer_op ~(is_ptr_case : string -> unit Codegenerator.t) (ptr : Expr.t)
      : unit Codegenerator.t =
    let open Codegenerator in
    let open Gil_syntax in
    let open Expr.Infix in
    let* _ =
      ite
        (is_type_of_expr ptr LLVMRuntimeTypes.Ptr)
        ~true_case:
          (let to_bind = fresh_sym () in
           let* _ = is_ptr_case to_bind in
           let* _ = add_return_of_value (Expr.PVar to_bind) in
           return ())
        ~false_case:
          (let* _ =
             add_cmd (fail_cmd "Pointer_operation_on_non_pointer" [ ptr ])
           in
           return ())
    in
    return ()

  let load_op ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    match exp_list with
    | [ ptr ] ->
        let { base; offset } = access_ptr ptr in
        pointer_op
          ~is_ptr_case:(fun bindr ->
            let tmp = fresh_sym () in
            (* bind a tmp because after the load we have {{ {{value type, value}}}}*)
            let* _ = add_cmd (Cmd.LAction (tmp, load_name, [ base; offset ])) in
            let* _ =
              add_cmd (Cmd.Assignment (bindr, Expr.list_nth (Expr.PVar tmp) 0))
            in
            return ())
          ptr
    | _ -> failwith "Invalid number of arguments"

  let store_op ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    match exp_list with
    | [ ptr; value ] ->
        let { base; offset } = access_ptr ptr in
        pointer_op
          ~is_ptr_case:(fun bindr ->
            (* in store we just return out the {{}}*)
            let* _ =
              add_cmd (Cmd.LAction (bindr, store_name, [ base; offset; value ]))
            in
            return ())
          ptr
    | _ -> failwith "Invalid number of arguments"

  let alloc_op ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    let open Expr.Infix in
    let ty_check =
      List.fold_left
        (fun acc check -> acc && check)
        Expr.true_
        (List.map
           (fun expr ->
             is_type_of_expr expr (LLVMRuntimeTypes.Int pointer_width))
           exp_list)
    in
    match exp_list with
    | [ low; hi ] ->
        let low = Expr.list_nth low 1 in
        let hi = Expr.list_nth hi 1 in

        let* _ =
          ite ty_check
            ~true_case:
              (let bindr = fresh_sym () in
               let* _ =
                 add_cmd (Cmd.LAction (bindr, alloc_name, [ low; hi ]))
               in
               let* _ =
                 add_return_of_value (Expr.list_nth (Expr.PVar bindr) 0)
               in
               return ())
            ~false_case:
              (let* _ = add_cmd (fail_cmd "Alloc_failed" exp_list) in
               return ())
        in
        return ()
    | _ -> failwith "Invalid number of arguments"

  let displace_pointer_op ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    let open Expr.Infix in
    match exp_list with
    | [ ptr; offset ] ->
        let type_checks =
          [
            is_type_of_expr ptr LLVMRuntimeTypes.Ptr;
            is_type_of_expr offset (LLVMRuntimeTypes.Int pointer_width);
          ]
        in
        let ty_check =
          List.fold_left (fun acc check -> acc && check) Expr.true_ type_checks
        in
        let* _ =
          ite ty_check
            ~true_case:
              (let { base; offset = ptr_offset } = access_ptr ptr in
               let new_offset =
                 OpFunctions.add_op_function
                   [ ptr_offset; Expr.list_nth offset 1 ]
                   {
                     width_of_result = Some pointer_width;
                     args = [ pointer_width; pointer_width ];
                   }
               in
               let new_ptr = update_pointer ptr new_offset in
               let* _ = add_return_of_value new_ptr in
               return ())
            ~false_case:
              (let* _ =
                 add_cmd
                   (fail_cmd "Pointer_operation_on_non_pointer" [ ptr; offset ])
               in
               return ())
        in
        return ()
    | _ -> failwith "Invalid number of arguments"

  let construct_simple_op
      ~(arity : int)
      ~(f : pointer_width:int -> Expr.t list -> unit Codegenerator.t)
      ~(pointer_width : int)
      (name : string) : Monomorphizer.basic_proc =
    op_function name arity (f ~pointer_width)

  let ops =
    [
      {
        name = "llvm_load";
        generator = SimpleOp (construct_simple_op ~arity:1 ~f:load_op);
      };
      {
        name = "llvm_store";
        generator = SimpleOp (construct_simple_op ~arity:2 ~f:store_op);
      };
      {
        name = "llvm_displace_pointer";
        generator =
          SimpleOp (construct_simple_op ~arity:2 ~f:displace_pointer_op);
      };
      {
        name = "llvm_alloca";
        generator = SimpleOp (construct_simple_op ~arity:2 ~f:alloc_op);
      };
    ]
end

(*
TODO(Ian): there's probably a nice way to make a product functor that
produces modules of the OpTemplates type by appending their 
dependencies and template_operations and renaming the deps to keep things separate etc etc.
*)
module Libc = struct
  let libc_prefix = "libc_"
  let libc_mul_name = libc_prefix ^ "bvmul_sizet"
  let libc_alloca_name = libc_prefix ^ "_llvm_alloca"

  let libc_dependencies ~(pointer_width : int) =
    [
      {
        name = "bvmul";
        output_name = libc_mul_name;
        spec =
          ValueSpec
            {
              flags = [ NoSignedWrap; NoUnsignedWrap ];
              shape =
                {
                  args = [ pointer_width; pointer_width ];
                  width_of_result = Some pointer_width;
                };
            };
      };
      {
        name = "llvm_alloca";
        output_name = libc_alloca_name;
        spec = SimpleSpec;
      };
      { name = "printf"; output_name = "printf"; spec = SimpleSpec };
      { name = "calloc"; output_name = "calloc"; spec = SimpleSpec };
      { name = "exit"; output_name = "exit"; spec = SimpleSpec };
    ]

  let constant_return_func
      ~(const : Expr.t)
      ~(pointer_width : int)
      (exp_list : Expr.t list) : unit Codegenerator.t =
    let open Codegenerator in
    let* _ = add_return_of_value const in
    return ()

  let const_success_func ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    constant_return_func
      ~const:
        (LLVMRuntimeTypes.make_expr_of_type_unsafe (Expr.int 0)
           (LLVMRuntimeTypes.Int pointer_width))
      ~pointer_width exp_list

  let type_checked
      (exp_list : Expr.t list)
      (type_ : LLVMRuntimeTypes.t list)
      (success : unit Codegenerator.t)
      (failure : unit Codegenerator.t) : unit Codegenerator.t =
    let open Codegenerator in
    let open Expr.Infix in
    let ty_check =
      List.fold_left
        (fun acc check -> acc && check)
        Expr.true_
        (List.map2 (fun ty expr -> is_type_of_expr expr ty) type_ exp_list)
    in
    let* _ = ite ty_check ~true_case:success ~false_case:failure in
    return ()

  let type_check_with_failure
      (exp_list : Expr.t list)
      (type_ : LLVMRuntimeTypes.t list)
      (success : unit Codegenerator.t) : unit Codegenerator.t =
    let open Codegenerator in
    let open Expr.Infix in
    type_checked exp_list type_ success
      (let* _ = add_cmd (fail_cmd "Type_check_failed" exp_list) in
       return ())

  let calloc ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    match exp_list with
    | [ count; size ] ->
        let types =
          [
            LLVMRuntimeTypes.Int pointer_width;
            LLVMRuntimeTypes.Int pointer_width;
          ]
        in
        type_check_with_failure exp_list types
          (* TODO(Ian): we can probably have some like combinator that gens a fresh sym for a bind and then forwards it to the next expr to make these things cleaner *)
          (let to_bind = fresh_sym () in
           let* _ =
             add_cmd
               (Cmd.Call
                  ( to_bind,
                    Expr.string libc_mul_name,
                    [ count; size ],
                    None,
                    None ))
           in
           let alloc_result = fresh_sym () in
           let size_var = Expr.PVar to_bind in
           let zero_const =
             Expr.list [ Expr.list_nth size_var 0; Expr.int 0 ]
           in
           let* _ =
             add_cmd
               (Cmd.LAction
                  (alloc_result, libc_alloca_name, [ zero_const; size_var ]))
           in
           let* _ = add_return_of_value (Expr.PVar alloc_result) in
           return ())
    | _ -> failwith "Invalid number of arguments"

  (* TODO(Ian): bit of a hack *)
  let exit ~(pointer_width : int) (exp_list : Expr.t list) :
      unit Codegenerator.t =
    let open Codegenerator in
    match exp_list with
    | [ code ] ->
        let* _ = add_cmd (Cmd.Logic (LCmd.Assume Expr.false_)) in
        let* _ = add_cmd Cmd.ReturnNormal in
        return ()
    | _ -> failwith "Invalid number of arguments"

  let libc_ops =
    [
      {
        name = "calloc";
        generator = SimpleOp (MemoryLib.construct_simple_op ~arity:2 ~f:calloc);
      };
      (* NOTE(Ian): Variadics add an argument that aggregates additional args into a list*)
      {
        name = "printf";
        generator =
          SimpleOp
            (MemoryLib.construct_simple_op ~arity:2 ~f:const_success_func);
      };
      {
        name = "fprintf";
        generator =
          SimpleOp
            (MemoryLib.construct_simple_op ~arity:3 ~f:const_success_func);
      };
      {
        name = "exit";
        generator = SimpleOp (MemoryLib.construct_simple_op ~arity:1 ~f:exit);
      };
    ]
end

module UtilityOps = struct
  let abs_op_function (exprs : Expr.t list) (shape : bv_op_shape) :
      Expr.t Codegenerator.t =
    let open Codegenerator in
    match exprs with
    | [ x ] ->
        (* x is extracted so we setup something to bind with ite *)
        let width = shape.width_of_result |> Option.get in
        let bindr = fresh_sym () in
        let join_block = fresh_sym () in
        let bexpr =
          Expr.BVExprIntrinsic
            ( BVOps.BVSlt,
              [ BvExpr (x, width); BvExpr (Expr.zero_bv width, width) ],
              None )
        in
        let neg_expr =
          Expr.BVExprIntrinsic (BVOps.BVNeg, [ BvExpr (x, width) ], Some width)
        in
        let* _ =
          ite bexpr
            ~true_case:
              (*neg *)
              (let* _ = add_cmd (Cmd.Assignment (bindr, neg_expr)) in
               let* _ = add_cmd (Cmd.Goto join_block) in
               return ())
            ~false_case:
              (let* _ = add_cmd (Cmd.Assignment (bindr, x)) in
               let* _ = add_cmd (Cmd.Goto join_block) in
               return ())
        in
        let* _ = new_block join_block in
        return (Expr.PVar bindr)
    | _ -> failwith "Invalid number of arguments"

  let generic_template_function
      ~(op : generalized_bv_op_function)
      ~(pointer_width : int)
      ~(flag_checks : bv_op_function list option)
      (name : string)
      (shape : bv_op_shape) =
    op_function name 1 (fun exp_list ->
        generalized_op_bv_scheme exp_list op flag_checks shape)
end

module LLVMTemplates : Monomorphizer.OpTemplates = struct
  open Monomorphizer
  open Monomorphizer.Template

  let flag_template_function
      (f :
        pointer_width:int ->
        flag_checks:bv_op_function list option ->
        string ->
        bv_op_shape ->
        basic_proc)
      (ops : (flags * bv_op_function) list) :
      flags:flags list ->
      pointer_width:int ->
      string ->
      bv_op_shape ->
      basic_proc =
    let new_f ~flags ~pointer_width name shape =
      let module S = Set.Make (Template.Flags) in
      let target_set = S.of_list flags in
      let checks =
        List.filter (fun (flags, _) -> S.mem flags target_set) ops
        |> List.map (fun (_, op) -> op)
      in
      let checks_or_opt = if List.is_empty checks then None else Some checks in
      f ~flag_checks:checks_or_opt ~pointer_width name shape
    in
    new_f

  let dep_funcs = [ Libc.libc_dependencies ]

  let dependencies ~pointer_width =
    List.map (fun dep -> dep ~pointer_width) dep_funcs |> List.flatten

  let template_operations =
    [
      {
        name = "bvabs";
        generator =
          ValueOp
            (flag_template_function
               (UtilityOps.generic_template_function
                  ~op:UtilityOps.abs_op_function)
               []);
      };
      {
        name = "bvmul";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.mul_op_function)
               [
                 (NoSignedWrap, OpFunctions.mul_op_nsw);
                 (NoUnsignedWrap, OpFunctions.mul_op_nuw);
               ]);
      };
      {
        name = "bvand";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.and_op_function)
               []);
      };
      {
        name = "bvor";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.or_op_function)
               []);
      };
      {
        name = "bvxor";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.xor_op_function)
               []);
      };
      {
        name = "bvsdiv";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.mul_op_function)
               [
                 (NoSignedWrap, OpFunctions.mul_op_nsw);
                 (NoUnsignedWrap, OpFunctions.mul_op_nuw);
               ]);
      };
      {
        name = "bvsrem";
        generator =
          ValueOp
            (flag_template_function
               (template_from_integer_op ~op:OpFunctions.srem_op_function)
               []);
      };
      {
        name = "bvadd";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern ~op:OpFunctions.add_op_function
                  ~commutative:true)
               [
                 (NoSignedWrap, OpFunctions.add_op_nsw);
                 (NoUnsignedWrap, OpFunctions.add_op_nuw);
               ]);
      };
      {
        name = "bvsub";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern ~op:OpFunctions.sub_function
                  ~commutative:false)
               [
                 (NoSignedWrap, OpFunctions.sub_function_overflow BVOps.BVSAddO);
                 ( NoUnsignedWrap,
                   OpFunctions.sub_function_overflow BVOps.BVUAddO );
               ]);
      };
      {
        name = "bvzext";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_unary ~op:OpFunctions.zext_function)
               []);
      };
      {
        name = "bvsext";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_unary ~op:OpFunctions.sext_function)
               []);
      };
      {
        name = "icmp_eq";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_eq)
               []);
      };
      {
        name = "icmp_ne";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_ne)
               []);
      };
      {
        name = "icmp_ugt";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_ugt)
               []);
      };
      {
        name = "icmp_uge";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_uge)
               []);
      };
      {
        name = "icmp_ult";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_ult)
               []);
      };
      {
        name = "icmp_ule";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_ule)
               []);
      };
      {
        name = "icmp_sgt";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_sgt)
               []);
      };
      {
        name = "icmp_sge";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_sge)
               []);
      };
      {
        name = "icmp_slt";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_slt)
               []);
      };
      {
        name = "icmp_sle";
        generator =
          ValueOp
            (flag_template_function
               (template_from_pattern_cmp ~op:OpFunctions.icmp_sle)
               []);
      };
    ]
    @ MemoryLib.ops @ Libc.libc_ops
end
