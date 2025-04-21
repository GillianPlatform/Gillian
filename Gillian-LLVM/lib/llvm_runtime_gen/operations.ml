open Gil_syntax
open Llvm_memory_model

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
    let* tval = true_case in
    let tlable = fresh_sym () in
    let flabel = fresh_sym () in
    let* _ = add_cmd (Gil_syntax.Cmd.GuardedGoto (expr, tlable, flabel)) in
    let* _ = new_block tlable in
    let* tval = true_case in
    let* _ = new_block flabel in
    return tval

  let ite expr ~true_case ~false_case =
    let* tval = true_case in
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
  fail_cmd "Type mismatch" exprs

let op_bv_scheme
    (literals : int list)
    (width : (Gil_syntax.Expr.t * int) list)
    (op : BVOps.t)
    (width_of_result : int option) : unit Codegenerator.t =
  let open Codegenerator in
  let open Gil_syntax.Expr in
  let open Gil_syntax.Expr.Infix in
  let expr_with_type =
    List.map
      (fun (expr, width) ->
        (expr, Llvm_memory_model.LLVMRuntimeTypes.Int width))
      width
  in
  let check =
    List.map (fun (expr, rty) -> is_type_of_expr expr rty) expr_with_type
    |> fun lst -> List.fold_right (fun x acc -> x && acc) lst Expr.true_
  in
  let* _ =
    ite check
      ~true_case:
        (let converted_args =
           List.map (fun (x, w) -> BvExpr (Expr.list_nth x 1, w)) width
         in
         let args =
           List.append
             (List.map (fun x -> Expr.Literal x) literals)
             converted_args
         in
         let* _ =
           add_return_of_value
             (Expr.BVExprIntrinsic (op, args, width_of_result))
         in
         return get_current_block_label)
      ~false_case:
        (let* _ = add_cmd (type_fail expr_with_type) in
         let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
         return get_current_block_label)
  in
  return ()

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

let pattern_function
    (expr1 : Expr.t)
    (expr2 : Expr.t)
    (ptr_width : int)
    (op : BVOps.t)
    (commutative : bool) =
  let open Codegenerator in
  let open TypePatterns in
  let open Gil_syntax.Expr.Infix in
  let case_statement_for_ptr (pval : Expr.t) (regular_val : Expr.t) =
    let pointer_offset = Expr.list_nth (Expr.list_nth pval 1) 1 in

    let int_val = Expr.list_nth regular_val 1 in
    let* _ =
      add_return_of_value
        (update_pointer pval
           (Expr.BVExprIntrinsic
              ( op,
                [
                  BvExpr (pointer_offset, ptr_width); BvExpr (int_val, ptr_width);
                ],
                Some ptr_width )))
    in
    return ()
  in
  let case_statement_for_int (regular_val0 : Expr.t) (regular_val1 : Expr.t) =
    let int_valx = Expr.list_nth regular_val0 1 in
    let int_valy = Expr.list_nth regular_val1 1 in
    let* _ =
      add_return_of_value
        (Expr.EList
           [
             Expr.list_nth regular_val0 0;
             Expr.list_nth regular_val1 0;
             Expr.BVExprIntrinsic
               ( op,
                 [ BvExpr (int_valx, ptr_width); BvExpr (int_valy, ptr_width) ],
                 Some ptr_width );
           ])
    in
    return ()
  in
  let default_statement = add_cmd (fail_cmd "No type pattern matched" []) in
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

let add_function =
  op_function "add" 2 (function
    | [ x; y ] -> pattern_function x y 32 BVOps.BVPlus true
    | _ -> failwith "Invalid number of arguments")

let () = Format.printf "%a" (Proc.pp_labeled ~pp_annot:Fmt.nop) add_function

(** Builds out a bitvector only function over a set of expr arguments *)
let example_compilation_for_less_than_10 =
  let open Codegenerator in
  let open Gil_syntax.Expr in
  let open Gil_syntax.Expr.Infix in
  let prog =
    let comp = int 1 < int 2 in
    let* _ =
      ite comp
        ~true_case:
          (let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
           return get_current_block_label)
        ~false_case:
          (let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
           return get_current_block_label)
    in
    return ()
  in
  let _, cmds = compile (empty_state ()) prog in
  List.iter
    (fun (lab, c) ->
      Format.printf "%a:%a\n" (Fmt.option Fmt.string) lab
        Gil_syntax.Cmd.pp_labeled c)
    cmds;
  ()
