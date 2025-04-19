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
  return get_current_block_label

let op_bv_scheme
    (literals : int list)
    (width : (Gil_syntax.Expr.t * int) list)
    (op : BVOps.t)
    (width_of_result : int option) : unit Codegenerator.t =
  let open Codegenerator in
  let open Gil_syntax.Expr in
  let open Gil_syntax.Expr.Infix in
  let check =
    List.map
      (fun (expr, width) ->
        is_type_of_expr expr (Llvm_memory_model.LLVMRuntimeTypes.Int width))
      width
    |> fun lst -> List.fold_right (fun x acc -> x && acc) lst Expr.true_
  in
  let* _ =
    ite check
      ~true_case:
        (let converted_args = List.map (fun (x, w) -> BvExpr (x, w)) width in
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
        (let* _ = add_cmd Gil_syntax.Cmd.ReturnNormal in
         return get_current_block_label)
  in
  return ()

let op_function
    (name : string)
    (arity : int)
    (f : Gil_syntax.Expr.t list -> unit Codegenerator.t) :
    ('annot, string) Gil_syntax.Proc.t =
  let open Codegenerator in
  let* _ = add_cmd (Gil_syntax.Cmd.Function (name, arity)) in
  let* _ =
    f (List.init arity (fun _ -> Expr.PVar (Gillian.Utils.Names.fresh_sym ())))
  in
  let* _ = add_cmd Gil_syntax.Cmd.EndFunction in
  return ()

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
