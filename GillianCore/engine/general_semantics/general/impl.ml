let v option default = Option.value ~default option

module type Modules = sig
  module Val : Val.S
  module ESubst : ESubst.S with type vt = Val.t and type t = Val.et
  module Store : Store.S with type vt = Val.t

  module State :
    State.S
      with type vt = Val.t
       and type st = ESubst.t
       and type store_t = Store.t

  module PC : ParserAndCompiler.S
  module External : External.T(PC.Annot).S
end

module Make' (M : Modules) = struct
  module Val = M.Val
  module ESubst = M.ESubst
  module Store = M.Store
  module State = M.State
  module PC = M.PC
  module External' = M.External
  module Annot = PC.Annot
  module Call_stack = Call_stack.Make (Val) (Store)
  module External = M.External (Val) (ESubst) (Store) (State) (Call_stack)
  module Choice = Choice.Dummy

  type value = Val.t [@@deriving yojson, show]
  type subst = ESubst.t
  type store = Store.t
  type state = State.t [@@deriving yojson, show]
  type state_err = State.err_t [@@deriving show, yojson]
  type state_value = State.vt [@@deriving yojson, show]
  type heap = State.heap_t
  type invariant_frames = (string * state) list [@@deriving yojson]
  type err = (value, state_err) Exec_err.t [@@deriving yojson, show]
  type init_data = State.init_data
  type annot = Annot.t
  type cmd = int Cmd.t

  type step_cont = {
    state : state;
    last_known_loc : Location.t option;
    callstack : Call_stack.t;
    invariant_frames : invariant_frames;
    ix : int;
    prev_ix : int;
    prev_loop_ids : string list;
    branch_count : int;
    symb_exec_next : bool;
    laction_fuel : int;
  }

  type step_err = {
    state : state;
    last_known_loc : Location.t option;
    callstack : Call_stack.t;
    ix : int;
    errors : err list;
  }

  type step_finish = {
    state : state;
    last_known_loc : Location.t option;
    flag : Flag.t;
    ret_val : state_value;
  }

  type step =
    | Step_cont of step_cont
    | Step_err of step_err
    | Step_finish of step_finish
    | Step_susp of string * step_cont

  type exec_result = (state, state_value, err) Exec_res.t

  let call_graph = Call_graph.make ~init_capacity:128 ()
  let reset_call_graph () = Call_graph.reset call_graph

  type loop_action =
    | Nothing
    | FrameOff of string
    | FrameOn of string list
    | Malformed

  type step_ctx = {
    prog : annot MP.prog;
    conf : step_cont;
    loop_ids : string list;
    loop_action : loop_action;
    pid : string;
    cmd : cmd;
    annot : annot;
    last_known_loc : Location.t option ref;
  }

  let make_cont
      ?state
      ?callstack
      ?invariant_frames
      ?ix
      ?prev_ix
      ?loop_ids
      ?branch_count
      ?(did_branch = false)
      ?symb_exec_next
      ?laction_fuel
      ctx =
    let state = v state ctx.conf.state in
    let last_known_loc = !(ctx.last_known_loc) in
    let callstack = v callstack ctx.conf.callstack in
    let invariant_frames = v invariant_frames ctx.conf.invariant_frames in
    let ix = v ix (ctx.conf.ix + 1) in
    let prev_ix = v prev_ix ctx.conf.ix in
    let prev_loop_ids = v loop_ids ctx.conf.prev_loop_ids in
    let branch_count =
      let b = ctx.conf.branch_count in
      match (branch_count, did_branch) with
      | None, false -> b
      | None, true -> b + 1
      | Some b, false -> b
      | Some _, true ->
          failwith
            "make_cont: Can't set did_branch and branch_count simultaneously!"
    in
    let symb_exec_next = v symb_exec_next ctx.conf.symb_exec_next in
    let laction_fuel = v laction_fuel ctx.conf.laction_fuel in
    Step_cont
      {
        state;
        last_known_loc;
        callstack;
        invariant_frames;
        ix;
        prev_ix;
        prev_loop_ids;
        branch_count;
        symb_exec_next;
        laction_fuel;
      }

  let make_err ?state ~errors ?callstack ?ix ctx =
    let state = v state ctx.conf.state in
    let last_known_loc = !(ctx.last_known_loc) in
    let callstack = v callstack ctx.conf.callstack in
    let ix = v ix ctx.conf.ix in
    Step_err { state; last_known_loc; callstack; ix; errors }

  let make_finish ?state ~flag ~ret_val ctx =
    let state = v state ctx.conf.state in
    let last_known_loc = !(ctx.last_known_loc) in
    Step_finish { state; last_known_loc; flag; ret_val }

  let eval_expr' state expr =
    try Ok (State.eval_expr state expr)
    with State.Internal_State_Error (errs, s) -> Error (errs, s)

  let eval_expr ?state ctx expr =
    let state = v state ctx.conf.state in
    eval_expr' state expr
    |> Result.map_error @@ fun (errors, state) ->
       let errors = List.map (fun x -> Exec_err.EState x) errors in
       make_err ~state ~errors ctx

  let eval_exprs ?state ctx exprs =
    List_utils.map_results (eval_expr ?state ctx) exprs

  let update_store (state : State.t) (x : string) (v : Val.t) : State.t =
    let store = State.get_store state in
    let () = Store.put store x v in
    let state' = State.set_store state store in
    state'

  (* Gives a unit function that returns the given callstack on the first call,
     then a copy of it on subsequent calls. *)
  let callstack_copier callstack =
    let taken = ref false in
    fun () ->
      if !taken then Call_stack.copy callstack
      else
        let () = taken := true in
        callstack

  let ( let@* ) r f =
    match r with
    | Ok v -> f v
    | Error e -> e

  let return = Seq.return
  let vanish = Seq.empty
  let ( let&* ) s f = Seq.concat_map f s
  let ( let&+ ) s f = Seq.map f s

  let ( let&** ) r f =
    match r with
    | Ok v -> f v
    | Error e -> return e
end

module type S = sig
  module M : Modules
  include module type of Make' (M)
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) =
struct
  module M = struct
    module Val = Val
    module ESubst = ESubst
    module Store = Store
    module State = State
    module PC = PC
    module External = External
  end

  include Make' (M)
end
