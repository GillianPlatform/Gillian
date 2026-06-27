(** @canonical Gillian.General.Call_stack

    Implementation of GIL call stacks *)

(** @canonical Gillian.General.Call_stack.S *)
module type S = sig
  type vt
  type store_t

  (** A call stack is a list records, each of which contains 1) identifier of
      the current procedure (string) 2) arguments of the current procedure (list
      of values) 3) calling store 4) a list of loop invariant identifiers 5)
      variable that should hold the return value 6) index of the calling
      procedure from whence the procedure was called 7) normal continuation
      index in the calling procedure 8) optional error continuation index in the
      calling procedure *)

  type stack_element = {
    pid : string;
    arguments : vt list;
    store : store_t option;
    loop_ids : string list;
    ret_var : Var.t;
    call_index : int;
    continue_index : int;
    error_index : int option;
  }
  [@@deriving yojson]

  type t = stack_element list [@@deriving yojson]

  val empty : t

  val push :
    t ->
    pid:string ->
    arguments:vt list ->
    ?store:store_t ->
    loop_ids:string list ->
    ret_var:Var.t ->
    call_index:int ->
    continue_index:int ->
    ?error_index:int ->
    unit ->
    t

  (** Get current procedure identifier

      @param cs Target call stack
      @return Identifier of the procedure currently being executed *)
  val get_cur_proc_id : t -> string

  (** Get current arguments

      @param cs Target call stack
      @return Arguments of the procedure currently being executed *)
  val get_cur_args : t -> vt list

  (** Get current loop identifiers

      @param cs Target call stack
      @return List of current loop identifiers *)
  val get_loop_ids : t -> string list

  (** Call stack copy

      @param cs Target call stack
      @return Copy of the given call stack *)
  val copy : t -> t

  (** @param cs Target call stack
      @return All the procedure names in order in the callstack *)
  val get_cur_procs : t -> string list

  (** Call stack recursive depth

      @param cs Target call stack
      @param pid Identified of the target procedure
      @return The number of times the pid been recursively called *)
  val recursive_depth : t -> string -> int

  (** Call stack pretty printer

      @param fmt Formatter
      @param cs Call stack to pretty print
      @return unit *)
  val pp : Format.formatter -> t -> unit
end

module Make (Val : Val.S) (Store : Store.S with type vt = Val.t) :
  S with type vt = Val.t and type store_t = Store.t = struct
  type vt = Val.t [@@deriving yojson]
  type store_t = Store.t [@@deriving yojson]

  (** Type of call stacks: a call stack is a list of tuples, each of which
      contains 1) identifier of the current procedure (string) 2) arguments of
      the current procedure (list of values) 3) calling store 4) variable that
      should hold the return value 5) index of the calling procedure from whence
      the procedure was called 6) normal continuation index in the calling
      procedure 7) optional error continuation index in the calling procedure *)
  type stack_element = {
    pid : string;
    arguments : vt list;
    store : store_t option;
    loop_ids : string list;
    ret_var : Var.t;
    call_index : int;
    continue_index : int;
    error_index : int option;
  }
  [@@deriving yojson]

  type t = stack_element list [@@deriving yojson]

  let empty = []

  let push
      cs
      ~pid
      ~arguments
      ?store
      ~loop_ids
      ~ret_var
      ~call_index
      ~continue_index
      ?error_index
      () =
    {
      pid;
      arguments;
      store;
      loop_ids;
      ret_var;
      call_index;
      continue_index;
      error_index;
    }
    :: cs

  (** Get current procedure identifier

      @param cs Target call stack
      @return Identifier of the procedure currently being executed *)

  let get_cur_proc_id (cs : t) : string =
    match cs with
    | { pid; _ } :: _ -> pid
    | _ -> raise (Failure "Malformed configuration")

  (** Get current arguments

      @param cs Target call stack
      @return Arguments of the procedure currently being executed *)
  let get_cur_args (cs : t) : Val.t list =
    match cs with
    | { arguments; _ } :: _ -> arguments
    | _ -> raise (Failure "Malformed configuration")

  (** Get current arguments

      @param cs Target call stack
      @return List of current loop identifiers *)
  let get_loop_ids = function
    | { loop_ids; _ } :: _ -> loop_ids
    | _ -> raise (Failure "Malformed configuration")

  (** Call stack copy

      @param cs Target call stack
      @return Copy of the given call stack *)
  let copy (cs : t) : t =
    List.map (fun cs -> { cs with store = Option.map Store.copy cs.store }) cs

  (** Call stack recursive depth

      @param cs Target call stack
      @param pid Identified of the target procedure
      @return The number of times the pid been recursively called *)
  let recursive_depth (cs : t) (pid : string) : int =
    List.fold_left
      (fun ac { pid = pid'; _ } -> if String.equal pid' pid then ac + 1 else ac)
      (-1) cs

  let get_cur_procs (cs : t) : string list =
    List.rev (List.fold_left (fun ac { pid; _ } -> pid :: ac) [] cs)

  let pp fmt cs = Fmt.(brackets (list ~sep:comma string)) fmt (get_cur_procs cs)
end
