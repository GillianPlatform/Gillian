module Location = Gillian.Gil_syntax.Location

type tl_ref = Stmt of int | Expr of int [@@deriving yojson, eq]

type cmd_kind =
  | Harness  (** The harness code preceding the main function *)
  | Internal  (** Commands in "internal" functions, e.g. m____nondet_int() *)
  | Hidden  (** Hidden commands, e.g. the =skip= at the end of an if/else *)
  | Normal of bool  (** Is this the final GIL cmd for the C stmt/expr? *)
  | Func_call of { is_internal : bool; is_end : bool }
  | Return
  | Unknown
[@@deriving yojson, eq, show]

type t = {
  origin_loc : Location.t option;
  loop_info : string list;
  tl_ref : tl_ref option;
  branch_kind : Branch_case.kind option;
  cmd_kind : cmd_kind; [@default Normal false]
}
[@@deriving yojson, make, eq]

let make_basic = make ?tl_ref:None ?branch_kind:None ?cmd_kind:None
let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }
let is_hidden (_ : t) = false

let set_end ?(is_end = true) t =
  let cmd_kind =
    match t.cmd_kind with
    | Normal _ -> Normal is_end
    | Func_call f -> Func_call { f with is_end }
    | _ -> t.cmd_kind
  in
  { t with cmd_kind }
