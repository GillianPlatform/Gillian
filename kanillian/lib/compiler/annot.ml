module Location = Gillian.Gil_syntax.Location

type tl_ref = Stmt of int | Expr of int [@@deriving yojson, eq]
type stmt_kind = Single | Multi of bool [@@deriving yojson, eq]

type t = {
  origin_loc : Location.t option;
  loop_info : string list;
  tl_ref : tl_ref option;
  stmt_kind : stmt_kind; [@default Multi false]
}
[@@deriving yojson, make, eq]

let make_basic = make ?tl_ref:None ?stmt_kind:None
let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }
let is_hidden (_ : t) = false
