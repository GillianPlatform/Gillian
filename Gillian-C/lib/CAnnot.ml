type t = {
  origin_loc : Gillian.Gil_syntax.Location.t option;
  loop_info : string list;
}
[@@deriving yojson, make]

let from_loc ?origin_loc () = make ?origin_loc ()
let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let is_hidden (_ : t) = false
