open Gillian.Concrete
module Loc = Gil_syntax.Loc

type t = (Loc.t, CObject.t * Values.t) Hashtbl.t

let pp fmt heap =
  let pp_obj fmt (loc, (obj, metadata)) = CObject.pp fmt (loc, obj, metadata) in
  (Fmt.hashtbl ~sep:(Fmt.any "@\n") pp_obj) fmt heap

let init () : t = Hashtbl.create Config.medium_tbl_size
let get (heap : t) (loc : Loc.t) = Hashtbl.find_opt heap loc

let set (heap : t) (loc : Loc.t) (obj_with_mv : CObject.t * Values.t) =
  Hashtbl.replace heap loc obj_with_mv

let remove (heap : t) (loc : Loc.t) = Hashtbl.remove heap loc
let copy (heap : t) = Hashtbl.copy heap
