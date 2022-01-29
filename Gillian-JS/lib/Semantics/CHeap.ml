open Gillian.Concrete

type t = (string, CObject.t * Values.t) Hashtbl.t

let pp fmt heap =
  let pp_obj fmt (loc, (obj, metadata)) = CObject.pp fmt (loc, obj, metadata) in
  (Fmt.hashtbl ~sep:(Fmt.any "@\n") pp_obj) fmt heap

let init () : t = Hashtbl.create Config.medium_tbl_size
let get (heap : t) (loc : string) = Hashtbl.find_opt heap loc

let set (heap : t) (loc : string) (obj_with_mv : CObject.t * Values.t) =
  Hashtbl.replace heap loc obj_with_mv

let remove (heap : t) (loc : string) = Hashtbl.remove heap loc
let copy (heap : t) = Hashtbl.copy heap
