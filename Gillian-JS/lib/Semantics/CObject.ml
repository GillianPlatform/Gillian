open Gillian.Concrete
module Id = Gillian.Gil_syntax.Id

type t = (string, Values.t) Hashtbl.t

let pp fmt (loc, obj, metadata) =
  let pp_kv fmt (prop, prop_val) =
    Fmt.pf fmt "%s: %a" prop Values.pp prop_val
  in
  Fmt.pf fmt "@[<h>%a|-> [ %a ], %a@]" Id.pp loc
    (Fmt.hashtbl ~sep:Fmt.comma pp_kv)
    obj Values.pp metadata

let init () : t = Hashtbl.create Config.medium_tbl_size
let get (obj : t) (prop : string) = Hashtbl.find_opt obj prop

let set (obj : t) (prop : string) (value : Values.t) =
  Hashtbl.replace obj prop value

let remove (obj : t) (prop : string) = Hashtbl.remove obj prop

let properties (obj : t) : string list =
  SS.elements
    (Hashtbl.fold (fun prop _ props -> SS.add prop props) obj SS.empty)
