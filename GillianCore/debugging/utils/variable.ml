type t = { name : string; value : string; type_ : string option; var_ref : int }
type ts = (int, t list) Hashtbl.t

let create_leaf (name : string) (value : string) ?(type_ = None) () : t =
  { name; value; type_; var_ref = 0 }

let create_node (name : string) (var_ref : int) ?(value = "") () : t =
  { name; value; type_ = Some "object"; var_ref }
