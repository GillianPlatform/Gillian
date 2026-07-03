(** Representation of variables for the debugger *)

(* TODO: this should contain a variable list *)

(** Describes a variable scope (e.g. store, heap, pure formulae) *)
type scope = { name : string; id : int } [@@deriving yojson]

(** A variable *)
type t = {
  name : string; [@default ""]
  value : string;
  type_ : string option;
  var_ref : int; [@default 0]
}
[@@deriving make, yojson]

(** A map of scope IDs to variables *)
type ts = (int, t list) Hashtbl.t [@@deriving yojson]

let create_leaf (name : string) (value : string) ?(type_ = None) () : t =
  { name; value; type_; var_ref = 0 }

let create_node (name : string) (var_ref : int) ?(value = "") () : t =
  { name; value; type_ = Some "object"; var_ref }

let compare_name v w = String.compare v.name w.name
let compare_value v w = String.compare v.value w.value
