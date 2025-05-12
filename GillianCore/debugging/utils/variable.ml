(** Representation of variables for the debugger *)

(* TODO: this should contain a variable list *)

(** Describes a variable scope (e.g. store, heap, pure formulae) *)
type scope = { name : string; id : int }

(** A variable *)
type t = { name : string; value : string; type_ : string option; var_ref : int }

(** A map of scope IDs to variables *)
type ts = (int, t list) Hashtbl.t

let create_leaf (name : string) (value : string) ?(type_ = None) () : t =
  { name; value; type_; var_ref = 0 }

let create_node (name : string) (var_ref : int) ?(value = "") () : t =
  { name; value; type_ = Some "object"; var_ref }

let top_level_scopes : scope list =
  let top_level_scope_names =
    (* [ "Store"; "Heap"; "Pure Formulae"; "Typing Environment"; "Predicates" ] *)
    [ "Pure Formulae"; "Typing Environment"; "Predicates" ]
  in
  List.mapi (fun i name -> { name; id = i + 1 }) top_level_scope_names
