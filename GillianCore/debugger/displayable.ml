(** Type used for display a type in a debugger *)
type debugger_tree =
  | Leaf of (string * string)
  | Node of (string * debugger_tree list)

module type S = sig
  (** Type to be be displayed *)
  type t

  (** Converts type into a debugger tree which can be displayed in the debugger *)
  val to_debugger_tree : t -> debugger_tree list
end

let list_to_debugger_tree
    (name : string) (to_string : 'a -> string) (lst : 'a list) : debugger_tree =
  let leaves =
    List.mapi
      (fun index element -> Leaf (string_of_int index, to_string element))
      lst
  in
  Node (name, leaves)
