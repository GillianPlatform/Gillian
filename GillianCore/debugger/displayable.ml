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
