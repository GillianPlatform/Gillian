module type S = sig
  (** Type to be be displayed *)
  type t

  (** Type used for display a type in a debugger *)
  type debugger_tree = Leaf of string | Node of debugger_tree list

  (** Converts type into a debugger tree which can be displayed in the debugger *)
  val to_debugger_tree : t -> debugger_tree
end
