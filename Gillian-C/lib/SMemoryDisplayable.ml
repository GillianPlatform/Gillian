module Make (SMemory : Gillian.Symbolic.Memory_S) = struct
  type t = SMemory.t

  type debugger_tree = Leaf of string | Node of debugger_tree list

  let to_debugger_tree _ =
    failwith
      "Please implement to_debugger_tree to enable displaying of the symbolic \
       memory in a debugger"
end
