(** Tarjan's strongly connected components algorithm (cycle detection *)

open Prelude

open struct
  type 'a state = {
    get_edges : 'a -> 'a list; [@main]
    mutable index : int; [@default 0]
    index_map : ('a, int) Hashtbl.t; [@default Hashtbl.create 0]
    lowlink : ('a, int) Hashtbl.t; [@default Hashtbl.create 0]
    on_stack : ('a, bool) Hashtbl.t; [@default Hashtbl.create 0]
    mutable stack : 'a list; [@default []]
    mutable sccs : 'a list list; [@default []]
  }
  [@@deriving make]

  (* Strong connect (core of Tarjan's algorithm) *)
  let rec strong_connect state v =
    (* Set the depth index for v to the smallest unused index *)
    Hashtbl.replace state.index_map v state.index;
    Hashtbl.replace state.lowlink v state.index;
    state.index <- state.index + 1;
    state.stack <- v :: state.stack;
    Hashtbl.replace state.on_stack v true;

    let successors = state.get_edges v in
    List.iter
      (fun w ->
        if not (Hashtbl.mem state.index_map w) then (
          (* Successor w has not yet been visited; recurse on it *)
          strong_connect state w;
          let v_low = Hashtbl.find state.lowlink v in
          let w_low = Hashtbl.find state.lowlink w in
          Hashtbl.replace state.lowlink v (min v_low w_low))
        else if Hashtbl.find state.on_stack w then
          (* Successor w is on stack and hence in the current SCC *)
          let v_low = Hashtbl.find state.lowlink v in
          let w_idx = Hashtbl.find state.index_map w in
          Hashtbl.replace state.lowlink v (min v_low w_idx))
      successors;

    (* If v is a root node, pop the stack and generate an SCC *)
    let v_low = Hashtbl.find state.lowlink v in
    let v_idx = Hashtbl.find state.index_map v in
    if v_low = v_idx then (
      let scc = ref [] in
      let rec pop_loop () =
        let w = List.hd state.stack in
        state.stack <- List.tl state.stack;
        Hashtbl.replace state.on_stack w false;
        scc := w :: !scc;
        if w <> v then pop_loop ()
      in
      pop_loop ();
      state.sccs <- !scc :: state.sccs)
end

(* Main entry point: returns a list of SCCs (each SCC is a list of node names) *)
let tarjan iter_vertices get_edges =
  let state = make_state get_edges in
  (* Collect all vertices (keys + values appearing in adjacency lists) *)
  let vertices = Hashset.empty () in
  iter_vertices (Hashset.add vertices);
  (* Run strongconnect for every unvisited vertex *)
  Hashset.iter
    (fun v ->
      if not (Hashtbl.mem state.index_map v) then strong_connect state v)
    vertices;
  state.sccs
