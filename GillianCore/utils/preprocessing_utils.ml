module M (GraphNode : sig
  type t

  val successors : t -> int -> int list
end) =
struct
  type predecessors = (string * int * int, int) Hashtbl.t
  type t = GraphNode.t

  let get_succ_pred (cmds : ('a * 'b * t) array) =
    let cmds =
      Array.map
        (fun x ->
          match x with
          | _, _, cmd -> cmd)
        cmds
    in

    let number_of_cmds = Array.length cmds in
    let succ_table = Array.make number_of_cmds [] in
    let pred_table = Array.make number_of_cmds [] in

    (* adding i to the predecessors of j *)
    let update_pred_table i j =
      if j < number_of_cmds && i < number_of_cmds then
        pred_table.(j) <- i :: pred_table.(j)
      else ()
    in

    (* adding i to the successors of j *)
    let update_succ_table i j =
      if j < number_of_cmds && i < number_of_cmds then
        succ_table.(j) <- i :: succ_table.(j)
      else ()
    in

    for u = 0 to number_of_cmds - 1 do
      List.iter
        (fun j ->
          update_succ_table j u;
          update_pred_table u j)
        (GraphNode.successors cmds.(u) u)
    done;

    for k = 0 to number_of_cmds - 1 do
      succ_table.(k) <- List.rev succ_table.(k);
      pred_table.(k) <- List.rev pred_table.(k)
    done;
    (succ_table, pred_table)

  let compute_which_preds pred =
    List.concat
      (List.mapi
         (fun u u_preds -> List.mapi (fun i v -> (v, u, i)) u_preds)
         (Array.to_list pred))

  let extend_which_pred
      (global_which_pred : predecessors)
      (cmds : ('a * 'b * t) array)
      (name : string) : unit =
    let _succ_table, pred_table = get_succ_pred cmds in
    let which_pred = compute_which_preds pred_table in
    List.iter
      (fun (prev_cmd, cur_cmd, i) ->
        Hashtbl.replace global_which_pred (name, prev_cmd, cur_cmd) i)
      which_pred
end
