type ntype = Proc | Pred | Lemma [@@deriving yojson]

module Node : sig
  type t = private {
    id : string;
    ntype : ntype;
    name : string;
    mutable children : string list;
  }
  [@@deriving yojson]

  val make : string -> ntype -> string -> string list -> t

  val add_child : t -> string -> unit

  val pp : Format.formatter -> t -> unit
end = struct
  type t = {
    id : string;
    ntype : ntype;
    name : string;
    mutable children : string list;
  }
  [@@deriving yojson]

  let make id ntype name children = { id; ntype; name; children }

  let add_child node child_id = node.children <- child_id :: node.children

  let pp fmt node =
    let pp_succ fmt = Fmt.pf fmt "    |--> <%s>" in
    let sep = Fmt.any "@\n" in
    Fmt.pf fmt "<%s>@\n%a@\n" node.id (Fmt.list ~sep pp_succ) node.children
end

let nodes_to_yojson tbl =
  tbl |> Hashtbl.to_seq |> List.of_seq |> [%to_yojson: (string * Node.t) list]

let nodes_of_yojson yj =
  let ( >| ) o f = Result.map f o in
  yj |> [%of_yojson: (string * Node.t) list] >| List.to_seq >| Hashtbl.of_seq

type t = {
  nodes : (string, Node.t) Hashtbl.t;
      [@to_yojson nodes_to_yojson] [@of_yojson nodes_of_yojson]
}
[@@deriving yojson]

type id = string

module IdSet = Containers.SS

let make ?(init_capacity = Config.small_tbl_size) () =
  { nodes = Hashtbl.create init_capacity }

let reset call_graph = Hashtbl.reset call_graph.nodes

let pp fmt call_graph =
  let node_list =
    Hashtbl.fold (fun _ node acc -> node :: acc) call_graph.nodes []
  in
  Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@\n") Node.pp) node_list

let id_of_proc_name proc_name = "proc_" ^ proc_name

let id_of_pred_name pred_name = "pred_" ^ pred_name

let id_of_lemma_name lemma_name = "lemma_" ^ lemma_name

let id_of_name name = function
  | Proc  -> id_of_proc_name name
  | Pred  -> id_of_pred_name name
  | Lemma -> id_of_lemma_name name

let get_node_opt call_graph id = Hashtbl.find_opt call_graph.nodes id

let get_node call_graph id =
  match get_node_opt call_graph id with
  | Some node -> node
  | None      -> failwith (Printf.sprintf "could not find node with id '%s'" id)

let get_or_make_node call_graph id ntype name =
  match get_node_opt call_graph id with
  | Some node -> node
  | None      ->
      let node = Node.make id ntype name [] in
      Hashtbl.add call_graph.nodes id node;
      node

let add_edge call_graph ntype name child_ntype child_name =
  let id = id_of_name name ntype in
  let child_id = id_of_name child_name child_ntype in
  let node = get_or_make_node call_graph id ntype name in
  ignore (get_or_make_node call_graph child_id child_ntype child_name);
  if not (List.mem child_id node.children) then Node.add_child node child_id

let add_proc call_graph proc_name =
  let proc_id = id_of_proc_name proc_name in
  ignore (get_or_make_node call_graph proc_id Proc proc_name)

let add_pred call_graph pred_name =
  let pred_id = id_of_pred_name pred_name in
  ignore (get_or_make_node call_graph pred_id Pred pred_name)

let add_lemma call_graph lemma_name =
  let lemma_id = id_of_lemma_name lemma_name in
  ignore (get_or_make_node call_graph lemma_id Lemma lemma_name)

let add_proc_call call_graph caller callee =
  add_edge call_graph Proc caller Proc callee

let add_pred_call call_graph caller callee =
  add_edge call_graph Pred caller Pred callee

let add_lemma_call call_graph caller callee =
  add_edge call_graph Lemma caller Lemma callee

let add_proc_pred_use call_graph proc_name pred_name =
  add_edge call_graph Proc proc_name Pred pred_name

let add_proc_lemma_use call_graph proc_name lemma_name =
  add_edge call_graph Proc proc_name Lemma lemma_name

let add_lemma_pred_use calL_graph lemma_name pred_name =
  add_edge calL_graph Lemma lemma_name Pred pred_name

let get_name call_graph id = (get_node call_graph id).name

let get_children call_graph id = (get_node call_graph id).children

let get_proc_names call_graph =
  Hashtbl.fold
    (fun id (node : Node.t) acc ->
      match node.ntype with
      | Proc -> node.name :: acc
      | _    -> acc)
    call_graph.nodes []

let get_pred_names call_graph =
  Hashtbl.fold
    (fun id (node : Node.t) acc ->
      match node.ntype with
      | Pred -> node.name :: acc
      | _    -> acc)
    call_graph.nodes []

let get_lemma_names call_graph =
  Hashtbl.fold
    (fun id (node : Node.t) acc ->
      match node.ntype with
      | Lemma -> node.name :: acc
      | _     -> acc)
    call_graph.nodes []

let contains_id call_graph id = Option.is_some (get_node_opt call_graph id)

let contains_proc call_graph proc_name =
  contains_id call_graph (id_of_proc_name proc_name)

let contains_pred call_graph pred_name =
  contains_id call_graph (id_of_pred_name pred_name)

let contains_lemma call_graph lemma_name =
  contains_id call_graph (id_of_lemma_name lemma_name)

let is_proc call_graph id =
  match (get_node call_graph id).ntype with
  | Proc -> true
  | _    -> false

let is_pred call_graph id =
  match (get_node call_graph id).ntype with
  | Pred -> true
  | _    -> false

let is_lemma call_graph id =
  match (get_node call_graph id).ntype with
  | Lemma -> true
  | _     -> false

let remove call_graph id = Hashtbl.remove call_graph.nodes id

let prune_procs call_graph proc_names =
  let proc_ids = List.map id_of_proc_name proc_names in
  List.iter (remove call_graph) proc_ids

let prune_lemmas call_graph lemma_names =
  let lemma_ids = List.map id_of_lemma_name lemma_names in
  List.iter (remove call_graph) lemma_ids

let to_reverse_graph call_graph =
  let reverse_graph = make () in
  let () =
    Hashtbl.iter
      (fun id (node : Node.t) ->
        List.iter
          (fun child_id ->
            let child = get_node call_graph child_id in
            add_edge reverse_graph child.ntype child.name node.ntype node.name)
          node.children;
        (* Include entry for node (in case it did not have any children) *)
        match node.ntype with
        | Proc  -> add_proc reverse_graph node.name
        | Pred  -> add_pred reverse_graph node.name
        | Lemma -> add_lemma reverse_graph node.name)
      call_graph.nodes
  in
  reverse_graph

let merge call_graph other_graph =
  let () =
    Hashtbl.iter
      (fun id node ->
        if not (contains_id call_graph id) then
          Hashtbl.add call_graph.nodes id node)
      other_graph.nodes
  in
  call_graph
