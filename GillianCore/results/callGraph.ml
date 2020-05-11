type ntype = Proc | Pred [@@deriving yojson { exn = true }]

module type NodeSig = sig
  type t = private {
    id : string;
    ntype : ntype;
    name : string;
    mutable children : string list;
  }
  [@@deriving yojson { exn = true }]

  val make : string -> ntype -> string -> string list -> t

  val add_child : t -> string -> unit

  val pp : Format.formatter -> t -> unit
end

module Node : NodeSig = struct
  type t = {
    id : string;
    ntype : ntype;
    name : string;
    mutable children : string list;
  }
  [@@deriving yojson { exn = true }]

  let make id ntype name children = { id; ntype; name; children }

  let add_child node child_id = node.children <- child_id :: node.children

  let pp fmt node =
    let pp_succ fmt = Fmt.pf fmt "    |--> <%s>" in
    let sep = Fmt.any "@\n" in
    Fmt.pf fmt "<%s>@\n%a@\n" node.id (Fmt.list ~sep pp_succ) node.children
end

type t = { nodes : (string, Node.t) Hashtbl.t }

let make () = { nodes = Hashtbl.create Config.small_tbl_size }

let reset call_graph = Hashtbl.reset call_graph.nodes

let pp fmt call_graph =
  let node_list =
    Hashtbl.fold (fun _ node acc -> node :: acc) call_graph.nodes []
  in
  Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@\n") Node.pp) node_list

let id_of_proc_name proc_name = "proc_" ^ proc_name

let id_of_pred_name pred_name = "pred_" ^ pred_name

let id_of_name name = function
  | Proc -> id_of_proc_name name
  | Pred -> id_of_pred_name name

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

let add_proc_call call_graph caller callee =
  add_edge call_graph Proc caller Proc callee

let add_pred_call call_graph caller callee =
  add_edge call_graph Pred caller Pred callee

let add_proc_pred_use call_graph proc_name pred_name =
  add_edge call_graph Proc proc_name Pred pred_name

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

let contains_proc call_graph proc_name =
  let proc_id = id_of_proc_name proc_name in
  Option.is_some (get_node_opt call_graph proc_id)

let contains_pred call_graph pred_name =
  let pred_id = id_of_pred_name pred_name in
  Option.is_some (get_node_opt call_graph pred_id)

let is_proc call_graph id =
  match (get_node call_graph id).ntype with
  | Proc -> true
  | _    -> false

let is_pred call_graph id =
  match (get_node call_graph id).ntype with
  | Pred -> true
  | _    -> false

let remove call_graph id = Hashtbl.remove call_graph.nodes id

let to_reverse_graph call_graph =
  let reverse_graph = make () in
  let () =
    Hashtbl.iter
      (fun id (node : Node.t) ->
        List.iter
          (fun child_id ->
            let child = get_node call_graph child_id in
            add_edge reverse_graph child.ntype child.name node.ntype node.name)
          node.children)
      call_graph.nodes
  in
  reverse_graph

let merge_graphs call_graph other_graph =
  let () =
    Hashtbl.iter
      (fun id node -> Hashtbl.replace call_graph.nodes id node)
      other_graph.nodes
  in
  call_graph

let to_yojson call_graph =
  `List
    (Hashtbl.fold
       (fun id node acc -> Node.to_yojson node :: acc)
       call_graph.nodes [])

let of_yojson_exn json =
  let call_graph = make () in
  let () =
    List.iter
      (fun json ->
        let node = Node.of_yojson_exn json in
        Hashtbl.add call_graph.nodes node.id node)
      (Yojson.Safe.Util.to_list json)
  in
  call_graph
