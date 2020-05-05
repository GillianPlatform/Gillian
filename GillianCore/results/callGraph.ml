module type NodeSig = sig
  type t = private {
    id : string;
    proc_name : string;
    mutable children : string list;
  }
  [@@deriving yojson { exn = true }]

  val make : string -> string -> string list -> t

  val add_child : t -> string -> unit

  val pp : Format.formatter -> t -> unit
end

module Node : NodeSig = struct
  type t = { id : string; proc_name : string; mutable children : string list }
  [@@deriving yojson { exn = true }]

  let make id proc_name children = { id; proc_name; children }

  let add_child node child_id = node.children <- child_id :: node.children

  let pp fmt node =
    let pp_succ fmt = Fmt.pf fmt "    |--> <%s>" in
    let sep = Fmt.any "@\n" in
    Fmt.pf fmt "<%s>@\n%a@\n" node.id (Fmt.list ~sep pp_succ) node.children
end

type t = { nodes : (string, Node.t) Hashtbl.t }

let make () = { nodes = Hashtbl.create Config.small_tbl_size }

let pp fmt call_graph =
  let node_list =
    Hashtbl.fold (fun _ node acc -> node :: acc) call_graph.nodes []
  in
  Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@\n") Node.pp) node_list

let id_of_proc_name proc_name = "proc_" ^ proc_name

let get_node_opt call_graph id = Hashtbl.find_opt call_graph.nodes id

let get_node call_graph id =
  match get_node_opt call_graph id with
  | Some node -> node
  | None      -> failwith (Printf.sprintf "could not find node with id '%s'" id)

let get_or_make_node call_graph id proc_name =
  match get_node_opt call_graph id with
  | Some node -> node
  | None      ->
      let node = Node.make id proc_name [] in
      Hashtbl.add call_graph.nodes id node;
      node

let add_edge call_graph id pname child_id child_pname =
  let node = get_or_make_node call_graph id pname in
  ignore (get_or_make_node call_graph child_id child_pname);
  if not (List.mem child_id node.children) then Node.add_child node child_id

let contains call_graph id = Option.is_some (get_node_opt call_graph id)

let get_proc_name call_graph id = (get_node call_graph id).proc_name

let get_children call_graph id = (get_node call_graph id).children

let remove call_graph id = Hashtbl.remove call_graph.nodes id

let to_reverse_graph call_graph =
  let reverse_graph = make () in
  let () =
    Hashtbl.iter
      (fun id (node : Node.t) ->
        let pname = node.proc_name in
        List.iter
          (fun child_id ->
            let child_pname = get_proc_name call_graph child_id in
            add_edge reverse_graph child_id child_pname id pname)
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
