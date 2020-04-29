module Json = Yojson.Basic
module Json_utils = Yojson.Basic.Util

module type NodeSig = sig
  type t = private {
    id : string;
    proc_name : string;
    mutable children : string list;
  }

  val make : string -> string -> string list -> t

  val add_child : t -> string -> unit

  val pp : Format.formatter -> t -> unit

  val to_json : t -> Json.t

  val from_json : Json.t -> t
end

module Node : NodeSig = struct
  type t = { id : string; proc_name : string; mutable children : string list }

  let make id proc_name children = { id; proc_name; children }

  let add_child node child_id = node.children <- child_id :: node.children

  let pp fmt node =
    let pp_succ fmt = Fmt.pf fmt "    |--> <%s>" in
    let sep = Fmt.any "@\n" in
    Fmt.pf fmt "<%s>@\n%a@\n" node.id (Fmt.list ~sep pp_succ) node.children

  let to_json node =
    let id_field = ("id", `String node.id) in
    let pname_field = ("proc_name", `String node.proc_name) in
    let children_field =
      ("children", `List (List.map (fun c -> `String c) node.children))
    in
    `Assoc [ id_field; pname_field; children_field ]

  let from_json json =
    let node_obj = Json_utils.to_assoc json in
    let id = Json_utils.to_string (List.assoc "id" node_obj) in
    let proc_name = Json_utils.to_string (List.assoc "proc_name" node_obj) in
    let children_list = Json_utils.to_list (List.assoc "children" node_obj) in
    let children = List.map Json_utils.to_string children_list in
    { id; proc_name; children }
end

type t = { nodes : (string, Node.t) Hashtbl.t }

let make () = { nodes = Hashtbl.create Config.small_tbl_size }

let pp fmt call_graph =
  let node_list =
    Hashtbl.fold (fun _ node acc -> node :: acc) call_graph.nodes []
  in
  Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@\n") Node.pp) node_list

let id_of_proc_name proc_name = "proc_" ^ proc_name

let get_or_make_node nodes id proc_name =
  match Hashtbl.find_opt nodes id with
  | Some node -> node
  | None      ->
      let node = Node.make id proc_name [] in
      Hashtbl.add nodes id node;
      node

let add_edge call_graph id pname child_id child_pname =
  let node = get_or_make_node call_graph.nodes id pname in
  ignore (get_or_make_node call_graph.nodes child_id child_pname);
  if not (List.mem child_id node.children) then Node.add_child node child_id

let to_reverse_graph call_graph =
  let get_pname id =
    match Hashtbl.find_opt call_graph.nodes id with
    | Some node -> node.proc_name
    | None      -> failwith (Printf.sprintf "could not find node with id %s" id)
  in
  let reverse_graph = make () in
  let () =
    Hashtbl.iter
      (fun id (node : Node.t) ->
        let pname = node.proc_name in
        List.iter
          (fun child_id ->
            let child_pname = get_pname child_id in
            add_edge reverse_graph child_id child_pname id pname)
          node.children)
      call_graph.nodes
  in
  reverse_graph

let to_json call_graph =
  `List
    (Hashtbl.fold
       (fun id node acc -> Node.to_json node :: acc)
       call_graph.nodes [])

let from_json json =
  let call_graph = make () in
  let () =
    List.iter
      (fun json ->
        let node = Node.from_json json in
        Hashtbl.add call_graph.nodes node.id node)
      (Json_utils.to_list json)
  in
  call_graph
