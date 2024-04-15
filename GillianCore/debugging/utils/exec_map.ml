(** A map of execution of a function (symbolic or otherwise).

  This is represented in a tree structure, where each node represents a command.

  Each command node comes in one of these forms:
  - Normal ( {!Cmd} ) - a normal command with one command node as its child.
  - Branching ({!BranchCmd}) - a command with one or more children, with each
    child identified by a branch case type; this represents a command that has
    multiple potential outcomes, depending on the program state (e.g. an
    if/else). Note that this is only interesting in symbolic execution; in
    concrete execution, they will always have exactly one branch case, thus
    functioning similarly to normal command nodes.
  - Final ({!FinalCmd}) - a command with no children; this represents the end
    of an execution branch, either due to errors, or normal termination (i.e.
    when returning from the function).
  
  These nodes also contain some (configurable) set of information - this
  usually includes the relevant report ID from the log database, a
  human-readable representation of the command, and any errors and
  matches that occurred during executing the command.
  
  The child of a {!Cmd} node may be {!Nothing}, as can any child of a
  {!BranchCmd} node; this represents that there is a command here in the code,
  but it hasn't yet been executed. This is to facilitate the debugger's
  step-by-step behaviour.
  
  A command node may also contain a submap; this embeds another exec map inside
  this command node, either described in full or referred to by name (see
  {!submap}). This is used to, for example, embed the body of a while-loop in
  the while-loop command itself. *)

(**/**)

module L = Logging
open Utils.Prelude
open Utils.Syntaxes.Option

(**/**)

(** The "kind" of a command in an exec map *)
type ('c, 'bd) next_kind =
  | One of 'bd  (** A command that doesn't branch or terminate *)
  | Many of ('c * 'bd) list  (** A branching command *)
  | Zero  (** A terminating command *)
[@@deriving yojson]

(** Maps a list of branches to [Normal] if empty, or [Branch] *)
let kind_of_cases = function
  | [] -> One ()
  | cases -> Many (List.map (fun case -> (case, ())) cases)

(** An exec map / node in an exec map; takes the following type parameters:
  - ['branch_case]: the type that identifies a branch case
  - ['cmd_data]: the type of the data attached to each non-[Nothing] node
  - ['branch_data]: additional data attached to each branch case
  *)
(* type ('branch_case, 'cmd_data, 'branch_data) t =
     | Nothing  (** An empty space; represents a command yet to be executed*)
     | Cmd of {
         data : 'cmd_data;
         mutable next : ('branch_case, 'cmd_data, 'branch_data) t;
       }  (** A non-branching command with one next command *)
     | BranchCmd of {
         data : 'cmd_data;
         nexts :
           ( 'branch_case,
             'branch_data * ('branch_case, 'cmd_data, 'branch_data) t )
           Hashtbl.t;
       }  (** A branching command, with one or more branch cases *)
     | FinalCmd of { data : 'cmd_data }
         (** A command with no subsequent ones, either due to normal termination or an error*)
   [@@deriving yojson] *)

type ('id, 'case, 'branch_data) next =
  | Single of ('id option * 'branch_data)
  | Branch of ('case * ('id option * 'branch_data)) list
[@@deriving yojson]

type ('id, 'case, 'data, 'bdata) node = {
  data : 'data;
  next : ('id, 'case, 'bdata) next option;
}
[@@deriving yojson]

type ('id, 'case, 'cmd_data, 'branch_data) entry =
  | Node of ('id, 'case, 'cmd_data, 'branch_data) node
  | Alias of 'id
[@@deriving yojson]

type ('id, 'branch_case, 'cmd_data, 'branch_data) map = {
  mutable root : 'id option;
  entries : ('id, ('id, 'branch_case, 'cmd_data, 'branch_data) entry) Hashtbl.t;
}
[@@deriving yojson]

(**/**)

(**/**)

(** A command in an exec map *)

(** Used for various map-traversal commands; signifies when to stop exploring paths *)
type stop_at =
  | StartOfPath  (** Stop at the start of the path, i.e. as soon as possible *)
  | EndOfPath  (** Stop at the end of the path, i.e. as late as possible*)
  | BeforeNothing
      (** As with [EndOfPath], but if the path ends with [Nothing], step back to the previous command *)

(** Data about a matching *)
type matching = {
  id : L.Report_id.t;
  kind : Matcher.match_kind;
  result : Match_map.match_result;
}
[@@deriving yojson]

type 't submap =
  | NoSubmap
  | Submap of 't  (** Embed an [Exec_map] as a submap *)
  | Proc of string  (** Embed the execution of another proc as a submap *)
[@@deriving yojson]

let make () = { root = None; entries = Hashtbl.create 1 }

(* Gets the node at an ID (while resolving aliases), along with its true ID *)
let rec get_with_id map id =
  match Hashtbl.find_opt map.entries id with
  | Some (Node node) -> Some (node, id)
  | Some (Alias id) -> get_with_id map id
  | None -> None

(* Gets the node at an ID (while resolving aliases) *)
let get map id = get_with_id map id |> Option.map fst

let map_node_extra map id f =
  match get_with_id map id with
  | Some (node, id) ->
      let new_node, aux = f node in
      let () = Hashtbl.replace map.entries id (Node new_node) in
      Some aux
  | None -> None

let map_node_extra_exn map id f =
  match map_node_extra map id f with
  | Some aux -> aux
  | None -> raise Not_found

(* Maps a node at an ID, returning true if found, and false if not *)
let map_node map id f =
  let f node = (f node, ()) in
  match map_node_extra map id f with
  | Some () -> true
  | None -> false

(* Maps a node at an ID. Raises [Not_found] if not found. *)
let map_node_exn map id f = if map_node map f id then () else raise Not_found

let insert map ~id ~all_ids node =
  let () =
    List.iter (fun id' -> Hashtbl.replace map.entries id' (Alias id)) all_ids
  in
  let () = Hashtbl.replace map.entries id (Node node) in
  ()

let get_exn map id =
  match get map id with
  | Some node -> node
  | None -> failwith "Exec_map.get"

(** Traverse the map depth-first, giving the path to the first node that matches the given predicate (or [None] otherwise) *)
let find_path pred map =
  let rec aux acc node =
    let/ () = if pred node then Some acc else None in
    let* next = node.next in
    match next with
    | Single (None, _) -> None
    | Single (Some id, _) ->
        let* next = get map id in
        aux acc next
    | Branch cases ->
        cases
        |> List.find_map (fun (case, (id, _)) ->
               let* id = id in
               let* next = get map id in
               aux (case :: acc) next)
  in
  let* root_id = map.root in
  let* root = get map root_id in
  aux [] root

(** Exception-raising equivalent to [find_path] *)
let find_path_exn pred map =
  match find_path pred map with
  | Some path -> path
  | None -> failwith "Exec_map.find_path"

(** Gets the node at the given path *)
let at_path ?(stop_at = EndOfPath) path map =
  let is_branch_empty case nexts =
    match List.assoc_opt case nexts with
    | Some (None, _) -> true
    | _ -> false
  in
  let rec aux path node =
    match (node.next, path, stop_at) with
    | _, [], StartOfPath -> Some node
    | Some (Single (None, _)), [], BeforeNothing -> Some node
    | Some (Branch nexts), [ case ], BeforeNothing
      when is_branch_empty case nexts -> Some node
    | Some (Single (Some id, _)), _, _ ->
        let* next = get map id in
        aux path next
    | Some (Branch nexts), case :: path, _ -> (
        match List.assoc_opt case nexts with
        | Some (Some id, _) ->
            let* next = get map id in
            aux path next
        | _ -> None)
    | _, [], (EndOfPath | BeforeNothing) -> Some node
    | _, _, _ -> None
  in
  let* root_id = map.root in
  let* root = get map root_id in
  aux (List.rev path) root

(** Exception-raising equivalent to [at_path] *)
let at_path_exn ?(stop_at = EndOfPath) path map =
  match at_path ~stop_at path map with
  | Some map -> map
  | None -> failwith "Exec_map.at_path"

(** An Exec_map to be passed to the debugger frontend and displayed *)
module Packaged = struct
  type id = L.Report_id.t [@@deriving yojson]
  type branch_case = Yojson.Safe.t [@@deriving yojson]

  type cmd_data = {
    id : L.Report_id.t;
    all_ids : L.Report_id.t list;
    display : string;
    matches : matching list;
    errors : string list;
    submap : id submap;
  }
  [@@deriving yojson]

  type branch_data = string [@@deriving yojson]

  type t = (L.Report_id.t, branch_case, cmd_data, branch_data) map
  [@@deriving yojson]

  (** Converts a GIL branch case to a packaged branch case *)
  let package_gil_case (case : Branch_case.t) : branch_case * branch_data =
    let json = Branch_case.to_yojson case in
    let display = Fmt.str "%a" Branch_case.pp_short case in
    (json, display)

  (** Converts an Exec_map to a packaged Exec_map *)
  let package
      package_id
      package_node
      ({ root; entries } : ('i, 'c, 'd, 'bd) map) : t =
    let map' =
      {
        root = Option.map package_id root;
        entries = Hashtbl.create (Hashtbl.length entries);
      }
    in
    let () =
      Hashtbl.iter
        (fun id entry ->
          let entry' =
            match entry with
            | Node node -> Node (package_node node)
            | Alias id -> Alias (package_id id)
          in
          Hashtbl.replace map'.entries (package_id id) entry')
        entries
    in
    map'
end
