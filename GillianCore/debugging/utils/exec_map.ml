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
  unifications that occurred during executing the command.
  
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

(**/**)

(** The "kind" of a command in an exec map *)
type ('c, 'bd) cmd_kind =
  | Normal  (** A command that doesn't branch or terminate *)
  | Branch of ('c * 'bd) list  (** A branching command *)
  | Final  (** A terminating command *)
[@@deriving yojson]

(** Maps a list of branches to [Normal] if empty, or [Branch] *)
let kind_of_cases = function
  | [] -> Normal
  | cases -> Branch (List.map (fun case -> (case, ())) cases)

(** An exec map / node in an exec map; takes the following type parameters:
  - ['branch_case]: the type that identifies a branch case
  - ['cmd_data]: the type of the data attached to each non-[Nothing] node
  - ['branch_data]: additional data attached to each branch case
  *)
type ('branch_case, 'cmd_data, 'branch_data) t =
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
[@@deriving yojson]

(** Used for various map-traversal commands; signifies when to stop exploring paths *)
type stop_at =
  | StartOfPath  (** Stop at the start of the path, i.e. as soon as possible *)
  | EndOfPath  (** Stop at the end of the path, i.e. as late as possible*)
  | BeforeNothing
      (** As with [EndOfPath], but if the path ends with [Nothing], step back to the previous command *)

(** Data about a unification *)
type unification = {
  id : L.ReportId.t;
  kind : Unifier.unify_kind;
  result : Unify_map.unify_result;
}
[@@deriving yojson]

type 't submap =
  | NoSubmap
  | Submap of 't  (** Embed an [Exec_map] as a submap *)
  | Proc of string  (** Embed the execution of another proc as a submap *)
[@@deriving yojson]

(** Traverse the map depth-first, giving the path to the first node that matches the given predicate (or [None] otherwise) *)
let find_path pred map =
  let rec aux acc = function
    | (Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data })
      when pred data -> Some acc
    | Cmd { next; _ } -> aux acc next
    | BranchCmd { nexts; _ } ->
        nexts |> Hashtbl.find_map (fun case (_, next) -> aux (case :: acc) next)
    | _ -> None
  in
  aux [] map

(** Exception-raising equivalent to [find_path] *)
let find_path_exn pred map =
  match find_path pred map with
  | Some path -> path
  | None -> failwith "Exec_map.find_path"

(** Gets the node at the given path *)
let at_path ?(stop_at = EndOfPath) path map =
  let rec aux path map =
    match (map, path, stop_at) with
    | map, [], StartOfPath -> Some map
    | (Cmd { next = Nothing; _ } as map), [], BeforeNothing -> Some map
    | (BranchCmd { nexts; _ } as map), [ case ], BeforeNothing
      when Hashtbl.find_opt nexts case |> Option.map snd = Some Nothing ->
        Some map
    | Cmd { next; _ }, _, _ -> aux path next
    | BranchCmd { nexts; _ }, case :: path, _ -> (
        match Hashtbl.find_opt nexts case with
        | Some (_, next) -> aux path next
        | None -> None)
    | map, [], (EndOfPath | BeforeNothing) -> Some map
    | _, _, _ -> None
  in
  aux (List.rev path) map

(** Exception-raising equivalent to [at_path] *)
let at_path_exn ?(stop_at = EndOfPath) path map =
  match at_path ~stop_at path map with
  | Some map -> map
  | None -> failwith "Exec_map.at_path"

(** An Exec_map to be passed to the debugger frontend and displayed *)
module Packaged = struct
  type branch_case = {
    kind : string;
    display : string * string;
        (** A friendly name for the branch kind and specific branch case to be displayed to the user *)
    json : Yojson.Safe.t;
        (** The JSON of the original branch case; this can be target language specific *)
  }
  [@@deriving yojson]

  (* Need this to avoid name conflict *)
  (**/**)

  type ('branch_case, 'cmd_data, 'branch_data) _map =
    ('branch_case, 'cmd_data, 'branch_data) t
  [@@deriving yojson]

  (**/**)

  type t = (branch_case, cmd_data, unit) _map

  and cmd_data = {
    ids : L.ReportId.t list;
    display : string;
    unifys : unification list;
    errors : string list;
    submap : t submap;
  }
  [@@deriving yojson]

  (** Converts a GIL branch case to a packaged branch case *)
  let package_gil_case (case : BranchCase.t) : branch_case =
    let json = BranchCase.to_yojson case in
    let kind, display =
      match case with
      | GuardedGoto b -> ("GuardedGoto", ("If/Else", Fmt.str "%B" b))
      | LCmd x -> ("LCmd", ("Logical command", Fmt.str "%d" x))
      | SpecExec fl -> ("SpecExec", ("Spec exec", Fmt.str "%a" Flag.pp fl))
      | LAction json ->
          let s = Yojson.Safe.to_string (`List json) in
          ("LAction", ("Logical action", s))
      | LActionFail x ->
          ("LActionFail", ("Logical action failure", Fmt.str "%d" x))
    in
    { kind; display; json }

  (** Converts an Exec_map to a packaged Exec_map *)
  let package package_data package_case (map : ('c, 'd, 'bd) _map) : t =
    let rec aux map =
      match map with
      | Nothing -> Nothing
      | Cmd { data; next } ->
          Cmd { data = package_data aux data; next = aux next }
      | BranchCmd { data; nexts } ->
          let data = package_data aux data in
          let nexts =
            nexts
            |> Hashtbl.map (fun case (bdata, next) ->
                   let case = package_case bdata case in
                   let next = aux next in
                   (case, ((), next)))
          in
          BranchCmd { data; nexts }
      | FinalCmd { data } ->
          let data = package_data aux data in
          FinalCmd { data }
    in
    aux map
end
