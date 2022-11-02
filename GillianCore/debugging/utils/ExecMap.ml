module L = Logging

type rid = L.ReportId.t [@@deriving yojson]

type ('c, 'bd) cmd_kind = Normal | Branch of ('c * 'bd) list | Final
[@@deriving yojson]

let kind_of_cases = function
  | [] -> Normal
  | cases -> Branch (List.map (fun case -> (case, ())) cases)

type ('branch_case, 'cmd_data, 'branch_data) t =
  | Nothing
  | Cmd of {
      data : 'cmd_data;
      mutable next : ('branch_case, 'cmd_data, 'branch_data) t;
    }
  | BranchCmd of {
      data : 'cmd_data;
      nexts :
        ( 'branch_case,
          'branch_data * ('branch_case, 'cmd_data, 'branch_data) t )
        Hashtbl.t;
    }
  | FinalCmd of { data : 'cmd_data }
[@@deriving yojson]

type stop_at = StartOfPath | EndOfPath | BeforeNothing

type unifys = (rid * Unifier.unify_kind * UnifyMap.unify_result) list
[@@deriving yojson]

type 't submap = NoSubmap | Submap of 't | Proc of string [@@deriving yojson]

let find_path_opt pred map =
  let rec aux acc = function
    | (Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data })
      when pred data -> Some acc
    | Cmd { next; _ } -> aux acc next
    | BranchCmd { nexts; _ } ->
        nexts |> Hashtbl.find_map (fun case (_, next) -> aux (case :: acc) next)
    | _ -> None
  in
  aux [] map

let find_path pred map =
  match find_path_opt pred map with
  | Some path -> path
  | None -> failwith "ExecMap.find_path"

let at_path_opt ?(stop_at = EndOfPath) path map =
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

let at_path ?(stop_at = EndOfPath) path map =
  match at_path_opt ~stop_at path map with
  | Some map -> map
  | None -> failwith "ExecMap.at_path"

module Packaged = struct
  type branch_case = {
    kind : string;
    display : string * string;
    json : Yojson.Safe.t;
  }
  [@@deriving yojson]

  type ('branch_case, 'cmd_data, 'branch_data) _map =
    ('branch_case, 'cmd_data, 'branch_data) t
  (* Need this to avoid name conflict *)
  [@@deriving yojson]

  type t = (branch_case, cmd_data, unit) _map

  and cmd_data = {
    ids : rid list;
    display : string;
    unifys : unifys;
    errors : string list;
    submap : t submap;
  }
  [@@deriving yojson]

  let package_case (case : BranchCase.t) : branch_case =
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
