module L = Logging
module DL = Debugger_log
open Syntaxes.Option

type rid = L.ReportId.t [@@deriving show, yojson]
type unify_result = Success | Failure [@@deriving yojson]

module Make (Verification : Verifier.S) = struct
  open Verification.SUnifier.Logging
  open L.LoggingConstants

  type result = unify_result = Success | Failure [@@deriving yojson]
  type kind = Unifier.unify_kind [@@deriving yojson]

  type substitution = {
    assert_id : rid; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  type assertion_data = {
    id : rid;
    fold : (rid * unify_result) option;
    assertion : string;
    substitutions : substitution list;
  }
  [@@deriving yojson]

  type unify_seg =
    | Assertion of assertion_data * unify_seg
    | UnifyResult of rid * unify_result
  [@@deriving yojson]

  type map = Direct of unify_seg | Fold of unify_seg list [@@deriving yojson]
  type t = kind * map [@@deriving yojson]

  let result_of_id id =
    let rec aux id =
      let children = L.LogQueryer.get_children_of id in
      if children = [] then
        Fmt.failwith "UnifyMap.result_of_id: report %a has no children!" pp_rid
          id;
      match
        children
        |> List.find_opt (fun (_, type_, _) -> type_ = ContentType.unify_result)
      with
      | Some (_, _, content) -> (
          let result_report =
            content |> Yojson.Safe.from_string |> UnifyResultReport.of_yojson
            |> Result.get_ok
          in
          match result_report with
          | Success _ -> true
          | Failure _ -> false)
      | None -> children |> List.exists (fun (id, _, _) -> aux id)
    in
    if aux id then Success else Failure

  let rec seg_result = function
    | Assertion (_, next) -> seg_result next
    | UnifyResult (_, result) -> result

  let result = function
    | _, Direct seg -> seg_result seg
    | _, Fold segs ->
        if segs |> List.exists (fun seg -> seg_result seg = Success) then
          Success
        else Failure

  let rec build_seg ?(prev_substs = []) (id, type_, content) : unify_seg =
    let module Subst = Verification.SUnifier.ESubst in
    if type_ = ContentType.assertion then
      let asrt_report =
        content |> Yojson.Safe.from_string |> AssertionReport.of_yojson
        |> Result.get_ok
      in
      let assertion =
        let asrt, _ = asrt_report.step in
        Fmt.str "%a" Asrt.pp asrt
      in
      let substitutions =
        asrt_report.subst |> Subst.to_list_pp
        |> List.map (fun subst ->
               List.find_opt
                 (fun prev -> [%eq: string * string] prev.subst subst)
                 prev_substs
               |> Option.value ~default:{ assert_id = id; subst })
      in
      let fold =
        let+ child_id, type_, _ =
          match L.LogQueryer.get_children_of id with
          | [] -> None
          | [ child ] -> Some child
          | _ ->
              Fmt.failwith
                "UnifyMap.build_seg: assertion %a has multiple children!" pp_rid
                id
        in
        if type_ <> ContentType.unify then
          Fmt.failwith
            "UnifyMap.build_seg: report %a (child of assertion %a) has type %s \
             (expected %s)!"
            pp_rid child_id pp_rid id type_ ContentType.unify;
        (child_id, result_of_id child_id)
      in
      let seg =
        match L.LogQueryer.get_next_report_ids id with
        | [] ->
            Fmt.failwith "UnifyMap.build_seg: assertion %a has no next!" pp_rid
              id
        | [ next_id ] ->
            let content, type_ =
              L.LogQueryer.get_report next_id |> Option.get
            in
            build_seg ~prev_substs:substitutions (next_id, type_, content)
        | _ ->
            Fmt.failwith "UnifyMap.build_seg: assertion %a has multiple nexts!"
              pp_rid id
      in
      Assertion ({ id; fold; assertion; substitutions }, seg)
    else if type_ = ContentType.unify_result then
      let result_report =
        content |> Yojson.Safe.from_string |> UnifyResultReport.of_yojson
        |> Result.get_ok
      in
      let result =
        match result_report with
        | Success _ -> Success
        | Failure _ -> Failure
      in
      UnifyResult (id, result)
    else
      Fmt.failwith
        "UnifyMap.build_seg: report %a has invalid type (%s) for unify_seg!"
        pp_rid id type_

  let build_case id : unify_seg =
    match L.LogQueryer.get_children_of ~roots_only:true id with
    | [] -> Fmt.failwith "UnifyMap.build_case: id %a has no children!" pp_rid id
    | [ child ] -> build_seg child
    | _ ->
        Fmt.failwith "UnifyMap.build_case: id %a has multiple children!" pp_rid
          id

  let build_cases id : unify_seg list =
    let rec aux id acc =
      let acc = build_case id :: acc in
      match L.LogQueryer.get_next_reports id with
      | [] -> acc
      | [ (id, type_, _) ] ->
          if type_ <> ContentType.unify_case then
            Fmt.failwith "UnifyMap.build_cases: %a has type %s (expected %s)!"
              pp_rid id type_ ContentType.unify_case
          else aux id acc
      | _ ->
          Fmt.failwith "UnifyMap.build_cases: unify_case %a has multiple nexts!"
            pp_rid id
    in

    aux id [] |> List.rev

  let build unify_id =
    let kind =
      let content, _ = L.LogQueryer.get_report unify_id |> Option.get in
      let unify_report =
        content |> Yojson.Safe.from_string |> UnifyReport.of_yojson
        |> Result.get_ok
      in
      unify_report.unify_kind
    in
    let map =
      let id, type_, _ =
        match L.LogQueryer.get_children_of ~roots_only:true unify_id with
        | [ child ] -> child
        | _ ->
            Fmt.failwith
              "UnifyMap.build: unify id %a should have one root child!" pp_rid
              unify_id
      in
      if type_ = ContentType.unify_case then
        let segs = build_cases id in
        Fold segs
      else
        let seg = build_case unify_id in
        Direct seg
    in
    (kind, map)
end
