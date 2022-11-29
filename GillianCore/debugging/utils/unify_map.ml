module L = Logging
module DL = Debugger_log
open Syntaxes.Option

type unify_result = Success | Failure [@@deriving yojson]

module Make (Verification : Verifier.S) = struct
  open Verification.SUnifier.Logging
  open L.LoggingConstants

  (** Describes {i why} this unification is happening *)
  type kind = Unifier.unify_kind [@@deriving yojson]

  (** A substitution, and the ID of the assertion where it was learned *)
  type substitution = {
    assert_id : L.ReportId.t; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  (** Represents one step of a unification *)
  type assertion_data = {
    id : L.ReportId.t;
    fold : (L.ReportId.t * unify_result) option;
    assertion : string;
    substitutions : substitution list;
  }
  [@@deriving yojson]

  (** A segment of unification *)
  type unify_seg =
    | Assertion of assertion_data * unify_seg  (** A single assertion *)
    | UnifyResult of L.ReportId.t * unify_result
        (** The end of this unification segment *)
  [@@deriving yojson]

  (** A unification map.
    Unification is either a single segment in the normal case, or potentially multiple segments when folding (i.e. when a predicate has multiple cases) *)
  type map = Direct of unify_seg | Fold of unify_seg list [@@deriving yojson]

  type t = kind * map [@@deriving yojson]

  (** Given the ID of a unification, queries the log database to determine wither that unification was successful *)
  let result_of_id id =
    let rec aux id =
      let children = L.LogQueryer.get_children_of id in
      if children = [] then
        Fmt.failwith "Unify_map.result_of_id: report %a has no children!"
          L.ReportId.pp id;
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
      | None ->
          children
          |> List.exists (fun (id, type_, _) ->
                 if type_ = ContentType.unify_case then aux id else false)
    in
    if aux id then Success else Failure

  (** Traverses a [unify_seg] to get its result *)
  let rec seg_result = function
    | Assertion (_, next) -> seg_result next
    | UnifyResult (_, result) -> result

  (** Gets the result of a unification.
    When the unification is a fold, one successful case is sufficient for overall success *)
  let result = function
    | _, Direct seg -> seg_result seg
    | _, Fold segs ->
        if segs |> List.exists (fun seg -> seg_result seg = Success) then
          Success
        else Failure

  (** Given the ID of an assertion or unify result, build the representative [unify_seg] *)
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
                "Unify_map.build_seg: assertion %a has multiple children!"
                L.ReportId.pp id
        in
        if type_ <> ContentType.unify then
          Fmt.failwith
            "Unify_map.build_seg: report %a (child of assertion %a) has type \
             %s (expected %s)!"
            L.ReportId.pp child_id L.ReportId.pp id type_ ContentType.unify;
        (child_id, result_of_id child_id)
      in
      let seg =
        match L.LogQueryer.get_next_report_ids id with
        | [] ->
            Fmt.failwith "Unify_map.build_seg: assertion %a has no next!"
              L.ReportId.pp id
        | [ next_id ] ->
            let content, type_ =
              L.LogQueryer.get_report next_id |> Option.get
            in
            build_seg ~prev_substs:substitutions (next_id, type_, content)
        | _ ->
            Fmt.failwith "Unify_map.build_seg: assertion %a has multiple nexts!"
              L.ReportId.pp id
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
        "Unify_map.build_seg: report %a has invalid type (%s) for unify_seg!"
        L.ReportId.pp id type_

  (** Given the ID of a (non-fold) unification or a unify case (when folding), build the representative [unify_seg] *)
  let build_case id : unify_seg =
    match L.LogQueryer.get_children_of ~roots_only:true id with
    | [] ->
        Fmt.failwith "Unify_map.build_case: id %a has no children!"
          L.ReportId.pp id
    | [ child ] -> build_seg child
    | _ ->
        Fmt.failwith "Unify_map.build_case: id %a has multiple children!"
          L.ReportId.pp id

  (** Given the ID of a folding unification, build the representative [unify_seg] of each case *)
  let build_cases id : unify_seg list =
    let rec aux id acc =
      let acc = build_case id :: acc in
      match L.LogQueryer.get_next_reports id with
      | [] -> acc
      | [ (id, type_, _) ] ->
          if type_ <> ContentType.unify_case then
            Fmt.failwith "Unify_map.build_cases: %a has type %s (expected %s)!"
              L.ReportId.pp id type_ ContentType.unify_case
          else aux id acc
      | _ ->
          Fmt.failwith
            "Unify_map.build_cases: unify_case %a has multiple nexts!"
            L.ReportId.pp id
    in

    aux id [] |> List.rev

  (** Given the ID of a unification, build the representative unification map *)
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
              "Unify_map.build: unify id %a should have one root child!"
              L.ReportId.pp unify_id
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
