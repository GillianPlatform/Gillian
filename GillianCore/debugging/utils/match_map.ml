(**/**)

module L = Logging
module DL = Debugger_log
open Syntaxes.Option
include Match_map_intf

(**/**)

(** Traverses a [match_seg] to get its result *)
let rec seg_result = function
  | Assertion (_, next) -> seg_result next
  | MatchResult (_, result) -> result

(** Gets the result of a matching.
  When the matching is a fold, one successful case is sufficient for overall success *)
let result = function
  | _, Direct seg -> seg_result seg
  | _, Fold segs ->
      if segs |> List.exists (fun seg -> seg_result seg = Success) then Success
      else Failure

module Make_builder : Make_builder =
functor
  (Verification : Verifier.S)
  ->
  struct
    open Verification.SMatcher.Logging
    open L.Logging_constants

    let result_of_id id =
      let rec aux id =
        let children = L.Log_queryer.get_children_of id in
        if children = [] then
          Fmt.failwith "Match_map.result_of_id: report %a has no children!"
            L.Report_id.pp id;
        match
          children
          |> List.find_opt (fun (_, type_, _) ->
                 type_ = Content_type.match_result)
        with
        | Some (_, _, content) -> (
            let result_report =
              content |> Yojson.Safe.from_string |> MatchResultReport.of_yojson
              |> Result.get_ok
            in
            match result_report with
            | Success _ -> true
            | Failure _ -> false)
        | None ->
            children
            |> List.exists (fun (id, type_, _) ->
                   if type_ = Content_type.match_case then aux id else false)
      in
      if aux id then Success else Failure

    let rec build_seg ?(prev_substs = []) (id, type_, content) : match_seg =
      let module Subst = SVal.SESubst in
      if type_ = Content_type.assertion then
        let asrt_report =
          content |> Yojson.Safe.from_string |> AssertionReport.of_yojson
          |> Result.get_ok
        in
        let assertion =
          let asrt, _ = asrt_report.step in
          Fmt.str "%a" Asrt.pp_simple asrt
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
            match L.Log_queryer.get_children_of id with
            | [] -> None
            | [ child ] -> Some child
            | _ ->
                Fmt.failwith
                  "Match_map.build_seg: assertion %a has multiple children!"
                  L.Report_id.pp id
          in
          if type_ <> Content_type.match_ then
            Fmt.failwith
              "Match_map.build_seg: report %a (child of assertion %a) has type \
               %s (expected %s)!"
              L.Report_id.pp child_id L.Report_id.pp id type_
              Content_type.match_;
          (child_id, result_of_id child_id)
        in
        let seg =
          match L.Log_queryer.get_next_report_ids id with
          | [] ->
              Fmt.failwith "Match_map.build_seg: assertion %a has no next!"
                L.Report_id.pp id
          | [ next_id ] ->
              let content, type_ =
                L.Log_queryer.get_report next_id |> Option.get
              in
              build_seg ~prev_substs:substitutions (next_id, type_, content)
          | _ ->
              Fmt.failwith
                "Match_map.build_seg: assertion %a has multiple nexts!"
                L.Report_id.pp id
        in
        Assertion ({ id; fold; assertion; substitutions }, seg)
      else if type_ = Content_type.match_result then
        let result_report =
          content |> Yojson.Safe.from_string |> MatchResultReport.of_yojson
          |> Result.get_ok
        in
        let result =
          match result_report with
          | Success _ -> Success
          | Failure _ -> Failure
        in
        MatchResult (id, result)
      else
        Fmt.failwith
          "Match_map.build_seg: report %a has invalid type (%s) for match_seg!"
          L.Report_id.pp id type_

    let build_case id : match_seg =
      match L.Log_queryer.get_children_of ~roots_only:true id with
      | [] ->
          Fmt.failwith "Match_map.build_case: id %a has no children!"
            L.Report_id.pp id
      | [ child ] -> build_seg child
      | _ ->
          Fmt.failwith "Match_map.build_case: id %a has multiple children!"
            L.Report_id.pp id

    let build_cases id : match_seg list =
      let rec aux id acc =
        let acc = build_case id :: acc in
        match L.Log_queryer.get_next_reports id with
        | [] -> acc
        | [ (id, type_, _) ] ->
            if type_ <> Content_type.match_case then
              Fmt.failwith
                "Match_map.build_cases: %a has type %s (expected %s)!"
                L.Report_id.pp id type_ Content_type.match_case
            else aux id acc
        | _ ->
            Fmt.failwith
              "Match_map.build_cases: match_case %a has multiple nexts!"
              L.Report_id.pp id
      in

      aux id [] |> List.rev

    let f match_id =
      let kind =
        let content, _ = L.Log_queryer.get_report match_id |> Option.get in
        let match_report =
          content |> Yojson.Safe.from_string |> MatchReport.of_yojson
          |> Result.get_ok
        in
        match_report.match_kind
      in
      let map =
        let id, type_, _ =
          match L.Log_queryer.get_children_of ~roots_only:true match_id with
          | [ child ] -> child
          | _ ->
              Fmt.failwith
                "Match_map.build: match id %a should have one root child!"
                L.Report_id.pp match_id
        in
        if type_ = Content_type.match_case then
          let segs = build_cases id in
          Fold segs
        else
          let seg = build_case match_id in
          Direct seg
      in
      (kind, map)
  end
