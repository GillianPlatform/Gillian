(**/**)

module L = Logging
module DL = Debugger_log
open Syntaxes.Option
include Match_map_intf

(**/**)

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

    let build_assertion_data ~pp_asrt ~prev_substs id content =
      let module Subst = SVal.SESubst in
      let asrt_report =
        content |> Yojson.Safe.from_string |> AssertionReport.of_yojson
        |> Result.get_ok
      in
      let assertion =
        let asrt, _ = asrt_report.step in
        Fmt.str "%a" pp_asrt asrt
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
            L.Report_id.pp child_id L.Report_id.pp id type_ Content_type.match_;
        let p =
          match asrt_report.step with
          | Pred (p, _), _ -> p
          | _ -> assertion
        in
        { id = child_id; kind = Fold p; result = result_of_id child_id }
      in
      { id; fold; assertion; substitutions }

    let rec build_map ~pp_asrt ?(prev_substs = []) ~nodes id type_ content =
      if type_ = Content_type.assertion then
        let data = build_assertion_data ~pp_asrt ~prev_substs id content in
        let next_ids = L.Log_queryer.get_next_report_ids id in
        let result =
          let () =
            if List.is_empty next_ids then
              Fmt.failwith "Match_map.build_seg: assertion %a has no next!"
                L.Report_id.pp id
          in
          List.fold_left
            (fun result next_id ->
              let content, type_ =
                L.Log_queryer.get_report next_id |> Option.get
              in
              let result' =
                build_map ~pp_asrt ~prev_substs:data.substitutions ~nodes
                  next_id type_ content
              in
              if result = Success then Success else result')
            Failure next_ids
        in
        let () = Hashtbl.replace nodes id (Assertion (data, next_ids)) in
        result
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
        let () = Hashtbl.replace nodes id (MatchResult (id, result)) in
        result
      else
        Fmt.failwith
          "Match_map.build_seg: report %a has invalid type (%s) for match map!"
          L.Report_id.pp id type_

    let f ?(pp_asrt = Asrt.pp_atom) match_id =
      let kind =
        let content, _ = L.Log_queryer.get_report match_id |> Option.get in
        let match_report =
          content |> Yojson.Safe.from_string |> MatchReport.of_yojson
          |> Result.get_ok
        in
        match_report.match_kind
      in
      let roots = L.Log_queryer.get_children_of ~roots_only:true match_id in
      let nodes = Hashtbl.create 1 in
      let roots, result =
        List.fold_left
          (fun (roots, result) (id, type_, content) ->
            let result' = build_map ~pp_asrt ~nodes id type_ content in
            let result = if result = Success then Success else result' in
            (id :: roots, result))
          ([], Failure) roots
      in
      { kind; roots; nodes; result }
  end
