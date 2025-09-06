open Effect.Shallow
include Choice_intf
module IdSet = Set.Make (Int64)

module Make (Types : Types) = struct
  type id = int64
  type case = Types.case
  type breakpoint = Types.breakpoint

  type 'case Effect.t +=
    | Choice : (id * case list) option -> (id * case) option Effect.t
    | Breakpoint : breakpoint -> (id * case) option Effect.t

  type 'res cont = ((id * case) option, 'res Seq.node) continuation
  type 'res choices = (case * 'res Seq.t) list

  module Map = struct
    let id_count = ref Int64.zero

    let get_id () =
      let id = !id_count in
      id_count := Int64.succ id;
      id

    module Branch = struct
      type 'res t = {
        case : case;
        mutable cont : 'res cont;
        mutable child_ids : IdSet.t;
      }

      let add_child branch child_id =
        let ids = branch.child_ids in
        let ids = IdSet.add child_id ids in
        branch.child_ids <- ids
    end

    type 'res t = {
      id : id;
      top_level_branches : (case, 'res Branch.t) Hashtbl.t;
      id_assoc : (id, 'res Branch.t) Hashtbl.t;
    }

    let find_branch map (id, case) =
      if id = map.id then Hashtbl.find map.top_level_branches case
      else Hashtbl.find map.id_assoc id

    let assoc_id (map : 'res t) branch (child_id : id) =
      let () = Branch.add_child branch child_id in
      Hashtbl.replace map.id_assoc child_id branch

    let make (choices : 'res choices) =
      let id = get_id () in
      let top_level_branches = Hashtbl.create 0 in
      let () =
        choices
        |> List.iter @@ fun (case, seq) ->
           let branch : 'res Branch.t =
             { case; cont = fiber (fun _ -> seq ()); child_ids = IdSet.empty }
           in
           Hashtbl.replace top_level_branches case branch
      in
      let map = { id; top_level_branches; id_assoc = Hashtbl.create 0 } in
      map

    let is_empty map =
      Hashtbl.length map.top_level_branches = 0
      && Hashtbl.length map.id_assoc = 0

    let remove_branch map ({ case; child_ids; _ } : 'res Branch.t) =
      Hashtbl.remove map.top_level_branches case;
      child_ids |> IdSet.to_seq |> Seq.iter (Hashtbl.remove map.id_assoc);
      is_empty map
  end

  let rec choose_deep map eff =
    let choice =
      match Effect.perform eff with
      | Some c -> c
      | None -> failwith "Expected a choice when inside a branch, got None!"
    in
    let branch = Map.find_branch map choice in
    let aux = choose_deep map in
    let handle_ret = function
      | Seq.Nil ->
          let is_empty = Map.remove_branch map branch in
          if is_empty then Seq.Nil else aux (Choice None)
      | Seq.Cons _ as cons -> cons
    in
    let handle_effect (type a) (eff : a Effect.t) =
      match eff with
      | Choice o ->
          Option.some @@ fun (k : (a, _) continuation) ->
          let () =
            o |> Option.iter @@ fun (id, _) -> Map.assoc_id map branch id
          in
          let () = branch.cont <- k in
          aux (Choice o)
      | Breakpoint b ->
          Option.some @@ fun (k : (a, _) continuation) ->
          let () = branch.cont <- k in
          aux (Breakpoint b)
      | _ -> None
    in
    let handler = { retc = handle_ret; exnc = raise; effc = handle_effect } in
    continue_with branch.cont (Some choice) handler

  let choose ?(force = false) choices =
    match choices with
    | [] -> fun () -> Seq.Nil
    | [ (_, cont) ] when not force -> cont
    | _ ->
        let map = Map.make choices in
        let cases = List.map fst choices in
        let eff = Choice (Some (map.id, cases)) in
        fun () -> choose_deep map eff

  let choose_const ?force choices =
    choose ?force (List.map (fun (case, x) -> (case, Seq.return x)) choices)

  let breakpoint b =
    let _ = Effect.perform (Breakpoint b) in
    ()

  let to_list seq =
    let open Effect.Deep in
    let stack = ref [] in
    try_with List.of_seq seq
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Breakpoint _ ->
                Some
                  (fun (k : (a, _) continuation) ->
                    let next = List_utils.hd_opt !stack in
                    continue k next)
            | Choice o ->
                Some
                  (fun (k : (a, _) continuation) ->
                    let stack' =
                      match o with
                      | Some (id, cases) ->
                          List.map (fun case -> (id, case)) cases @ !stack
                      | None -> !stack
                    in
                    let next = Some (List.hd stack') in
                    let () = stack := List.tl stack' in
                    continue k next)
            | _ -> None);
      }
end

module Dummy = struct
  include Make (struct
    type case = int
    type breakpoint = unit
  end)

  let choose conts =
    let choices = conts |> List.mapi @@ fun i c -> (i, c) in
    choose choices

  let choose_const vals = choose (List.map Seq.return vals)
end
