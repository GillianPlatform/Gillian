type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t

type t = (string * int, bool) hashtbl

let of_yojson yj =
  let ( >| ) o f = Result.map f o in
  yj |> [%of_yojson: ((string * int) * bool) list] >| List.to_seq
  >| Hashtbl.of_seq

let to_yojson s =
  s |> Hashtbl.to_seq |> List.of_seq
  |> [%to_yojson: ((string * int) * bool) list]

let make () : t = Hashtbl.create Config.small_tbl_size

let reset = Hashtbl.reset

let set_result results test_name test_id verified =
  Hashtbl.replace results (test_name, test_id) verified

let remove results test_name =
  Hashtbl.filter_map_inplace
    (fun (name, _) verified ->
      if not (String.equal name test_name) then Some verified else None)
    results

let prune results test_names = List.iter (remove results) test_names

let merge results other_results =
  let () =
    Hashtbl.iter
      (fun id verified -> Hashtbl.replace results id verified)
      other_results
  in
  results

let check_previously_verified ?(printer = fun _ -> ()) results cur_verified =
  Hashtbl.fold
    (fun (name, _) verified acc ->
      if not (Containers.SS.mem name cur_verified) then (
        let msg = "Reading one previous result for " ^ name ^ "... " in
        Logging.tmi (fun fmt -> fmt "%s" msg);
        Fmt.pr "%s" msg;
        printer verified;
        verified && acc)
      else true && acc)
    results true
