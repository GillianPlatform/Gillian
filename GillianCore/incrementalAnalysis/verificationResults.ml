type t = (string * int, bool) Hashtbl.t

let make () : t = Hashtbl.create Config.small_tbl_size

let reset : t -> unit = Hashtbl.reset

let set_result results proc_name spec_id verified =
  Hashtbl.replace results (proc_name, spec_id) verified

let remove results proc_name =
  Hashtbl.filter_map_inplace
    (fun (pname, _) verified ->
      if not (String.equal pname proc_name) then Some verified else None)
    results

let prune results proc_names = List.iter (remove results) proc_names

let merge results other_results =
  let () =
    Hashtbl.iter
      (fun id verified -> Hashtbl.replace results id verified)
      other_results
  in
  results

let check_previously_verified ?(printer = fun _ -> ()) results cur_verified =
  Hashtbl.fold
    (fun (pname, _) verified acc ->
      if not (Containers.SS.mem pname cur_verified) then (
        let msg = "Reading one previous spec of procedure " ^ pname ^ "... " in
        Logging.tmi (fun fmt -> fmt "%s" msg);
        Fmt.pr "%s" msg;
        printer verified;
        verified && acc )
      else true && acc)
    results true

let to_yojson results =
  `List
    (Hashtbl.fold
       (fun (name, id) verified acc ->
         let fields =
           [
             ("proc_name", `String name);
             ("spec_id", `Int id);
             ("verified", `Bool verified);
           ]
         in
         `Assoc fields :: acc)
       results [])

let of_yojson_exn json =
  let open Yojson.Safe.Util in
  let results = make () in
  let () =
    List.iter
      (fun json ->
        let result = to_assoc json in
        let proc = to_string (List.assoc "proc_name" result) in
        let spec_id = to_int (List.assoc "spec_id" result) in
        let verified = to_bool (List.assoc "verified" result) in
        set_result results proc spec_id verified)
      (to_list json)
  in
  results
