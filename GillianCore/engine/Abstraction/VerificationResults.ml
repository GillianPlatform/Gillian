module Json = Yojson.Basic
module Json_utils = Yojson.Basic.Util

type t = (string * int, bool) Hashtbl.t

let make () : t = Hashtbl.create Config.small_tbl_size

let set_result results proc_name spec_id verified =
  Hashtbl.replace results (proc_name, spec_id) verified

let merge_results results other_results =
  let () =
    Hashtbl.iter
      (fun id verified -> Hashtbl.replace results id verified)
      other_results
  in
  results

let to_json results =
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

let from_json json =
  let results = make () in
  let () =
    List.iter
      (fun json ->
        let result = Json_utils.to_assoc json in
        let proc = Json_utils.to_string (List.assoc "proc_name" result) in
        let spec_id = Json_utils.to_int (List.assoc "spec_id" result) in
        let verified = Json_utils.to_bool (List.assoc "verified" result) in
        set_result results proc spec_id verified)
      (Json_utils.to_list json)
  in
  results
