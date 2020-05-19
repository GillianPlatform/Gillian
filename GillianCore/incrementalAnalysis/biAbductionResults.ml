type t = (string, Spec.t) Hashtbl.t

let make () : t = Hashtbl.create Config.small_tbl_size

let reset : t -> unit = Hashtbl.reset

let set_spec results proc_name spec = Hashtbl.replace results proc_name spec

let get_all_specs ?(filter = fun _ -> true) results =
  Hashtbl.fold
    (fun name spec acc -> if filter name then spec :: acc else acc)
    results []

let remove results proc_name = Hashtbl.remove results proc_name

let prune results proc_names = List.iter (remove results) proc_names

let merge results other_results =
  let () =
    Hashtbl.iter
      (fun proc_name spec -> set_spec results proc_name spec)
      other_results
  in
  results

let to_yojson results =
  `List
    (Hashtbl.fold
       (fun _ (spec : Spec.t) acc -> Spec.to_yojson spec :: acc)
       results [])

let of_yojson_exn json =
  let open Yojson.Safe.Util in
  let results = make () in
  let () =
    List.iter
      (fun json ->
        let name = to_string (List.assoc "spec_name" (to_assoc json)) in
        let spec = Spec.of_yojson_exn json in
        set_spec results name spec)
      (to_list json)
  in
  results
