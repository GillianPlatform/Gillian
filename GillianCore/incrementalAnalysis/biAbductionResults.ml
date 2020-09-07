type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t

type t = (string, Spec.t) hashtbl [@@deriving yojson]

let make () : t = Hashtbl.create Config.small_tbl_size

let reset = Hashtbl.reset

let set_spec = Hashtbl.replace

let contains_spec = Hashtbl.mem

let get_spec_exn = Hashtbl.find

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
