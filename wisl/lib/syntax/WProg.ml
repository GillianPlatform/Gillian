open VisitorUtils

type t = {
  context : WProc.t list;
  predicates : WPred.t list;
  lemmas : WLemma.t list;
  datatypes : WDatatype.t list;
  functions : WFunc.t list;
}

let get_context p = p.context

let pp_context =
  WPrettyUtils.pp_list ~sep:(format_of_string "@,@,")
    ~suf:(format_of_string "@]@.") WProc.pp

let pp fmt = function
  | prog -> Format.fprintf fmt "%a" pp_context prog.context

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let never_called_during_symb prog =
  let pmap =
    List.fold_left
      (fun map p -> StringMap.add (WProc.get_name p) p map)
      StringMap.empty prog.context
  in
  let allp = StringSet.of_list (List.map WProc.get_name prog.context) in
  let has_spec pname =
    let p = StringMap.find pname pmap in
    WProc.has_spec p
  in
  let have_spec = StringSet.filter has_spec allp in
  let rec find_fixed_point compare f a =
    let b = f a in
    if compare a b = 0 then b else find_fixed_point compare f b
  in
  let fold_proc pname set =
    let p = StringMap.find_opt pname pmap in
    StringSet.union set
      (StringSet.of_list (Option.fold ~some:WProc.procs_called ~none:[] p))
  in
  let step set = StringSet.fold fold_proc set set in
  let called = find_fixed_point StringSet.compare step have_spec in
  let not_called_names = StringSet.diff allp called in
  let not_called =
    List.map
      (fun x -> StringMap.find x pmap)
      (StringSet.elements not_called_names)
  in
  not_called

let get_pred prog name =
  let rec aux = function
    | [] -> None
    | p :: r -> if String.equal (WPred.get_name p) name then Some p else aux r
  in
  aux prog.predicates

let get_proc prog name =
  let rec aux = function
    | [] -> None
    | p :: r -> if String.equal (WProc.get_name p) name then Some p else aux r
  in
  aux prog.context

let get_by_id ?(proc_name = None) prog id =
  match id with
  | None -> `None
  | Some id -> (
      let aux_proc = list_visitor_builder WProc.get_by_id id in
      let aux_pred = list_visitor_builder WPred.get_by_id id in
      let aux_lemma = list_visitor_builder WLemma.get_by_id id in
      let proc_getter = WProc.get_by_id id in
      match proc_name with
      | None ->
          aux_proc prog.context
          |>> (aux_pred, prog.predicates)
          |>> (aux_lemma, prog.lemmas)
      | Some p -> (
          match List.find_opt (fun pp -> pp.WProc.name = p) prog.context with
          | None -> `None
          | Some ff -> proc_getter ff))

let get_proc_name_of_element prog id =
  let is_in_proc p =
    match WProc.get_by_id id p with
    | `None -> false
    | _ -> true
  in
  let rec find_p l =
    match l with
    | p :: r -> if is_in_proc p then WProc.get_name p else find_p r
    | _ -> ""
  in
  find_p prog.context
