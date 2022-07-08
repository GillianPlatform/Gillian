open VisitorUtils

type t = {
  context : WFun.t list;
  predicates : WPred.t list;
  lemmas : WLemma.t list;
}

let get_context p = p.context

let pp_context =
  WPrettyUtils.pp_list ~sep:(format_of_string "@,@,")
    ~suf:(format_of_string "@]@.") WFun.pp

let pp fmt = function
  | prog -> Format.fprintf fmt "%a" pp_context prog.context

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let never_called_during_symb prog =
  let fmap =
    List.fold_left
      (fun map f -> StringMap.add (WFun.get_name f) f map)
      StringMap.empty prog.context
  in
  let allf = StringSet.of_list (List.map WFun.get_name prog.context) in
  let has_spec fname =
    let f = StringMap.find fname fmap in
    WFun.has_spec f
  in
  let have_spec = StringSet.filter has_spec allf in
  let rec find_fixed_point compare f a =
    let b = f a in
    if compare a b = 0 then b else find_fixed_point compare f b
  in
  let fold_fun fname set =
    let f = StringMap.find_opt fname fmap in
    StringSet.union set
      (StringSet.of_list (Option.fold ~some:WFun.functions_called ~none:[] f))
  in
  let step set = StringSet.fold fold_fun set set in
  let called = find_fixed_point StringSet.compare step have_spec in
  let not_called_names = StringSet.diff allf called in
  let not_called =
    List.map
      (fun x -> StringMap.find x fmap)
      (StringSet.elements not_called_names)
  in
  not_called

let get_pred prog name =
  let rec aux = function
    | [] -> None
    | p :: r -> if String.equal (WPred.get_name p) name then Some p else aux r
  in
  aux prog.predicates

let get_fun prog name =
  let rec aux = function
    | [] -> None
    | p :: r -> if String.equal (WFun.get_name p) name then Some p else aux r
  in
  aux prog.context

let get_by_id ?(fname = None) prog id =
  match id with
  | None -> `None
  | Some id -> (
      let aux_f = list_visitor_builder WFun.get_by_id id in
      let aux_p = list_visitor_builder WPred.get_by_id id in
      let aux_l = list_visitor_builder WLemma.get_by_id id in
      let fun_getter = WFun.get_by_id id in
      match fname with
      | None ->
          aux_f prog.context |>> (aux_p, prog.predicates)
          |>> (aux_l, prog.lemmas)
      | Some f -> (
          match List.find_opt (fun ff -> ff.WFun.name = f) prog.context with
          | None -> `None
          | Some ff -> fun_getter ff))

let get_function_name_of_element prog id =
  let is_in_function f =
    match WFun.get_by_id id f with
    | `None -> false
    | _ -> true
  in
  let rec find_f l =
    match l with
    | f :: r -> if is_in_function f then WFun.get_name f else find_f r
    | _ -> ""
  in
  find_f prog.context
