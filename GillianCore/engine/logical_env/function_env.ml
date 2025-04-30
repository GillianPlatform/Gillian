type t = (string, Func.t) Hashtbl.t

let function_env : t option ref = ref None
let init tbl = function_env := Some tbl
let is_initialised () = Option.is_some !function_env

let get_function_param_types fname =
  let phi = !function_env in
  let func = Option.map (fun phi -> Hashtbl.find_opt phi fname) phi in
  let param_types =
    Option.map (fun f -> f.Func.func_params) (Option.join func)
  in
  let types = Option.map (List.map snd) param_types in
  types

let get_functions () =
  let phi = !function_env in
  Option.value ~default:[]
  @@ Option.map (fun phi -> List.of_seq (Hashtbl.to_seq_values phi)) phi
