module type NameMap = sig
  (** (before * after) list *)
  val action_substitutions : (string * string) list

  (** (before * after) list *)
  val pred_substitutions : (string * string) list
end

let subst (substs : (string * string) list) (s : string) : string =
  match List.find_opt (fun (before, _) -> before = s) substs with
  | Some (_, after) -> after
  | None -> s

let unsubst (substs : (string * string) list) (s : string) : string =
  match List.find_opt (fun (_, after) -> after = s) substs with
  | Some (before, _) -> before
  | None -> s

module Make (Mapper : NameMap) (S : MyMonadicSMemory.S) :
  MyMonadicSMemory.S with type t = S.t = struct
  include S
  open Mapper

  let action_from_str s = subst action_substitutions s |> S.action_from_str
  let action_to_str a = S.action_to_str a |> unsubst action_substitutions
  let pred_from_str s = subst pred_substitutions s |> S.pred_from_str
  let pred_to_str p = S.pred_to_str p |> unsubst pred_substitutions
end
