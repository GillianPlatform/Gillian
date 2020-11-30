module Annot = Gillian.Gil_syntax.Annot
module Flag = Gillian.Gil_syntax.Flag

(* JSIL procedures extended with string labels *)
type t = {
  name : string;
  body : (Annot.t * string option * LabCmd.t) array;
  params : string list;
  spec : Spec.t option;
}

let pp fmt labproc =
  let { name; body; params; spec } = labproc in
  let len_opt = function
    | None   -> 0
    | Some s -> String.length s
  in
  let max_size_lab =
    Array.fold_left (fun prev (_, curr, _) -> max (len_opt curr) prev) 0 body
  in
  let pp_white fmt k =
    for i = 0 to k - 1 do
      Fmt.pf fmt " "
    done
  in
  let pp_cmd_triple fmt (_, lab, cmd) =
    match lab with
    | None   -> Fmt.pf fmt "%a%a" pp_white (max_size_lab + 1) LabCmd.pp cmd
    | Some l ->
        Fmt.pf fmt "%s:%a%a" l pp_white
          (max_size_lab - String.length l)
          LabCmd.pp cmd
  in
  Fmt.pf fmt "@[%a@\n@[<v 2>proc %s(%a) {@\n%a@\n@]@\n};@\n@]"
    Fmt.(option Spec.pp)
    spec name
    Fmt.(list ~sep:(Fmt.any ", ") Fmt.string)
    params
    Fmt.(array ~sep:(any ";@\n") pp_cmd_triple)
    body
