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

let check_spec_pvars
    (allowed_variables : string list) (procedures : (string, t) Hashtbl.t) :
    unit =
  (* Step 1 - Get the specs for each procedure, and add the return and error variables to the list of allowed variables
     * -----------------------------------------------------------------------------------
  *)
  let specs : Spec.t list =
    Hashtbl.fold
      (fun proc_name (proc : t) acc ->
        match proc.spec with
        | None   -> acc
        | Some s ->
            {
              s with
              params = s.params @ [ Names.return_variable ] @ allowed_variables;
            }
            :: acc)
      procedures []
  in

  (* Step 2 - Function to check for any assertion in the spec
     * -----------------------------------------------------------------------------------
  *)
  let check_spec_assertion_pvars
      (spec_name : string)
      (pre : bool) (* true for pre, false for post *)
      (spec_params : string list)
      (previously_normalised : bool)
      (assertion : Asrt.t) : unit =
    let msg_construct_type =
      match pre with
      | true  -> "precondition"
      | false -> "postcondition"
    in

    let _ =
      List.map
        (fun pvar ->
          let valid_pvar = List.mem pvar spec_params in
          match valid_pvar || previously_normalised with
          | true  -> ()
          | false ->
              raise
                (Failure
                   (Printf.sprintf "Undefined variable %s in the %s of %s." pvar
                      msg_construct_type spec_name)))
        (Containers.SS.elements (Asrt.pvars assertion))
    in
    ()
  in

  (* Step 3 - Run this function on the pre and all the post's of every spec
     * -----------------------------------------------------------------------------------
  *)
  let _ =
    List.map
      (fun (spec : Spec.t) ->
        let spec_params = spec.params in
        List.map
          (fun (single_spec : Spec.st) ->
            check_spec_assertion_pvars spec.name true spec_params
              spec.normalised single_spec.pre;
            List.map
              (fun post ->
                check_spec_assertion_pvars spec.name false spec_params
                  spec.normalised post)
              single_spec.posts)
          spec.sspecs)
      specs
  in
  ()

let check_proc_spec_correspondence (procedures : (string, t) Hashtbl.t) : unit =
  Hashtbl.iter
    (fun _ (proc : t) ->
      match proc.spec with
      | None      -> ()
      | Some spec -> (
          (* Check the arguments correspond
             * -----------------------------------------------------------------------------------
          *)
          (match List.length proc.params = List.length spec.params with
          | true  -> ()
          | false ->
              raise
                (Failure
                   (Printf.sprintf
                      "The spec and procedure definitions for %s have \
                       different number of arguments."
                      proc.name)));
          match proc.params = spec.params with
          | true  -> ()
          | false ->
              raise
                (Failure
                   (Printf.sprintf
                      "The spec and procedure definitions for %s have \
                       different arguments."
                      proc.name))))
    procedures
