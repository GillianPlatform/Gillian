open Containers

type t = TypeDef__.pred = {
  pred_name : string;  (** Name of the predicate  *)
  pred_source_path : string option;
  pred_internal : bool;
  pred_num_params : int;  (** Number of parameters   *)
  pred_params : (string * Type.t option) list;  (** Actual parameters      *)
  pred_ins : int list;  (** Ins                    *)
  pred_definitions : ((string * string list) option * Asrt.t) list;
      (** Predicate definitions  *)
  pred_pure : bool;  (** Is the predicate pure  *)
  pred_normalised : bool;  (** If the predicate has been previously normalised *)
}

(** Creates/populates a Hashtbl from the predicate list pred_defs *)
let init (preds : t list) : (string, t) Hashtbl.t =
  let pred_def_tbl = Hashtbl.create Config.small_tbl_size in
  List.iter
    (fun pred_def -> Hashtbl.add pred_def_tbl pred_def.pred_name pred_def)
    preds;
  pred_def_tbl

let ins_and_outs (pred : t) : SI.t * SI.t =
  let ins_set = SI.of_list pred.pred_ins in
  let _, outs =
    List.fold_left
      (fun (i, lst) (_, _) ->
        if SI.mem i ins_set then (i + 1, lst) else (i + 1, i :: lst))
      (0, []) pred.pred_params
  in
  let outs_set = SI.of_list outs in
  (ins_set, outs_set)

let in_params (pred : t) : string list =
  let ins_set = SI.of_list pred.pred_ins in
  let _, ins =
    List.fold_left
      (fun (i, ins) (x, _) ->
        if SI.mem i ins_set then (i + 1, x :: ins) else (i + 1, ins))
      (0, []) pred.pred_params
  in
  List.rev ins

let in_args (pred : t) (args : 'a list) : 'a list =
  let ins_set = SI.of_list pred.pred_ins in
  let _, in_args =
    List.fold_left
      (fun (i, ins) x ->
        if SI.mem i ins_set then (i + 1, x :: ins) else (i + 1, ins))
      (0, []) args
  in
  List.rev in_args

let out_params (pred : t) : string list =
  let ins_set = SI.of_list pred.pred_ins in
  let _, outs =
    List.fold_left
      (fun (i, outs) (x, _) ->
        if SI.mem i ins_set then (i + 1, outs) else (i + 1, x :: outs))
      (0, []) pred.pred_params
  in
  List.rev outs

let out_args (pred : t) (args : 'a list) : 'a list =
  let ins_set = SI.of_list pred.pred_ins in
  let _, out_args =
    List.fold_left
      (fun (i, outs) x ->
        if SI.mem i ins_set then (i + 1, outs) else (i + 1, x :: outs))
      (0, []) args
  in
  List.rev out_args

let pp fmt pred =
  let show_ins = List.length pred.pred_ins != List.length pred.pred_params in
  let pp_param fmt' (i, (p, topt)) =
    let pp_t fmt'' t = Fmt.pf fmt'' " : %s" (Type.str t) in
    let () = if show_ins && List.mem i pred.pred_ins then Fmt.string fmt' "+" in
    Fmt.pf fmt' "%s%a" p (Fmt.option pp_t) topt
  in
  let pp_params = Fmt.iter_bindings ~sep:Fmt.comma List.iteri pp_param in
  let pp_id_exs fmt' (id, exs) =
    if List.length exs > 0 then
      Fmt.pf fmt' "[%s: %a] " id (Fmt.list ~sep:(Fmt.any ", ") Fmt.string) exs
    else Fmt.pf fmt' "[%s] " id
  in
  let pp_def fmt' (id_exs, asser) =
    Fmt.pf fmt' "%a%a" (Fmt.option pp_id_exs) id_exs Asrt.pp asser
  in
  let pp_path_opt fmt = function
    | None   -> Fmt.pf fmt "@nopath@\n"
    | Some _ -> ()
  in
  let pp_internal fmt = function
    | true  -> Fmt.pf fmt "@internal@\n"
    | false -> ()
  in
  Fmt.pf fmt "%a%a@[<hov 2>@[<h>pred %s(%a) :@]@\n%a;@]@\n" pp_path_opt
    pred.pred_source_path pp_internal pred.pred_internal pred.pred_name
    pp_params pred.pred_params
    Fmt.(list ~sep:(any ",@\n") (hovbox ~indent:2 pp_def))
    pred.pred_definitions

let check_pvars (predicates : (string, t) Hashtbl.t) : unit =
  let check_pred_pvars (pred_name : string) (predicate : t) : unit =
    (* Step 1 - Extract all the program variables used in the definition
       * -----------------------------------------------------------------------------------
    *)
    let all_pred_pvars : string list =
      List.concat
        (List.map
           (fun (_, ass) -> SS.elements (Asrt.pvars ass))
           predicate.pred_definitions)
    in

    (* Step 2 - Check all predicates
       * -----------------------------------------------------------------------------------
    *)
    let string_of_params =
      List.map (fun (pvar, _) -> pvar) predicate.pred_params
    in
    let _ =
      List.map
        (fun (pvar : string) ->
          let valid_pvar = List.mem pvar string_of_params in
          match valid_pvar || predicate.pred_normalised with
          | true  -> ()
          | false ->
              raise
                (Failure
                   (Printf.sprintf
                      "Undefined variable %s in the definition of predicate %s."
                      pvar pred_name)))
        all_pred_pvars
    in
    ()
  in

  Hashtbl.iter check_pred_pvars predicates

(**
   GIL Predicates can have non-pvar parameters - to say that a given parameter
   always has a certain value...
  *)
let explicit_param_types (preds : (string, t) Hashtbl.t) (pred : t) : t =
  let pt_asrt (a : Asrt.t) : Asrt.t =
    let f_a_after a : Asrt.t =
      match (a : Asrt.t) with
      | Pred (name, les) ->
          let pred =
            try Hashtbl.find preds name
            with _ ->
              raise
                (Failure
                   ( "DEATH. parameter_types: predicate " ^ name
                   ^ " does not exist." ))
          in
          let ac_types =
            List.fold_left
              (fun ac_types ((_, t_x), le) ->
                match t_x with
                | None     -> ac_types
                | Some t_x -> (le, t_x) :: ac_types)
              []
              (List.combine pred.pred_params les)
          in
          Star (Types ac_types, a)
      | _                -> a
    in
    Asrt.map None (Some f_a_after) None None a
  in

  let new_asrts =
    List.fold_right
      (fun (x, t_x) new_asrts ->
        match t_x with
        | None     -> new_asrts
        | Some t_x -> Asrt.Types [ (PVar x, t_x) ] :: new_asrts)
      pred.pred_params []
  in
  let new_defs =
    List.map
      (fun (oid, a) -> (oid, Asrt.star (a :: new_asrts)))
      pred.pred_definitions
  in
  let new_defs = List.map (fun (oid, a) -> (oid, pt_asrt a)) new_defs in
  { pred with pred_definitions = new_defs }

let combine_ins_outs (pred : t) (ins : 'a list) (outs : 'a list) : 'a list =
  let in_indexes = SI.of_list pred.pred_ins in
  let max_index = List.length pred.pred_params in

  let rec loop ins outs all cur_index =
    if cur_index = max_index then all
    else if SI.mem cur_index in_indexes then
      match ins with
      | []       -> raise (Failure "DEATH. combine_ins_outs")
      | hd :: tl -> loop tl outs (hd :: all) (cur_index + 1)
    else
      match outs with
      | []       -> raise (Failure "DEATH. combine_ins_outs")
      | hd :: tl -> loop ins tl (hd :: all) (cur_index + 1)
  in
  List.rev (loop ins outs [] 0)

let iter_ins_outs
    (pred : t)
    (fins : 'a -> unit)
    (fouts : 'b -> unit)
    ((ins, outs) : 'a list * 'b list) : unit =
  let in_indexes = SI.of_list pred.pred_ins in
  let max_index = List.length pred.pred_params in
  let rec loop ins outs cur_index =
    if cur_index = max_index then ()
    else if SI.mem cur_index in_indexes then (
      match ins with
      | []       -> raise (Failure "DEATH. iter_ins_outs")
      | hd :: tl ->
          fins hd;
          loop tl outs (cur_index + 1) )
    else
      match outs with
      | []       -> raise (Failure "DEATH. iter_ins_outs")
      | hd :: tl ->
          fouts hd;
          loop ins tl (cur_index + 1)
  in
  loop ins outs 0

let pp_ins_outs (pred : t) pp_in pp_out =
  let pp_iter2 iter pp_a pp_b ppf v =
    let chpp pp ppff v =
      let is_first = ref true in
      if !is_first then is_first := false else Fmt.comma ppff ();
      pp ppff v
    in
    let pp_a, pp_b = (chpp pp_a ppf, chpp pp_b ppf) in
    iter pp_a pp_b v
  in
  pp_iter2 (iter_ins_outs pred) pp_in pp_out

let empty_pred_tbl () = Hashtbl.create Config.small_tbl_size

let get (pred_defs : (string, t) Hashtbl.t) (name : string) : t =
  try Hashtbl.find pred_defs name
  with _ -> raise (Failure "DEATH. PRED NOT FOUND!")
