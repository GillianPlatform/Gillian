(** GIL Procedures *)

(** Labeled procedures. Every command is annotated with a label, and the gotos indicate to which label one should jump.
    Labels can be of any type. However, we say "labeled" when the labels are strings, and "indexed" when the labels are integers.
    Most functions in Gillian that work with indexed procedures assume for efficiency that the label of the i-th command is always Some i
    (starting from 0).
    *)
type ('annot, 'label) t = ('annot, 'label) TypeDef__.proc = {
  proc_name : string;
  proc_source_path : string option;
  proc_body : ('annot * 'label option * 'label Cmd.t) array;
  proc_params : string list;
  proc_spec : Spec.t option;
}

let get_params proc = proc.proc_params

let pp ~(show_labels : bool) ~(pp_label : 'a Fmt.t) fmt labproc =
  let {
    proc_name = name;
    proc_body = body;
    proc_params = params;
    proc_spec = spec;
    _;
  } =
    labproc
  in
  let len_lab l = String.length ((Fmt.to_to_string pp_label) l) in
  let len_opt l = Option.fold ~none:0 ~some:len_lab l in
  let max_size_lab =
    Array.fold_left (fun prev (_, curr, _) -> max (len_opt curr) prev) 0 body
  in
  let pp_white fmt k =
    for _ = 0 to k - 1 do
      Fmt.pf fmt " "
    done
  in
  let pp_cmd_triple fmt (_, lab, cmd) =
    if show_labels then
      match lab with
      | None   ->
          Fmt.pf fmt "%a%a" pp_white (max_size_lab + 2) (Cmd.pp ~pp_label) cmd
      | Some l ->
          Fmt.pf fmt "%a:%a%a" pp_label l pp_white
            (max_size_lab - len_lab l + 1)
            (Cmd.pp ~pp_label) cmd
    else Fmt.pf fmt "%a" (Cmd.pp ~pp_label) cmd
  in
  let pp_spec_opt fmt = function
    | None      -> ()
    | Some spec -> Spec.pp fmt spec
  in
  Fmt.pf fmt "@[%a@\n@[<v 2>proc %s(%a) {@\n%a@\n@]@\n};@\n@]" pp_spec_opt spec
    name
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    params
    (Fmt.array ~sep:(Fmt.any ";@\n") pp_cmd_triple)
    body

let pp_labeled fmt c = pp ~show_labels:true ~pp_label:Fmt.string fmt c

let pp_indexed fmt c = pp ~show_labels:false ~pp_label:Fmt.int fmt c

let line_info (proc : (Annot.t, 'a) t) : (string * int * int) list =
  let pname = proc.proc_name in
  List.mapi
    (fun i (annot, _, _) ->
      match Annot.get_line_offset annot with
      | None   -> (pname, i, -1)
      | Some n -> (pname, i, n))
    (Array.to_list proc.proc_body)

let indexed_of_labeled (lproc : (Annot.t, string) t) : (Annot.t, int) t =
  let no_of_cmds = Array.length lproc.proc_body in

  (* First create a mapping Hashtabl lab -> line *)
  let map_labels_to_numbers =
    let mapping = Hashtbl.create no_of_cmds in
    for i = 0 to no_of_cmds - 1 do
      match lproc.proc_body.(i) with
      | _, Some str, _ -> Hashtbl.add mapping str i
      | _              -> ()
    done;
    mapping
  in

  let find_with_error hash lab =
    match Hashtbl.find_opt hash lab with
    | Some result -> result
    | None        -> raise (Failure ("Could not find label: " ^ lab))
  in

  (* Replace labels with numbers in the procedure commands *)
  let convert_to_indexed mapping =
    let cmds_nolab =
      Array.mapi
        (fun i x ->
          match x with
          | annot, _, cmd -> (annot, Some i, cmd))
        lproc.proc_body
    in
    let cmds =
      Array.map
        (function
          | spec, l, labeled_cmd ->
              let indexed_cmd : int Cmd.t =
                match (labeled_cmd : string Cmd.t) with
                | Skip                       -> Cmd.Skip
                | Assignment (x, e)          -> Cmd.Assignment (x, e)
                | LAction (x, la_name, es)   -> Cmd.LAction (x, la_name, es)
                | Goto lab                   -> Cmd.Goto
                                                  (find_with_error mapping lab)
                | GuardedGoto (e, lt, lf)    ->
                    Cmd.GuardedGoto
                      (e, find_with_error mapping lt, find_with_error mapping lf)
                | Call (x, e, le, ol, subst) ->
                    Cmd.Call
                      ( x,
                        e,
                        le,
                        ( match ol with
                        | None     -> None
                        | Some lab -> Some (find_with_error mapping lab) ),
                        subst )
                | ECall (x, e, le, ol)       ->
                    Cmd.ECall
                      ( x,
                        e,
                        le,
                        match ol with
                        | None     -> None
                        | Some lab -> Some (find_with_error mapping lab) )
                | Apply (x, le, ol)          ->
                    Cmd.Apply
                      ( x,
                        le,
                        match ol with
                        | None     -> None
                        | Some lab -> Some (find_with_error mapping lab) )
                | Arguments var              -> Cmd.Arguments var
                | PhiAssignment xargs        -> Cmd.PhiAssignment xargs
                | ReturnNormal               -> Cmd.ReturnNormal
                | ReturnError                -> Cmd.ReturnError
                | Fail (et, es)              -> Cmd.Fail (et, es)
                | Logic lcmd                 -> Cmd.Logic lcmd
              in
              (spec, l, indexed_cmd))
        cmds_nolab
    in
    cmds
  in
  let mapping = map_labels_to_numbers in
  let b = convert_to_indexed mapping in
  let proc : (Annot.t, int) t = { lproc with proc_body = b } in
  proc

let check_proc_spec_correspondence
    (procedures : (string, (Annot.t, 'a) t) Hashtbl.t) : unit =
  Hashtbl.iter
    (fun _ (proc : (Annot.t, 'a) t) ->
      match proc.proc_spec with
      | None      -> ()
      | Some spec -> (
          (* Check the arguments correspond
           * -----------------------------------------------------------------------------------
           *)
          match proc.proc_params = spec.spec_params with
          | true  -> ()
          | false ->
              raise
                (Failure
                   (Fmt.str
                      "The spec and procedure definitions for %s have \
                       different arguments.\n\
                       @[<h>%a@] in procedure, @[<h>%a@] in params"
                      proc.proc_name (Fmt.Dump.list Fmt.string) proc.proc_params
                      (Fmt.Dump.list Fmt.string) spec.spec_params)) ))
    procedures
