(** {b GIL annot}. *)
type t = {
  line_offset : int option;  (** Better not to know what this is for *)
  origin_id : int;  (** Origin Id, that should be abstracted away *)
  loop_info : string list;
}

(**/**)

let init ?(line_offset = None) ?(origin_id = -1) ?(loop_info = []) () =
  { line_offset; origin_id; loop_info }

let get_loop_info (annot : t) = annot.loop_info

let set_loop_info (annot : t) (loop_info : string list) =
  { annot with loop_info }

let get_line_offset annot = annot.line_offset

let line_info_to_str (line_info : (string * int * int) list) : string =
  let strs =
    List.map
      (fun (pname, i, j) -> Printf.sprintf "(%s, %i, %i)" pname i j)
      line_info
  in
  String.concat "\n" strs
