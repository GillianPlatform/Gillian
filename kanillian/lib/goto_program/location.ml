(** This is a much simplified version of CBMC's locations.
    Gillian only handles this, so there's not much more we can do.
    Maybe there is a way to extract a better gillian location from
    CBMC locs, but it's not worth it right now. *)
type t = {
  origin_id : int;
  source : string option;
  line : int option;
  col : int option;
  comment : string option;
}
[@@deriving show { with_path = false }]

(* Right now, this is a bit of a hack, the origin_id should be kept separately for every node.
   That would entail more granularity, but that would also mean modifying the whole AST and that's
   not a priority right now. *)

let make ?source ?line ?col ?comment origin_id =
  { source; line; col; origin_id; comment }

let of_irep (irep : Irep.t) : t =
  let open Irep.Infix in
  match irep with
  | { id = Nil; _ } -> make irep.unique_id
  | { id = EmptyString; _ } -> (
      match irep $? File with
      | None -> make irep.unique_id
      | Some file_rep ->
          let source = Irep.as_just_string file_rep in
          let line = Option.map Irep.as_just_int (irep $? Line) in
          let col = Option.map Irep.as_just_int (irep $? Column) in
          let comment = Option.map Irep.as_just_string (irep $? Comment) in
          make ~source ?line ?col ?comment irep.unique_id)
  | _ -> Gerror.unexpected ~irep "wrong Irep location"

let sloc_in_irep irep =
  let open Irep.Infix in
  irep $? CSourceLocation |> Option.value ~default:Irep.nil |> of_irep
