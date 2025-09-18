open Gillian.Concrete
module Literal = Gillian.Gil_syntax.Literal

type t = (string * int, Values.t) Hashtbl.t

let init () = Hashtbl.create 1
let get heap loc offset = Hashtbl.find_opt heap (loc, offset)
let set heap loc offset value = Hashtbl.replace heap (loc, offset) value

let alloc heap size =
  let loc = Gillian.Utils.Generators.fresh_loc () in
  let rec aux current_offset =
    if current_offset < 0 then ()
    else
      let () = Hashtbl.add heap (loc, current_offset) Literal.Null in
      aux (current_offset - 1)
  in
  let () = aux (size - 1) in
  loc

let remove heap loc offset = Hashtbl.remove heap (loc, offset)

let dispose heap loc =
  Hashtbl.filter_map_inplace
    (fun (l, _) v -> if loc = l then None else Some v)
    heap

let copy heap = Hashtbl.copy heap

(* small things useful for printing *)

let rec insert (a, b) l =
  match l with
  | [] -> [ (a, b) ]
  | (x, y) :: r when x < a -> (x, y) :: insert (a, b) r
  | l -> (a, b) :: l

let get_beautiful_list heap =
  let add_cell_list (loc, offset) value lis =
    let rec aux rest =
      match rest with
      | [] -> [ (loc, [ (offset, value) ]) ]
      | (locp, assocs) :: r when String.equal loc locp ->
          (locp, insert (offset, value) assocs) :: r
      | a :: r -> a :: aux r
    in
    aux lis
  in
  Hashtbl.fold add_cell_list heap []

let str heap =
  let vstr v = Format.asprintf "%a" Values.pp v in
  let one_loc_str loc l =
    String.concat "\n"
      (List.map
         (fun (offset, value) ->
           Printf.sprintf "(%s, %i) -> %s" loc offset (vstr value))
         l)
  in
  let bl = get_beautiful_list heap in
  let all_string =
    String.concat "\n\n"
      (List.map (fun (loc, assocs) -> one_loc_str loc assocs) bl)
  in
  Printf.sprintf "{\n%s\n}" all_string
