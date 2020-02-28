open Format

let pp_list
    ?(pre = format_of_string "@[")
    ?(sep = format_of_string ", ")
    ?(last = sep)
    ?(suf = format_of_string "@]")
    ?(empty = format_of_string "")
    pp_elt
    f
    l =
  let rec aux f = function
    | []         -> assert false
    | [ e ]      -> fprintf f "%a" pp_elt e
    | [ e1; e2 ] -> fprintf f "%a%(%)%a" pp_elt e1 last pp_elt e2
    | e :: l     -> fprintf f "%a%(%)%a" pp_elt e sep aux l
  in
  match l with
  | []          -> Format.fprintf f "%(%)" empty
  | _ :: _ as l -> Format.fprintf f "%(%)%a%(%)" pre aux l suf

let pp_option ?(empty = format_of_string "") pp_elt fmt opt =
  match opt with
  | None     -> Format.fprintf fmt empty
  | Some elt -> pp_elt fmt elt
