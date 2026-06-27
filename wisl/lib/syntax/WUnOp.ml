type t = NOT | LEN | REV | HEAD | TAIL

let is_logic_only u =
  match u with
  | HEAD | TAIL | LEN -> true
  | _ -> false

let pp fmt u =
  let s = Format.fprintf fmt "@[%s@]" in
  match u with
  | NOT -> s "!"
  | HEAD -> s "hd@ "
  | TAIL -> s "tl@ "
  | LEN -> s "len@ "
  | REV -> s "rev@ "

let str = Format.asprintf "%a" pp
