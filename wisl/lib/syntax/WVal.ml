type t = Bool of bool | Num of int | Str of string | Null | VList of t list

let rec pp fmt v =
  match v with
  | Bool true  -> Format.fprintf fmt "@[%s@]" "true"
  | Bool false -> Format.fprintf fmt "@[%s@]" "false"
  | Num n      -> Format.fprintf fmt "@[%i@]" n
  | Str s      -> Format.fprintf fmt "@[\"%s\"@]" s
  | Null       -> Format.fprintf fmt "@[%s@]" "null"
  | VList l    ->
      WPrettyUtils.pp_list ~pre:(format_of_string "@[[")
        ~suf:(format_of_string "]@]")
        ~empty:(format_of_string "@[nil@]")
        pp fmt l

let str = Format.asprintf "%a" pp
