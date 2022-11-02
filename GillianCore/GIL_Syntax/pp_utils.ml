(** If the string is an indentifier, don't change it, otherwiser quote it. *)
let maybe_quote_ident str =
  let matches = Str.string_match (Str.regexp "[A-Za-z][A-Za-z0-9_]*") str 0 in
  if matches && Str.match_end () == String.length str then str
  else "\"" ^ str ^ "\""
