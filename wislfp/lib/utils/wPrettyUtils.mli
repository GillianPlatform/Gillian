val pp_list :
  ?pre:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  ?sep:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?last:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  ?suf:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  ?empty:('m, 'n, 'o, 'p, 'p, 'm) format6 ->
  (Format.formatter -> 'q -> unit) ->
  Format.formatter ->
  'q list ->
  unit

val to_str : (Format.formatter -> 'a -> unit) -> 'a -> string
