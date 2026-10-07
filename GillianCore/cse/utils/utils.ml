let string_to_char_list s = List.init (String.length s) (String.get s)

let string_from_char_list s =
  let b = Buffer.create (List.length s) in
  List.iter (Buffer.add_char b) s;
  Buffer.contents b

let string_from_index (i : Extracted.index) =
  match i with
  | IdxNum n -> string_of_int n
  | IdxSym s -> string_from_char_list s

let string_from_identifier (id : Extracted.identifier) =
  match id with
  | IdSimple s -> string_from_char_list s
  | IdIndexed (s, idx) ->
      let head = string_from_char_list s in
      String.concat " " (head :: List.map string_from_index idx)

let sexp_of_index i =
  Sexplib.Sexp.Atom (string_from_index i)

(* Numeric literals reach us as prefixed function symbols: the encoder names
   them [int_literal_5] and [decimal_literal_3/2] so that the metatheory can
   tell a literal apart from the other function symbols by its name alone
   (see [f_int_literal] and [f_decimal_literal] in Theory/Reals_Ints.v).
   SMT-LIB has no such symbols -- the solver wants the literal itself -- so
   naming and printing part ways here, exactly as they already do for string
   literals, whose symbol is the quoted SMT text. *)

let int_literal_prefix = "int_literal_"
let decimal_literal_prefix = "decimal_literal_"

let chop_prefix prefix s =
  let n = String.length prefix in
  if String.length s >= n && String.sub s 0 n = prefix then
    Some (String.sub s n (String.length s - n))
  else None

(* An SMT-LIB numeral is non-negative, so a negative integer is the negation
   of one. *)
let sexp_of_numeral text =
  if text = "" then None
  else if text.[0] = '-' then
    Some
      (Sexplib.Sexp.List
         [ Sexplib.Sexp.Atom "-";
           Sexplib.Sexp.Atom (String.sub text 1 (String.length text - 1)) ])
  else Some (Sexplib.Sexp.Atom text)

(* A decimal is written with a point, so that it is a real rather than an
   integer wherever it appears.  [decimal_literal_<num>/<den>] denotes the
   exact rational, which the quotient represents for every rational -- a
   decimal expansion would not, having none for 1/3. *)
let sexp_of_decimal text =
  let decimal_point text =
    Option.map
      (fun s -> Sexplib.Sexp.Atom (s ^ ".0"))
      (if text = "" then None else Some text)
  in
  let real_of_numeral text =
    if text <> "" && text.[0] = '-' then
      Option.map
        (fun a -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "-"; a ])
        (decimal_point (String.sub text 1 (String.length text - 1)))
    else decimal_point text
  in
  match String.index_opt text '/' with
  | None -> None
  | Some i -> (
      let num = String.sub text 0 i
      and den = String.sub text (i + 1) (String.length text - i - 1) in
      match (real_of_numeral num, real_of_numeral den) with
      | Some num', Some den' ->
          if den = "1" then Some num'
          else Some (Sexplib.Sexp.List [ Sexplib.Sexp.Atom "/"; num'; den' ])
      | _ -> None)

let sexp_of_literal_symbol s =
  match chop_prefix int_literal_prefix s with
  | Some text -> sexp_of_numeral text
  | None -> (
      match chop_prefix decimal_literal_prefix s with
      | Some text -> sexp_of_decimal text
      | None -> None)

let sexp_of_identifier (id : Extracted.identifier) =
  match id with
  | IdSimple s -> (
      let s = string_from_char_list s in
      match sexp_of_literal_symbol s with
      | Some literal -> literal
      | None -> Sexplib.Sexp.Atom s)
  | IdIndexed (s, idx) ->
      Sexplib.Sexp.List
        (Sexplib.Sexp.Atom "_"
         :: Sexplib.Sexp.Atom (string_from_char_list s)
         :: List.map sexp_of_index idx)

let list_to_gmap_string lst =
  let lst' = List.map (fun (k, v) -> (string_to_char_list k, v)) lst in
  Extracted.list_to_map
    (Extracted.map_insert (Extracted.gmap_partial_alter Extracted.String.eq_dec Extracted.String.countable))
    (Extracted.gmap_empty Extracted.String.eq_dec Extracted.String.countable)
    lst'

let hashtbl_to_gmap_string tbl =
  let lst = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  list_to_gmap_string lst
