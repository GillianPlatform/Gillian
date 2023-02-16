include Loggable_intf

type 'a log_f = Format.formatter -> 'a -> unit

(** Calls the pretty print function of a loggable on its content *)
let pp (loggable : t) (formatter : Format.formatter) =
  match loggable with
  | L (t, content) ->
      let (module T) = t in
      T.pp formatter content

(** Gets logger funcs for HTML output *)
let pp_html (loggable : t) (formatter : Format.formatter) =
  match loggable with
  | L (t, content) ->
      let (module T) = t in
      T.pp_html formatter content

(** Converts a loggable to Yojson *)
let loggable_to_yojson = function
  | L (t, content) ->
      let (module T) = t in
      T.to_yojson content

(** Returns a loggable, given the required functions and content *)
let make
    (type a)
    (pp : a log_f)
    ?(pp_html : a log_f option)
    (of_yojson : Yojson.Safe.t -> (a, string) result)
    (to_yojson : a -> Yojson.Safe.t)
    (content : a) : t =
  let pp_html = Option.value ~default:pp pp_html in
  let module M = struct
    type t = a

    let pp = pp
    let pp_html = pp_html
    let of_yojson = of_yojson
    let to_yojson = to_yojson
  end in
  L ((module M), content)

(** Returns a loggable given a string to be logged *)
let make_string (s : string) : t =
  let pp = Fmt.string in
  let of_yojson yojson =
    match yojson with
    | `String s -> Ok s
    | _ -> Error "Cannot parse yojson to a string"
  in
  let to_yojson s = `String s in
  make pp of_yojson to_yojson s
