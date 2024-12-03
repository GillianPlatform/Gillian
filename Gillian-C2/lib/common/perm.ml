type t = Freeable | Writable | Readable | Nonempty [@@deriving yojson]

let to_int = function
  | Freeable -> 4
  | Writable -> 3
  | Readable -> 2
  | Nonempty -> 1

let opt_to_int = function
  | None -> 0
  | Some x -> to_int x

module Infix = struct
  let ( >% ), ( <% ), ( <=% ), ( >=% ), ( =% ) =
    let make op a b = op (to_int a) (to_int b) in
    (make ( > ), make ( < ), make ( <= ), make ( >= ), make ( = ))

  let ( >%? ), ( <%? ), ( <=%? ), ( >=%? ), ( =%? ) =
    let make op a b = op (opt_to_int a) (opt_to_int b) in
    (make ( > ), make ( < ), make ( <= ), make ( >= ), make ( = ))
end

let min pa pb =
  let open Infix in
  if pa <=% pb then pa else pb

let to_string = function
  | Freeable -> "Freeable"
  | Writable -> "Writable"
  | Readable -> "Readable"
  | Nonempty -> "Nonempty"

let pp ft t = Format.fprintf ft "%s" (to_string t)

let opt_to_string = function
  | None -> "None"
  | Some p -> to_string p

let of_string = function
  | "Freeable" -> Freeable
  | "Writable" -> Writable
  | "Readable" -> Readable
  | "Nonempty" -> Nonempty
  | str -> failwith ("Unknown permission : " ^ str)

let opt_of_string = function
  | "None" -> None
  | str -> Some (of_string str)
