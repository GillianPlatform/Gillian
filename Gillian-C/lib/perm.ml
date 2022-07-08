type t = Compcert.Memtype.permission =
  | Freeable
  | Writable
  | Readable
  | Nonempty
[@@deriving yojson]

let to_int = function
  | Freeable -> 4
  | Writable -> 3
  | Readable -> 2
  | Nonempty -> 1

let opt_to_int = function
  | None -> 0
  | Some x -> to_int x

let pp fmt = function
  | Freeable -> Fmt.pf fmt "Freeable"
  | Writable -> Fmt.pf fmt "Writable"
  | Readable -> Fmt.pf fmt "Readable"
  | Nonempty -> Fmt.pf fmt "Nonempty"

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
