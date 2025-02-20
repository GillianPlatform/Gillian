type t = int64 [@@deriving yojson]

let next =
  let current = ref Int64.zero in
  fun () ->
    let c = !current in
    current := Int64.succ !current;
    c

let equal = Int64.equal
let pp = Fmt.int64
let of_string_opt = Int64.of_string_opt
