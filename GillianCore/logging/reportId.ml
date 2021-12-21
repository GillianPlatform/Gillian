type t = int64

let next =
  let current = ref Int64.zero in
  fun () ->
    let c = !current in
    current := Int64.succ !current;
    c

let equal = Int64.equal

let pp = Fmt.int64
