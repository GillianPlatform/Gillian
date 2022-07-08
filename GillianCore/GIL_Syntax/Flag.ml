(** {b Return flags for GIL specifications}. *)
type t = TypeDef__.flag =
  | Normal  (** Normal return *)
  | Error  (** Error return *)
[@@deriving yojson, show]

(** GIL spec return flag *)
let str (flag : t) : string =
  match flag with
  | Normal -> "normal"
  | Error -> "error"

let pp = Fmt.of_to_string str
let compare = Stdlib.compare

module MyFlag = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyFlag)
