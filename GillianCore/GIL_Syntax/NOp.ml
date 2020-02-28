(* GIL n-ary Operators *)

type t = TypeDef__.nop =
  (* List concatenation *)
  | LstCat
  (* Set management *)
  | SetUnion
  | SetInter

let str (x : t) =
  match x with
  | LstCat   -> "l+"
  | SetUnion -> "-u-"
  | SetInter -> "-i-"
