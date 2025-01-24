
let pointer_width = ref 64

type t =
  | Mint of int
  | Mfloat32
  | Mfloat64
  | Mptr
[@@deriving eq, yojson]

(* Physical equality: values should be decoded the same way *)
let phy_equal a b =
  match (a, b) with
  | Mint w1, Mint w2 -> w1 = w2
  | Mfloat32, Mfloat32 | Mfloat64, Mfloat64 | Mptr, Mptr -> true
  | Mint w, Mptr -> w = !pointer_width
  | Mptr, Mint w -> w = !pointer_width
  | _ -> false

let of_string = function
  | "float32" -> Mfloat32
  | "float64" -> Mfloat64
  | "ptr" -> Mptr
  | x ->
      let lst = String.split_on_char '-' x in
      if List.length lst = 2 && String.equal (List.hd lst) "i" then
        let st = List.nth lst 1 in
        Mint (int_of_string st)
      else failwith ("invalid chunk " ^ x)

let to_string = function
  | Mint i -> "i-" ^ Int.to_string i
  | Mfloat32 -> "float32"
  | Mfloat64 -> "float64"
  | Mptr -> "ptr"


let pp fmt chunk = Fmt.pf fmt "%s" (to_string chunk)
let size = function
  | Mint i -> i / 8
  | Mfloat32 -> 4
  | Mfloat64 -> 8
  | Mptr -> !pointer_width / 8

let size_expr chunk = Gil_syntax.Expr.int (size chunk)

let align chunk =
  (raise (Failure "align not implemented"))

let ptr = Mptr

let could_be_ptr = function
  | Mptr -> true
  | Mint w -> w = !pointer_width
  | _ -> false
