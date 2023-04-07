(** Utility functions for floating point arithmetic *)

(** Checks if a float is an integer
    
  Note that [-0] is {b not} considered an integer *)
let is_int (f : float) : bool =
  let f' = float_of_int (int_of_float f) in
  f = f' && copysign 1.0 f = copysign 1.0 f'

(** Checks if a flot is not NaN or infinite *)
let is_normal (f : float) =
  let fc = Float.classify_float f in
  not (fc = FP_infinite || fc = FP_nan)

(** Rounds a float towards 0
    
  Returns 0 if NaN, and unchanged if infinite*)
let to_int n =
  match classify_float n with
  | FP_nan -> 0.
  | FP_infinite -> n
  | FP_zero -> n
  | FP_normal | FP_subnormal ->
      (if n < 0. then -1. else 1.) *. floor (abs_float n)

(** Same as {!to_int}, but overflows as if it's a signed 32-bit int *)
let to_int32 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i32 = 2. ** 32. in
      let i31 = 2. ** 31. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int32bit =
        let smod = mod_float posint i32 in
        if smod < 0. then smod +. i32 else smod
      in
      if int32bit >= i31 then int32bit -. i32 else int32bit
  | _ -> 0.

(** Same as {!to_int32}, but for unsigned 32-bit ints *)
let to_uint32 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i32 = 2. ** 32. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int32bit =
        let smod = mod_float posint i32 in
        if smod < 0. then smod +. i32 else smod
      in
      int32bit
  | _ -> 0.

(** Same as {!to_uint32}, but for unsigned 16-bit ints *)
let to_uint16 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i16 = 2. ** 16. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int16bit =
        let smod = mod_float posint i16 in
        if smod < 0. then smod +. i16 else smod
      in
      int16bit
  | _ -> 0.

let int64_bitwise_not = Z.lognot
let int64_bitwise_and = Z.logand
let int64_bitwise_or = Z.logor
let int64_bitwise_xor = Z.logxor
let int64_left_shift x y = Z.shift_left x (Z.to_int y)

let int64_right_shift x y =
  let l = Int64.of_float x in
  let r = int_of_float y in
  Int64.to_float (Int64.shift_right l r)

let uint64_right_shift x y =
  let l = Int64.of_float x in
  let r = int_of_float y in
  Int64.to_float (Int64.shift_right_logical l r)

let int32_bitwise_not x = Int32.to_float (Int32.lognot (Int32.of_float x))

let int32_bitwise_and x y =
  Int32.to_float (Int32.logand (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_or x y =
  Int32.to_float (Int32.logor (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_xor x y =
  Int32.to_float (Int32.logxor (Int32.of_float x) (Int32.of_float y))

let int32_left_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_left l r)

let int32_right_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_right l r)

let uint32_right_shift x y = Z.shift_right x (Z.to_int y)

let uint32_right_shift_f x y =
  let i31 = 2. ** 31. in
  let i32 = 2. ** 32. in
  let signedx = if x >= i31 then x -. i32 else x in
  let left = Int32.of_float signedx in
  let right = int_of_float y mod 32 in
  let r = Int32.to_float (Int32.shift_right_logical left right) in
  if r < 0. then r +. i32 else r

let uint64_int_right_shift x y = Z.shift_right x (Z.to_int y)

(** Stringifies a float, adapting based on its size, or whether it's an integer
    
  Assumes the float is normal and positive *)
let string_of_pos_float num =
  (* Is the number an integer? *)
  let inum = int_of_float num in
  if is_int num then string_of_int inum (* It is not an integer *)
  else if num > 1e+9 && num < 1e+21 then Printf.sprintf "%.0f" num
  else if 1e-5 <= num && num < 1e-4 then
    let s = Float.to_string (num *. 10.) in
    let len = String.length s in
    "0.0" ^ String.sub s 2 (len - 2)
  else if 1e-6 <= num && num < 1e-5 then
    let s = Float.to_string (num *. 100.) in
    let len = String.length s in
    "0.00" ^ String.sub s 2 (len - 2)
  else
    let re = Str.regexp "e\\([-+]\\)0" in
    (* e+0 -> e+ *)
    Str.replace_first re "e\\1" (Float.to_string num)

(** Stringifies a float, considering negative and abnormal cases *)
let rec float_to_string_inner n =
  if Float.is_nan n then "NaN"
  else if n = 0.0 || n = -0.0 then "0"
  else if n < 0.0 then "-" ^ float_to_string_inner (-.n)
  else if n = Float.infinity then "Infinity"
  else string_of_pos_float n
