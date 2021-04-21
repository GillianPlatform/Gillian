(**
	GIL Literals
*)

type t = TypeDef__.literal =
  | Undefined  (** The literal [undefined] *)
  | Null  (** The literal [null] *)
  | Empty  (** The literal [empty] *)
  | Constant  of Constant.t  (** GIL constants ({!type:GIL_constant}) *)
  | Bool      of bool  (** GIL booleans: [true] and [false] *)
  | Int       of int  (** GIL integers: TODO: understand size *)
  | Num       of float  (** GIL floats - double-precision 64-bit IEEE 754 *)
  | String    of string  (** GIL strings *)
  | Loc       of string  (** GIL object locations *)
  | Type      of Type.t  (** GIL types ({!type:Type.t}) *)
  | LList     of t list  (** Lists of GIL literals *)
  | Nono
[@@deriving yojson]

let rec equal a b =
  match (a, b) with
  | Undefined, Undefined | Null, Null | Nono, Nono | Empty, Empty -> true
  | Constant x, Constant y -> x = y
  | Bool x, Bool y -> x == y
  | Int x, Int y -> Int.equal x y
  | Num x, Num y -> Float.equal x y
  | String x, String y | Loc x, Loc y -> String.equal x y
  | Type x, Type y -> x = y
  | LList la, LList lb -> (
      try List.for_all2 equal la lb with Invalid_argument _ -> false)
  | _ -> false

(** Print *)
let rec pp fmt x =
  match x with
  | Undefined  -> Fmt.string fmt "undefined"
  | Null       -> Fmt.string fmt "null"
  | Empty      -> Fmt.string fmt "empty"
  | Nono       -> Fmt.string fmt "none"
  | Constant c -> Fmt.string fmt (Constant.str c)
  | Bool b     -> if b then Fmt.string fmt "true" else Fmt.string fmt "false"
  | Int i      -> Fmt.pf fmt "%ai" Fmt.int i
  | Num n      -> Fmt.pf fmt "%F" n
  | String x   -> Fmt.pf fmt "\"%s\"" x
  | Loc loc    -> Fmt.string fmt loc
  | Type t     -> Fmt.string fmt (Type.str t)
  | LList ll   -> Fmt.pf fmt "{{ %a }}" (Fmt.list ~sep:Fmt.comma pp) ll

(** Typing *)
let type_of (x : t) : Type.t =
  match x with
  | Undefined  -> UndefinedType
  | Null       -> NullType
  | Empty      -> EmptyType
  | Constant _ -> NumberType
  | Bool _     -> BooleanType
  | Int _      -> IntType
  | Num _      -> NumberType
  | String _   -> StringType
  | Loc _      -> ObjectType
  | Type _     -> TypeType
  | LList _    -> ListType
  | Nono       -> NoneType

let evaluate_constant (c : Constant.t) : t =
  match c with
  | Min_float      -> Num 5e-324
  | Max_float      -> Num max_float
  | MaxSafeInteger -> Num ((2. ** 53.) -. 1.)
  | Epsilon        -> Num epsilon_float
  | Random         -> Num (Random.float (1.0 -. epsilon_float))
  | Pi             -> Num (4.0 *. atan 1.0)
  | UTCTime        ->
      let t = Unix.gettimeofday () in
      let usec, _ = Float.modf t in
      let gct = Unix.gmtime t in
      let gctime, _ = Unix.mktime gct in
      let gctime = gctime +. usec in
      let _, tg = Float.modf (gctime *. 1e+3) in
      Num (float_of_int (int_of_float tg))
  | LocalTime      ->
      let t = Unix.gettimeofday () in
      let usec, _ = Float.modf t in
      let lct = Unix.localtime t in
      let lctime, _ = Unix.mktime lct in
      let lctime = lctime +. usec in
      let _, tl = Float.modf (lctime *. 1e+3) in
      Num (float_of_int (int_of_float tl))

let from_list lits = LList lits

let to_list lit =
  match lit with
  | LList les -> Some les
  | _         -> None

let get_base_lits super_visit (make : t -> 'a) env : t -> 'a list = function
  | LList _ as l -> super_visit env l
  | l            -> [ make l ]

let base_elements (lit : t) : t list =
  let v =
    object
      inherit [_] Visitors.reduce as super

      inherit Visitors.Utils.non_ordered_list_monoid

      method! visit_literal = get_base_lits super#visit_literal (fun x -> x)
    end
  in
  v#visit_literal () lit
