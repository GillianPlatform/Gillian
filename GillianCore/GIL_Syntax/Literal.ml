(**
	GIL Literals
*)

type t = TypeDef__.literal =
  | Undefined  (** The literal [undefined] *)
  | Null  (** The literal [null] *)
  | Empty  (** The literal [empty] *)
  | Constant of Constant.t  (** GIL constants ({!type:GIL_constant}) *)
  | Bool of bool  (** GIL booleans: [true] and [false] *)
  | Int of Z.t  (** GIL integers *)
  | Num of float  (** GIL floats - double-precision 64-bit IEEE 754 *)
  | String of string  (** GIL strings *)
  | Loc of string  (** GIL object locations *)
  | Type of Type.t  (** GIL types ({!type:Type.t}) *)
  | LList of t list  (** Lists of GIL literals *)
  | Nono
[@@deriving ord]

let rec equal la lb =
  match (la, lb) with
  | Undefined, Undefined | Null, Null | Empty, Empty | Nono, Nono -> true
  | Constant cl, Constant cr -> Constant.equal cr cl
  | Bool bl, Bool br -> Bool.equal bl br
  | Int zl, Int zr -> Z.equal zl zr
  | Num za, Num zb -> Int.equal (Stdlib.compare za zb) 0
  | String sl, String sr | Loc sl, Loc sr -> String.equal sl sr
  | Type tl, Type tr -> Type.equal tl tr
  | LList ll, LList lr -> List.for_all2 equal ll lr
  | _ -> false

let to_yojson = TypeDef__.literal_to_yojson
let of_yojson = TypeDef__.literal_of_yojson

(** Print *)
let rec pp fmt x =
  match x with
  | Undefined -> Fmt.string fmt "undefined"
  | Null -> Fmt.string fmt "null"
  | Empty -> Fmt.string fmt "empty"
  | Nono -> Fmt.string fmt "none"
  | Constant c -> Fmt.string fmt (Constant.str c)
  | Bool b -> if b then Fmt.string fmt "true" else Fmt.string fmt "false"
  | Int i -> Fmt.pf fmt "%ai" Z.pp_print i
  | Num n -> Fmt.pf fmt "%F" n
  | String x -> Fmt.pf fmt "\"%s\"" x
  | Loc loc -> Fmt.string fmt loc
  | Type t -> Fmt.string fmt (Type.str t)
  | LList ll -> Fmt.pf fmt "{{ %a }}" (Fmt.list ~sep:Fmt.comma pp) ll

(** Typing *)
let type_of (x : t) : Type.t =
  match x with
  | Undefined -> UndefinedType
  | Null -> NullType
  | Empty -> EmptyType
  | Constant _ -> NumberType
  | Bool _ -> BooleanType
  | Int _ -> IntType
  | Num _ -> NumberType
  | String _ -> StringType
  | Loc _ -> ObjectType
  | Type _ -> TypeType
  | LList _ -> ListType
  | Nono -> NoneType

let evaluate_constant (c : Constant.t) : t =
  match c with
  | Min_float -> Num 5e-324
  | Max_float -> Num max_float
  | MaxSafeInteger -> Num ((2. ** 53.) -. 1.)
  | Epsilon -> Num epsilon_float
  | Random -> Num (Random.float (1.0 -. epsilon_float))
  | Pi -> Num (4.0 *. atan 1.0)
  | UTCTime ->
      let t = Unix.gettimeofday () in
      let usec, _ = Float.modf t in
      let gct = Unix.gmtime t in
      let gctime, _ = Unix.mktime gct in
      let gctime = gctime +. usec in
      let _, tg = Float.modf (gctime *. 1e+3) in
      Num (float_of_int (int_of_float tg))
  | LocalTime ->
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
  | _ -> None

let get_base_lits super_visit (make : t -> 'a) env : t -> 'a list = function
  | LList _ as l -> super_visit env l
  | l -> [ make l ]

let base_elements (lit : t) : t list =
  let v =
    object
      inherit [_] Visitors.reduce as super
      inherit Visitors.Utils.non_ordered_list_monoid
      method! visit_literal = get_base_lits super#visit_literal (fun x -> x)
    end
  in
  v#visit_literal () lit
