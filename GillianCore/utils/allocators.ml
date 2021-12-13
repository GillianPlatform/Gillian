let resetters : (unit -> unit) list ref = ref []

let register_resetter f = resetters := f :: !resetters

let reset_all () = List.iter (fun f -> f ()) !resetters

module type S = sig
  type t [@@deriving yojson]

  val alloc : unit -> t

  val dealloc : t -> unit

  val eq : t -> t -> bool

  val reset : unit -> unit
end

module type S_with_stringify = sig
  include S

  val to_string : t -> string

  val of_string : string -> t
end

module Basic () = struct
  open Containers

  type t = int [@@deriving yojson]

  let counter = ref 0

  let freed = ref SI.empty

  let alloc () =
    let curr = !counter in
    let () = counter := !counter + 1 in
    curr

  let dealloc i = freed := SI.add i !freed

  let eq = Int.equal

  let reset () =
    counter := 0;
    freed := SI.empty

  let () = register_resetter reset

  let of_string = int_of_string

  let to_string = string_of_int
end

module Make_with_prefix
    (A : S_with_stringify) (P : sig
      val prefix : string
    end) : S with type t = string = struct
  type t = string [@@deriving yojson]

  let construct x = P.prefix ^ A.to_string x

  let deconstruct str =
    let lp = String.length P.prefix in
    let l = String.length str in
    if String.sub str 0 lp = P.prefix then
      A.of_string (String.sub str lp (l - lp))
    else
      failwith
        ("This allocated value doesn't start with prefix " ^ P.prefix
       ^ " as expected")

  let alloc () = construct (A.alloc ())

  let dealloc s = A.dealloc (deconstruct s)

  let eq = String.equal

  let reset = A.reset
end
