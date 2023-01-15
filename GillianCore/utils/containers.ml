(** Some common instances of {!Set} *)

(**/**)

module MyInt = struct
  type t = int

  let compare = Stdlib.compare
end

module MyNumber = struct
  type t = float

  let compare = Stdlib.compare
end

module MyBool = struct
  type t = bool

  let compare = Stdlib.compare
end

(**/**)

(** A {!Set} of [string]s*)
module SS = struct
  include Set.Make (String)

  let to_yojson (set : t) : Yojson.Safe.t =
    `List (set |> to_seq |> List.of_seq |> List.map (fun e -> `String e))

  let of_yojson (yojson : Yojson.Safe.t) : (t, string) result =
    let str_of_yojson j =
      match j with
      | `String s -> Ok s
      | _ -> Fmt.error "Invalid json string: %a" Yojson.Safe.pp j
    in
    match yojson with
    | `List l ->
        List.fold_left
          (fun set e ->
            match (set, str_of_yojson e) with
            | Ok set, Ok s -> Ok (add s set)
            | (Error _ as err), _ | _, (Error _ as err) -> err)
          (Ok empty) l
    | _ -> Fmt.error "Invalid json set: %a" Yojson.Safe.pp yojson
end

(** A {!Set} of [int]s*)
module SI = Set.Make (MyInt)

(** A {!Set} of [bool]s*)
module SB = Set.Make (MyBool)

(** A {!Set} of [float]s*)
module SN = Set.Make (MyNumber)
