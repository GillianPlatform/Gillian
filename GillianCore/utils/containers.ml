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

module SS = struct
  (** Extension of SS with functions to serialize to and deserialize from
    yojson. A SS is a represented as a list of key-value pairs, where a
    key-value pair is list of two elements. *)

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

module SI = Set.Make (MyInt)
module SB = Set.Make (MyBool)
module SN = Set.Make (MyNumber)
