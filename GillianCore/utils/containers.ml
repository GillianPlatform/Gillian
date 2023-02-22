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

(** @canonical Gillian.Utils.Containers.SS *)
module SS = struct
  (** A {!Set} of [string]s *)

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

(** @canonical Gillian.Utils.Containers.SI *)
module SI = struct
  (** A {!Set} of [int]s *)

  include Set.Make (MyInt)
end

(** @canonical Gillian.Utils.Containers.SB *)
module SB = struct
  (** A {!Set} of [bool]s *)

  include Set.Make (MyBool)
end

(** @canonical Gillian.Utils.Containers.SN *)
module SN = struct
  (** A {!Set} of [float]s *)

  include Set.Make (MyNumber)
end
