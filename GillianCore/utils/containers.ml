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

module SS = Set.Make (String)
module SI = Set.Make (MyInt)
module SB = Set.Make (MyBool)
module SN = Set.Make (MyNumber)

let to_key_set (table : (string, 'b) Hashtbl.t) : SS.t =
  Hashtbl.fold (fun key _ keys -> SS.add key keys) table SS.empty
