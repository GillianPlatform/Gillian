module type S = sig
  type t [@@deriving yojson]
end

module Dummy : S with type t = unit = struct
  type t = unit

  let of_yojson = function
    | `Null -> Ok ()
    | _ -> Error "Provided init_data but did not implement its parser"

  let to_yojson () = `Null
end
