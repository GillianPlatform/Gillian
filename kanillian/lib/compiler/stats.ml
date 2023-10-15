module J = struct
  include Yojson.Safe.Util

  let iter_obj f = function
    | `Assoc l -> List.iter f l
    | _ -> Error.user_error "Not a json object"

  module Infix = struct
    let ( $? ) j m =
      match member m j with
      | `Null -> None
      | other -> Some other
  end
end

module Unhandled = struct
  type feature =
    | ByCompositValueCopyInto
    | OutputStmt
    | FlexibleArrayMember
    | CompositNondet of Type.t
    | BinOp of Ops.Binary.t * (Type.t * Type.t) option
    | UnOp of Ops.Unary.t
    | Cast of Type.t * Type.t
    | ExprIrep of Irep_lib.Id.t * string
    | StmtIrep of Irep_lib.Id.t

  let feature_to_string = function
    | ByCompositValueCopyInto -> "ByCompositValueCopyInto"
    | CompositNondet ty -> "CompositNondet::" ^ Type.show_simple ty
    | OutputStmt -> "OutputStmt"
    | FlexibleArrayMember -> "FlexibleArrayMember"
    | UnOp unop -> "Unop::" ^ Ops.Unary.show unop
    | BinOp (b, types) -> (
        "Binop::" ^ Ops.Binary.show b
        ^
        match types with
        | None -> ""
        | Some (ta, tb) ->
            "::" ^ Type.show_simple ta ^ "::" ^ Type.show_simple tb)
    | Cast (from, to_) ->
        "Cast::" ^ Type.show_simple from ^ "::" ^ Type.show_simple to_
    | ExprIrep (id, msg) -> "ExprIrep::" ^ Irep_lib.Id.to_string id ^ "::" ^ msg
    | StmtIrep id -> "StmtIrep::" ^ Irep_lib.Id.to_string id

  let stats : (feature, int) Hashtbl.t = Hashtbl.create 1

  let json_stats () =
    let assoc =
      Hashtbl.to_seq stats
      |> Seq.map (fun (feature, count) ->
             (feature_to_string feature, `Int count))
      |> List.of_seq
    in
    `Assoc assoc

  let signal (feature : feature) =
    let current_count =
      Option.value ~default:0 (Hashtbl.find_opt stats feature)
    in
    Hashtbl.replace stats feature (current_count + 1)
end

let report file =
  let unhandled = Unhandled.json_stats () in
  let json = `Assoc [ ("at_compilation", unhandled) ] in
  Yojson.Safe.to_file file json
