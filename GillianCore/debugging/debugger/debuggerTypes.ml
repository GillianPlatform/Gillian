(* TODO: these types should be categorised and put in separate files. *)

type stop_reason =
  | Step
  | ReachedStart
  | ReachedEnd
  | Breakpoint
  | ExecutionError
[@@deriving yojson]

type frame = {
  index : int;
  name : string;
  source_path : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

(* TODO: this should contain a variable list *)
type scope = { name : string; id : int }

type variable = {
  name : string;
  value : string;
  type_ : string option;
  var_ref : int;
}

type variables = (int, variable list) Hashtbl.t
type exception_info = { id : string; description : string option }

let create_leaf_variable (name : string) (value : string) ?(type_ = None) () :
    variable =
  { name; value; type_; var_ref = 0 }

let create_node_variable (name : string) (var_ref : int) ?(value = "") () :
    variable =
  { name; value; type_ = Some "object"; var_ref }
