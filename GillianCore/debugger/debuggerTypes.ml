type stop_reason =
  | Step
  | ReachedStart
  | ReachedEnd
  | Breakpoint
  | ExecutionError

type frame = {
  index : int;
  name : string;
  source_path : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

type scope = { name : string; id : int }

type variable = {
  name : string;
  value : string;
  type_ : string option;
  var_ref : int;
}

type variables = (int, variable list) Hashtbl.t

type exception_info = { id : string; description : string option }
