type t = {
  id : Report_id.t;
  title : string;
  elapsed_time : float;
  previous : Report_id.t option;
  parent : Report_id.t option;
  content : Loggable.t;
  severity : Logging_constants.Severity.t;
  type_ : string;
}
