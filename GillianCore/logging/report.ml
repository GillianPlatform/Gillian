type severity = Info | Log | Success | Error | Warning
[@@deriving enum, yojson]

type t = {
  id : ReportId.t;
  title : string;
  elapsed_time : float;
  previous : ReportId.t option;
  parent : ReportId.t option;
  content : Loggable.t;
  severity : severity;
  type_ : string;
}
