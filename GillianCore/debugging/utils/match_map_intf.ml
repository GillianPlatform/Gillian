module Types = struct
  (** Describes {i why} this matching is happening *)
  type kind = Matcher.match_kind [@@deriving yojson]

  type match_result = Success | Failure [@@deriving yojson]

  (** A substitution, and the ID of the assertion where it was learned *)
  type substitution = {
    assert_id : Logging.Report_id.t; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  (** Represents one step of a matching *)
  type assertion_data = {
    id : Logging.Report_id.t;
        (** The report ID of the assertion in the log database *)
    fold : (Logging.Report_id.t * match_result) option;
        (** The ID of the fold matching and its result, if this assertion requires a fold *)
    assertion : string;  (** The string representation of this assertion *)
    substitutions : substitution list;
        (** A list of the substitutions learned from this assertion specifically *)
  }
  [@@deriving yojson]

  (** A segment of matching *)
  type match_seg =
    | Assertion of assertion_data * match_seg  (** A single assertion *)
    | MatchResult of Logging.Report_id.t * match_result
        (** The end of this matching segment *)
  [@@deriving yojson]

  (** A matching map.
    matching is either a single segment in the normal case, or potentially multiple segments when folding (i.e. when a predicate has multiple cases) *)
  type map = Direct of match_seg | Fold of match_seg list [@@deriving yojson]

  type t = kind * map [@@deriving yojson]
end

include Types

module type Build = sig
  (** Given the ID of a matching, build the representative matching map *)
  val f : Logging.Report_id.t -> t
end

module type Make_builder = functor (Verification : Verifier.S) -> Build

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  val result : t -> match_result

  (** {!Build.f}: Given the ID of a matching, build the representative matching map *)
  module type Build = Build

  (**/**)

  module type Make_builder = Make_builder

  (**/**)

  module Make_builder : Make_builder
end
