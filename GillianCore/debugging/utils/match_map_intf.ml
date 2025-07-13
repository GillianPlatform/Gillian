module Types = struct
  (** Describes {i why} this matching is happening *)
  type kind = Matcher.match_kind [@@deriving yojson]

  type match_result = Success | Failure
  [@@deriving yojson, show { with_path = false }]

  (** A substitution, and the ID of the assertion where it was learned *)
  type substitution = {
    assert_id : Logging.Report_id.t; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  (** Data about a matching *)
  type matching = {
    id : Logging.Report_id.t;
    kind : Matcher.match_kind;
    result : match_result;
  }
  [@@deriving yojson]

  (** Represents one step of a matching *)
  type assertion_data = {
    id : Logging.Report_id.t;
        (** The report ID of the assertion in the log database *)
    fold : matching option;
        (** The ID of the fold matching and its result, if this assertion requires a fold *)
    assertion : string;  (** The string representation of this assertion *)
    substitutions : substitution list;
        (** A list of the substitutions learned from this assertion specifically *)
  }
  [@@deriving yojson]

  type next = Logging.Report_id.t list [@@deriving yojson]

  type step =
    | Assertion of assertion_data
    | RecoveryTactic of Matcher.recovery_tactic
  [@@deriving yojson]

  type node = step * bool option * next [@@deriving yojson]

  type t = {
    kind : kind;
    roots : Logging.Report_id.t list;
    nodes : (Logging.Report_id.t, node) Hashtbl.t;
    result : match_result;
  }
  [@@deriving yojson]
end

include Types

module type Build = sig
  (** Given the ID of a matching, build the representative matching map *)
  val f : ?pp_asrt:Asrt.atom Fmt.t -> Logging.Report_id.t -> t
end

module type Make_builder = functor (Verification : Verifier.S) -> Build

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  (** {!Build.f}: Given the ID of a matching, build the representative matching map *)
  module type Build = Build

  (**/**)

  module type Make_builder = Make_builder

  (**/**)

  module Make_builder : Make_builder
end
