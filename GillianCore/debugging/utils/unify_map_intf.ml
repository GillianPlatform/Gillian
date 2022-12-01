module Types = struct
  (** Describes {i why} this unification is happening *)
  type kind = Unifier.unify_kind [@@deriving yojson]

  type unify_result = Success | Failure [@@deriving yojson]

  (** A substitution, and the ID of the assertion where it was learned *)
  type substitution = {
    assert_id : Logging.ReportId.t; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  (** Represents one step of a unification *)
  type assertion_data = {
    id : Logging.ReportId.t;
        (** The report ID of the assertion in the log database *)
    fold : (Logging.ReportId.t * unify_result) option;
        (** The ID of the fold unification and its result, if this assertion requires a fold *)
    assertion : string;  (** The string representation of this assertion *)
    substitutions : substitution list;
        (** A list of the substitutions learned from this assertion specifically *)
  }
  [@@deriving yojson]

  (** A segment of unification *)
  type unify_seg =
    | Assertion of assertion_data * unify_seg  (** A single assertion *)
    | UnifyResult of Logging.ReportId.t * unify_result
        (** The end of this unification segment *)
  [@@deriving yojson]

  (** A unification map.
    Unification is either a single segment in the normal case, or potentially multiple segments when folding (i.e. when a predicate has multiple cases) *)
  type map = Direct of unify_seg | Fold of unify_seg list [@@deriving yojson]

  type t = kind * map [@@deriving yojson]
end

include Types

module type Build = sig
  (** Given the ID of a unification, build the representative unification map *)
  val f : Logging.ReportId.t -> t
end

module type Make_builder = functor (Verification : Verifier.S) -> Build

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  val result : t -> unify_result

  (** {!Build.f}: Given the ID of a unification, build the representative unification map *)
  module type Build = Build

  (**/**)

  module type Make_builder = Make_builder

  (**/**)

  module Make_builder : Make_builder
end
