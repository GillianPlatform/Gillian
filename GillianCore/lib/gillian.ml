(** Modules for the GIL intermediate language and its syntax *)
module Gil_syntax = struct
  (** @inline *)
  include Gil_syntax
end

(** The GIL parser

  This is parametric over target languages, via the [Annot] module parameter. *)
module Gil_parsing = struct
  (** @inline *)
  include Gil_parsing
end

(** Gillian's command-line interface *)
module Command_line = struct
  (** @inline *)
  include Command_line
end

(** Interfaces for symbolic exectuion, including symbolic memory model *)
module Symbolic = struct
  (** @inline *)
  include Engine.Symbolic
end

(** Interfaces for concrete exectuion, including concrete memory model *)
module Concrete = struct
  (** @inline *)
  include Engine.Concrete
end

(** Various interfaces, used across execution modes *)
module General = struct
  (** @inline *)
  include Engine.General
end

module Bulk = Bulk
module Monadic = Monadic
module ParserAndCompiler = ParserAndCompiler

(** Modules for the debugger and related TL-lifting *)
module Debugger = struct
  module Logging = Debugger_log.Public
  module Utils = Debugger_utils
  module Lifter = Debugger_lifter
end

module Logic = struct
  module Reduction = Engine.Reduction
  module FOSolver = Engine.FOSolver
  module Simplifications = Engine.Simplifications
end

module Abstraction = struct
  module MP = Engine.MP
  module Verifier = Engine.Verifier
end

(* module Test262 = Test262_main *)

(** Modules for logging (to the file log and the report database) *)
module Logging = struct
  open Logging
  module Logging_constants = Logging_constants

  (** Types and functions for controlling log levels *)
  module Mode = Mode

  module Report_id = Report_id

  (** Module specifying functions required for a type to be loggable *)
  module Loggable = Loggable

  (** Module for querying the reports stored by the database reporter.
    Queries will return None if the Log_queryer is not enabled. *)
  module Log_queryer = Log_queryer

  (** Logs a message at the [Normal] logging level given a message format *)
  let normal = normal

  (** Logs a message at the [Verbose] logging level given a message format *)
  let verbose = verbose

  (** Logs a message at the [TMI] logging level given a message format *)
  let tmi = tmi

  (** Writes the string and then raises a failure. *)
  let fail = fail

  (** Logs a type given a {!Loggable.t} and its content
    type (which should be one of the predefined strings in
    {!module-Logging_constants.module-Content_type}). Returns the logged report id if it has
    been logged. *)
  module Specific = Specific

  (** Functions for managing phases *)
  module Phase = Phase

  (** Functions for managing the parent of new reports *)
  module Parent = Parent

  (** A dummy pretty-printer

    Use this when you need a pretty printer,
    but don't expect it to actually be seen anywhere *)
  let dummy_pp = dummy_pp

  (** Prints a message to all available reporters, and stdout if applicable. *)
  let print_to_all = print_to_all
end

module IncrementalAnalysis = IncrementalAnalysis

(** General utilities *)
module Utils = struct
  (** @inline *)
  include Utils
end
