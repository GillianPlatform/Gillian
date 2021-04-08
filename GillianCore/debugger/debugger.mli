module type S = sig
  val launch : string -> unit

  val terminate : unit -> unit
end

module Make (PC : ParserAndCompiler.S) (Verification : Verifier.S) : S
