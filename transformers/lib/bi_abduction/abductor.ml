open Gillian
open Gillian.Symbolic
open Gillian.General
open Gil_syntax
module MP = Gillian.Abstraction.MP
module L = Gillian.Logging
module SSubst = Symbolic.Subst

(** This functor expects to receive a symbolic memory that was *not* transformed
    through the Bi_abd combinator. *)
module Make
    (Init_data : Gillian.Combinators.MyMonadicSMemory.ID)
    (SMemory : Gillian.Combinators.MyMonadicSMemory.S)
    (PC : ParserAndCompiler.S with type init_data = Init_data.t)
    (External : External.T(PC.Annot).S) :
  Abductor.S with type init_data = PC.init_data and type annot = PC.Annot.t =
struct
  (* We first construct memory that performs bi-abduction *)
  module BiMemory = Gillian.Combinators.Bi_abd.Make (SMemory)

  (* We lift to legacy memory signature *)
  module BiMemoryMonadicLegacy =
    Gillian.Combinators.MyMonadicSMemory.Make (BiMemory) (Init_data)

  module BiMemoryLegacy = Monadic.MonadicSMemory.Lift (BiMemoryMonadicLegacy)

  (* We lift it to Gillian states *)
  module BiSState = SState.Make (BiMemoryLegacy)

  module NonBiMemory =
    Gillian.Combinators.MyMonadicSMemory.Make (SMemory) (Init_data)

  module NonBiMemoryLegacy = Monadic.MonadicSMemory.Lift (NonBiMemory)
  module NonBiSState = SState.Make (NonBiMemoryLegacy)

  (* We define a helper to run functions under bi-abduction *)

  module Normaliser = Abstraction.Normaliser.Make (BiSState)

  (* We can now use the standard Abductor loop 
     (which iterates over the functions of the program to generate specifications).
  *)

  module BiProcess = struct
    type annot = PC.Annot.t
    type state_t = BiSState.t
    type init_data = Init_data.t

    module NonBiState = NonBiSState

    let normalise_assertion ~init_data ~prog ~pvars assertion =
      match
        Normaliser.normalise_assertion ~init_data ~pred_defs:prog.MP.preds
          ~pvars assertion
      with
      | Ok l -> List.map fst l
      | Error _ -> []

    let bistate_to_state_and_af (bi_state : state_t) =
      let Gillian.Combinators.Bi_abd.{ anti_frame; state } =
        BiSState.get_heap bi_state
      in
      let current =
        NonBiState.make_s_from_heap
          ~store:(BiSState.get_store bi_state)
          ~heap:state
          ~pfs:(BiSState.get_pfs bi_state)
          ~gamma:(BiSState.get_typ_env bi_state)
          ~spec_vars:(BiSState.get_spec_vars bi_state)
      in
      (* To avoid unfeasible matching plans, we bring up equalities that avoid variable disconnection. *)
      let anti_frame =
        let spatial =
          Gillian.Combinators.Fix.to_asrt ~pred_to_str:SMemory.pred_to_str
            anti_frame
        in
        let equalities =
          BiSState.get_pfs bi_state |> Pure_context.to_list
          |> List.filter (function
               | Expr.BinOp (_, Equal, _) -> true
               | _ -> false)
          |> List.map (fun e -> Asrt.Pure e)
        in
        spatial @ equalities
      in
      (current, anti_frame)
  end

  include Abductor.Make_raw (PC) (BiSState) (BiProcess) (External)
end

module Cli
    (Init_data : Gillian.Combinators.MyMonadicSMemory.ID)
    (SMemory : Gillian.Combinators.MyMonadicSMemory.S)
    (PC : ParserAndCompiler.S with type init_data = Init_data.t)
    (External : External.T(PC.Annot).S) =
struct
  module Gil_parsing = Gil_parsing.Make (PC.Annot)
  module Abductor = Make (Init_data) (SMemory) (PC) (External)

  include
    Gillian.Command_line.Act_console.Make (Init_data) (PC) (Abductor)
      (Gil_parsing)
end
