open Gillian
open Gillian.Symbolic
open Gillian.General
open Gillian.Gil_syntax
open Utils.Containers
module MP = Gillian.Abstraction.MP
module L = Gillian.Logging

(** This functor expects to receive a symbolic memory that was *not* transformed
    through the Bi_abd combinator. *)
module Make
    (SMemory : States.MyMonadicSMemory.S)
    (Init_data : States.MyMonadicSMemory.ID)
    (PC : ParserAndCompiler.S with type init_data = Init_data.t)
    (External : External.T(PC.Annot).S) =
struct
  (* We first construct memory that performs bi-abduction *)
  module BiMemory = States.Bi_abd.Make (SMemory)

  (* We lift to legacy memory signature *)
  module BiMemoryMonadicLegacy =
    States.MyMonadicSMemory.Make (BiMemory) (Init_data)

  module BiMemoryLegacy = Monadic.MonadicSMemory.Lift (BiMemoryMonadicLegacy)

  (* We lift it to Gillian states *)
  module BiSState = SState.Make (BiMemoryLegacy)
  module SPState = PState.Make (BiSState)
  module NonBiMemory = States.MyMonadicSMemory.Make (SMemory) (Init_data)
  module NonBiMemoryLegacy = Monadic.MonadicSMemory.Lift (NonBiMemory)
  module NonBiSState = SState.Make (NonBiMemoryLegacy)
  module NonBiSPState = PState.Make (NonBiSState)

  (* We define a helper to run functions under bi-abduction *)

  module Normaliser = Abstraction.Normaliser.Make (SPState)

  (* We can now use the standard Abductor loop 
     (which iterates over the functions of the program to generate specifications).
  *)

  module BiProcess = struct
    type annot = PC.Annot.t
    type state_t = SPState.t
    type init_data = Init_data.t

    let normalise_assertion ~init_data ~prog ~pvars assertion =
      match
        Normaliser.normalise_assertion ~init_data ~pred_defs:prog.MP.preds
          ~pvars assertion
      with
      | Ok l -> List.map fst l
      | Error _ -> []

    let make_spec
        name
        params
        (_initial_state : state_t)
        (final_state : state_t)
        fl =
      let pvars = SS.of_list (Utils.Names.return_variable :: params) in
      let States.Bi_abd.{ anti_frame = _; state } =
        SPState.get_heap final_state
      in
      let final_state =
        NonBiSPState.make_p_from_heap ~pred_defs:final_state.pred_defs
          ~store:(SPState.get_store final_state)
          ~heap:state
          ~pfs:(SPState.get_pfs final_state)
          ~gamma:(SPState.get_typ_env final_state)
          ~spec_vars:(SPState.get_spec_vars final_state)
          ~wands:(SPState.get_wands final_state)
          ~preds:(SPState.get_preds final_state)
      in
      let post = NonBiSPState.to_assertions ~to_keep:pvars final_state in
      let pre = [ Asrt.Emp ] in
      let sspec =
        Spec.
          {
            ss_pre = (pre, None);
            ss_posts = [ (post, None) ];
            ss_variant = None;
            ss_flag = fl;
            ss_label = None;
            ss_to_verify = false;
          }
      in
      let spec =
        Spec.
          {
            spec_name = name;
            spec_params = params;
            spec_sspecs = [ sspec ];
            spec_normalised = true;
            spec_incomplete = true;
            spec_to_verify = false;
            spec_location = None;
          }
      in
      Some spec
    (* So first we need to extract a state that corresponds to the *)
  end

  module Abductor = Abductor.Make_raw (PC) (SPState) (BiProcess) (External)
end
