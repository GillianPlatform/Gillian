open Cgil_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

module Gil_to_c_lifter
    (Verification : Gillian.Abstraction.Verifier.S
                      with type annot = CParserAndCompiler.Annot.t) =
struct
  include
    Gillian.Debugger.Lifter.Gil_lifter.Make (CParserAndCompiler) (Verification)
      (SMemory)

  let add_variables = MonadicSMemory.Lift.add_variables
end

module CLI =
  Gillian.Command_line.Make (Global_env) (CMemory) (SMemory)
    (CParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)
    (Gil_to_c_lifter)

let () = CLI.main ()
