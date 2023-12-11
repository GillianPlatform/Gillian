open Memory_model
open Kanillian_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)
module Init_data = Gillian.General.Init_data.Dummy

module Gil_to_c_lifter
    (Verification : Gillian.Abstraction.Verifier.S
                      with type annot = KParserAndCompiler.Annot.t) =
struct
  include
    Gillian.Debugger.Lifter.Gil_lifter.Make (KParserAndCompiler) (Verification)
      (SMemory)

  let add_variables = MonadicSMemory.Lift.add_variables
end

module CLI =
  Gillian.Command_line.Make (Init_data) (CMemory) (SMemory) (KParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list = []
    end)
    (Gil_to_c_lifter)

let () = CLI.main ()
