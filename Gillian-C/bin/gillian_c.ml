open Cgil_lib

module Gil_to_c_lifter
    (Verification :
      Gillian.Abstraction.Verifier.S
        with type annot = CParserAndCompiler.Annot.t) =
struct
  include
    Gillian.Debugger.Lifter.Gil_lifter.Make
      (MonadicSMemory)
      (CParserAndCompiler)
      (Verification)

  let get_variables =
    let open MonadicSMemory.Lift in
    get_variables' ~add_heap_variables
end

module CLI =
  Gillian.Command_line.Make (Global_env) (CMemory) (MonadicSMemory)
    (CParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module CRunner); (module SRunner) ]
    end)
    (Gil_to_c_lifter)

let () = CLI.main ()
