open Cgil_lib
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

module Gil_to_c_lifter
    (Verification : Gillian.Abstraction.Verifier.S
                      with type annot = CParserAndCompiler.Annot.t) =
struct
  include
    Gillian.Debugger.Lifter.Gil_lifter.Make (SMemory) (CParserAndCompiler)
      (Verification)

  let add_variables = MonadicSMemory.Lift.add_variables

  let get_variables
      _
      Gillian.Debugger.Utils.{ store; memory; pfs; types; preds }
      _ =
    let open Debugger_utils.Variable in
    let variables = Hashtbl.create 0 in
    (* New scope ids must be higher than last top level scope id to prevent
        duplicate scope ids *)
    let scope_id = ref (List.length top_level_scopes) in
    let get_new_scope_id () =
      let () = scope_id := !scope_id + 1 in
      !scope_id
    in
    let lifted_scopes =
      let lifted_scopes =
        add_variables ~store ~memory ~is_gil_file:false ~get_new_scope_id
          variables
      in
      let pure_formulae_vars =
        Option.fold ~some:get_pure_formulae_vars ~none:[] pfs
      in
      let type_env_vars = Option.fold ~some:get_type_env_vars types ~none:[] in
      let pred_vars = Option.fold ~some:get_pred_vars preds ~none:[] in
      let vars_list = [ pure_formulae_vars; type_env_vars; pred_vars ] in
      let () =
        List.iter2
          (fun (scope : scope) vars -> Hashtbl.replace variables scope.id vars)
          top_level_scopes vars_list
      in
      lifted_scopes
    in
    (lifted_scopes, variables)
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
