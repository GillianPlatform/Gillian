open Utils
open Gillian.Monadic
open Gil_syntax
module Global_env = Cgil_lib.Global_env

let init_data = ref Global_env.empty
let set_init_data d = init_data := d

module EnvAg = struct
  include Agreement

  let execute_action a ins s =
    let open Delayed.Syntax in
    let* res = execute_action a ins s in
    match (a, res) with
    | Load, Error e when can_fix e && !Gillian.Utils.Config.under_approximation
      -> Delayed.vanish ()
    | _ -> Delayed.return res

  let get_fixes _ = []
  let assertions _ = []
end

module M : MyMonadicSMemory =
  Mapper
    (struct
      let action_substitutions = [ ("getdef", "load") ]
      let pred_substitutions = [ ("def", "value") ]
    end)
    (struct
      include OpenALocPMap (EnvAg)

      let empty () : t =
        Cgil_lib.String_map.fold
          (fun k v acc ->
            States.MyUtils.SMap.add k
              (Some (Expr.Lit (Global_env.serialize_def v)))
              acc)
          !init_data States.MyUtils.SMap.empty
    end)
