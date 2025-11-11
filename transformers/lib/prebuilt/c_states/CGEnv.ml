open Gil_syntax
module Global_env = Cgil_lib.Global_env

let init_data = ref Global_env.empty
let set_init_data d = init_data := d

module M : States.MyMonadicSMemory.S =
  States.Mapper.Make
    (struct
      let action_substitutions = [ ("getdef", "load") ]
      let pred_substitutions = [ ("def", "value") ]
    end)
    (struct
      include States.PMap.Make (States.PMap.ALocImpl) (States.Agreement)

      let empty () : t =
        let map =
          Cgil_lib.String_map.fold
            (fun k v acc ->
              States.MyUtils.SMap.add k
                (Some (Expr.Lit (Global_env.serialize_def v)))
                acc)
            !init_data States.MyUtils.SMap.empty
        in
        (map, None)
    end)
