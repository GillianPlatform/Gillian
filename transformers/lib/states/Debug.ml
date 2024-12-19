module Make (M : MyMonadicSMemory.S) = struct
  open M

  (*
  Sample output:

  State actions:
  - {{ <rets...> }} := [<action>](<args...>)

  State predicates:
  - <<pred>>(<ins...> ; <outs...>)
  *)
  let print_info () : unit =
    let actions = list_actions () in
    let preds = list_preds () in
    Printf.printf "\n\n=================== State Info ===================\n\n";
    Printf.printf "Actions:\n";
    List.iter
      (fun (name, args, rets) ->
        Printf.printf "  - {{ %s }} := [%s](%s)\n" (String.concat ", " rets)
          (M.action_to_str name) (String.concat ", " args))
      actions;
    Printf.printf "\nPredicates:\n";
    List.iter
      (fun (name, ins, outs) ->
        Printf.printf "  - <%s>(%s ; %s)\n" (M.pred_to_str name)
          (String.concat ", " ins) (String.concat ", " outs))
      preds;
    Printf.printf "\n=================================================\n\n"
end
