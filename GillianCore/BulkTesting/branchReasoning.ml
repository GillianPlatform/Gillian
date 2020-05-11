let makeFormater format pp fmt e = Fmt.pf fmt "%s" (format (Fmt.str "%a" pp e))

type branches = ExactlyOne | AllOfThem | AtLeastOne

(** Builds a message explaining the reason of failure when the `AllOfThem` is used.
    In this case, it explained every case that didn't match the expected patternd *)
let build_message_all ?(fmtexp = fun x -> x) flag const_opt lst_fail_msg =
  let pp_one fmt m = Fmt.pf fmt "- One of them %s" m in
  Fmt.str "Expected %s%s\nBut : @[<v 2>%a@]"
    (fmtexp ("all branches to finish in " ^ Flag.str flag))
    (fmtexp
       (Option.fold ~none:""
          ~some:(fun (c, _) -> "with constraint " ^ c)
          const_opt))
    (Fmt.list ~sep:Fmt.cut pp_one)
    lst_fail_msg

(* That function returns true if the result is what is expected,
     and false with a message *)
let oneResInMode
    ?(fmtexp = fun x -> x)
    ?(fmtrcv = fun x -> x)
    ~pp_what_branch_did
    flag
    const_opt
    res =
  let pp_rcv pp fmt x = (makeFormater fmtrcv) pp fmt x in
  let pp_exp pp fmt x = (makeFormater fmtexp) pp fmt x in
  match (res, const_opt) with
  | Engine.ExecRes.RSucc (fl, v, state), None when fl = flag -> (true, "")
  | RSucc (fl, v, state), Some (cn, f) when fl = flag ->
      if f v state then (true, "")
      else
        let failure_message =
          Fmt.str
            "successfully finished in %a mode\n\
             but didn't validate the constraint: %a" (pp_exp Flag.pp) flag
            (pp_rcv Fmt.string) cn
        in
        (false, failure_message)
  | RSucc (fl, _, _), _ ->
      let failure_message =
        Fmt.str
          "successfully finished in %a mode\n\
           But was expected to finish successfully in %a mode" (pp_rcv Flag.pp)
          fl (pp_exp Flag.pp) flag
      in
      (false, failure_message)
  | _ ->
      let failure_message =
        Fmt.str
          "was expected to finish successfully in %a mode\nbut actually %a"
          (pp_exp Flag.pp) flag
          (pp_rcv pp_what_branch_did)
          res
      in
      (false, failure_message)

let resInMode
    ?(fmtexp = fun x -> x)
    ?(fmtrcv = fun x -> x)
    ~pp_what_branch_did
    branches
    flag
    const_opt
    res_l =
  match branches with
  | ExactlyOne -> (
      match res_l with
      | [ res ] ->
          let pass, m =
            oneResInMode ~fmtexp ~fmtrcv ~pp_what_branch_did flag const_opt res
          in
          if pass then ((fun _ -> ""), true) else ((fun _ -> "Test " ^ m), false)
      | _       ->
          let failure_message =
            String.concat ""
              [
                "Expected ";
                fmtexp "exactly on branch at the end";
                "\nBut received";
                fmtrcv (string_of_int (List.length res_l) ^ " branches");
              ]
          in
          ((fun _ -> "Test " ^ failure_message), false) )
  | AllOfThem  ->
      let list_failure_messages =
        List.fold_left
          (fun acc res ->
            let pass, m =
              oneResInMode ~fmtexp ~fmtrcv ~pp_what_branch_did flag const_opt
                res
            in
            if pass then acc else m :: acc)
          [] res_l
      in
      if List.length list_failure_messages = 0 then ((fun _ -> ""), true)
      else
        ( (fun _ ->
            build_message_all ~fmtexp flag const_opt list_failure_messages),
          false )
  | AtLeastOne ->
      let rec go_through acc = function
        | []       ->
            let failure_message =
              Fmt.str
                "Expected at least one branch to finish successfuly in %s \
                 mode%s.\n\
                 but %s"
                (fmtexp (Flag.str flag))
                (fmtexp
                   (Option.fold ~none:""
                      ~some:(fun (c, _) -> "with constraint : " ^ c)
                      const_opt))
                (fmtrcv "none of them did.")
            in
            ((fun _ -> failure_message), false)
        | res :: r ->
            let pass, m =
              oneResInMode ~fmtexp ~fmtrcv ~pp_what_branch_did flag const_opt
                res
            in
            if pass then ((fun _ -> ""), true) else go_through (m :: acc) r
      in
      go_through [] res_l
