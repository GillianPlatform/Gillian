module SMemory = Engine.SMemory

module Make (S : SMemory.S) = struct
  let perf : (string, int * float) Hashtbl.t ref = ref (Hashtbl.create 0)

  let measure_ name f =
    let start = Unix.gettimeofday () in
    let res = f () in
    let stop = Unix.gettimeofday () in
    let time = (stop -. start) *. 1000.0 in
    let c, old = try Hashtbl.find !perf name with Not_found -> (0, 0.0) in
    Hashtbl.replace !perf name (c + 1, old +. time);
    res

  let m1 name f x = measure_ name (fun () -> f x)
  let m2 name f x y = measure_ name (fun () -> f x y)
  let m3 name f x y z = measure_ name (fun () -> f x y z)
  let m4 name f x y z w = measure_ name (fun () -> f x y z w)

  include S

  let init = m1 "init" init
  let get_init_data = m1 "get_init_data" get_init_data
  let clear = m1 "clear" clear

  let execute_action a =
    m3 (Format.asprintf "execute_action/%s" a) (execute_action a)

  let consume p = m3 (Format.asprintf "consume/%s" p) (consume p)
  let produce p = m3 (Format.asprintf "produce/%s" p) (produce p)
  let is_overlapping_asrt = m1 "is_overlapping_asrt" is_overlapping_asrt
  let copy = m1 "copy" copy

  let substitution_in_place ~pfs ~gamma =
    m2 "substitution_in_place" (substitution_in_place ~pfs ~gamma)

  let clean_up ?keep = m1 "clean_up" (clean_up ?keep)
  let lvars = m1 "lvars" lvars
  let alocs = m1 "alocs" alocs
  let assertions ?to_keep = m1 "assertions" (assertions ?to_keep)
  let mem_constraints = m1 "mem_constraints" mem_constraints
  let get_recovery_tactic = m2 "get_recovery_tactic" get_recovery_tactic

  let get_failing_constraint =
    m1 "get_failing_constraint" get_failing_constraint

  let get_fixes = m1 "get_fixes" get_fixes
  let can_fix = m1 "can_fix" can_fix
  let sure_is_nonempty = m1 "sure_is_nonempty" sure_is_nonempty
  let split_further = m4 "split_further" split_further

  let pp ft _ =
    let open Fmt in
    let vals =
      Hashtbl.to_seq !perf |> List.of_seq
      |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
    in
    let pp_one ft (name, (count, time)) =
      pf ft "@[%s: %.4fms (%d)@]" name time count
    in
    (list ~sep:(any "@\n") pp_one) ft vals

  let () = at_exit (fun () -> Fmt.pr "PERF: @.@[%a@]@.@." pp ())
end
