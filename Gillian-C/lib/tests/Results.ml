module Results = Cgil_lib.Results
open Results

let pp_terminated fmt = function
  | `Success i -> Fmt.pf fmt "success: %i" i
  | `Failed s  -> Fmt.pf fmt "failed with: %s" s

let equal_terminated a b =
  match (a, b) with
  | `Success i, `Success j when Int.equal i j -> true
  | `Failed s, `Failed t when String.equal s t -> true
  | _ -> false

let terminated = Alcotest.testable pp_terminated equal_terminated

type cmd = Skip | Incr | Branch of int * int | Stop | Fail of string

let skip = Skip

let incr = Incr

let branch i j = Branch (i, j)

let stop = Stop

let fail s = Fail s

type prog = cmd array

type state = { counter : int; pc : int }

let eq_state a b = Int.equal a.counter b.counter && Int.equal a.pc b.pc

let pp_state fmt st = Fmt.pf fmt "<%i; %i>" st.counter st.pc

let incr_pc st = { st with pc = st.pc + 1 }

let incr_counter st = { st with counter = st.counter + 1 }

let incr_both st = incr_pc @@ incr_counter st

let goto st i = { st with pc = i }

type exec_monad = (state, [ `Success of int | `Failed of string ]) t

let exec_monad =
  Alcotest.testable
    (Results.pp ~pp_cont:pp_state ~pp_term:pp_terminated)
    (Results.equal ~eq_cont:eq_state ~eq_term:equal_terminated)

let stop_with state = terminate (`Success state.counter)

let exec_cmd ~(state : state) (cmd : cmd) : exec_monad =
  match cmd with
  | Skip          -> return (incr_pc state)
  | Incr          -> return (incr_both state)
  | Branch (i, j) -> branch2 (goto state i) (goto state j)
  | Stop          -> stop_with state
  | Fail s        -> terminate (`Failed s)

let init_state = { counter = 0; pc = 0 }

let init_exec = return init_state

let rec exec_prog ?(exec = init_exec) prog =
  let open Infix in
  let res = exec >>= fun state -> exec_cmd ~state prog.(state.pc) in
  if Results.terminated res then res.terminated else exec_prog ~exec:res prog

let one_by_one_test =
  let open Syntax in
  let run () =
    let res =
      let* state = init_exec in
      let state = incr_both state in
      let* state = branch2 (incr_pc state) (incr_both @@ incr_pc state) in
      stop_with (incr_pc state)
    in
    Alcotest.(check exec_monad)
      "Should all terminate with expected"
      { to_continue = []; terminated = [ `Success 2; `Success 1 ] }
      res
  in
  ("one by one", `Quick, run)

let basic_test =
  let prog_test =
    [|
      incr;
      (* 0 *)
      incr;
      (* 1 *)
      skip;
      (* 2 *)
      incr;
      (* 3 *)
      branch 5 9;
      (* 4 *)
      skip;
      (* 5 *)
      skip;
      (* 6 *)
      incr;
      (* 7 *)
      stop;
      (* 8 *)
      skip;
      (* 9 *)
      fail "test" (* 10 *);
    |]
  in
  let run () =
    let terminated_states = exec_prog prog_test in
    Alcotest.(check (list terminated))
      "Should terminate as expected"
      [ `Success 4; `Failed "test" ]
      terminated_states
  in
  ("basic test", `Quick, run)

let more_branch_test =
  let prog_test =
    [|
      branch 1 4;
      (* 0 *)
      branch 2 3;
      (* 1 *)
      fail "a";
      (* 2 *)
      branch 4 10;
      (* 3 *)
      branch 5 9;
      (* 4 *)
      incr;
      (* 5 *)
      incr;
      (* 6 *)
      incr;
      (* 7 *)
      incr;
      (* 8 *)
      stop;
      (* 9 *)
      fail "b" (* 10 *);
    |]
  in
  let run () =
    let terminated_states = exec_prog prog_test in
    Alcotest.(check (list terminated))
      "Should terminate as expected"
      [
        `Success 4; `Success 4; `Success 0; `Failed "b"; `Failed "a"; `Success 0;
      ]
      terminated_states
  in
  ("more branch test", `Quick, run)

let tests = [ basic_test; one_by_one_test; more_branch_test ]
