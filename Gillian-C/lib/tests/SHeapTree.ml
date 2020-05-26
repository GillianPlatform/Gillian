open Cgil_lib
open Gillian
open Gil_syntax
open Checkers
open Alcotest

let n = Expr.num

let ( let* ) = Result.bind

module AlmostConcrete = struct
  let pfs = Engine.PFS.init ()

  let gamma = Engine.TypEnv.init ()

  let get = SHeapTree.get ~pfs ~gamma

  let set = SHeapTree.set ~pfs ~gamma

  let free = SHeapTree.free ~pfs ~gamma

  let alloc = SHeapTree.alloc

  let read_after_alloc () =
    let mem = alloc (n (-8.)) (n 16.) in
    let res = get mem (n (-8.)) Mint64 in
    match res with
    | Error e       -> failf "Read after alloc errored with \"%a\""
                         SHeapTree.pp_err e
    | Ok (value, _) ->
        check sval "Reading right after alloc should yield undefined" SUndefined
          value

  let read_after_alloc_middle () =
    let mem = alloc (n (-8.)) (n 16.) in
    let res = get mem (n 0.) Mint64 in
    match res with
    | Error e       -> failf "Read after alloc errored with \"%a\""
                         SHeapTree.pp_err e
    | Ok (value, _) ->
        check sval "Reading right after alloc should succeed yield undefined"
          SUndefined value

  let read_after_write () =
    let res =
      let mem = alloc (n (-8.)) (n 16.) in
      let* mem = set mem (n (-8.)) Mint64 (SVlong (n 16.)) in
      let* value, _ = get mem (n (-8.)) Mint64 in
      Ok value
    in
    check (result sval err)
      "Reading right after alloc should succeed yield the right value"
      (Ok (SVlong (n 16.)))
      res

  let read_after_free () =
    let e =
      let mem = alloc (n (-8.)) (n 32.) in
      let* mem = free mem (n (-8.)) (n 32.) in
      check heaptree "After freeing, the memory is freed" Freed mem;
      get mem (n 0.) Mint32
    in
    check get_result "Read after free should result in an error"
      (Error UseAfterFree) e

  let write_after_free () =
    let e =
      let mem = alloc (n (-8.)) (n 32.) in
      let* mem = free mem (n (-8.)) (n 32.) in
      check heaptree "After freeing, the memory is freed" Freed mem;
      set mem (n 0.) Mint32 (SVint (n 1000.))
    in
    check (result heaptree err) "Read after free should result in an error"
      (Error UseAfterFree) e

  let write_buffer_overrun () =
    let e =
      let mem = alloc (n (-8.)) (n 32.) in
      set mem (n 36.) Mint32 (SVint (n 1000.))
    in
    check (result heaptree err) "Read after free should result in an error"
      (Error BufferOverrun) e

  let read_buffer_overrun () =
    let e =
      let mem = alloc (n (-8.)) (n 32.) in
      get mem (n 36.) Mint32
    in
    check get_result "Read after free should result in an error"
      (Error BufferOverrun) e

  let tests =
    [
      ("read after alloc", `Quick, read_after_alloc);
      ("read after alloc in the middle", `Quick, read_after_alloc);
      ("read after write", `Quick, read_after_write);
      ("read after free", `Quick, read_after_free);
      ("write after free", `Quick, write_after_free);
      ("read buffer overrun", `Quick, read_buffer_overrun);
      ("write buffer overrun", `Quick, write_buffer_overrun);
    ]
end
