let test_suites : unit Alcotest.test list =
  [
    ("Gillian-C.SHeapTree.no-logic", SHeapTree.No_logic.tests);
    ("Gillian-C.SHeapTree.with-logic", SHeapTree.With_logic.tests);
    ("Gillian-C.Results", Results.tests);
    ("Gillian-C.SatResults", SatResults.tests);
  ]

let () = Alcotest.run "Gillian-C" test_suites
