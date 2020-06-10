let test_suites : unit Alcotest.test list =
  [
    ("Gillian-C.SHeapTree.no-logic", SHeapTree.AlmostConcrete.tests);
    ("Gillian-C.Results", Results.tests);
    ("Gillian-C.SatResults", SatResults.tests);
  ]

let () = Alcotest.run "Gillian-C" test_suites
