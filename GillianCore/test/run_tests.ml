let test_suites : unit Alcotest.test list =
  [
    ("Gil_syntax.Reducers", Gil_syntax_tests.Visitors.tests);
    ("Engine.UP", Up_tests.tests);
  ]

let () = Alcotest.run "Gillian" test_suites
