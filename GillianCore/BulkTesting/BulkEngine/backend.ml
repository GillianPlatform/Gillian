module type S = sig
  type category

  type matcher

  val check_not_throw : matcher -> (unit -> unit) -> unit

  val register_expectations_for_category :
    expectation:(matcher -> 'test -> 'outcome -> unit) ->
    test_runner:(matcher -> 'test -> 'outcome) ->
    category ->
    (string, 'test) Hashtbl.t ->
    unit

  val run : unit -> unit
end
