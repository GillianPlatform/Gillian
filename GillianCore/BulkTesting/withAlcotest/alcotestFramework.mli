module Make (Outcome : Outcome.S) : sig
  val custom_checkers : AlcotestCheckers.Make(Outcome).matcher
end
