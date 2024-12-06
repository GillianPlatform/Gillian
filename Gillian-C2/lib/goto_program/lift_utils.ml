let lift_option f (irep : Irep.t) =
  match irep.id with
  | Nil -> None
  | _ -> Some (f irep)

let exactly_one ?(msg = "") (irep : Irep.t) =
  match irep.sub with
  | [ a ] -> a
  | _ -> Gerror.unexpected ~irep (msg ^ " does not have exactly one sub")

let exactly_two ?(msg = "") (irep : Irep.t) =
  match irep.sub with
  | [ a; b ] -> (a, b)
  | _ -> Gerror.unexpected ~irep (msg ^ " does not have exactly two subs")

let exactly_three ?(msg = "") (irep : Irep.t) =
  match irep.sub with
  | [ a; b; c ] -> (a, b, c)
  | _ -> Gerror.unexpected ~irep (msg ^ " does not have exactly three subs")
