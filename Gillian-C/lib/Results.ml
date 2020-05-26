(** This shall be move somewhere INSIDE Gillian later *)

type ('to_continue, 'terminated) t = {
  to_continue : 'to_continue list;
  terminated : 'terminated list;
}

let pp ~pp_cont ~pp_term fmt t =
  Fmt.pf fmt "@[to continue: @[%a@]@,terminated: @[%a@]@]"
    (Fmt.Dump.list pp_cont) t.to_continue (Fmt.Dump.list pp_term) t.terminated

let equal ~eq_cont ~eq_term a b =
  try
    List.for_all2 eq_cont a.to_continue b.to_continue
    && List.for_all2 eq_term a.terminated b.terminated
  with Invalid_argument _ -> false

let empty = { to_continue = []; terminated = [] }

let return x = { to_continue = [ x ]; terminated = [] }

let terminate x = { to_continue = []; terminated = [ x ] }

let terminated x = List.compare_length_with x.to_continue 0 = 0

let branch2 x y = { to_continue = [ x; y ]; terminated = [] }

let merge a b =
  {
    to_continue = a.to_continue @ b.to_continue;
    terminated = a.terminated @ b.terminated;
  }

let concat l =
  let rec aux l acc =
    match l with
    | []     -> acc
    | x :: r -> aux r (merge x acc)
  in
  aux l empty

let bind res f =
  let { to_continue = tc; terminated = term } =
    concat (List.map f res.to_continue)
  in
  { to_continue = tc; terminated = term @ res.terminated }

let map { to_continue; terminated } f =
  { to_continue = List.map f to_continue; terminated }

module Infix = struct
  let ( >>= ) = bind

  let ( >>| ) = map
end

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end
