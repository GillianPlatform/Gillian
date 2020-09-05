open Gillian.Gil_syntax

module With_pc = struct
  type 'a t = 'a * Pc.t

  let make ~pc value = (value, pc)

  let equal eq_content (va, pca) (vb, pcb) =
    try Pc.equal pca pcb && eq_content va vb with Invalid_argument _ -> false

  let map (v, pc) f = (f v, pc)

  let merge (_, pca) (v, pcb) =
    let extended_pc = Pc.merge pca pcb in
    (v, extended_pc)

  let bind ((v, _) as x : 'a t) (f : 'a -> 'b t) : 'b t = merge x (f v)

  let pp pp_content =
    Fmt.record ~sep:Fmt.semi
      [
        Fmt.field "content" (fun x -> fst x) pp_content;
        Fmt.field "pc" (fun x -> snd x) Pc.pp;
      ]
end

type ('to_continue, 'terminated) t =
  ('to_continue With_pc.t, 'terminated With_pc.t) Results.t

let terminate ~pc ?(learned = []) (x : 'b) : (_, 'b) t =
  let pc = Pc.extend pc learned in
  Results.terminate (x, pc)

let return ~pc ?(learned = []) (x : 'a) : ('a, _) t =
  let pc = Pc.extend pc learned in
  Results.return (x, pc)

let make ~pfs ~gamma (x : 'a) : ('a, _) t =
  Results.
    {
      to_continue = [ With_pc.make ~pc:(Pc.make ~pfs ~gamma ()) x ];
      terminated = [];
    }

let pp ~pp_cont ~pp_term =
  Results.pp ~pp_cont:(With_pc.pp pp_cont) ~pp_term:(With_pc.pp pp_term)

let equal ~eq_cont ~eq_term =
  Results.equal ~eq_cont:(With_pc.equal eq_cont)
    ~eq_term:(With_pc.equal eq_term)

let empty = Results.empty

let branch_on_sat
    ~(pc : Pc.t)
    formula
    ~(then_branch : Pc.t -> ('a, 'b) t)
    ~(else_branch : Pc.t -> ('a, 'b) t) : ('a, 'b) t =
  let sat = FOSolver.sat ~pc in
  let neg = Formula.Not formula in
  let fsat = sat formula in
  let negsat = if not fsat then true else sat neg in
  let if_res = if fsat then then_branch (Pc.extend pc [ formula ]) else empty in
  let else_res = if negsat then else_branch (Pc.extend pc [ neg ]) else empty in
  Results.merge if_res else_res

let map (x : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t =
  Results.map x (fun x -> With_pc.map x f)

let bind (x : ('a, 'e) t) (f : 'a With_pc.t -> ('b, 'e) t) : ('b, 'e) t =
  Results.bind x f

let of_res ~pc = function
  | Ok x    -> return ~pc x
  | Error x -> terminate ~pc x

let of_results ~pc res =
  let f x = With_pc.make ~pc x in
  Results.
    {
      to_continue = List.map f res.to_continue;
      terminated = List.map f res.terminated;
    }

let of_option ~pc ~none = function
  | Some x -> return ~pc x
  | None   -> terminate ~pc none

module Syntax = struct
  let ( let** ) = bind

  let ( let++ ) = map
end
