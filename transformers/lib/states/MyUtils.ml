open Gillian.Utils
open Gillian.Monadic
open Gil_syntax

module type IDs = sig
  val id1 : string
  val id2 : string
end

type ided = ID1 of string | ID2 of string | NotIDed of string

let str_rem_length s n = String.sub s n (String.length s - n)

module Identifier (I : IDs) = struct
  let get_ided s =
    if String.starts_with s ~prefix:I.id1 then
      ID1 (str_rem_length s (String.length I.id1))
    else if String.starts_with s ~prefix:I.id2 then
      ID2 (str_rem_length s (String.length I.id2))
    else NotIDed s
end

let pp_bindings ~pp_k ~pp_v iter fmt m =
  let pp_binding fmt (k, v) = Fmt.pf fmt "%a -> @[%a@]" pp_k k pp_v v in
  Fmt.pf fmt "@[<v>%a@]"
    (Fmt.iter_bindings ~sep:(Fmt.any "@\n") iter @@ fun ft b -> pp_binding ft b)
    m

module type SymExprMap = sig
  include Prelude.Map.S with type key = Expr.t

  val sym_find_opt : key -> 'a t -> (key * 'a) option Delayed.t

  val sym_find_default :
    key -> 'a t -> default:(unit -> 'a) -> (key * 'a) Delayed.t

  val sym_find_res : key -> 'a t -> err:'b -> (key * 'a, 'b) result Delayed.t

  val sym_compose :
    ('a -> 'a -> 'a Delayed.t) -> (Expr.t * 'a) list -> 'a t -> 'a t Delayed.t

  val sym_merge : ('a -> 'a -> 'a Delayed.t) -> 'a t -> 'a t -> 'a t Delayed.t

  val make_pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ExpMapMake (Check : sig
  val check :
    Expr.t ->
    then_:(unit -> 'a Delayed.t) ->
    else_:(unit -> 'a Delayed.t) ->
    'a Delayed.t
end) : SymExprMap = struct
  module Temp = Prelude.Map.Make (Expr)
  include Temp

  let sym_find_opt k m =
    match Temp.find_opt k m with
    | Some v -> Delayed.return (Some (k, v)) (* Direct match *)
    | None ->
        let open Delayed.Syntax in
        let* { matching; _ } = Delayed.leak_pc_copy () in
        let rec find_match = function
          | [] -> Delayed.return None
          | (k', v) :: tl -> (
              match (k, k') with
              (* THIS IS ONLY TRUE IF WE'RE NOT MATCHING ! *)
              | Expr.ALoc l1, Expr.ALoc l2 when not matching ->
                  if String.equal l1 l2 then Delayed.return (Some (k', v))
                  else find_match tl
              (* This is already done by the #==, but putting it here speeds it up a tiny bit :) *)
              | Expr.Lit (Loc l1), Expr.Lit (Loc l2)
              | Expr.Lit (String l1), Expr.Lit (String l2) ->
                  if String.equal l1 l2 then Delayed.return (Some (k', v))
                  else find_match tl
              | Expr.ALoc l1, Expr.ALoc l2 when matching && String.equal l1 l2
                -> Delayed.return (Some (k', v))
              | _ ->
                  Check.check
                    Expr.Infix.(k' == k)
                    ~then_:(fun () -> Delayed.return (Some (k', v)))
                    ~else_:(fun () -> find_match tl))
        in
        find_match (bindings m)

  let sym_find_default k m ~default =
    let open Delayed.Syntax in
    let+ res = sym_find_opt k m in
    match res with
    | Some (k, v) -> (k, v)
    | None -> (k, default ())

  let sym_find_res k m ~err =
    let open Delayed.Syntax in
    let+ res = sym_find_opt k m in
    match res with
    | Some (k, v) -> Ok (k, v)
    | None -> Error err

  (** Symbolically composes a map with a list of entries, composing entries when
      they are found to match. *)
  let sym_compose
      (compose : 'a -> 'a -> 'a Delayed.t)
      (l : (Expr.t * 'a) list)
      (m : 'a t) : 'a t Delayed.t =
    let open Delayed.Syntax in
    let compose_binding m (k, v) =
      let* m = m in
      let* r = sym_find_opt k m in
      match r with
      | Some (k', v') ->
          let+ v'' = compose v v' in
          add k' v'' m
      | None -> Delayed.return (add k v m)
    in
    List.fold_left compose_binding (Delayed.return m) l

  let sym_merge compose m1 m2 = sym_compose compose (bindings m2) m1
  let make_pp pp_v = pp_bindings ~pp_k:Expr.pp ~pp_v iter
end

module ExpMap = ExpMapMake (struct
  let check = Delayed.branch_on
end)

module ExpMapEnt = ExpMapMake (struct
  let check = Delayed.if_sure
end)

let pp_opt pp_v fmt = function
  | Some v -> Format.fprintf fmt "Some %a" pp_v v
  | None -> Format.pp_print_string fmt "None"

let deep_map f = List.map @@ List.map f

(** Faster than Delayed.resolve_loc, attempts to resolve a location. This may
    result in extending the path condition. Returns None if the input can
    definitely not be a location. *)
let get_loc =
  let open Delayed.Syntax in
  let open Delayed_option in
  function
  | Expr.Lit (Loc loc) -> some loc
  | Expr.ALoc loc -> some loc
  | e when not (Expr.is_concrete e) -> (
      let* loc = Delayed.resolve_loc e in
      match loc with
      | Some loc -> some loc
      | None ->
          let open Expr.Infix in
          let loc_name = ALoc.alloc () in
          some ~learned:[ e == ALoc loc_name ] loc_name)
  | _ -> none ()

module SMap = Gillian.Utils.Prelude.Map.Make (struct
  include String

  let of_yojson = function
    | `String s -> Ok s
    | _ -> Error "string_of_yojson: expected string"

  let to_yojson s = `String s
end)

let bind_vanish_on_err (x : ('a, 'e) result Delayed.t) (f : 'a -> 'b Delayed.t)
    : 'b Delayed.t =
  Delayed.bind x (function
    | Ok x -> f x
    | Error _ -> Delayed.vanish ())

module Syntax = struct
  let ( let*? ) = bind_vanish_on_err
end
