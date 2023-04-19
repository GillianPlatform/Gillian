(** @canonical Gillian.Utils.Syntaxes

  Syntax-extending helper functions for [Result] and [Option] *)

(** @canonical Gillian.Utils.Syntaxes.Result *)
module Result = struct
  let ( let+ ) f o = Result.map o f
  let ( let* ) o f = Result.bind o f

  (** Bind error *)
  let ( let- ) o f =
    match o with
    | Ok _ as ok -> ok
    | Error e -> f e
end

(** @canonical Gillian.Utils.Syntaxes.Option *)
module Option = struct
  let ( let+ ) f o = Option.map o f
  let ( let* ) o f = Option.bind o f
end

(** @canonical Gillian.Utils.Syntaxes.List *)
module List = struct
  let ( let+ ) x f = List.map f x
  let ( let* ) x f = List.concat_map f x
end
