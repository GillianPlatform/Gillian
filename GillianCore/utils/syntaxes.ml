(** @canonical Gillian.Utils.Syntaxes

  Syntax-extending helper functions for [Result] and [Option] *)

(** @canonical Gillian.Utils.Result *)
module Result = struct
  let ( let+ ) f o = Result.map o f
  let ( let* ) o f = Result.bind o f
end

(** @canonical Gillian.Utils.Option *)
module Option = struct
  let ( let+ ) f o = Option.map o f
  let ( let* ) o f = Option.bind o f
end
