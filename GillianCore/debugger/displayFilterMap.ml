module type S = sig
  (** Filter maps the store *)
  val filter_map_store : (string * Expr.t) list -> (string * string) list
end

(** Default filter map which simply converts the values to pretty printed
    strings. No filtering is applied. *)
module Default = struct
  let filter_map_store store =
    List.map
      (fun (var, value) ->
        let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
        (var, value))
      store
end
