let should_be_filtered = function
  (* The next 4 names contain arrays of size infinity.
     We don't handle that for now, we don't know if that's necessary *)
  | "__CPROVER_thread_key_dtors"
  | "__CPROVER_thread_keys"
  | "__CPROVER_memory"
  | "__CPROVER_threads_exited" -> true
  (* We hijack the __CPROVER_initialize function to do our own initialization.
     Plus, it initializes the symbols above that contain arrays of size infinity and
     that is not working well. *)
  | "__CPROVER_initialize" -> true
  | _ -> false

module Global_var = struct
  type t = {
    type_ : Type.t;
    symbol : string;
    value : Expr.t option;
    location : Location.t;
  }
end

module Func = struct
  type t = {
    params : Param.t list;
    body : Stmt.t option;
        (** If the body is empty,
            it means that the function is not defined,
            and it should return nondet *)
    return_type : Type.t;
    location : Location.t;
    symbol : string;
  }
end

type t = {
  vars : (string, Global_var.t) Hashtbl.t;
  funs : (string, Func.t) Hashtbl.t;
  types : (string, Type.t) Hashtbl.t;
  constrs : (string, unit) Hashtbl.t;
}

let of_symtab ~machine (symtab : Symtab.t) : t =
  let env =
    {
      vars = Hashtbl.create 1;
      funs = Hashtbl.create 1;
      types = Hashtbl.create 1;
      constrs = Hashtbl.create 1;
    }
  in
  symtab
  |> Hashtbl.iter (fun name (sym : Irep_lib.Symbol.t) ->
         (* A bit hacky, not sure which should be kept and which shouldn't... *)
         if should_be_filtered name then ()
         else
           let location = Location.of_irep sym.location in
           let type_ = Type.of_irep ~machine sym.type_ in
           let value = SymbolValue.of_irep ~machine ~type_ sym.value in
           if sym.is_type then Hashtbl.add env.types name type_
           else
             match type_ with
             | Bool ->
                 (* FIXME: We can't write pure bools in memory, so let's ignore these for now
                    A solution would be to keep track of those, and convert them
                    to and from u8 every time *)
                 ()
             | Code { params; return_type } ->
                 let body =
                   match value with
                   | SVNone -> None
                   | Stmt s -> Some s
                   | Expr _ ->
                       Gerror.unexpected "function body is not a statment"
                 in
                 let func =
                   Func.{ symbol = name; params; return_type; location; body }
                 in
                 let () =
                   match return_type with
                   | Constructor -> Hashtbl.add env.constrs name ()
                   | _ -> ()
                 in
                 Hashtbl.add env.funs name func
             | _ ->
                 if not (sym.is_static_lifetime || String.equal name "return'")
                 then ()
                 else
                   let value =
                     match value with
                     | SVNone -> None
                     | Expr e -> Some e
                     | _ ->
                         Gerror.unexpected "variable value is not an expression"
                   in
                   let var =
                     Global_var.{ symbol = name; type_; value; location }
                   in
                   Hashtbl.add env.vars name var);
  env

let fold_functions f prog acc = Hashtbl.fold f prog.funs acc
let fold_variables f prog acc = Hashtbl.fold f prog.vars acc

let is_zst ~prog ~machine (ty : Type.t) : bool =
  match ty with
  | Bool | Code _ | Constructor | IncompleteStruct _ -> false
  | StructTag t
    when match Hashtbl.find prog.types t with
         | IncompleteStruct _ -> true
         | _ -> false -> false
  | _ ->
      let tag_lookup = Hashtbl.find prog.types in
      (not (Type.Overflow_result.is_overflow_result ~tag_lookup ty))
      && Type.bit_size_of ~machine ~tag_lookup ty == 0
