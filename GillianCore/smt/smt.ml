open Gil_syntax
open Simple_smt
open Syntaxes.Option
open Type

type typenv = (string, Type.t) Hashtbl.t
type model = unit  (* TODO *)

module Type = struct
  let to_atom t = atom (str t)

  let typedef = all |> List.map (fun t -> declare_sort (str t) 0)
end
let ( >- ) expr typ = as_type expr (Type.to_atom typ)

let encoding_cache : (Formula.Set.t, sexp) Hashtbl.t =
  Hashtbl.create Utils.Config.big_tbl_size

let setup () = failwith "TODO"

let rec encode_assertion ~gamma ~llen_lvars (a : Formula.t) : sexp =
  let f = encode_assertion ~gamma ~llen_lvars in
  let fe = encode_logical_expression ~gamma ~llen_lvars in
  match a with
  | Not a -> bool_not (get_bool (f a)) >- BooleanType
  | Eq (leq, lew) -> failwith "TODO"
  | FLess (le1, le2) -> failwith "TODO"
  | FLessEq (le1, le2) -> failwith "TODO"
  | ILess (le1, le2) -> failwith "TODO"
  | ILessEq (leq, le2) -> failwith "TODO"
  | Impl (a1, a2) -> failwith "TODO"
  | StrLess (_, _) -> failwith "TODO"
  | True -> failwith "TODO"
  | False -> failwith "TODO"
  | Or (a1, a2) -> failwith "TODO"
  | And (a1, a2) -> failwith "TODO"
  | SetMem (le1, le2) -> failwith "TODO"
  | SetSub (le1, le2) -> failwith "TODO"
  | ForAll (bt, a) -> failwith "TODO"
  | IsInt e -> failwith "TODO"

and encode_logical_expression ~gamma ~llen_lvars (e : Expr.t) : sexp =
  failwith "TODO"

let encode_assertion_top_level
    ~(gamma : typenv)
    ~(llen_lvars : SS.t)
    (a : Formula.t) : sexp =
  try (encode_assertion ~gamma ~llen_lvars (Formula.push_in_negations a)).expr
  with Z3.Error s as exn ->
    let msg =
      Fmt.str "Failed to encode %a in gamma %a with error %s\n" Formula.pp a
        pp_tyenv gamma s
    in
    Logging.print_to_all msg;
    raise exn

(** Gets the list of lvar names that are only used in llen *)
let lvars_only_in_llen (assertions : Formula.Set.t) : SS.t =
  let inspector =
    object
      inherit [_] Visitors.iter as super
      val mutable llen_vars = SS.empty
      val mutable other_vars = SS.empty
      method get_diff = SS.diff llen_vars other_vars

      method! visit_expr () e =
        match e with
        | UnOp (UnOp.LstLen, Expr.LVar l) -> llen_vars <- SS.add l llen_vars
        | LVar l -> other_vars <- SS.add l other_vars
        | _ -> super#visit_expr () e
    end
  in
  assertions |> Formula.Set.iter (inspector#visit_formula ());
  inspector#get_diff

  (** For a given set of pure formulae and its associated gamma, return the corresponding encoding *)
let encode_assertions (assertions : Formula.Set.t) (gamma : typenv) : sexp list =
  (* Check if the assertions have previously been cached *)
  let- () = Hashtbl.find_opt encoding_cache assertions in
  let llen_lvars = lvars_only_in_llen assertions in
  (* Encode assertions *)
  let encoded_assertions =
    List.map
      (encode_assertion_top_level ~gamma ~llen_lvars)
      (Formula.Set.elements assertions)
  in
  (* Cache *)
  let () = Hashtbl.replace encoding_cache assertions encoded_assertions in
  encoded_assertions

let check_sat_nocache (fs : Formula.Set.t) (gamma : typenv) : model option =
  let encoded_assertions = encode_assertions fs gamma in
  ignore encoded_assertions;
  failwith "TODO"

let is_sat (fs : Formula.Set.t) (gamma : typenv) : bool =
  ignore (fs, gamma);
  failwith "TODO"

let check_sat (fs : Formula.Set.t) (gamma : typenv) : bool * model option =
  ignore (fs, gamma);
  failwith "TODO"