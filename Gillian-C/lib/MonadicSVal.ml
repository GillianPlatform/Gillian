include SVal
open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

exception NotACompCertValue of Expr.t

module Patterns = struct
  open Formula.Infix

  let int_typ, float_typ, single_typ, long_typ =
    let open Expr in
    let open CConstants.VTypes in
    let num_typ typ_str x =
      (typeof x) #== (type_ ListType)
      #&& ((list_length x) #== (num 2.))
      #&& ((list_nth x 0) #== (string typ_str))
      #&& ((typeof (list_nth x 1)) #== (type_ NumberType))
    in
    ( num_typ int_type,
      num_typ float_type,
      num_typ single_type,
      num_typ long_type )

  let undefined x = x #== (Expr.Lit Undefined)

  let obj x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (num 2.))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ NumberType))
end

let of_gil_expr sval_e =
  let open Formula.Infix in
  let open Patterns in
  Logging.verbose (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
  match%ent sval_e with
  | undefined  -> DO.some SUndefined
  | obj        ->
      let loc_expr = Expr.list_nth sval_e 0 in
      let ofs = Expr.list_nth sval_e 1 in
      let* loc_opt = Delayed.resolve_loc loc_expr in
      let loc, learned =
        match loc_opt with
        | Some l -> (l, [])
        | None   ->
            let aloc = ALoc.alloc () in
            let learned = [ loc_expr #== (ALoc aloc) ] in
            (aloc, learned)
      in
      DO.some ~learned (Sptr (loc, ofs))
  | int_typ    -> DO.some (SVint (Expr.list_nth sval_e 1))
  | float_typ  -> DO.some (SVfloat (Expr.list_nth sval_e 1))
  | long_typ   -> DO.some (SVlong (Expr.list_nth sval_e 1))
  | single_typ -> DO.some (SVsingle (Expr.list_nth sval_e 1))
  | _          -> DO.none ()

let of_gil_expr_exn sval_e =
  let* value_opt = of_gil_expr sval_e in
  match value_opt with
  | None       -> raise (NotACompCertValue sval_e)
  | Some value -> Delayed.return value

let to_gil_expr sval =
  let exp, typings = to_gil_expr sval in
  let typing_pfs =
    List.map
      (fun (e, t) ->
        let open Expr in
        let open Formula.Infix in
        (typeof e) #== (type_ t))
      typings
  in
  Delayed.return ~learned:typing_pfs exp
