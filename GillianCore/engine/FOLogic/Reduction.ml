open SVal

(* When reduction fails *)
exception ReductionException of Expr.t * string

module L = Logging
module CStore = Store.Make (CVal.M)

let normalise_cat (f : Expr.t -> Expr.t) (les : Expr.t list) : Expr.t =
  (* Recursively process each catted list and destroy inner LstCats *)
  let nles =
    List.concat
      (List.map
         (fun x ->
           match f x with
           | NOp (LstCat, les) -> les
           | _                 -> [ x ])
         les)
  in
  (* Bring lists of literals together, part 1 *)
  let lsts, nles =
    List.fold_left
      (fun (lsts, ac) nle ->
        match (nle : Expr.t) with
        | Lit (LList lst) -> (lsts @ List.map (fun x -> Expr.Lit x) lst, ac)
        | EList lst       -> (lsts @ lst, ac)
        | _               -> ([], ac @ [ Expr.EList lsts ] @ [ nle ]))
      ([], []) nles
  in
  (* Bring lists of literals together, part 2 *)
  let nles = nles @ [ Expr.EList lsts ] in
  (* Filter out empty lists *)
  let nles =
    List.filter
      (fun (x : Expr.t) ->
        match x with
        | Lit (LList []) | EList [] -> false
        | _                         -> true)
      nles
  in
  let result : Expr.t =
    match nles with
    | []    -> EList []
    | [ x ] -> x
    | _     -> NOp (LstCat, nles)
  in
  if result <> NOp (LstCat, les) then
    L.verboser (fun fmt ->
        fmt "NormCat: %a -> %a" Expr.pp (NOp (LstCat, les)) Expr.pp result);
  result

(** Rewrite logical expressions involving lists as:
      n_list ::= {{ E_1,  ..., E_n }} @ E | {{ E_1, ..., E_n }}
  where E is not of the form {{ E_1,  ..., E_n }}
 **)
let rec normalise_list_expressions (le : Expr.t) : Expr.t =
  let f = normalise_list_expressions in

  let result =
    match (le : Expr.t) with
    (* Literals **)
    | Lit (LList lst) -> Expr.from_lit_list (LList lst)
    (* Literals, variables, alocs *)
    | Lit _ | LVar _ | ALoc _ | PVar _ -> le
    (* Binary Operators **)
    | NOp (LstCat, les) -> normalise_cat f les
    | BinOp (le, LstNth, n) -> (
        match (f le, f n) with
        | EList lst, Lit (Num n) -> (
            try List.nth lst (int_of_float n)
            with Failure _ ->
              raise
                (ReductionException
                   (BinOp (le, LstNth, Lit (Num n)), "Invalid List Expression"))
            )
        | NOp (LstCat, EList lst :: _), Lit (Num n)
          when n < float_of_int (List.length lst) -> (
            try List.nth lst (int_of_float n)
            with Failure _ ->
              raise
                (ReductionException
                   (BinOp (le, LstNth, Lit (Num n)), "Invalid List Expression"))
            )
        | NOp (LstCat, EList lst :: tl), Lit (Num n)
          when n >= float_of_int (List.length lst) ->
            BinOp
              ( NOp (LstCat, tl),
                LstNth,
                Lit (Num (n -. float_of_int (List.length lst))) )
        | le, n -> BinOp (le, LstNth, n) )
    | BinOp (le1, op, le2) -> BinOp (f le1, op, f le2)
    (* Unary Operators **)
    | UnOp (Car, lst) -> (
        match f lst with
        | EList lst -> (
            try List.hd lst
            with Failure _ ->
              raise
                (ReductionException
                   (UnOp (Car, EList lst), "Invalid List Expression")) )
        | NOp (LstCat, EList lst_l :: _) -> (
            try List.hd lst_l
            with Failure _ ->
              raise
                (ReductionException (UnOp (Car, lst), "Invalid List Expression"))
            )
        | lst -> UnOp (Car, lst) )
    | UnOp (Cdr, lst) -> (
        match f lst with
        | EList lst -> (
            try EList (List.tl lst)
            with Failure _ ->
              raise
                (ReductionException
                   (UnOp (Cdr, EList lst), "Invalid List Expression")) )
        | NOp (LstCat, EList lst_l :: tl) -> (
            try NOp (LstCat, EList (List.tl lst_l) :: tl)
            with Failure _ ->
              raise
                (ReductionException (UnOp (Cdr, lst), "Invalid List Expression"))
            )
        | lst -> UnOp (Cdr, lst) )
    | UnOp (LstLen, le) -> (
        match f le with
        | EList lst -> Lit (Num (float_of_int (List.length lst)))
        | NOp (LstCat, EList lst :: tl) ->
            BinOp
              ( Lit (Num (float_of_int (List.length lst))),
                FPlus,
                f (UnOp (LstLen, NOp (LstCat, tl))) )
        | le -> UnOp (LstLen, le) )
    | UnOp (op, le) -> UnOp (op, f le)
    | NOp (op, les) -> NOp (op, List.map f les)
    (* Uninteresting cases **)
    | EList lst -> EList (List.map f lst)
    | ESet lst -> ESet (List.map f lst)
    | LstSub (le1, le2, le3) -> LstSub (f le1, f le2, f le3)
    (*
    | LstSub(le1, le2, le3) ->
      (match f le1, f le2, f le3 with
      | EList lst, Lit (Num _start), Lit (Num _end) ->
          (try EList (Array.to_list (Array.sub (Array.of_list lst) (int_of_float _start) (int_of_float _end)))
            with _ -> raise (ReductionException (LstSub(le1, le2, le3), "Invalid List Expression")))
      | Lit (LList lst), Lit (Num _start), Lit (Num _end) ->
          (try Lit (LList (Array.to_list (Array.sub (Array.of_list lst) (int_of_float _start) (int_of_float _end))))
            with _ -> raise (ReductionException (LstSub(le1, le2, le3), "Invalid List Expression")))
      | le1, le2, le3 -> LstSub(le1, le2, le3)
      )
    *)
  in

  result

(*  -----------------------------------------------------
  Resolving locations and lists
  -----------------------------------------------------
  _____________________________________________________
*)

let resolve_list (le : Expr.t) (pfs : Formula.t list) : Expr.t =
  let rec search x pfs =
    match (pfs : Formula.t list) with
    | [] -> None
    | Eq (LVar x', le) :: rest when x' = x -> (
        let le' = normalise_list_expressions le in
        match le' with
        | EList _ | NOp (LstCat, _) -> Some le'
        | _                         -> search x rest )
    | Eq (le, LVar x') :: rest when x' = x -> (
        let le' = normalise_list_expressions le in
        match le' with
        | EList _ | NOp (LstCat, _) -> Some le'
        | _                         -> search x rest )
    | _ :: rest -> search x rest
  in

  match normalise_list_expressions le with
  | LVar x -> (
      match search x pfs with
      | Some le -> le
      | None    -> LVar x )
  | le     -> le

let reshape_list (le_list : Expr.t) (len : int) : Expr.t list * Expr.t =
  match le_list with
  | EList lst ->
      let lst_l = Array.to_list (Array.sub (Array.of_list lst) 0 len) in
      let lst_r =
        Array.to_list
          (Array.sub (Array.of_list lst) len (List.length lst - len))
      in
      (lst_l, EList lst_r)
  | NOp (LstCat, EList lst_l :: lst_r) ->
      let lst_l' = Array.to_list (Array.sub (Array.of_list lst_l) 0 len) in
      let lst_l'' =
        Array.to_list
          (Array.sub (Array.of_list lst_l) len (List.length lst_l - len))
      in
      if List.length lst_l'' > 0 then
        (lst_l', NOp (LstCat, EList lst_l'' :: lst_r))
      else (lst_l', NOp (LstCat, lst_r))
  | _ -> raise (Failure "DEATH: List could not be reshaped")

let resolve_var_to_location (lvar : string) (pfs : Formula.t list) :
    (string * SSubst.t) option =
  let original_pfs =
    List.map
      (fun (a : Formula.t) ->
        ( match (a : Formula.t) with
          | Eq (le1, le2) -> (
              let le1' = normalise_list_expressions le1 in
              match (le1' : Expr.t) with
              | EList _ | NOp (LstCat, _) -> Eq (le1', le2)
              | _                         ->
                  let le2' = normalise_list_expressions le2 in
                  Eq (le1', le2') )
          | _             -> a
          : Formula.t ))
      pfs
  in

  let subst = SSubst.init [] in

  let rec shallow_loop pfs traversed_pfs found_other_bindings =
    match (pfs : Formula.t list) with
    | [] -> (None, found_other_bindings)
    | Eq (LVar cur_lvar, ALoc loc) :: rest
    | Eq (ALoc loc, LVar cur_lvar) :: rest ->
        if cur_lvar = lvar then (Some loc, found_other_bindings)
        else
          let found_other = SSubst.mem subst cur_lvar in
          SSubst.put subst cur_lvar (ALoc loc);
          shallow_loop rest (List.hd pfs :: traversed_pfs) found_other
    | Eq (LVar cur_lvar, Lit (Loc loc)) :: rest
    | Eq (Lit (Loc loc), LVar cur_lvar) :: rest ->
        if cur_lvar = lvar then (Some loc, found_other_bindings)
        else
          let found_other = SSubst.mem subst cur_lvar in
          SSubst.put subst cur_lvar (Lit (Loc loc));
          shallow_loop rest (List.hd pfs :: traversed_pfs) found_other
    | Eq (le1, le2) :: rest -> (
        match le1 with
        | EList le1_lst | NOp (LstCat, EList le1_lst :: _) -> (
            let le2' = resolve_list le2 (traversed_pfs @ rest) in
            match le2' with
            | EList le2_lst | NOp (LstCat, EList le2_lst :: _) -> (
                let min_len = min (List.length le2_lst) (List.length le1_lst) in
                let le1_lst_l, le1_lst_r = reshape_list le1 min_len in
                let le2_lst_l, le2_lst_r = reshape_list le2' min_len in
                if List.length le1_lst_l <> List.length le2_lst_l then
                  raise (Failure "DEATH")
                else
                  match
                    shallow_loop_lists le1_lst_l le2_lst_l found_other_bindings
                  with
                  | None, new_found_other_bindings ->
                      shallow_loop rest
                        (List.hd pfs :: traversed_pfs)
                        new_found_other_bindings
                  | Some loc, new_found_other_bindings ->
                      (Some loc, new_found_other_bindings) )
            | _ ->
                shallow_loop rest
                  (List.hd pfs :: traversed_pfs)
                  found_other_bindings )
        | _ ->
            shallow_loop rest
              (List.hd pfs :: traversed_pfs)
              found_other_bindings )
    | _ :: rest ->
        shallow_loop rest (List.hd pfs :: traversed_pfs) found_other_bindings
  and shallow_loop_lists lst_1 lst_2 found_other_bindings =
    shallow_loop
      (List.map2
         (fun (le1 : Expr.t) (le2 : Expr.t) -> (Eq (le1, le2) : Formula.t))
         lst_1 lst_2)
      [] found_other_bindings
  in

  let rec loop pfs =
    match shallow_loop pfs [] false with
    | Some loc, _ -> Some (loc, subst)
    | None, false -> None
    | None, true  ->
        loop (List.map (SSubst.substitute_formula subst ~partial:true) pfs)
  in

  loop original_pfs

let resolve_expr_to_location (pfs : Formula.t list) (e : Expr.t) :
    (string * SSubst.t) option =
  L.(
    verboser (fun m ->
        m "resolve_expr: %s with pfs:\n%s\n"
          ((Fmt.to_to_string Expr.pp) e)
          (String.concat "\n" (List.map (Fmt.to_to_string Formula.pp) pfs))));
  let ac_var, ac_loc =
    List.fold_left
      (fun (ac_var, ac_loc) fo ->
        match ac_loc with
        | Some _ -> (ac_var, ac_loc)
        | None   -> (
            match (fo : Formula.t) with
            | Eq (LVar x, e') when e' = e -> (Some x, None)
            | Eq (e', LVar x) when e' = e -> (Some x, None)
            | Eq (ALoc l, e') when e' = e -> (ac_var, Some l)
            | Eq (Lit (Loc l), e') when e' = e -> (ac_var, Some l)
            | Eq (e', ALoc l) when e' = e -> (ac_var, Some l)
            | Eq (e', Lit (Loc l)) when e' = e -> (ac_var, Some l)
            | _ ->
                L.(
                  verboser (fun m ->
                      m "false with: %s\n" ((Fmt.to_to_string Formula.pp) fo)));
                (ac_var, ac_loc) ))
      (None, None) pfs
  in

  match (ac_var, ac_loc) with
  | _, Some loc -> Some (loc, SSubst.init [])
  | Some x, _   -> resolve_var_to_location x pfs
  | _           -> None

(***************************)
(* TYPING HELPER FUNCTIONS *)
(***************************)

let typable (gamma : TypEnv.t) (le : Expr.t) (target_type : Type.t) : bool =
  let t, success, _ = Typing.type_lexpr gamma le in
  if success then
    Option.fold
      ~some:(fun t -> t = target_type)
      ~none:
        ( match le with
        | LVar _ | PVar _ -> true
        | _               -> false )
      t
  else
    let msg : string =
      Fmt.str "TYPE ERROR: %a not typable in typing environment %a" Expr.pp le
        TypEnv.pp gamma
    in
    L.fail msg

(* Lists *)
let lexpr_is_list (gamma : TypEnv.t) (le : Expr.t) : bool =
  typable gamma le ListType

(* Strings *)
let lexpr_is_string (gamma : TypEnv.t) (le : Expr.t) : bool =
  typable gamma le StringType

(* Numbers *)
let lexpr_is_number ?(gamma = TypEnv.init ()) (le : Expr.t) : bool =
  typable gamma le NumberType

(* Booleans *)
let lexpr_is_bool (gamma : TypEnv.t) (le : Expr.t) : bool =
  typable gamma le BooleanType

(* Sets *)
let lexpr_is_set (gamma : TypEnv.t) (le : Expr.t) : bool =
  typable gamma le SetType

let get_equal_expressions (pfs : PFS.t) nle =
  PFS.fold_left
    (fun ac a ->
      match (a : Formula.t) with
      | Eq (le1, le2) when le1 = nle -> le2 :: ac
      | Eq (le2, le1) when le1 = nle -> le2 :: ac
      | _ -> ac)
    [] pfs

(**********************************)
(* Pure formulae helper functions *)
(**********************************)

let find_first_equality_in_pfs (pfs : PFS.t) (le : Expr.t) : Expr.t option =
  let lpfs = PFS.to_list pfs in
  let lpfs =
    List.find_opt
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> x = le || y = le
        | _                 -> false)
      lpfs
  in
  let result =
    Option.map
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> if x = le then y else x
        | _                 ->
            raise
              (Exceptions.Impossible
                 "find_first_equality_in_pfs: guarantee by match/filter"))
      lpfs
  in
  result

(***********************************)
(* LIST REASONING HELPER FUNCTIONS *)
(***********************************)

(* Finding the length of a list *)
let rec get_length_of_list (lst : Expr.t) : int option =
  let f = get_length_of_list in

  match lst with
  | PVar _            -> None
  | LVar _            -> None
  | Lit (LList l)     -> Some (List.length l)
  | EList l           -> Some (List.length l)
  | NOp (LstCat, les) -> (
      let lens = List.map f les in
      match List.exists (fun x -> x = None) lens with
      | true  -> None
      | false ->
          let lens = List.map Option.get lens in
          let lens = List.fold_left (fun ac x -> ac + x) 0 lens in
          Some lens )
  | _                 ->
      raise
        (Failure
           (Printf.sprintf "get_length_of_list: list equals %s, impossible"
              ((Fmt.to_to_string Expr.pp) lst)))

(* Finding the nth element of a list *)
let rec get_nth_of_list (pfs : PFS.t) (lst : Expr.t) (idx : int) : Expr.t option
    =
  let f = get_nth_of_list pfs in

  let err_msg = "get_nth_of_list: index out of bounds." in

  (* If we can compute the length of the list, then the index needs to be compatible *)
  let olen = get_length_of_list lst in
  let _ =
    match olen with
    | None     -> ()
    | Some len ->
        if len <= idx then raise (ReductionException (Lit Nono, err_msg))
  in

  match lst with
  (* Nothing can be done for variables *)
  | PVar x | LVar x ->
      let lst' = resolve_list lst (PFS.to_list pfs) in
      if lst = lst' then None else f lst' idx
  (* Base lists of literals and logical expressions *)
  | Lit (LList l) ->
      assert (idx < List.length l);
      Some (Lit (List.nth l idx))
  | EList l ->
      assert (idx < List.length l);
      Some (List.nth l idx)
  | NOp (LstCat, lel :: ler) ->
      Option.value ~default:None
        (Option.map
           (fun llen ->
             let lst, idx =
               if idx < llen then (lel, idx) else (NOp (LstCat, ler), idx - llen)
             in
             f lst idx)
           (get_length_of_list lel))
  | _ ->
      raise
        (Failure
           (Printf.sprintf "get_nth_of_list: list equals %s, impossible"
              ((Fmt.to_to_string Expr.pp) lst)))

(* Finding the nth element of a list *)
let rec get_head_and_tail_of_list
    (pfs : PFS.t) (unacceptable : Expr.Set.t) (lst : Expr.t) :
    (Expr.t * Expr.t) option =
  let f = get_head_and_tail_of_list pfs unacceptable in

  match lst with
  (* Nothing can be done for variables *)
  | PVar _ -> None
  | LVar _ -> (
      let ole = find_first_equality_in_pfs pfs lst in
      match ole with
      | None -> None
      | Some le when Expr.Set.mem le unacceptable -> None
      | Some le ->
          get_head_and_tail_of_list pfs (Expr.Set.add lst unacceptable) le )
  (* Base lists of literals and logical expressions *)
  | Lit (LList l) ->
      if l = [] then None else Some (Lit (List.hd l), Lit (LList (List.tl l)))
  | EList l -> if l = [] then None else Some (List.nth l 0, EList (List.tl l))
  | NOp (LstCat, lel :: ler) ->
      Option.value ~default:None
        (Option.map
           (fun (hd, tl) -> Some (hd, Expr.NOp (LstCat, tl :: ler)))
           (f lel))
  | _ -> None

(*************************************)
(* STRING REASONING HELPER FUNCTIONS *)
(*************************************)

(* Finding the length of a string *)
let rec get_length_of_string (str : Expr.t) : int option =
  let f = get_length_of_string in

  match str with
  | PVar _                 -> None
  | LVar _                 -> None
  | Lit (String s)         -> Some (String.length s)
  | BinOp (sl, StrCat, sr) ->
      Option.value ~default:None
        (Option.map (fun ll -> Option.map (fun lr -> ll + lr) (f sr)) (f sl))
  | _                      ->
      raise
        (Failure
           (Printf.sprintf "get_length_of_string: string equals %s, impossible"
              ((Fmt.to_to_string Expr.pp) str)))

(* Finding the nth element of a list *)
let rec get_nth_of_string (str : Expr.t) (idx : int) : Expr.t option =
  let f = get_nth_of_string in

  let err_msg = "get_nth_of_string: index out of bounds." in

  (* If we can compute the length of the list, then the index needs to be compatible *)
  let olen = get_length_of_string str in
  let _ =
    match olen with
    | None     -> ()
    | Some len ->
        if len <= idx then raise (ReductionException (Lit Nono, err_msg))
  in

  let result : Expr.t option =
    match str with
    (* Nothing can be done for variables *)
    | PVar _ -> None
    | LVar _ -> None
    (* Base lists of literals and logical expressions *)
    | Lit (String s) ->
        assert (idx < String.length s);
        Some (Lit (String (String.sub s idx 1)))
    | BinOp (ls, StrCat, rs) ->
        Option.value ~default:None
          (Option.map
             (fun llen ->
               let lst, idx =
                 if idx < llen then (ls, idx) else (rs, idx - llen)
               in
               f lst idx)
             (get_length_of_string ls))
    | _ ->
        raise
          (Failure
             (Printf.sprintf "get_nth_of_string: string equals %s, impossible"
                ((Fmt.to_to_string Expr.pp) str)))
  in
  result

(**********************************)
(* SET REASONING HELPER FUNCTIONS *)
(**********************************)

let is_different (pfs : Formula.t list) (li : Expr.t) (lj : Expr.t) :
    bool option =
  match li = lj with
  | true  -> Some false
  | false -> (
      match (li, lj) with
      | Expr.Lit x, Expr.Lit y when x <> y -> Some true
      | _, _ ->
          if
            List.mem (Formula.Not (Formula.Eq (li, lj))) pfs
            || List.mem (Formula.Not (Formula.Eq (lj, li))) pfs
          then Some true
          else None )

(* I dont understand this! *)
let rec set_member (pfs : Formula.t list) m s =
  let f = set_member pfs m in
  match s with
  | Expr.LVar x              -> m = s
  | Expr.ESet s              -> List.mem m s
  | Expr.NOp (SetUnion, les) -> List.exists (fun x -> f x) les
  | Expr.NOp (SetInter, les) -> List.for_all (fun x -> f x) les
  | _                        -> List.mem (Formula.SetMem (m, s)) pfs

let rec not_set_member pfs m s =
  let f = not_set_member pfs m in
  match s with
  | Expr.NOp (SetUnion, les) -> List.for_all (fun x -> f x) les
  | Expr.NOp (SetInter, les) -> List.exists (fun x -> f x) les
  | Expr.ESet les            ->
      List.for_all (fun le -> is_different pfs m le = Some true) les
  | _                        -> List.mem
                                  (Formula.Not (Formula.SetMem (m, s)))
                                  pfs

let rec set_subset pfs s s' =
  let f = set_subset pfs s in
  match s' with
  | Expr.LVar _              -> s = s'
  | Expr.NOp (SetUnion, les) -> List.exists (fun x -> f x) les
  | Expr.NOp (SetInter, les) -> List.for_all (fun x -> f x) les
  | _                        -> (
      match s with
      | Expr.ESet les -> List.for_all (fun x -> set_member pfs x s') les
      | _             -> false )

let rec contained_in_union (pfs : Formula.t list) (le1 : Expr.t) (le2 : Expr.t)
    =
  L.(
    tmi (fun m ->
        m "Contained in union: %s %s"
          ((Fmt.to_to_string Expr.pp) le1)
          ((Fmt.to_to_string Expr.pp) le2)));
  match le2 with
  | LVar _ -> (
      match pfs with
      | [] -> false
      | Eq (le, NOp (SetUnion, les)) :: rest when le = le2 ->
          if List.mem le1 les then true else contained_in_union rest le1 le2
      | _ :: rest -> contained_in_union rest le1 le2 )
  | _      -> false

let all_different pfs les =
  let result = ref true in
  let len = List.length les in
  let les = Array.of_list les in
  let i = ref 0 in
  while !result && !i < len - 1 do
    let j = ref (!i + 1) in
    while !result && !j < len do
      let li, lj = (les.(!i), les.(!j)) in
      if is_different pfs li lj <> Some true then result := false;
      j := !j + 1
    done;
    i := !i + 1
  done;
  !result

(* Is the list la a prefix of the list lb? *)
let rec list_prefix (pfs : PFS.t) (la : Expr.t) (lb : Expr.t) : bool * Expr.t =
  let f = list_prefix pfs in
  let la =
    match la with
    | NOp (LstCat, [ x ]) -> x
    | _                   -> la
  in
  let lb =
    match la with
    | NOp (LstCat, [ x ]) -> x
    | _                   -> lb
  in
  (* L.verboser (fun fmt -> fmt "List prefix: %a of %a" Expr.pp la Expr.pp lb); *)
  let nono = (false, Expr.Lit Nono) in
  if la = lb then (true, EList [])
  else
    match (la, lb) with
    (* Direct prefix - trivial *)
    | la, NOp (LstCat, x :: y) when la = x -> (true, NOp (LstCat, y))
    (* EList, EList - trivial *)
    | EList la, EList lb ->
        let lla = List.length la in
        let llb = List.length lb in
        if lla > llb then nono
        else
          let prefix = Array.to_list (Array.sub (Array.of_list lb) 0 lla) in
          let suffix =
            Array.to_list (Array.sub (Array.of_list lb) lla (llb - lla))
          in
          if la = prefix then (true, EList suffix) else nono
    | EList la, NOp (LstCat, x :: y) -> (
        match list_prefix pfs (EList la) x with
        | true, suffix -> (true, NOp (LstCat, suffix :: y))
        | _            -> nono )
    | NOp (LstCat, x :: y), lb -> (
        match f x lb with
        | false, _     -> nono
        | true, suffix -> f (NOp (LstCat, y)) suffix )
    | LVar a, lb -> (
        let lvars_lb = Expr.lvars lb in
        let candidates = get_equal_expressions pfs la in
        let candidates =
          List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
        in
        let candidates =
          List.filter
            (fun (x, lvars) ->
              Containers.SS.inter lvars_lb lvars <> Containers.SS.empty)
            candidates
        in
        match candidates with
        | [ (x, _) ] -> f x lb
        (* Get more general equalities *)
        | _ -> (
            let candidates = get_equal_expressions pfs lb in
            let candidates =
              List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
            in
            let candidates =
              List.filter
                (fun (x, lvars) -> Containers.SS.mem a lvars)
                candidates
            in
            match candidates with
            | [ (x, _) ] -> f (LVar a) x
            | _          -> nono ) )
    | _, _ -> nono

(*************)
(* REDUCTION *)
(*************)

(**
  Reduction of logical expressions

  - gamma is used for:
  - pfs  are used for: Car, Cdr, SetDiff
*)
let rec reduce_lexpr_loop
    ?(unification = false)
    ?(reduce_lvars = false)
    (pfs : PFS.t)
    (gamma : TypEnv.t)
    (le : Expr.t) =
  let f = reduce_lexpr_loop ~unification ~reduce_lvars pfs gamma in
  let result : Expr.t =
    match le with
    | BinOp (Lit (LList ll), Equal, Lit (LList lr)) -> Lit (Bool (ll = lr))
    | BinOp (EList le, Equal, Lit (LList ll))
    | BinOp (Lit (LList ll), Equal, EList le) ->
        if List.length ll <> List.length le then Lit (Bool false)
        else if ll = [] then Lit (Bool true)
        else
          let eqs = List.map2 (fun x y -> Expr.BinOp (x, Equal, Lit y)) le ll in
          let conj =
            List.fold_left
              (fun ac x -> Expr.BinOp (ac, BAnd, x))
              (List.hd eqs) (List.tl eqs)
          in
          f conj
    | BinOp (EList ll, Equal, EList lr) ->
        if List.length ll <> List.length lr then Lit (Bool false)
        else if ll = [] then Lit (Bool true)
        else
          let eqs = List.map2 (fun x y -> Expr.BinOp (x, Equal, y)) ll lr in
          let conj =
            List.fold_left
              (fun ac x -> Expr.BinOp (ac, BAnd, x))
              (List.hd eqs) (List.tl eqs)
          in
          f conj
    | BinOp (ALoc x, Equal, ALoc y) when not unification -> Lit (Bool (x = y))
    | LVar x when reduce_lvars -> (
        let equals = get_equal_expressions pfs (LVar x) in
        let lit_equals =
          List.filter
            (fun eq ->
              match eq with
              | Expr.Lit _ -> true
              | _          -> false)
            equals
        in
        match lit_equals with
        | []         -> LVar x
        | Lit l :: _ -> Lit l
        | _          ->
            raise
              (Exceptions.Impossible "reduce_lexpr: guaranteed by match/filter")
        )
    (* Base lists *)
    | EList les -> (
        let fles = List.map f les in
        let all_literals =
          List.for_all
            (fun x ->
              match x with
              | Expr.Lit _ -> true
              | _          -> false)
            fles
        in
        match all_literals with
        | false -> Expr.EList fles
        | true  ->
            let lits =
              List.map
                (fun x ->
                  match x with
                  | Expr.Lit x -> x
                  | _          ->
                      raise
                        (Exceptions.Impossible
                           "reduce_lexpr: guaranteed by match/filter"))
                fles
            in
            Lit (LList lits) )
    (* Base sets *)
    | ESet les -> ESet (Expr.Set.elements (Expr.Set.of_list (List.map f les)))
    (* Number-to-string-to-number-to-string-to... *)
    | UnOp (ToNumberOp, UnOp (ToStringOp, le)) -> (
        let fle = f le in
        match fle with
        | Lit (Num n) -> Lit (Num n)
        | fle         -> (
            let tfle, how, _ = Typing.type_lexpr gamma fle in
            match (how, tfle) with
            | true, Some NumberType -> fle
            | _, _                  -> UnOp (ToNumberOp, UnOp (ToStringOp, fle))
            ) )
    | UnOp (LstRev, UnOp (LstRev, le)) -> f le
    (* Less than and lessthaneq *)
    | UnOp (UNot, BinOp (le1, LessThan, le2)) ->
        f (BinOp (f le2, LessThanEqual, f le1))
    | UnOp (UNot, BinOp (le1, LessThanEqual, le2)) ->
        f (BinOp (f le2, LessThan, f le1))
    (* Special equality *)
    | BinOp
        (BinOp (LVar x, FPlus, UnOp (FUnaryMinus, LVar y)), Equal, Lit (Num 0.))
      -> BinOp (LVar x, Equal, LVar y)
    (* List indexing *)
    | BinOp (le, LstNth, idx) -> (
        let fle = f le in
        let fidx = f idx in
        match fidx with
        (* Index is a non-negative integer *)
        | Lit (Num n) when Arith_Utils.is_int n && 0. <= n -> (
            match lexpr_is_list gamma fle with
            | true  ->
                Option.value
                  ~default:(Expr.BinOp (fle, LstNth, fidx))
                  (get_nth_of_list pfs fle (int_of_float n))
            | false ->
                let err_msg = "LstNth(list, index): list is not a GIL list." in
                raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
            )
        (* Index is a number, but is either not an integer or is negative *)
        | Lit (Num n) ->
            let err_msg =
              "LstNth(list, index): index is non-integer or smaller than zero."
            in
            raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
        (* All other cases *)
        | _ -> BinOp (fle, LstNth, fidx) )
    (* String indexing *)
    | BinOp (le, StrNth, idx) -> (
        let fle = f le in
        let fidx = f idx in
        match fidx with
        (* Index is a non-negative integer *)
        | Lit (Num n) when Arith_Utils.is_int n && 0. <= n -> (
            match lexpr_is_string gamma fle with
            | true  ->
                Option.value
                  ~default:(Expr.BinOp (fle, StrNth, fidx))
                  (get_nth_of_string fle (int_of_float n))
            | false ->
                let err_msg =
                  "StrNth(str, index): string is not a GIL string."
                in
                raise
                  (ReductionException (Expr.BinOp (fle, StrNth, fidx), err_msg))
            )
        (* Index is a number, but is either not an integer or is negative *)
        | Lit (Num n) ->
            let err_msg =
              "StrNth(str, index): index is non-integer or smaller than zero."
            in
            raise (ReductionException (Expr.BinOp (fle, StrNth, fidx), err_msg))
        (* All other cases *)
        | _ -> BinOp (fle, StrNth, fidx) )
    | NOp (SetUnion, les) -> (
        let fles = List.map f les in
        (* Flatten unions *)
        let unions, rest =
          List.partition
            (fun x ->
              match x with
              | Expr.NOp (SetUnion, _) -> true
              | _                      -> false)
            fles
        in
        let unions =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.NOp (SetUnion, ls) -> ls
                | _                       ->
                    raise (Failure "LSetUnion: flattening unions: impossible.")
              in
              ac @ ls)
            [] unions
        in
        let fles = unions @ rest in
        (* Join ESets *)
        let lesets, rest =
          List.partition
            (fun x ->
              match x with
              | Expr.ESet _ -> true
              | _           -> false)
            fles
        in
        let lesets =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.ESet ls -> ls
                | _            -> raise
                                    (Failure
                                       "LSetUnion: joining ESets: impossible.")
              in
              ac @ ls)
            [] lesets
        in
        let lesets = Expr.Set.elements (Expr.Set.of_list lesets) in
        let fles = Expr.ESet lesets :: rest in
        (* Remove empty sets *)
        let fles = List.filter (fun s -> s <> Expr.ESet []) fles in
        (* Remove duplicates *)
        let fles = Expr.Set.elements (Expr.Set.of_list fles) in
        match fles with
        | []    -> ESet []
        | [ x ] -> x
        | _     -> NOp (SetUnion, fles) )
    | NOp (LstCat, les) -> normalise_cat f les
    | NOp (SetInter, [ BinOp (le1, SetDiff, le2); ESet le3 ]) ->
        f (NOp (SetInter, [ le2; BinOp (ESet le3, SetDiff, le1) ]))
    | NOp (SetInter, les) -> (
        let fles = List.map f les in
        (* Flatten intersections *)
        let inters, rest =
          List.partition
            (fun x ->
              match x with
              | Expr.NOp (SetInter, _) -> true
              | _                      -> false)
            fles
        in
        let inters =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.NOp (SetInter, ls) -> ls
                | _                       ->
                    raise
                      (Failure
                         "LSetInter: flattening intersections: impossible.")
              in
              ac @ ls)
            [] inters
        in
        let fles = inters @ rest in
        (* Join ESets *)
        let lesets, rest =
          List.partition
            (fun x ->
              match x with
              | Expr.ESet _ -> true
              | _           -> false)
            fles
        in
        let lesets =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.ESet ls -> ls
                | _            -> raise
                                    (Failure
                                       "LSetUnion: joining ESets: impossible.")
              in
              ac @ ls)
            [] lesets
        in
        let lesets = Expr.Set.elements (Expr.Set.of_list lesets) in
        let fles = Expr.ESet lesets :: rest in
        (* If there is an empty set, the intersection is empty *)
        if List.mem (Expr.ESet []) fles then Expr.ESet []
        else
          let fles = Expr.Set.elements (Expr.Set.of_list fles) in
          match fles with
          | []    -> ESet []
          | [ x ] -> x
          | _     -> NOp (SetInter, fles) )
    | UnOp (op, le) -> (
        let fle = f le in
        let def = Expr.UnOp (op, fle) in
        match fle with
        | Lit lit -> (
            try Lit (CExprEval.evaluate_unop op lit) with
            | CExprEval.TypeError err_msg ->
                raise (ReductionException (def, err_msg))
            | CExprEval.EvaluationError err_msg ->
                raise (ReductionException (def, err_msg))
            | e -> raise e )
        | _       -> (
            match op with
            | UNot -> (
                match fle with
                | UnOp (UNot, ex)      -> f ex
                | BinOp (ex, BAnd, ey) ->
                    f (BinOp (UnOp (UNot, ex), BOr, UnOp (UNot, ey)))
                | BinOp (ex, BOr, ey)  ->
                    f (BinOp (UnOp (UNot, ex), BAnd, UnOp (UNot, ey)))
                | _                    -> def )
            (* The TypeOf operator *)
            | TypeOf -> (
                let tfle, how, _ = Typing.type_lexpr gamma fle in
                match how with
                | false ->
                    let err_msg = "LTypeOf(le): expression is not typable." in
                    raise (ReductionException (def, err_msg))
                | true  -> (
                    match tfle with
                    | None   -> def
                    | Some t -> Lit (Type t) ) )
            (* List head *)
            | Car -> (
                match lexpr_is_list gamma fle with
                | true  ->
                    let ohdtl =
                      get_head_and_tail_of_list pfs Expr.Set.empty fle
                    in
                    Option.fold ~some:(fun (hd, _) -> f hd) ~none:def ohdtl
                | false ->
                    let err_msg = "UnOp(Car, list): list is not a GIL list." in
                    raise (ReductionException (def, err_msg)) )
            (* List tail *)
            | Cdr -> (
                match lexpr_is_list gamma fle with
                | true  ->
                    let ohdtl =
                      get_head_and_tail_of_list pfs Expr.Set.empty fle
                    in
                    Option.fold ~some:(fun (_, tl) -> f tl) ~none:def ohdtl
                | false ->
                    let err_msg = "UnOp(Cdr, list): list is not a GIL list." in
                    raise (ReductionException (def, err_msg)) )
            (* List length *)
            | LstLen -> (
                match lexpr_is_list gamma fle with
                | true  -> (
                    match fle with
                    | Lit (LList le)    ->
                        Lit (Num (float_of_int (List.length le)))
                    | EList le          -> Lit
                                             (Num (float_of_int (List.length le)))
                    | NOp (LstCat, les) ->
                        let les =
                          List.map (fun x -> Expr.UnOp (LstLen, x)) les
                        in
                        let le =
                          List.fold_left
                            (fun ac x -> Expr.BinOp (ac, FPlus, x))
                            (List.hd les) (List.tl les)
                        in
                        f le
                    | _                 -> def )
                | false ->
                    let err_msg =
                      "UnOp(LstLen, list): list is not a GIL list."
                    in
                    raise (ReductionException (def, err_msg)) )
            (* List reverse *)
            | LstRev -> (
                match lexpr_is_list gamma fle with
                | true  -> (
                    match fle with
                    | Lit (LList le)    -> Lit (LList (List.rev le))
                    | EList le          -> EList (List.rev le)
                    | NOp (LstCat, les) ->
                        NOp
                          ( LstCat,
                            List.rev
                              (List.map (fun x -> Expr.UnOp (LstRev, x)) les) )
                    | _                 -> def )
                | false ->
                    let err_msg =
                      "UnOp(LstRev, list): list is not a GIL list."
                    in
                    raise (ReductionException (def, err_msg)) )
            (* List reverse *)
            | SetToList -> (
                match fle with
                | ESet le -> EList (Expr.Set.elements (Expr.Set.of_list le))
                | _       -> def )
            (* String length *)
            | StrLen -> (
                match lexpr_is_string gamma fle with
                | true  ->
                    let len = get_length_of_string fle in
                    Option.fold
                      ~some:(fun len -> Expr.Lit (Num (float_of_int len)))
                      ~none:def len
                | false ->
                    let err_msg =
                      "UnOp(StrLen, list): string is not a GIL string."
                    in
                    raise (ReductionException (def, err_msg)) )
            | FUnaryMinus when lexpr_is_number ~gamma def ->
                simplify_arithmetic_lexpr pfs gamma def
            | _ -> UnOp (op, fle) ) )
    | LstSub (le1, le2, le3) when unification -> (
        let fle1 = f le1 in
        let fle2 = f le2 in
        let fle3 = f le3 in
        match (fle1, fle2, fle3) with
        | NOp (LstCat, EList les :: _), Lit (Num s), Lit (Num l)
          when List.length les >= int_of_float (s +. l) ->
            f (LstSub (EList les, fle2, fle3))
        | NOp (LstCat, EList les :: y), Lit (Num s), fle3
          when List.length les = int_of_float s ->
            f (LstSub (NOp (LstCat, y), Lit (Num 0.), fle3))
        | LVar x, Lit (Num 0.), UnOp (LstLen, LVar y) when y = x -> LVar x
        | EList lst, Lit (Num _start), Lit (Num _end) -> (
            try
              EList
                (Array.to_list
                   (Array.sub (Array.of_list lst) (int_of_float _start)
                      (int_of_float _end)))
            with _ ->
              raise
                (ReductionException
                   (LstSub (le1, le2, le3), "Invalid List Expression")) )
        | Lit (LList lst), Lit (Num _start), Lit (Num _end) -> (
            try
              Lit
                (LList
                   (Array.to_list
                      (Array.sub (Array.of_list lst) (int_of_float _start)
                         (int_of_float _end))))
            with _ ->
              raise
                (ReductionException
                   (LstSub (le1, le2, le3), "Invalid List Expression")) )
        | _ -> LstSub (fle1, fle2, fle3) )
    | LstSub (le1, le2, le3) -> (
        let fle1 = f le1 in
        let fle2 = f le2 in
        let fle3 = f le3 in
        L.verboser (fun fmt ->
            fmt "REDUCTION: LstSub(%a, %a, %a)" Expr.pp fle1 Expr.pp fle2
              Expr.pp fle3);
        match (fle1, fle2, fle3) with
        | _, _, Lit (Num 0.) -> EList []
        | flx, Lit (Num 0.), UnOp (LstLen, fle1) when flx = fle1 -> fle1
        | NOp (LstCat, [ x ]), fle2, fle3 -> f (LstSub (x, fle2, fle3))
        | NOp (LstCat, flx :: _), Lit (Num 0.), UnOp (LstLen, fle1)
          when flx = fle1 -> fle1
        | fle1, UnOp (LstLen, lx), fle3 when fst (list_prefix pfs lx fle1) ->
            let _, suffix = list_prefix pfs lx fle1 in
            f (LstSub (suffix, Lit (Num 0.), fle3))
        | fle1, Lit (Num 0.), UnOp (LstLen, LVar lx)
          when let candidates = get_equal_expressions pfs fle1 in
               let candidates =
                 List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
               in
               let candidates =
                 List.filter
                   (fun (x, lvars) -> Containers.SS.mem lx lvars)
                   candidates
               in
               let candidates =
                 List.filter
                   (fun (x, _) ->
                     x = Expr.LVar lx
                     ||
                     match x with
                     | NOp (LstCat, [ x ]) when x = LVar lx -> true
                     | NOp (LstCat, x :: _) when x = LVar lx -> true
                     | _ -> false)
                   candidates
               in
               candidates <> [] -> (
            L.verboser (fun fmt -> fmt "Got in!");
            let candidates = get_equal_expressions pfs fle1 in
            let candidates =
              List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
            in
            let candidates =
              List.filter
                (fun (x, lvars) -> Containers.SS.mem lx lvars)
                candidates
            in
            L.verboser (fun fmt ->
                fmt "Candidates: %s"
                  (String.concat ", "
                     (List.map
                        (fun (c, _) -> Fmt.to_to_string Expr.pp c)
                        candidates)));
            match fst (List.hd candidates) with
            | LVar ly when ly = lx -> LVar lx
            | NOp (LstCat, [ flx; _ ]) when flx = LVar lx -> LVar lx
            | _ ->
                raise
                  (Exceptions.Impossible
                     "reduce_lexpr: guaranteed by match/filter") )
        | NOp (LstCat, EList les :: _), Lit (Num s), Lit (Num f)
          when List.length les >= int_of_float (s +. f) ->
            LstSub (EList les, fle2, fle3)
        | EList lst, Lit (Num _start), Lit (Num _end) -> (
            try
              EList
                (Array.to_list
                   (Array.sub (Array.of_list lst) (int_of_float _start)
                      (int_of_float _end)))
            with _ ->
              raise
                (ReductionException
                   (LstSub (le1, le2, le3), "Invalid List Expression")) )
        | Lit (LList lst), Lit (Num _start), Lit (Num _end) -> (
            try
              Lit
                (LList
                   (Array.to_list
                      (Array.sub (Array.of_list lst) (int_of_float _start)
                         (int_of_float _end))))
            with _ ->
              raise
                (ReductionException
                   (LstSub (le1, le2, le3), "Invalid List Expression")) )
        | NOp (LstCat, lel :: ler), fle2, fle3 -> (
            L.(
              tmi (fun m ->
                  m "RED: %s"
                    ((Fmt.to_to_string Expr.pp) (LstSub (fle1, fle2, fle3)))));
            let fle2 = f (substitute_for_length pfs fle2) in
            let fle3 = f (substitute_for_length pfs fle3) in
            let start_in_first = f (BinOp (UnOp (LstLen, lel), Minus, fle2)) in
            let start_beyond_first =
              f (BinOp (fle2, Minus, UnOp (LstLen, lel)))
            in
            let end_in_first =
              f (BinOp (UnOp (LstLen, lel), Minus, BinOp (fle2, FPlus, fle3)))
            in
            let end_beyond_first =
              f (BinOp (BinOp (fle2, FPlus, fle3), Minus, UnOp (LstLen, lel)))
            in

            L.(
              tmi (fun m ->
                  m
                    "Start in first: %s\n\
                     End in first: %s\n\
                     Start beyond first: %s\n\
                     End beyond first%s"
                    ((Fmt.to_to_string Expr.pp) start_in_first)
                    ((Fmt.to_to_string Expr.pp) end_in_first)
                    ((Fmt.to_to_string Expr.pp) start_beyond_first)
                    ((Fmt.to_to_string Expr.pp) end_beyond_first)));
            match
              ( check_ge_zero start_in_first,
                check_ge_zero start_beyond_first,
                check_ge_zero end_in_first,
                check_ge_zero end_beyond_first )
            with
            (* Sublist certainly starts beyond first *)
            | _, Some true, _, _ ->
                f (LstSub (NOp (LstCat, ler), start_beyond_first, fle3))
            (* Sublist fully contained in first *)
            | Some true, _, Some true, _ -> f (LstSub (lel, fle2, fle3))
            (* Sublist contains part of first *)
            | Some true, _, _, Some true ->
                let prefix = f (LstSub (lel, fle2, start_in_first)) in
                let suffix =
                  f
                    (LstSub
                       ( NOp (LstCat, ler),
                         Lit (Num 0.),
                         BinOp (fle3, Minus, start_in_first) ))
                in
                f (NOp (LstCat, [ prefix; suffix ]))
            (* Sublist starts in first, but we don't know more *)
            | Some true, _, _, _ -> (
                match (lel, fle2) with
                (* Cut to start from 0. *)
                | EList les, Lit (Num l2) when l2 > 0. ->
                    L.verboser (fun fmt -> fmt "Case 1");
                    let les' : Expr.t =
                      EList
                        (Array.to_list
                           (Array.sub (Array.of_list les) (int_of_float l2)
                              (List.length les - int_of_float l2)))
                    in
                    f (LstSub (NOp (LstCat, les' :: ler), Lit (Num 0.), fle3))
                | EList les, Lit (Num 0.) ->
                    L.verboser (fun fmt -> fmt "Case 2");
                    let l1 = List.length les in
                    let p3, m3 = collect_pluses_minuses fle3 in
                    let nump, pluses = numbers_and_rest p3 in
                    let numm, minuses = numbers_and_rest m3 in
                    let nump = int_of_float nump in
                    (* Must have something positive and nothing negative in numbers *)
                    if nump > 0 && nump <= l1 && numm = 0. then (
                      L.verboser (fun fmt ->
                          fmt "List: %a Start: %d End: %d" Expr.pp (EList les)
                            nump
                            (List.length les - nump));
                      let les' : Expr.t =
                        EList
                          (Array.to_list (Array.sub (Array.of_list les) 0 nump))
                      in
                      let les'' : Expr.t =
                        EList
                          (Array.to_list
                             (Array.sub (Array.of_list les) nump
                                (List.length les - nump)))
                      in
                      f
                        (NOp
                           ( LstCat,
                             [
                               les';
                               LstSub
                                 ( NOp (LstCat, les'' :: ler),
                                   Lit (Num 0.),
                                   BinOp
                                     (fle3, Minus, Lit (Num (float_of_int nump)))
                                 );
                             ] )) )
                    else LstSub (fle1, fle2, fle3)
                | _, _ -> LstSub (fle1, fle2, fle3) )
            | _ -> LstSub (fle1, fle2, fle3) )
        | _, _, _ -> LstSub (fle1, fle2, fle3) )
    (* CHECK: Times and Div are the same, how does the 'when' scope? *)
    | BinOp (lel, op, ler) -> (
        let flel = f lel in
        let fler = f ler in
        let def = Expr.BinOp (flel, op, fler) in
        match (flel, fler) with
        | Lit ll, Lit lr -> (
            try
              Lit
                (CExprEval.evaluate_binop (CStore.init []) op (Lit ll) (Lit lr))
            with
            | CExprEval.TypeError err_msg ->
                raise (ReductionException (def, err_msg))
            | CExprEval.EvaluationError err_msg ->
                raise (ReductionException (def, err_msg))
            | e -> raise e )
        | _              -> (
            match op with
            | Equal -> (
                if flel = fler then Lit (Bool true)
                else if
                  PFS.mem pfs (Eq (flel, fler)) || PFS.mem pfs (Eq (fler, flel))
                then Lit (Bool true)
                else if
                  PFS.mem pfs (Not (Eq (flel, fler)))
                  || PFS.mem pfs (Not (Eq (fler, flel)))
                then Lit (Bool false)
                else
                  let t1, _, _ = Typing.type_lexpr gamma flel in
                  let t2, _, _ = Typing.type_lexpr gamma fler in
                  match (t1, t2) with
                  | Some t1, Some t2 ->
                      if t1 = t2 then def else Lit (Bool false)
                  | _, _             -> def )
            | (FPlus | Minus) when lexpr_is_number ~gamma def ->
                simplify_arithmetic_lexpr pfs gamma def
            (* | FPlus when (lexpr_is_number ~gamma def) ->
                 (match flel, fler with
                 (* 0 is the neutral *)
                 | Lit (Num 0.), x
                 | x, Lit (Num 0.) -> x
                 | Lit (Num x), _ when (x == nan) -> Lit (Num nan)
                 | _, Lit (Num x) when (x == nan) -> Lit (Num nan)
                 (* This can be more general *)
                 | BinOp (Lit (Num x), FPlus, y), Lit (Num z) -> BinOp (Lit (Num (x +. z)), FPlus, y)
                 | Lit (Num z), BinOp (Lit (Num x), FPlus, y) -> BinOp (Lit (Num (z +. x)), FPlus, y)
                 (* Associate to the right *)
                 | BinOp (flell, FPlus, flelr), fler -> BinOp (flell, FPlus, BinOp (flelr, FPlus, fler))
                 (* Rest *)
                 | _, _ -> def
                 )
               | Minus when (lexpr_is_number ~gamma def) ->
                 (match flel, fler with
                 (* 0 is the neutral *)
                 | Lit (Num 0.), x -> UnOp (FUnaryMinus, x)
                 | x, Lit (Num 0.) -> x
                 | Lit (Num x), _ when (x == nan) -> Lit (Num nan)
                 | _, Lit (Num x) when (x == nan) -> Lit (Num nan)
                 (* Transform to unary minus *)
                 | _, _ -> BinOp (flel, FPlus, (UnOp (FUnaryMinus, fler)))
                 ) *)
            | Times when lexpr_is_number ~gamma def -> (
                match (flel, fler) with
                (* 1 is the neutral *)
                | Lit (Num 1.), x | x, Lit (Num 1.) -> x
                | Lit (Num x), _ when x == nan -> Lit (Num nan)
                | _, Lit (Num x) when x == nan -> Lit (Num nan)
                | BinOp (Lit (Num x), Times, y), Lit (Num z) ->
                    BinOp (Lit (Num (x *. z)), Times, y)
                | Lit (Num z), BinOp (Lit (Num x), Times, y) ->
                    BinOp (Lit (Num (z *. x)), Times, y)
                (* Rest *)
                | _, _ -> def )
            | Div when lexpr_is_number ~gamma def -> (
                match (flel, fler) with
                (* 1 is the neutral *)
                | Lit (Num 1.), x | x, Lit (Num 1.) -> x
                (* Rest *)
                | _, _ -> def )
            | BAnd when lexpr_is_bool gamma def -> (
                match (flel, fler) with
                (* 1 is the neutral *)
                | Lit (Bool true), x | x, Lit (Bool true) -> x
                | Lit (Bool false), _ | _, Lit (Bool false) -> Lit (Bool false)
                (* Rest *)
                | _, _ ->
                    let fal, nfal = Option.get (Formula.lift_logic_expr flel) in
                    let far, nfar = Option.get (Formula.lift_logic_expr fler) in
                    if PFS.mem pfs nfal || PFS.mem pfs nfar then
                      Lit (Bool false)
                    else if PFS.mem pfs fal then f fler
                    else if PFS.mem pfs far then f flel
                    else BinOp (flel, BAnd, fler) )
            | BOr when lexpr_is_bool gamma def -> (
                match (flel, fler) with
                (* 1 is the neutral *)
                | Lit (Bool true), _ | _, Lit (Bool true) -> Lit (Bool true)
                | Lit (Bool false), x | x, Lit (Bool false) -> x
                (* Rest *)
                | _, _ ->
                    let fal, nfal = Option.get (Formula.lift_logic_expr flel) in
                    let far, nfar = Option.get (Formula.lift_logic_expr fler) in
                    if PFS.mem pfs fal || PFS.mem pfs far then Lit (Bool true)
                    else if PFS.mem pfs nfal then f fler
                    else if PFS.mem pfs nfar then f flel
                    else BinOp (flel, BOr, fler) )
            | StrCat when lexpr_is_string gamma def -> (
                match (flel, fler) with
                (* Empty list is the neutral *)
                | x, Lit (String "") | Lit (String ""), x -> x
                (* Rest *)
                | BinOp (el, StrCat, Lit (String s1)), Lit (String s2) ->
                    f (BinOp (el, StrCat, Lit (String (s1 ^ s2))))
                | _, _ -> def )
            | SetDiff when lexpr_is_set gamma def -> (
                let pfs = PFS.to_list pfs in
                if contained_in_union pfs flel fler then ESet []
                else
                  match (flel, fler) with
                  | x, y when x = y -> ESet []
                  | ESet [], _ -> ESet []
                  | x, ESet [] -> x
                  | ESet left, ESet right
                    when Expr.all_literals left && Expr.all_literals right ->
                      ESet
                        (Expr.Set.elements
                           (Expr.Set.diff (Expr.Set.of_list left)
                              (Expr.Set.of_list right)))
                  | ESet left, s when Expr.all_literals left ->
                      if List.for_all (fun x -> set_member pfs x s) left then
                        ESet []
                      else def
                  | NOp (SetUnion, les), _ ->
                      let diffs =
                        List.map (fun le -> f (BinOp (le, SetDiff, fler))) les
                      in
                      NOp (SetUnion, diffs)
                  | _, NOp (SetUnion, les) ->
                      f
                        (NOp
                           ( SetInter,
                             List.map
                               (fun le -> Expr.BinOp (flel, SetDiff, le))
                               les ))
                  | x, ESet [ el ]
                    when List.mem (Formula.Not (SetMem (el, x))) pfs -> x
                  | LVar _, _ ->
                      if set_subset pfs flel fler then ESet [] else def
                  | ESet les, fler -> (
                      (* We must know that the elements of les are all different, and for that we need the pure formulae *)
                      match all_different pfs les with
                      | false -> def
                      | true  ->
                          let _, rest =
                            List.partition (fun x -> set_member pfs x fler) les
                          in
                          if
                            List.for_all
                              (fun x -> not_set_member pfs x fler)
                              rest
                          then ESet rest
                          else BinOp (ESet rest, SetDiff, fler) )
                  | _, _ -> def )
            (* let hM = f (BinOp (flel, SetSub, fler)) in
               (match hM with
               | Lit (Bool true) -> ESet []
               | _ -> def)) *)
            | BSetMem when lexpr_is_bool gamma def -> (
                match (flel, fler) with
                | _, ESet []    -> Lit (Bool false)
                | _, ESet [ x ] -> BinOp (flel, Equal, x)
                | le, ESet les  -> (
                    match List.mem le les with
                    | true  -> Lit (Bool true)
                    | false -> (
                        match le with
                        | Lit _ ->
                            if Expr.all_literals les then Lit (Bool false)
                            else def
                        | _     -> def ) )
                | _, _          -> def )
            | BSetSub when lexpr_is_bool gamma def -> (
                match (flel, fler) with
                | ESet [], _ -> Lit (Bool true)
                | _, ESet [] -> Lit (Bool false)
                | ESet left, ESet right
                  when Expr.all_literals left && Expr.all_literals right ->
                    Lit
                      (Bool
                         (Expr.Set.subset (Expr.Set.of_list left)
                            (Expr.Set.of_list right)))
                | LVar v, NOp (SetUnion, les) ->
                    if List.mem flel les then Lit (Bool true) else def
                | _, _ -> def )
            | LessThan -> (
                match (flel, fler) with
                | UnOp (LstLen, _), Lit (Num n) when n <= 0. -> Lit (Bool false)
                | UnOp (LstLen, le), Lit (Num 1.) -> BinOp (le, Equal, EList [])
                | _, _ -> def )
            | _ -> def ) )
    (* The remaining cases cannot be reduced *)
    | _ -> le
  in

  let result = normalise_list_expressions result in
  let final_result =
    if compare le result <> 0 then (
      L.(
        tmi (fun m ->
            m "\tReduce_lexpr: %s -> %s"
              ((Fmt.to_to_string Expr.pp) le)
              ((Fmt.to_to_string Expr.pp) result)));
      f result )
    else result
  in

  final_result

and reduce_lexpr
    ?(unification = false)
    ?(reduce_lvars = false)
    ?(pfs = PFS.init ())
    ?(gamma = TypEnv.init ())
    (le : Expr.t) =
  reduce_lexpr_loop ~unification ~reduce_lvars pfs gamma le

and simplify_arithmetic_lexpr (pfs : PFS.t) (gamma : TypEnv.t) le =
  let f = reduce_lexpr_loop pfs gamma in
  match le with
  | BinOp (l, FPlus, Lit (Num 0.)) | BinOp (Lit (Num 0.), FPlus, l) -> l
  (* Binary minus to unary minus *)
  | BinOp (l, Minus, r) -> f (BinOp (l, FPlus, UnOp (FUnaryMinus, r)))
  (* Unary minus distributes over + *)
  | UnOp (FUnaryMinus, e) -> (
      match e with
      | BinOp (l, FPlus, r) ->
          f (BinOp (UnOp (FUnaryMinus, l), FPlus, UnOp (FUnaryMinus, r)))
      | _                   -> le )
  (* FPlus - we collect the positives and the negatives, see what we have and deal with them *)
  | BinOp (l, FPlus, r) -> compose_pluses_minuses (collect_pluses_minuses le)
  | _ -> le

and collect_pluses_minuses (le : Expr.t) : Expr.t list * Expr.t list =
  match le with
  | BinOp (l, FPlus, r)   ->
      let pl, ml = collect_pluses_minuses l in
      let pr, mr = collect_pluses_minuses r in
      (List.sort Stdlib.compare (pl @ pr), List.sort Stdlib.compare (ml @ mr))
  | UnOp (FUnaryMinus, e) -> ([], [ e ])
  | _                     -> ([ le ], [])

and compose_pluses_minuses (pluses_and_minuses : Expr.t list * Expr.t list) :
    Expr.t =
  let pluses, minuses = pluses_and_minuses in
  let nump, pluses = numbers_and_rest pluses in
  let numm, minuses = numbers_and_rest minuses in
  let pluses_stay = List.filter (fun x -> not (List.mem x minuses)) pluses in
  let minuses_stay =
    List.map
      (fun minus -> Expr.UnOp (FUnaryMinus, minus))
      (List.filter (fun x -> not (List.mem x pluses)) minuses)
  in
  let result =
    List.fold_right
      (fun plus (ac : Expr.t) ->
        if ac = Lit (Num 0.) then plus else BinOp (plus, FPlus, ac))
      (pluses_stay @ minuses_stay)
      (Lit (Num 0.))
  in
  let diff = nump -. numm in
  if diff = 0. then result
  else if result = Lit (Num 0.) then Lit (Num diff)
  else BinOp (Lit (Num diff), FPlus, result)

and numbers_and_rest (numbers : Expr.t list) =
  List.fold_left
    (fun (nump, restp) num ->
      match num with
      | Expr.Lit (Num x) -> (nump +. x, restp)
      | _                -> (nump, num :: restp))
    (0., []) numbers

and substitute_for_length pfs le =
  (* L.(verboser (fun m -> m "Inside sub_for_len: %s" ((Fmt.to_to_string Expr.pp) le))); *)
  let len = PFS.length pfs in
  (* L.(verboser (fun m -> m "We have the pfs of length: %d" len)); *)
  let idx = ref 0 in
  let result = ref le in
  while !idx < len do
    let form = PFS.nth_get pfs !idx in
    (* L.(verboser (fun m -> m "Formula: %s" ((Fmt.to_to_string Formula.pp) form))); *)
    ( match form with
    | Eq (UnOp (LstLen, lst'), res) | Eq (res, UnOp (LstLen, lst')) ->
        result :=
          substitute_in_numeric_expr res (Expr.UnOp (LstLen, lst')) !result
    | _ -> () );
    idx := !idx + 1
  done;
  !result

and check_ge_zero le : bool option =
  let pluses, minuses = collect_pluses_minuses le in
  let nump, pluses = numbers_and_rest pluses in
  let numm, minuses = numbers_and_rest minuses in
  if not (numm = 0. && minuses == []) then None
  else if nump < 0. then None
  else
    List.fold_left
      (fun ac plus ->
        if ac <> Some true then ac
        else
          match plus with
          | Expr.UnOp (LstLen, _) -> ac
          | _                     -> None)
      (Some true) pluses

and substitute_in_numeric_expr (le_to_find : Expr.t) (le_to_subst : Expr.t) le =
  let f = substitute_in_numeric_expr le_to_find le_to_subst in
  let list_subset la lb = List.for_all (fun x -> List.mem x lb) la in
  let list_dif la lb = List.filter (fun x -> not (List.mem x lb)) la in
  match le with
  | le when lexpr_is_number le -> (
      let plf, mif = collect_pluses_minuses le_to_find in
      let ple, mie = collect_pluses_minuses le in
      match (list_subset plf ple, list_subset mif mie) with
      | true, true ->
          L.(
            verboser (fun m ->
                m "Subset found: %s %s %s"
                  ((Fmt.to_to_string Expr.pp) le)
                  ((Fmt.to_to_string Expr.pp) le_to_find)
                  ((Fmt.to_to_string Expr.pp) le_to_subst)));
          let pls, mis = collect_pluses_minuses le_to_subst in
          let pluses = pls @ list_dif ple plf in
          let minuses = mis @ list_dif mie mif in
          compose_pluses_minuses (pluses, minuses)
      | _          -> le )
  | LstSub (le1, le2, le3) -> LstSub (le1, f le2, f le3)
  | _ -> le

and substitute_in_numeric_formula (le_to_find : Expr.t) (le_to_subst : Expr.t) f
    =
  let result =
    Formula.map None None
      (Some (substitute_in_numeric_expr le_to_find le_to_subst))
      f
  in
  result

and substitute_in_pfs (le_to_find : Expr.t) (le_to_subst : Expr.t) (pfs : PFS.t)
    : unit =
  let len = PFS.length pfs in
  let idx = ref 0 in
  while !idx < len do
    let form = PFS.nth_get pfs !idx in
    let form' =
      match form with
      | Eq (lx, ly) when lx = le_to_subst || ly = le_to_subst -> form
      | _ -> substitute_in_numeric_formula le_to_find le_to_subst form
    in
    PFS.nth_set pfs !idx form';
    idx := !idx + 1
  done

let rec reduce_formula_loop
    (unification : bool) (pfs : PFS.t) (gamma : TypEnv.t) (a : Formula.t) :
    Formula.t =
  let f = reduce_formula_loop unification pfs gamma in
  let fe = reduce_lexpr_loop ~unification pfs gamma in

  let result : Formula.t =
    match a with
    | Eq (Lit (LList ll), Lit (LList lr)) -> if ll = lr then True else False
    | Eq (EList le, Lit (LList ll)) | Eq (Lit (LList ll), EList le) ->
        if List.length ll <> List.length le then False
        else if ll = [] then True
        else
          let eqs = List.map2 (fun x y -> Formula.Eq (x, Lit y)) le ll in
          let conj =
            List.fold_left
              (fun ac x -> Formula.And (ac, x))
              (List.hd eqs) (List.tl eqs)
          in
          f conj
    | Eq (EList ll, EList lr) ->
        if List.length ll <> List.length lr then False
        else if ll = [] then True
        else
          let eqs = List.map2 (fun x y -> Formula.Eq (x, y)) ll lr in
          let conj =
            List.fold_left
              (fun ac x -> Formula.And (ac, x))
              (List.hd eqs) (List.tl eqs)
          in
          f conj
    | And (a1, a2) -> (
        let fa1 = f a1 in
        let fa2 = f a2 in
        match (fa1, fa2) with
        | False, _ | _, False -> False
        | True, a | a, True   -> a
        | _, _                -> And (fa1, fa2) )
    | Or (a1, a2) -> (
        let fa1 = f a1 in
        let fa2 = f a2 in
        match (fa1, fa2) with
        | False, a | a, False -> a
        | True, a | a, True   -> True
        | _, _                ->
            if PFS.mem pfs fa1 || PFS.mem pfs fa2 then True
            else if PFS.mem pfs (Not fa1) then f fa2
            else if PFS.mem pfs (Not fa2) then f fa1
            else Or (fa1, fa2) )
    (* JOSE: why the recursive call? *)
    | Not a -> (
        let fa = f a in
        match a with
        | True            -> False
        | False           -> True
        | Not a           -> a
        | Or (a1, a2)     -> f (And (Not a1, Not a2))
        | And (a1, a2)    -> f (Or (Not a1, Not a2))
        | Less (e1, e2)   -> f (LessEq (e2, e1))
        | LessEq (e1, e2) -> f (Less (e2, e1))
        | _               -> Not fa )
    | Eq (e1, e2) -> (
        let re1 = fe e1 in
        let re2 = fe e2 in
        (* Warning - NaNs, infinities, this and that, this is not good enough *)
        let eq = re1 = re2 in
        if eq then True
        else
          let t1, s1, _ = Typing.type_lexpr gamma re1 in
          let t2, s2, _ = Typing.type_lexpr gamma re2 in
          if
            s1 && s2
            &&
            match (t1, t2) with
            | Some t1, Some t2 -> t1 <> t2
            | _, _             -> false
          then False
          else
            let ite a b : Formula.t = if a = b then True else False in
            let default e1 e2 re1 re2 : Formula.t =
              let a' : Formula.t = Eq (re1, re2) in
              if re1 = e1 && re2 = e2 then a' else f a'
            in

            match (re1, re2) with
            | ALoc _, Lit (Loc _) | Lit (Loc _), ALoc _ -> False
            | ALoc x, ALoc y when (not unification) && x <> y -> False
            | EList [], x | x, EList [] | Lit (LList []), x | x, Lit (LList [])
              -> (
                match x with
                | Lit (LList lst) when List.length lst > 0 -> False
                | EList lst when List.length lst > 0 -> False
                | NOp (LstCat, les) ->
                    if
                      List.exists
                        (fun (x : Expr.t) ->
                          match x with
                          | EList le when List.length le > 0 -> true
                          | Lit (LList le) when List.length le > 0 -> true
                          | _ -> false)
                        les
                    then False
                    else Eq (re1, re2)
                | _ -> Eq (re1, re2) )
            (* Lifting *)
            | Lit (Bool true), BinOp (x, Equal, y)
            | BinOp (x, Equal, y), Lit (Bool true) -> Eq (x, y)
            | Lit (Bool true), UnOp (UNot, BinOp (x, Equal, y)) ->
                Not (Eq (x, y))
            | UnOp (UNot, BinOp (x, Equal, y)), Lit (Bool true) ->
                Not (Eq (x, y))
            | Lit (Bool false), BinOp (x, Equal, y) -> Not (Eq (x, y))
            | BinOp (x, Equal, y), Lit (Bool false) -> Not (Eq (x, y))
            | Lit (Bool false), UnOp (UNot, BinOp (x, Equal, y)) ->
                Not (Eq (x, y))
            | UnOp (UNot, BinOp (x, Equal, y)), Lit (Bool false) ->
                Not (Eq (x, y))
            | UnOp (LstLen, le), le'
              when match le' with
                   | Lit (Num _) -> false
                   | _           -> true ->
                substitute_in_pfs le' (UnOp (LstLen, le)) pfs;
                Eq (UnOp (LstLen, le), le')
            | le', UnOp (LstLen, le)
              when match le' with
                   | Lit (Num _) -> false
                   | _           -> true ->
                substitute_in_pfs le' (UnOp (LstLen, le)) pfs;
                Eq (UnOp (LstLen, le), le')
            | UnOp (LstRev, ll), UnOp (LstRev, rl) -> f (Eq (ll, rl))
            (* TODO: This is a specialised simplification, not sure for what, disabled for now
               | UnOp  (LstRev, full_list), BinOp (UnOp (LstRev, plist_left), LstCat, plist_right)
               | BinOp (UnOp (LstRev, plist_left), LstCat, plist_right), UnOp (LstRev, full_list)
                   ->
                   f (Eq (full_list, BinOp (UnOp (LstRev, plist_right), LstCat, plist_left))) *)
            | LstSub (e1, Lit (Num 0.), el), e2 when e1 = e2 ->
                f (Eq (UnOp (LstLen, e1), el))
            | NOp (LstCat, fl :: rl), NOp (LstCat, fr :: rr) when fl = fr ->
                f (Eq (NOp (LstCat, rl), NOp (LstCat, rr)))
            | le1, le2
              when ( match le1 with
                   | LVar _ -> false
                   | _      -> true )
                   && ( match le2 with
                      | LVar _ -> false
                      | _      -> true )
                   && lexpr_is_list gamma le1 && lexpr_is_list gamma le2 -> (
                let htl1, htl2 =
                  ( get_head_and_tail_of_list pfs Expr.Set.empty le1,
                    get_head_and_tail_of_list pfs Expr.Set.empty le2 )
                in
                match (htl1, htl2) with
                | Some (hl1, tl1), Some (hl2, tl2) ->
                    f (And (Eq (hl1, hl2), Eq (tl1, tl2)))
                | None, Some _ -> (
                    match le1 with
                    | Lit (LList _) | EList _ -> False
                    | _                       -> Eq (re1, re2) )
                | Some _, None -> (
                    match le2 with
                    | Lit (LList _) | EList _ -> False
                    | _                       -> Eq (re1, re2) )
                | None, None -> Eq (re1, re2) )
            (* Strings #1 *)
            | Lit (String ls), BinOp (Lit (String rs), StrCat, s)
            | BinOp (Lit (String rs), StrCat, s), Lit (String ls) -> (
                let lls = String.length ls in
                let lrs = String.length rs in
                match Stdlib.compare lls lrs with
                | -1 -> False
                | 0  -> if ls <> rs then False else f (Eq (s, Lit (String "")))
                | 1  ->
                    let sub = String.sub ls 0 lrs in
                    if sub <> rs then False
                    else
                      f (Eq (s, Lit (String (String.sub ls lrs (lls - lrs)))))
                | _  ->
                    raise
                      (Exceptions.Impossible
                         "reduce_formula: guaranteed by match/filter") )
            (* String #2 *)
            | BinOp (sl1, StrCat, sr1), BinOp (sl2, StrCat, sr2) when sl1 = sl2
              -> f (Eq (sr1, sr2))
            | BinOp (sl1, StrCat, sr1), BinOp (sl2, StrCat, sr2) when sr1 = sr2
              -> f (Eq (sl1, sl2))
            (* String #3 *)
            | BinOp (sl, StrCat, sr), s when sl = s ->
                f (Eq (sr, Lit (String "")))
            | BinOp (sl, StrCat, sr), s when sr = s ->
                f (Eq (sl, Lit (String "")))
            | s, BinOp (sl, StrCat, sr) when sl = s ->
                f (Eq (sr, Lit (String "")))
            | s, BinOp (sl, StrCat, sr) when sr = s ->
                f (Eq (sl, Lit (String "")))
            | BinOp (sl, StrCat, sr), Lit (String "") ->
                f (And (Eq (sl, Lit (String "")), Eq (sr, Lit (String ""))))
            (* Num-to-String injectivity *)
            | UnOp (ToStringOp, le1), UnOp (ToStringOp, le2) ->
                f (Eq (le1, le2))
            (* Num-to-String understanding *)
            | UnOp (ToStringOp, le1), Lit (String s)
            | Lit (String s), UnOp (ToStringOp, le1) -> (
                match s with
                | "" -> False
                | "Infinity" | "-Infinity" | "NaN" -> default e1 e2 re1 re2
                | _ -> (
                    let num = try Some (Float.of_string s) with _ -> None in
                    match num with
                    | Some num -> Eq (le1, Lit (Num num))
                    | None     -> False ) )
            (* The empty business *)
            | _, Lit Empty -> (
                match re1 with
                | Lit l when l <> Empty -> False
                | EList _ | ESet _ -> False
                | _ -> default e1 e2 re1 re2 )
            | Lit l1, Lit l2 -> ite l1 l2
            | Lit Nono, PVar x | PVar x, Lit Nono -> default e1 e2 re1 re2
            (* JOSE: Why are we considering the case of a logical variable being bound to None? *)
            | Lit Nono, LVar x | LVar x, Lit Nono -> (
                let tx = TypEnv.get gamma x in
                match tx with
                | None    -> default e1 e2 re1 re2
                | Some tx ->
                    if tx = NoneType then default e1 e2 re1 re2 else False )
            | Lit Nono, e | e, Lit Nono -> False
            | Lit (Bool true), BinOp (e1, LessThan, e2) -> Less (e1, e2)
            | Lit (Bool false), BinOp (e1, LessThan, e2) -> Not (Less (e1, e2))
            (* FPlus theory -> theory? I would not go that far *)
            | le1, le2 when lexpr_is_number le1 && lexpr_is_number le2 ->
                (* Collect the pluses and minuses and separate them into constants and rest *)
                let pluses1, minuses1 = collect_pluses_minuses le1 in
                let nump1, pluses1 = numbers_and_rest pluses1 in
                let numm1, minuses1 = numbers_and_rest minuses1 in
                let pluses2, minuses2 = collect_pluses_minuses le2 in
                let nump2, pluses2 = numbers_and_rest pluses2 in
                let numm2, minuses2 = numbers_and_rest minuses2 in

                (* Calculate the constant and place it on the left *)
                let nump = nump1 -. nump2 in
                let numm = numm1 -. numm2 in
                let num = nump -. numm in

                let numl, numr = if num > 0. then (num, 0.) else (0., -.num) in
                let list_diff la lb =
                  List.filter (fun x -> not (List.mem x lb)) la
                in
                let pll = list_diff pluses1 pluses2 in
                let plr = list_diff pluses2 pluses1 in
                let mil = list_diff minuses1 minuses2 in
                let mir = list_diff minuses2 minuses1 in

                let fl =
                  compose_pluses_minuses
                    ( (if numl = 0. then [] else [ Expr.Lit (Num numl) ]) @ pll,
                      mil )
                in
                let fr =
                  compose_pluses_minuses
                    ( (if numr = 0. then [] else [ Expr.Lit (Num numr) ]) @ plr,
                      mir )
                in
                Eq (fl, fr)
            (* Very special cases *)
            | UnOp (TypeOf, BinOp (_, StrCat, _)), Lit (Type t)
              when t <> StringType -> False
            | UnOp (TypeOf, BinOp (_, BSetMem, _)), Lit (Type t)
              when t <> BooleanType -> False
            (* Set unions *)
            | ( NOp (SetUnion, [ ls; ESet [ lx ] ]),
                NOp (SetUnion, [ rs; ESet [ rx ] ]) )
              when lx = rx ->
                if
                  PFS.mem pfs (Not (SetMem (lx, ls)))
                  && PFS.mem pfs (Not (SetMem (lx, rs)))
                then f (Eq (ls, rs))
                else default e1 e2 re1 re2
            | _, _ -> default e1 e2 re1 re2 )
    | Less (e1, e2) -> (
        let re1 = fe e1 in
        let re2 = fe e2 in
        match (re1, re2) with
        | Lit (Num x), Lit (Num y) -> if x < y then True else False
        | UnOp (LstLen, _), Lit (Num 0.) -> False
        | UnOp (LstLen, le), Lit (Num 1.) -> Eq (le, EList [])
        | re1, re2 ->
            if PFS.mem pfs (LessEq (re2, re1)) then False
            else if PFS.mem pfs (Less (re2, re1)) then False
            else Less (re1, re2) )
    | LessEq (e1, e2) -> (
        let re1 = fe e1 in
        let re2 = fe e2 in
        match (re1, re2) with
        | Lit (Num x), Lit (Num y) -> if x <= y then True else False
        | re1, re2                 ->
            if PFS.mem pfs (LessEq (re2, re1)) then Eq (re1, re2)
            else if PFS.mem pfs (Less (re1, re2)) then True
            else if PFS.mem pfs (Less (re2, re1)) then False
            else LessEq (re1, re2) )
    | SetMem (leb, NOp (SetUnion, lle)) ->
        let rleb = fe leb in
        let formula : Formula.t =
          match lle with
          | []        -> False
          | le :: lle ->
              let rle = fe le in
              List.fold_left
                (fun ac le ->
                  ( let rle = fe le in
                    Or (ac, SetMem (rleb, rle))
                    : Formula.t ))
                (SetMem (rleb, rle))
                lle
        in
        let result = f formula in
        result
    | SetMem (leb, NOp (SetInter, lle)) ->
        let rleb = fe leb in
        let formula : Formula.t =
          match lle with
          | []        -> False
          | le :: lle ->
              let rle = fe le in
              List.fold_left
                (fun ac le ->
                  ( let rle = fe le in
                    And (ac, SetMem (rleb, rle))
                    : Formula.t ))
                (SetMem (rleb, rle))
                lle
        in
        let result = f formula in
        result
    | SetMem (leb, BinOp (lel, SetDiff, ler)) ->
        let rleb = fe leb in
        let rlel = fe lel in
        let rler = fe ler in
        let result = f (And (SetMem (rleb, rlel), Not (SetMem (rleb, rler)))) in
        result
    | SetMem (leb, ESet les) ->
        let rleb = fe leb in
        let rles = List.map (fun le -> fe le) les in
        let result : Formula.t list =
          List.map (fun le -> (Eq (rleb, le) : Formula.t)) rles
        in
        let result =
          List.fold_left
            (fun ac eq ->
              ( match (ac : Formula.t) with
                | False -> eq
                | _     -> Or (ac, eq)
                : Formula.t ))
            False result
        in
        f result
    | ForAll (bt, a) ->
        (* Think about quantifier instantiation *)
        (* Collect binders that are in gamma *)
        let binders_in_gamma =
          List.map (fun (b, _) -> (b, TypEnv.get gamma b)) bt
        in
        let ra = f a in
        let vars = Formula.lvars a in
        let bt = List.filter (fun (b, _) -> Containers.SS.mem b vars) bt in
        let result =
          match bt with
          | [] -> ra
          | _  -> ForAll (bt, ra)
        in

        (* Reinstate binders *)
        List.iter
          (fun (b, t) ->
            match t with
            | None   -> TypEnv.remove gamma b
            | Some t -> TypEnv.update gamma b t)
          binders_in_gamma;
        result
    | _ -> a
  in

  let final_result =
    if a <> result && not (a == result) then (
      L.(
        tmi (fun m ->
            m "Reduce_assertion: %a -> %a" Formula.pp a Formula.pp result));
      L.(tmi (fun m -> m "%a" TypEnv.pp gamma));
      f result )
    else result
  in

  final_result

let reduce_formula
    ?(unification = false)
    ?(pfs : PFS.t = PFS.init ())
    ?(gamma = TypEnv.init ())
    (a : Formula.t) : Formula.t =
  reduce_formula_loop unification pfs gamma a

exception PFSFalse

exception WrongType

module MyET = struct
  type nonrec t = Expr.t * Type.t

  let compare = Stdlib.compare
end

module ETSet = Set.Make (MyET)

let reduce_types (a : Asrt.t) : Asrt.t =
  let rec separate (a : Asrt.t) =
    match a with
    | Pure True     -> ([], [])
    | Pure False    -> raise PFSFalse
    | Star (a1, a2) ->
        let fa1, ft1 = separate a1 in
        let fa2, ft2 = separate a2 in
        (fa1 @ fa2, ft1 @ ft2)
    | Types ets     -> ([], ets)
    | _             -> ([ a ], [])
  in

  try
    let others, ets = separate a in

    let ets = ETSet.elements (ETSet.of_list ets) in
    match (others, ets) with
    | [], []  -> Pure True
    | [], ets -> Types ets
    | a, ets  ->
        let result = Asrt.star a in
        if ets = [] then result else Star (Types ets, result)
  with PFSFalse -> Pure False

(* Reduction of assertions *)
let rec reduce_assertion_loop
    (unification : bool) (pfs : PFS.t) (gamma : TypEnv.t) (a : Asrt.t) : Asrt.t
    =
  let f = reduce_assertion_loop unification pfs gamma in

  let result =
    match a with
    (* Empty heap *)
    | Emp -> Asrt.Emp
    (* Star *)
    | Star (a1, a2) -> (
        let fa1 = f a1 in
        let fa2 = f a2 in
        match ((fa1 : Asrt.t), (fa2 : Asrt.t)) with
        | Pure False, _ | _, Pure False -> Asrt.Pure False
        | Pure True, a | a, Pure True -> a
        | _, _ -> Star (fa1, fa2) )
    (* Predicates *)
    | Pred (name, les) ->
        Pred (name, List.map (reduce_lexpr_loop ~unification pfs gamma) les)
    (* Pure assertions *)
    | Pure f -> Pure (reduce_formula_loop unification pfs gamma f)
    (* Types *)
    | Types lvt -> (
        try
          let lvt =
            List.fold_right
              (fun (e, t) ac ->
                match (e : Expr.t) with
                | Lit lit ->
                    if t <> Literal.type_of lit then raise WrongType else ac
                | _       -> (e, t) :: ac)
              lvt []
          in
          if lvt = [] then Asrt.Pure True else Types lvt
        with WrongType -> Pure False )
    (* General action *)
    | GA (act, l_ins, l_outs) ->
        GA
          ( act,
            List.map (reduce_lexpr_loop ~unification pfs gamma) l_ins,
            List.map (reduce_lexpr_loop ~unification pfs gamma) l_outs )
  in
  if a <> result && not (a == result) then (
    L.(tmi (fun m -> m "Reduce_assertion: %a -> %a" Asrt.pp a Asrt.pp result));
    f result )
  else result

let reduce_assertion
    ?(unification = false)
    ?(pfs = PFS.init ())
    ?(gamma = TypEnv.init ())
    (a : Asrt.t) : Asrt.t =
  let a = reduce_types a in
  reduce_assertion_loop unification pfs gamma a
