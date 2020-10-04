open SVal

(* When reduction fails *)
exception ReductionException of Expr.t * string

module L = Logging
module CStore = Store.Make (CVal.M)

let normalise_cat (f : Expr.t -> Expr.t) (les : Expr.t list) : Expr.t =
  (* L.verbose (fun fmt ->
      fmt "inside normalise cat: %a" Expr.pp (NOp (LstCat, les))); *)
  (* Recursively process each catted list and destroy inner LstCats *)
  let nles =
    List.concat
      (List.mapi
         (fun i x ->
           (* L.verbose (fun fmt -> fmt "Cat: %d: %a" i Expr.pp x); *)
           let fx = f x in
           match fx with
           | NOp (LstCat, les) -> les
           | _                 -> [ fx ])
         les)
  in
  (* L.verbose (fun fmt -> fmt "nles, v1: %a" Expr.pp (NOp (LstCat, nles))); *)
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
  (* if result <> NOp (LstCat, les) then
     L.verbose (fun fmt ->
         fmt "NormCat: %a -> %a" Expr.pp (NOp (LstCat, les)) Expr.pp result); *)
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

(**********************************)
(* Pure formulae helper functions *)
(**********************************)

let find_equalities (pfs : PFS.t) (le : Expr.t) : Expr.t list =
  let lpfs = PFS.to_list pfs in
  let lpfs =
    List.find_all
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> x = le || y = le
        | _                 -> false)
      lpfs
  in
  let result =
    List.map
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> if x = le then y else x
        | _                 ->
            raise
              (Exceptions.Impossible
                 "find_equalities_in_pfs: guarantee by match/filter"))
      lpfs
  in
  result

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
  List.rev
    (PFS.fold_left
       (fun ac a ->
         match (a : Formula.t) with
         | Eq (le1, le2) when le1 = nle -> le2 :: ac
         | Eq (le2, le1) when le1 = nle -> le2 :: ac
         | _ -> ac)
       [] pfs)

(***********************************)
(* LIST REASONING HELPER FUNCTIONS *)
(***********************************)

(* Finding the length of a list *)
let rec get_length_of_list (lst : Expr.t) : int option =
  let f = get_length_of_list in

  match lst with
  | PVar _             -> None
  | LVar _             -> None
  | Lit (LList l)      -> Some (List.length l)
  | EList l            -> Some (List.length l)
  | LstSub (_, _, len) -> (
      match len with
      | Lit (Num len) when Float.is_integer len -> Some (int_of_float len)
      | _ -> None )
  | NOp (LstCat, les)  -> (
      let lens = List.map f les in
      match List.exists (fun x -> x = None) lens with
      | true  -> None
      | false ->
          let lens = List.map Option.get lens in
          let lens = List.fold_left (fun ac x -> ac + x) 0 lens in
          Some lens )
  | _                  ->
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
  | LstSub (lst, Lit (Num start), Lit (Num len))
    when Float.is_integer start && Float.is_integer len -> (
      let start = int_of_float start in
      let len = int_of_float len in
      match lst with
      | EList l       ->
          assert (idx < len);
          assert (start + idx < List.length l);
          Some (List.nth l (start + idx))
      | Lit (LList l) ->
          assert (idx < len);
          assert (start + idx < List.length l);
          Some (Lit (List.nth l (start + idx)))
      | LVar x        -> (
          let eqs = find_equalities pfs (LVar x) in
          let candidates = List.map (fun e -> f e idx) eqs in
          let candidates = List.filter (fun x -> x <> None) candidates in
          match candidates with
          | []     -> None
          | x :: _ -> Some (Option.get x) )
      | _             -> None )
  | LstSub _ -> None
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
      let ole = get_equal_expressions pfs lst in
      match ole with
      | [] -> None
      | le :: _ when Expr.Set.mem le unacceptable -> None
      | le :: _ ->
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
      | Expr.Lit x, Lit y when x <> y -> Some true
      | UnOp (ToStringOp, _), Lit (String "")
      | Lit (String ""), UnOp (ToStringOp, _) -> Some true
      | UnOp (ToStringOp, _), Lit (String s)
        when not (Str.string_match (Str.regexp "0-9") (Str.first_chars s 1) 0)
        -> Some true
      | Lit (String s), UnOp (ToStringOp, _)
        when not (Str.string_match (Str.regexp "0-9") (Str.first_chars s 1) 0)
        -> Some true
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
  (* L.verbose (fun fmt -> fmt "List prefix: %a of %a" Expr.pp la Expr.pp lb); *)
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

(** Canonical symbolic numbers *)
type cnum = { conc : float; symb : float Expr.Map.t }

(* CNumber singleton - use carefully *)
let cnum_singleton (e : Expr.t) : cnum =
  match e with
  | Lit (Num n) -> { conc = n; symb = Expr.Map.empty }
  | _           -> { conc = 0.; symb = Expr.Map.singleton e 1. }

(* CNumber unary minus *)
let cnum_uminus (cn : cnum) : cnum =
  { conc = -.cn.conc; symb = Expr.Map.map (fun x -> -.x) cn.symb }

(* CNumber addition *)
let cnum_plus (cn1 : cnum) (cn2 : cnum) =
  let conc = cn1.conc +. cn2.conc in
  let symb =
    Expr.Map.fold
      (fun e v sum ->
        match Expr.Map.find_opt e sum with
        | None    -> Expr.Map.add e v sum
        | Some v' -> (
            let v = v +. v' in
            match v = 0. with
            | true  -> Expr.Map.remove e sum
            | false -> Expr.Map.add e v sum ))
      cn2.symb cn1.symb
  in
  { conc; symb }

(* CNumber subtraction *)
let cnum_minus (cn1 : cnum) (cn2 : cnum) = cnum_plus cn1 (cnum_uminus cn2)

(* CNumber constant multiplication *)
let cnum_cmult (c : float) (cn : cnum) =
  { conc = cn.conc *. c; symb = Expr.Map.map (fun v -> v *. c) cn.symb }

let cnum_cdiv (c : float) (cn : cnum) =
  assert (c <> 0.);
  { conc = cn.conc /. c; symb = Expr.Map.map (fun v -> v /. c) cn.symb }

let cnum_is_cposint (cn : cnum) : bool =
  Expr.Map.is_empty cn.symb && Float.is_integer cn.conc && cn.conc >= 0.

(* CNumber to Expression *)
let cnum_to_expr (cn : cnum) : Expr.t =
  match Expr.Map.is_empty cn.symb with
  | true  -> Expr.Lit (Num cn.conc)
  | false -> (
      let erest =
        Expr.Map.fold
          (fun e v erest ->
            let factor = if v = 1. then e else BinOp (Lit (Num v), FTimes, e) in
            if erest = Expr.Lit Nono then factor
            else BinOp (erest, FPlus, factor))
          cn.symb (Lit Nono)
      in
      match cn.conc = 0. with
      | true  -> erest
      | false -> BinOp (Lit (Num cn.conc), FPlus, erest) )

(* Expression to CNumber *)
let rec expr_to_cnum (e : Expr.t) =
  let f = expr_to_cnum in
  match e with
  (* Unary minus *)
  | UnOp (FUnaryMinus, e) -> cnum_uminus (f e)
  (* Addition and subtraction *)
  | BinOp (e1, FPlus, e2) -> cnum_plus (f e1) (f e2)
  | BinOp (e1, FMinus, e2) -> cnum_minus (f e1) (f e2)
  (* Multiplication with one operand being a number *)
  | BinOp (Lit (Num c), FTimes, e) | BinOp (e, FTimes, Lit (Num c)) ->
      cnum_cmult c (f e)
  (* Error cases *)
  | _ -> cnum_singleton e

let rec canonicalise (e : Expr.t) : Expr.t =
  let sort (e1 : Expr.t) (e2 : Expr.t) : Expr.t * Expr.t =
    if Stdlib.compare e1 e2 > 0 then (e2, e1) else (e1, e2)
  in

  let f = canonicalise in
  match e with
  | BinOp (e1, FPlus, e2) ->
      let fe1 = f e1 in
      let fe2 = f e2 in
      let fe1, fe2 = sort fe1 fe2 in
      BinOp (fe1, FPlus, fe2)
  (* Binary minus to unary minus *)
  | BinOp (e1, FMinus, e2) -> f (BinOp (e1, FPlus, UnOp (FUnaryMinus, e2)))
  | UnOp (FUnaryMinus, e) -> (
      let ce = f e in
      match ce with
      | BinOp (e1, FPlus, e2) ->
          BinOp (f (UnOp (FUnaryMinus, e1)), FPlus, f (UnOp (FUnaryMinus, e2)))
      | _                     -> UnOp (FUnaryMinus, ce) )
  | BinOp (e1, FTimes, e2) -> (
      let fe1 = f e1 in
      let fe2 = f e2 in
      match (fe1, fe2) with
      | BinOp (e1, FPlus, e2), fe2 ->
          f (BinOp (BinOp (e1, FTimes, fe2), FPlus, BinOp (e2, FTimes, fe2)))
      | fe1, BinOp (e1, FPlus, e2) ->
          f (BinOp (BinOp (fe1, FTimes, e1), FPlus, BinOp (fe1, FTimes, e2)))
      | fe1, fe2                   ->
          let fe1, fe2 = sort fe1 fe2 in
          BinOp (fe1, FTimes, fe2) )
  | _ -> e

let cut (el : Expr.t) (er : Expr.t) : bool * Expr.t * Expr.t =
  (* L.verbose (fun fmt -> fmt "In cut: %a, %a" Expr.pp el Expr.pp er); *)
  let success = ref false in
  let cl = expr_to_cnum (canonicalise el) in
  let cr = expr_to_cnum (canonicalise er) in
  let nl, nr =
    if cl.conc = 0. || cr.conc = 0. then (cl.conc, cr.conc)
    else (
      success := true;
      if cl.conc > cr.conc then (cl.conc -. cr.conc, 0.)
      else (0., cr.conc -. cl.conc) )
  in
  let restl, restr =
    Expr.Map.fold
      (fun e vr (restl, restr) ->
        match Expr.Map.find_opt e restl with
        | None    -> (restl, restr)
        | Some vl -> (
            match vl = vr with
            | true  -> (Expr.Map.remove e restl, Expr.Map.remove e restr)
            | false ->
                if vl > vr then
                  (Expr.Map.add e (vl -. vr) restl, Expr.Map.remove e restr)
                else (Expr.Map.remove e restl, Expr.Map.add e (vr -. vl) restr)
            ))
      cr.symb (cl.symb, cr.symb)
  in
  let cl, cr = ({ conc = nl; symb = restl }, { conc = nr; symb = restr }) in
  (!success, cnum_to_expr cl, cnum_to_expr cr)

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

  (* L.verbose (fun fmt -> fmt "Reducing Expr: %a" Expr.pp le); *)
  let rec find_lstsub_inn (lst : Expr.t) (start : Expr.t) =
    match lst with
    | LstSub (lst', start', _) -> find_lstsub_inn lst' start'
    | _                        -> (lst, start)
  in

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
              (Exceptions.Impossible
                 "reduce_lexpr: LVar x when reducing lvars: guaranteed by \
                  match/filter") )
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
                           "reduce_lexpr: all literals: guaranteed by \
                            match/filter"))
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
    | UnOp (UNot, BinOp (le1, FLessThan, le2)) ->
        f (BinOp (f le2, FLessThanEqual, f le1))
    | UnOp (UNot, BinOp (le1, FLessThanEqual, le2)) ->
        f (BinOp (f le2, FLessThan, f le1))
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
        | Lit (Int n) when 0 <= n -> (
            match lexpr_is_list gamma fle with
            | true  ->
                Option.value
                  ~default:(Expr.BinOp (fle, LstNth, fidx))
                  (get_nth_of_list pfs fle n)
            | false ->
                let err_msg =
                  Fmt.str "LstNth(%a, %a): list is not a GIL list." Expr.pp fle
                    Expr.pp idx
                in
                L.normal (fun fmt -> fmt "%s" err_msg);
                raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
            )
        | Lit (Num n) when Arith_Utils.is_int n && 0. <= n -> (
            match lexpr_is_list gamma fle with
            | true  ->
                Option.value
                  ~default:(Expr.BinOp (fle, LstNth, fidx))
                  (get_nth_of_list pfs fle (int_of_float n))
            | false ->
                let err_msg =
                  Fmt.str "LstNth(%a, %a): list is not a GIL list." Expr.pp fle
                    Expr.pp idx
                in
                L.normal (fun fmt -> fmt "%s" err_msg);
                raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
            )
        (* Index is a number, but is either not an integer or is negative *)
        | Lit (Int _) | Lit (Num _) ->
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
    | NOp (LstCat, fst :: rest) when PFS.mem pfs (Eq (fst, EList [])) ->
        f (NOp (LstCat, rest))
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
    | UnOp (FUnaryMinus, UnOp (FUnaryMinus, e)) -> f e
    | UnOp (LstLen, LstSub (_, _, e)) -> f e
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
                    | Lit (LList le)     ->
                        Lit (Num (float_of_int (List.length le)))
                    | EList le           -> Lit
                                              (Num
                                                 (float_of_int (List.length le)))
                    | NOp (LstCat, les)  ->
                        let les =
                          List.map (fun x -> Expr.UnOp (LstLen, x)) les
                        in
                        let le =
                          List.fold_left
                            (fun ac x -> Expr.BinOp (ac, FPlus, x))
                            (List.hd les) (List.tl les)
                        in
                        f le
                    | LstSub (_, _, len) -> len
                    | _                  -> def )
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
    (* Nested L-sub *)
    | LstSub (LstSub (ile1, ile2, ile3), fle2, fle3)
      when match find_lstsub_inn ile1 ile2 with
           | LVar x, _
             when List.exists
                    (fun x ->
                      match x with
                      | Expr.LVar _ -> false
                      | _           -> true)
                    (find_equalities pfs (LVar x)) -> true
           | _, LVar x
             when List.exists
                    (fun x ->
                      match x with
                      | Expr.LVar _ -> false
                      | _           -> true)
                    (find_equalities pfs (LVar x)) -> true
           | _ -> false -> (
        let fle1 = Expr.LstSub (ile1, ile2, ile3) in
        let base_expr = Expr.LstSub (fle1, fle2, fle3) in

        let inn_lst, inn_start = find_lstsub_inn ile1 ile2 in
        match (inn_lst, inn_start) with
        | LVar x, _
          when List.exists
                 (fun x ->
                   match x with
                   | Expr.LVar _ -> false
                   | _           -> true)
                 (find_equalities pfs (LVar x)) ->
            (* L.verbose (fun fmt ->
                fmt "Reducing: %a\n1st: Innermost list and start: %a and %a"
                  Expr.pp base_expr Expr.pp inn_lst Expr.pp inn_start); *)
            let eqs =
              List.filter
                (fun x ->
                  match x with
                  | Expr.LVar _ -> false
                  | _           -> true)
                (find_equalities pfs (LVar x))
            in
            let subst_expr = List.hd eqs in
            let att_exp =
              Expr.subst_expr_for_expr (LVar x) subst_expr base_expr
            in
            let reduced_att_exp = f att_exp in
            (* L.verbose (fun fmt ->
                fmt "1st: Attempted and reduced expr: %a and %a" Expr.pp att_exp
                  Expr.pp reduced_att_exp); *)
            if att_exp = reduced_att_exp then LstSub (fle1, fle2, fle3)
            else reduced_att_exp
        | _, LVar x
          when List.exists
                 (fun x ->
                   match x with
                   | Expr.LVar _ -> false
                   | _           -> true)
                 (find_equalities pfs (LVar x)) ->
            (* L.verbose (fun fmt ->
                fmt "Reducing: %a\n2nd: Innermost list and start: %a and %a"
                  Expr.pp base_expr Expr.pp inn_lst Expr.pp inn_start); *)
            let eqs =
              List.filter
                (fun x ->
                  match x with
                  | Expr.LVar _ -> false
                  | _           -> true)
                (find_equalities pfs (LVar x))
            in
            let subst_expr = List.hd eqs in
            let att_exp =
              Expr.subst_expr_for_expr (LVar x) subst_expr base_expr
            in
            let reduced_att_exp = f att_exp in
            (* L.verbose (fun fmt ->
                fmt "2nd: Attempted and reduced expr: %a and %a" Expr.pp att_exp
                  Expr.pp reduced_att_exp); *)
            if att_exp = reduced_att_exp then LstSub (fle1, fle2, fle3)
            else reduced_att_exp
        | _, _ -> LstSub (fle1, fle2, fle3) )
    | LstSub (l, Lit (Num n), BinOp (UnOp (LstLen, l'), FMinus, Lit (Num n')))
      when l = l' && n = n'
           &&
           let eqs = get_equal_expressions pfs l in
           List.exists
             (fun e ->
               match e with
               | Expr.NOp (LstCat, EList les :: _) ->
                   List.length les = int_of_float n
               | NOp (LstCat, Lit (LList les) :: _) ->
                   List.length les = int_of_float n
               | _ -> false)
             eqs ->
        let eqs = get_equal_expressions pfs l in
        let cat =
          List.filter_map
            (fun e ->
              match e with
              | Expr.NOp (LstCat, EList les :: rest)
                when List.length les = int_of_float n ->
                  Some (Expr.NOp (LstCat, rest))
              | NOp (LstCat, Lit (LList les) :: rest)
                when List.length les = int_of_float n ->
                  Some (NOp (LstCat, rest))
              | _ -> None)
            eqs
        in
        f (List.hd cat)
    | LstSub (le1, le2, le3) -> (
        let fle1 = f le1 in
        let fle2 = substitute_for_list_length pfs (f le2) in
        let fle3 = substitute_for_list_length pfs (f le3) in
        L.tmi (fun fmt ->
            fmt "REDUCTION: LstSub(%a, %a, %a)" Expr.pp fle1 Expr.pp fle2
              Expr.pp fle3);
        match (fle1, fle2, fle3) with
        | _, _, Lit (Num 0.) -> EList []
        | flx, Lit (Num 0.), UnOp (LstLen, fle1) when flx = fle1 -> fle1
        | NOp (LstCat, [ x ]), fle2, fle3 -> f (LstSub (x, fle2, fle3))
        | NOp (LstCat, flx :: _), Lit (Num 0.), UnOp (LstLen, fle1)
          when flx = fle1 -> fle1
        | NOp (LstCat, flx :: _), Lit (Num 0.), Lit (Num n)
          when let eqs = get_equal_expressions pfs flx in
               List.exists
                 (fun e ->
                   match e with
                   | Expr.EList les -> List.length les >= int_of_float n
                   | Lit (LList les) -> List.length les >= int_of_float n
                   | NOp (LstCat, EList les :: _) ->
                       List.length les >= int_of_float n
                   | NOp (LstCat, Lit (LList les) :: _) ->
                       List.length les >= int_of_float n
                   | _ -> false)
                 eqs ->
            let eqs = get_equal_expressions pfs flx in
            let eqs =
              List.filter_map
                (fun e ->
                  match e with
                  | Expr.EList les when List.length les >= int_of_float n ->
                      Some (Expr.EList les)
                  | Lit (LList les) when List.length les >= int_of_float n ->
                      Some (Lit (LList les))
                  | NOp (LstCat, EList les :: _)
                    when List.length les >= int_of_float n -> Some (EList les)
                  | NOp (LstCat, Lit (LList les) :: _)
                    when List.length les >= int_of_float n ->
                      Some (Lit (LList les))
                  | _ -> None)
                eqs
            in
            f (LstSub (List.hd eqs, Lit (Num 0.), Lit (Num n)))
        | le, Lit (Num 0.), Lit (Num n)
          when ( match le with
               | EList _ | Lit (LList _) -> false
               | _                       -> true )
               &&
               let eqs = get_equal_expressions pfs le in
               let eqs =
                 match le with
                 | LVar x ->
                     List.filter
                       (fun eq -> not (Containers.SS.mem x (Expr.lvars eq)))
                       eqs
                 | _      -> eqs
               in
               List.exists
                 (fun e ->
                   match e with
                   | Expr.EList les -> List.length les >= int_of_float n
                   | Lit (LList les) -> List.length les >= int_of_float n
                   | NOp (LstCat, EList les :: _) ->
                       List.length les >= int_of_float n
                   | NOp (LstCat, Lit (LList les) :: _) ->
                       List.length les >= int_of_float n
                   | _ -> false)
                 eqs ->
            L.tmi (fun fmt -> fmt "Expected case");
            let eqs = get_equal_expressions pfs le in
            let eqs =
              List.filter_map
                (fun e ->
                  match e with
                  | Expr.EList les when List.length les >= int_of_float n ->
                      Some (Expr.EList les)
                  | Lit (LList les) when List.length les >= int_of_float n ->
                      Some (Lit (LList les))
                  | NOp (LstCat, EList les :: _)
                    when List.length les >= int_of_float n -> Some (EList les)
                  | NOp (LstCat, Lit (LList les) :: _)
                    when List.length les >= int_of_float n ->
                      Some (Lit (LList les))
                  | _ -> None)
                eqs
            in
            L.tmi (fun fmt ->
                fmt "EQs: %a" Fmt.(brackets (list ~sep:comma Expr.pp)) eqs);
            f (LstSub (List.hd eqs, Lit (Num 0.), Lit (Num n)))
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
            L.verbose (fun fmt -> fmt "Got in!");
            let candidates = get_equal_expressions pfs fle1 in
            let candidates =
              List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
            in
            let candidates =
              List.filter
                (fun (x, lvars) -> Containers.SS.mem lx lvars)
                candidates
            in
            let leading_candidate = fst (List.hd candidates) in
            match leading_candidate with
            | LVar ly when ly = lx -> LVar lx
            | NOp (LstCat, flx :: _) when flx = LVar lx -> LVar lx
            | _ ->
                raise
                  (Exceptions.Impossible
                     "reduce_lexpr: candidates: guaranteed by match/filter") )
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
              L.verbose (fun fmt ->
                  fmt "ILE: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
              raise
                (ReductionException
                   (LstSub (fle1, fle2, fle3), "Invalid List Expression")) )
        | Lit (LList lst), Lit (Num _start), Lit (Num _end) -> (
            try
              Lit
                (LList
                   (Array.to_list
                      (Array.sub (Array.of_list lst) (int_of_float _start)
                         (int_of_float _end))))
            with _ ->
              L.verbose (fun fmt ->
                  fmt "ILE: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
              raise
                (ReductionException
                   (LstSub (fle1, fle2, fle3), "Invalid List Expression")) )
        (* COMPLEX: LSTSUB AND LSTCAT *)
        | NOp (LstCat, lel :: ler), fle2, fle3
          when (* Sub starts after first cat *)
               let lel_len = Expr.UnOp (LstLen, lel) in
               let diff = f (BinOp (fle2, FMinus, lel_len)) in
               check_ge_zero ~top_level:true pfs diff = Some true ->
            L.verbose (fun fmt ->
                fmt "LSUB: Start after first: %a" Expr.pp
                  (LstSub (fle1, fle2, fle3)));
            let result =
              f
                (LstSub
                   ( NOp (LstCat, ler),
                     BinOp (fle2, FMinus, Expr.UnOp (LstLen, lel)),
                     fle3 ))
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Start after first result: %a" Expr.pp result);
            result
        | NOp (LstCat, EList lel :: ler), Lit (Num n), fle3 when n > 0. ->
            (* Sub starts inside first cat *)
            L.verbose (fun fmt ->
                fmt "LSUB: Start inside first: %a" Expr.pp
                  (LstSub (fle1, fle2, fle3)));
            let rest_of_lel =
              Expr.LstSub
                ( EList lel,
                  fle2,
                  BinOp
                    (Lit (Num (float_of_int (List.length lel))), FMinus, fle2)
                )
            in
            let result =
              f (LstSub (NOp (LstCat, rest_of_lel :: ler), Lit (Num 0.), fle3))
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Start inside first result: %a" Expr.pp result);
            result
        | NOp (LstCat, lel :: ler), Lit (Num 0.), fle3
          when (* Sub ends after first cat *)
               let lel_len = Expr.UnOp (LstLen, lel) in
               let diff = f (BinOp (fle3, FMinus, lel_len)) in
               check_ge_zero ~top_level:true pfs diff = Some true ->
            L.verbose (fun fmt ->
                fmt "LSUB: Contains first: %a" Expr.pp
                  (LstSub (fle1, fle2, fle3)));
            let result =
              f
                (NOp
                   ( LstCat,
                     [
                       lel;
                       LstSub
                         ( NOp (LstCat, ler),
                           Lit (Num 0.),
                           BinOp (fle3, FMinus, UnOp (LstLen, lel)) );
                     ] ))
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Contains first result: %a" Expr.pp result);
            result
        | _ -> LstSub (fle1, fle2, fle3) )
    (* CHECK: FTimes and Div are the same, how does the 'when' scope? *)
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
            | (FPlus | FMinus) when lexpr_is_number ~gamma def ->
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
               | FMinus when (lexpr_is_number ~gamma def) ->
                 (match flel, fler with
                 (* 0 is the neutral *)
                 | Lit (Num 0.), x -> UnOp (FUnaryMinus, x)
                 | x, Lit (Num 0.) -> x
                 | Lit (Num x), _ when (x == nan) -> Lit (Num nan)
                 | _, Lit (Num x) when (x == nan) -> Lit (Num nan)
                 (* Transform to unary minus *)
                 | _, _ -> BinOp (flel, FPlus, (UnOp (FUnaryMinus, fler)))
                 ) *)
            | FTimes when lexpr_is_number ~gamma def -> (
                match (flel, fler) with
                (* 1 is the neutral *)
                | Lit (Num 1.), x | x, Lit (Num 1.) -> x
                | Lit (Num x), _ when x == nan -> Lit (Num nan)
                | _, Lit (Num x) when x == nan -> Lit (Num nan)
                | BinOp (Lit (Num x), FTimes, y), Lit (Num z) ->
                    BinOp (Lit (Num (x *. z)), FTimes, y)
                | Lit (Num z), BinOp (Lit (Num x), FTimes, y) ->
                    BinOp (Lit (Num (z *. x)), FTimes, y)
                (* Rest *)
                | _, _ -> def )
            | FDiv when lexpr_is_number ~gamma def -> (
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
                  | ESet left, ESet right ->
                      L.verbose (fun fmt -> fmt "Inside relevant SetDiff case.");
                      let candidate_result =
                        Expr.Set.elements
                          (Expr.Set.diff (Expr.Set.of_list left)
                             (Expr.Set.of_list right))
                      in
                      L.verbose (fun fmt ->
                          fmt "Candidate result: %a"
                            Fmt.(brackets (list ~sep:comma Expr.pp))
                            candidate_result);
                      let result =
                        if
                          List.for_all
                            (fun x -> not_set_member pfs x (ESet right))
                            candidate_result
                        then Expr.ESet candidate_result
                        else def
                      in
                      L.verbose (fun fmt ->
                          fmt "Actual result: %a" Expr.pp result);
                      result
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
            | FLessThan -> (
                match (flel, fler) with
                | UnOp (LstLen, _), Lit (Num n) when n <= 0. -> Lit (Bool false)
                | UnOp (LstLen, le), Lit (Num 1.) -> BinOp (le, Equal, EList [])
                | _ ->
                    let success, el, er = cut flel fler in
                    let f = if success then f else fun x -> x in
                    f (BinOp (el, FLessThan, er))
                    (* | _, _ ->
                        f
                          (BinOp
                             (BinOp (flel, FMinus, fler), FLessThan, Lit (Num 0.))) *)
                )
            | FLessThanEqual -> (
                (* L.verbose (fun fmt ->
                    fmt "Reducing <=: %a, %a" Expr.pp flel Expr.pp fler); *)
                let success, el, er = cut flel fler in
                match success with
                | false -> (
                    match
                      check_ge_zero ~top_level:true pfs
                        (f (BinOp (fler, FMinus, flel)))
                    with
                    | Some x -> Lit (Bool x)
                    | None   -> def )
                | true  -> f (BinOp (el, FLessThanEqual, er)) )
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
  | BinOp (l, FMinus, r) -> f (BinOp (l, FPlus, UnOp (FUnaryMinus, r)))
  (* Unary minus distributes over + *)
  | UnOp (FUnaryMinus, e) -> (
      match e with
      | BinOp (l, FPlus, r) ->
          f (BinOp (UnOp (FUnaryMinus, l), FPlus, UnOp (FUnaryMinus, r)))
      | _                   -> le )
  (* FPlus - we collect the positives and the negatives, see what we have and deal with them *)
  | BinOp (l, FPlus, r) ->
      let cl = expr_to_cnum l in
      let cr = expr_to_cnum r in
      cnum_to_expr (cnum_plus cl cr)
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
  (* FIXME: THIS IS A DISASTER *)
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

and check_ge_zero ?(top_level = false) (pfs : PFS.t) (e : Expr.t) : bool option
    =
  (* L.verbose (fun fmt -> fmt "Check >= 0: %a" Expr.pp e); *)
  let f = check_ge_zero pfs in
  match e with
  | Lit (Num n) -> Some (n >= 0.)
  | UnOp (LstLen, _) | UnOp (StrLen, _) -> Some true
  | (LVar _ | PVar _) when not top_level ->
      if
        List.exists
          (fun pf -> PFS.mem pfs pf)
          [ Formula.LessEq (Lit (Num 0.), e); Formula.Less (Lit (Num 0.), e) ]
      then Some true
      else if PFS.mem pfs (Formula.Less (e, Lit (Num 0.))) then Some false
      else None
  | LVar _ | PVar _ -> None
  | UnOp (FUnaryMinus, e') -> Option.map (fun b -> not b) (f e')
  | _ -> (
      let ce = expr_to_cnum e in
      match ce.conc >= 0. with
      | false -> None
      | true  ->
          Expr.Map.fold
            (fun e' c result ->
              if result <> Some true then result
              else if e' = e then None
              else if c > 0. then f e'
              else
                match f (UnOp (FUnaryMinus, e')) with
                | Some true -> Some true
                | _         -> None)
            ce.symb (Some true) )

and substitute_in_numeric_expr (le_to_find : Expr.t) (le_to_subst : Expr.t) le =
  let f = substitute_in_numeric_expr le_to_find le_to_subst in
  match le_to_find with
  (* Understand if le_to_find appears with a precise coefficient, and if it does, substitute *)
  | le_tf when lexpr_is_number le_tf -> (
      let c_le_tf = expr_to_cnum le_tf in
      let c_le_tf_symb = Expr.Map.bindings c_le_tf.symb in
      match c_le_tf_symb with
      | [] -> le
      | _  -> (
          let c_le = expr_to_cnum le in
          let coeffs =
            List.map
              (fun (factor, _) -> Expr.Map.find_opt factor c_le.symb)
              c_le_tf_symb
          in
          match List.for_all (fun c -> c <> None) coeffs with
          | false -> le
          | true  -> (
              let scaled_coeffs =
                List.map2
                  (fun c (_, s) -> Option.get c /. s)
                  coeffs c_le_tf_symb
              in
              L.verbose (fun fmt ->
                  fmt "SINE :: letf: %a, le: %a" Expr.pp le_to_find Expr.pp le);
              L.verbose (fun fmt ->
                  fmt "Coefficients in le    :: %a"
                    Fmt.(brackets (list ~sep:comma float))
                    (List.map (fun (_, v) -> v) c_le_tf_symb));
              L.verbose (fun fmt ->
                  fmt "Coefficients in le_tf :: %a"
                    Fmt.(brackets (list ~sep:comma float))
                    scaled_coeffs);
              let coeff = List.hd scaled_coeffs in
              match List.for_all (fun x -> x = coeff) scaled_coeffs with
              | false -> le
              | true  -> (
                  let base_diff = c_le.conc -. c_le_tf.conc in
                  match
                    (c_le.conc >= 0. && base_diff >= 0.)
                    || (c_le.conc < 0. && base_diff <= 0.)
                  with
                  | false -> le
                  | true  ->
                      let c_le_tf = cnum_cmult coeff c_le_tf in
                      let c_diff = cnum_minus c_le c_le_tf in
                      L.verbose (fun fmt ->
                          fmt "After subtraction: %a" Expr.pp
                            (cnum_to_expr c_diff));
                      let c_to_subst =
                        cnum_cmult coeff (expr_to_cnum le_to_subst)
                      in
                      let result = cnum_to_expr (cnum_plus c_diff c_to_subst) in
                      L.verbose (fun fmt ->
                          fmt "After re-addition: %a" Expr.pp result);
                      result ) ) ) )
  (* Recursively for LstSub *)
  | LstSub (lst, start, num) -> LstSub (lst, f start, f num)
  | _ -> le

and substitute_for_specific_length
    (pfs : PFS.t) (len_to_subst : Expr.t) (le : Expr.t) : Expr.t =
  let len_expr = Expr.UnOp (LstLen, len_to_subst) in
  let eqs = find_equalities pfs len_expr in
  let results =
    List.map (fun eq -> substitute_in_numeric_expr eq len_expr le) eqs
  in
  let results = List.filter (fun x -> x <> le) results in
  match results with
  | [ result ] -> result
  | _          -> le

and substitute_for_list_length (pfs : PFS.t) (le : Expr.t) : Expr.t =
  let len_eqs =
    List.filter_map
      (fun pf ->
        match pf with
        | Formula.Eq (UnOp (LstLen, LVar x), lex)
          when match lex with
               | UnOp (LstLen, LVar _) | Lit _ -> false
               | _ -> true -> Some (Expr.LVar x, lex)
        | Eq (lex, UnOp (LstLen, LVar x))
          when match lex with
               | UnOp (LstLen, LVar _) | Lit _ -> false
               | _ -> true -> Some (Expr.LVar x, lex)
        | _ -> None)
      (PFS.to_list pfs)
  in
  List.fold_left
    (fun le (len_expr, lex) -> substitute_for_specific_length pfs len_expr le)
    le len_eqs

let resolve_expr_to_location (pfs : PFS.t) (gamma : TypEnv.t) (e : Expr.t) :
    string option =
  let max_fuel = 5 in

  let rec resolve_expr_to_location_aux
      (fuel : int) (tried : Expr.Set.t) (to_try : Expr.t list) : string option =
    let f = resolve_expr_to_location_aux (fuel - 1) in
    match fuel = 0 with
    | true  -> None
    | false -> (
        match to_try with
        | []        -> None
        | e :: rest -> (
            match e with
            | Lit (Loc loc) | ALoc loc -> Some loc
            | _                        -> (
                let equal_e = get_equal_expressions pfs e in
                let equal_e = List.map (reduce_lexpr ~pfs ~gamma) equal_e in
                let ores =
                  List.find_opt
                    (fun x ->
                      match x with
                      | Expr.ALoc _ | Lit (Loc _) -> true
                      | _                         -> false)
                    equal_e
                in
                match ores with
                | Some (ALoc loc) | Some (Lit (Loc loc)) -> Some loc
                | _ -> (
                    let lvars_e =
                      List.map
                        (fun x -> Expr.LVar x)
                        (Containers.SS.elements (Expr.lvars e))
                    in
                    let found_subst =
                      List.map
                        (fun e -> (e, get_equal_expressions pfs e))
                        lvars_e
                    in
                    let found_subst =
                      List.filter_map
                        (fun (e, es) ->
                          match es with
                          | []      -> None
                          | es :: _ -> Some (e, es))
                        found_subst
                    in
                    let subst_e =
                      List.fold_left
                        (fun (e : Expr.t) (e_to, e_with) ->
                          Expr.subst_expr_for_expr ~to_subst:e_to
                            ~subst_with:e_with e)
                        e found_subst
                    in
                    let subst_e = reduce_lexpr ~pfs ~gamma subst_e in
                    match subst_e with
                    | ALoc loc | Lit (Loc loc) -> Some loc
                    | _                        ->
                        let new_tried = Expr.Set.add e tried in
                        let new_to_try = equal_e @ [ subst_e ] in
                        let new_to_try =
                          List.filter
                            (fun e -> not (Expr.Set.mem e new_tried))
                            new_to_try
                        in
                        f new_tried new_to_try ) ) ) )
  in

  resolve_expr_to_location_aux max_fuel Expr.Set.empty [ e ]

let rec reduce_formula_loop
    ?(top_level = false)
    (unification : bool)
    (pfs : PFS.t)
    (gamma : TypEnv.t)
    (a : Formula.t) : Formula.t =
  let f = reduce_formula_loop unification pfs gamma in
  let fe = reduce_lexpr_loop ~unification pfs gamma in
  (* L.verbose (fun fmt -> fmt "Reducing Formula: %a" Formula.pp a); *)
  let result : Formula.t =
    match a with
    | Eq (e1, e2) when e1 = e2 && lexpr_is_list gamma e1 -> True
    (* FIXME: INTEGER BYTE-BY-BYTE BREAKDOWN *)
    | Eq
        ( Lit (Num n),
          BinOp (BinOp (Lit (Num 256.), FTimes, LVar b1), FPlus, LVar b0) )
      when top_level
           && PFS.mem pfs (LessEq (Lit (Num 0.), LVar b0))
           && PFS.mem pfs (LessEq (Lit (Num 0.), LVar b1))
           && PFS.mem pfs (Less (LVar b0, Lit (Num 256.)))
           && PFS.mem pfs (Less (LVar b1, Lit (Num 256.))) ->
        if n > 65535. then False
        else
          let vb1 = floor (n /. 256.) in
          let vb0 = n -. vb1 in
          Formula.And (Eq (LVar b1, Lit (Num vb1)), Eq (LVar b0, Lit (Num vb0)))
    | Eq
        ( BinOp (BinOp (Lit (Num 256.), FTimes, LVar b1), FPlus, LVar b0),
          Lit (Num n) )
      when top_level
           && PFS.mem pfs (LessEq (Lit (Num 0.), LVar b0))
           && PFS.mem pfs (LessEq (Lit (Num 0.), LVar b1))
           && PFS.mem pfs (Less (LVar b0, Lit (Num 256.)))
           && PFS.mem pfs (Less (LVar b1, Lit (Num 256.))) ->
        if n > 65535. then False
        else
          let vb1 = floor (n /. 256.) in
          let vb0 = n -. vb1 in
          Formula.And (Eq (LVar b1, Lit (Num vb1)), Eq (LVar b0, Lit (Num vb0)))
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
    | Eq (NOp (LstCat, les), LVar x)
      when List.mem (Expr.LVar x) les
           && List.exists
                (fun e ->
                  match e with
                  | Expr.EList (_ :: _) | Lit (LList (_ :: _)) -> true
                  | _ -> false)
                les -> False
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
            | UnOp (LstRev, ll), UnOp (LstRev, rl) -> f (Eq (ll, rl))
            (* TODO: This is a specialised simplification, not sure for what, disabled for now
               | UnOp  (LstRev, full_list), BinOp (UnOp (LstRev, plist_left), LstCat, plist_right)
               | BinOp (UnOp (LstRev, plist_left), LstCat, plist_right), UnOp (LstRev, full_list)
                   ->
                   f (Eq (full_list, BinOp (UnOp (LstRev, plist_right), LstCat, plist_left))) *)
            | LstSub (e1, Lit (Num 0.), el), e2 when e1 = e2 ->
                f (Eq (UnOp (LstLen, e1), el))
            | LstSub (lst, start, Lit (Num n)), EList decomposition
              when let n = int_of_float n in
                   List.length decomposition = n
                   &&
                   let indices = List.mapi (fun i _ -> i) decomposition in
                   List.for_all2
                     (fun d i ->
                       d = Expr.BinOp (re1, LstNth, Lit (Num (float_of_int i))))
                     decomposition indices -> True
            | NOp (LstCat, fl :: rl), NOp (LstCat, fr :: rr) when fl = fr ->
                f (Eq (NOp (LstCat, rl), NOp (LstCat, rr)))
            | NOp (LstCat, fl :: rl), NOp (LstCat, fr :: rr)
              when List.hd (List.rev (fl :: rl)) = List.hd (List.rev (fr :: rr))
              ->
                f
                  (Eq
                     ( NOp (LstCat, List.rev (List.tl (List.rev (fl :: rl)))),
                       NOp (LstCat, List.rev (List.tl (List.rev (fr :: rr)))) ))
            | ( LVar lst,
                NOp (LstCat, LstSub (LVar lst', Lit (Num 0.), split) :: rest) )
              when lst = lst'
                   && PFS.mem pfs (Less (UnOp (LstLen, LVar lst), split)) ->
                False
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
                         "reduce_formula: string stuff: guaranteed by \
                          match/filter") )
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
            | Lit (Bool true), BinOp (e1, FLessThan, e2) -> Less (e1, e2)
            | Lit (Bool false), BinOp (e1, FLessThan, e2) -> Not (Less (e1, e2))
            (* FPlus theory -> theory? I would not go that far *)
            | le1, le2 when lexpr_is_number le1 && lexpr_is_number le2 ->
                let success, le1', le2' = cut le1 le2 in
                if success then Eq (le1', le2') else Eq (le1, le2)
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
    | Less (e1, e2) ->
        if PFS.mem pfs (LessEq (e2, e1)) then False
        else if PFS.mem pfs (Less (e2, e1)) then False
        else
          let le = Option.get (Formula.to_expr (Less (e1, e2))) in
          let re = fe le in
          let result, _ = Option.get (Formula.lift_logic_expr re) in
          result
    | LessEq (e1, e2) ->
        if PFS.mem pfs (LessEq (e2, e1)) then Eq (e1, e2)
        else if PFS.mem pfs (Less (e1, e2)) then True
        else if PFS.mem pfs (Less (e2, e1)) then False
        else
          let le = Option.get (Formula.to_expr (LessEq (e1, e2))) in
          let re = fe le in
          let result, _ = Option.get (Formula.lift_logic_expr re) in
          result
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
  reduce_formula_loop ~top_level:true unification pfs gamma a

let find_list_length_eqs (pfs : PFS.t) (e : Expr.t) : cnum list =
  let llen_expr = Expr.UnOp (LstLen, e) in
  let found_lengths =
    PFS.fold_left
      (fun found pf ->
        match pf with
        | Eq (e1, e2) when e1 = llen_expr -> expr_to_cnum e2 :: found
        | Eq (e1, e2) when e2 = llen_expr -> expr_to_cnum e1 :: found
        | _ -> found)
      [] pfs
  in
  List.rev found_lengths

let relate_llen
    (pfs : PFS.t) (gamma : TypEnv.t) (e : Expr.t) (lcat : Expr.t list) :
    (Formula.t * Containers.SS.t) option =
  (* Loop *)
  let rec relate_llen_loop (llen : cnum) (ac : Expr.t list) (lcat : Expr.t list)
      : (Expr.t list * int) option =
    let decide () =
      if cnum_is_cposint llen then Some (ac, int_of_float llen.conc) else None
    in
    match lcat with
    | []        -> decide ()
    | e :: rest -> (
        let e_llen = reduce_lexpr ~pfs ~gamma (Expr.UnOp (LstLen, e)) in
        let e_llen =
          let eqs = find_equalities pfs e_llen in
          match
            List.filter
              (fun e ->
                match e with
                | Expr.Lit (Num _) -> true
                | e
                  when Containers.SS.subset (Expr.lvars e)
                         (Expr.lvars (cnum_to_expr llen)) -> true
                | _ -> false)
              eqs
          with
          | [] -> e_llen
          | _  -> List.hd eqs
        in
        let new_llen = cnum_minus llen (expr_to_cnum e_llen) in
        L.verbose (fun fmt ->
            fmt "relate_llen_loop: %a consumed, %a left" Expr.pp e Expr.pp
              (cnum_to_expr new_llen));
        match e_llen with
        | Lit (Num n) -> (
            match new_llen.conc >= 0. with
            | true  -> relate_llen_loop new_llen (ac @ [ e ]) rest
            | false -> decide () )
        | _           -> (
            match Expr.Map.find_opt e_llen new_llen.symb with
            (* Non-integer coefficient, fail *)
            | Some n when not (Float.is_integer n) -> None
            (* Subtracted too much, decide *)
            | Some n when n < 0. -> decide ()
            (* Otherwise, proceed *)
            | _ -> relate_llen_loop new_llen (ac @ [ e ]) rest ) )
  in

  (* Auxiliary function *)
  let relate_llen_aux (e : Expr.t) (llen : cnum) (lcat : Expr.t list) =
    L.verbose (fun fmt ->
        fmt "Relate llen aux: %a: %a, %a" Expr.pp e Expr.pp (cnum_to_expr llen)
          Fmt.(brackets (list ~sep:semi Expr.pp))
          lcat);
    Option.map
      (fun (les, n) ->
        let new_vars = Array.to_list (Array.init n (fun _ -> LVar.alloc ())) in
        let new_lvars = List.map (fun x -> Expr.LVar x) new_vars in
        let new_lvars =
          match new_lvars with
          | [] -> []
          | _  -> [ Expr.EList new_lvars ]
        in
        let pf = Formula.Eq (e, NOp (LstCat, les @ new_lvars)) in
        L.verbose (fun fmt -> fmt "Constructed equality: %a" Formula.pp pf);
        (pf, Containers.SS.of_list new_vars))
      (relate_llen_loop llen [] lcat)
  in

  L.verbose (fun fmt ->
      fmt "Relate llen: %a, %a" Expr.pp e
        Fmt.(brackets (list ~sep:semi Expr.pp))
        lcat);
  (* Get info about list length, if any *)
  let llens = find_list_length_eqs pfs e in
  List.fold_left
    (fun result llen ->
      if result <> None then result else relate_llen_aux e llen lcat)
    None llens

let understand_lstcat
    (pfs : PFS.t) (gamma : TypEnv.t) (lcat : Expr.t list) (rcat : Expr.t list) :
    (Formula.t * Containers.SS.t) option =
  L.verbose (fun fmt ->
      fmt "Understanding LstCat: %a, %a"
        Fmt.(brackets (list ~sep:semi Expr.pp))
        lcat
        Fmt.(brackets (list ~sep:semi Expr.pp))
        rcat);
  match (lcat, rcat) with
  | [], _ | _, [] | [ _ ], _ | _, [ _ ] ->
      raise
        (Failure
           "INTERNAL ERROR: understand_lstcat: empty or one-element \
            concatenation")
  | el :: restl, er :: restr -> (
      match relate_llen pfs gamma el rcat with
      | Some result -> Some result
      | None        -> relate_llen pfs gamma er lcat )

let get_llen_difference
    (pfs : PFS.t) (gamma : TypEnv.t) (el : Expr.t) (er : Expr.t) : int option =
  match (get_length_of_list el, get_length_of_list er) with
  (* Can find lengths, it's a Christmas miracle *)
  | Some lx, Some ly -> Some (lx - ly)
  | _ -> (
      L.verbose (fun fmt ->
          fmt "Searching for length diff: %a and %a" Expr.pp el Expr.pp er);
      let lel = Expr.UnOp (LstLen, el) in
      let ler = Expr.UnOp (LstLen, er) in
      let candidates =
        PFS.fold_left
          (fun ac pf ->
            match reduce_formula ~pfs ~gamma pf with
            | Eq (e1, e2) when lexpr_is_number ~gamma e1 -> (
                (* L.verbose (fun fmt -> fmt "Candidate pf: %a" Formula.pp pf); *)
                let diff =
                  expr_to_cnum (canonicalise (BinOp (e1, FMinus, e2)))
                in
                match
                  ( Expr.Map.find_opt lel diff.symb,
                    Expr.Map.find_opt ler diff.symb )
                with
                | Some cl, Some cr ->
                    assert (cl <> 0. && cr <> 0.);
                    (* L.verbose (fun fmt ->
                        fmt "Got coefficients: %f and %f" cl cr); *)
                    let ndiff = cl +. cr in
                    if ndiff = 0. then
                      let new_diff_rest =
                        Expr.Map.remove ler (Expr.Map.remove lel diff.symb)
                      in
                      let diff = { conc = diff.conc; symb = new_diff_rest } in
                      let diff = cnum_cdiv (-.cl) diff in
                      cnum_to_expr diff :: ac
                    else ac
                | _                -> ac )
            | _ -> ac)
          [] pfs
      in
      let candidates =
        List.filter_map
          (fun e ->
            match e with
            | Expr.Lit (Num n) -> Some n
            | _                -> None)
          candidates
      in
      let candidates = List.sort_uniq Stdlib.compare candidates in
      match candidates with
      | [ n ] when Float.is_integer n ->
          L.verbose (fun fmt -> fmt "Found answer: %d" (int_of_float n));
          Some (int_of_float n)
      | _ -> None )

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
    | Pure f ->
        Pure (reduce_formula_loop ~top_level:true unification pfs gamma f)
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
