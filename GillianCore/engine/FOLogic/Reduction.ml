(** @canonical Gillian.Symbolic.FO_logic.Reduction *)

(* When reduction fails *)
exception ReductionException of Expr.t * string

module L = Logging
module CStore = Store.Make (CVal.M)

let _256 = Z.of_int 256
let _65535 = Z.of_int 65535

let normalise_cat (f : Expr.t -> Expr.t) (les : Expr.t list) : Expr.t =
  (* L.verbose (fun fmt ->
      fmt "inside normalise cat: %a" Expr.pp (NOp (LstCat, les))); *)
  (* Recursively process each catted list and destroy inner LstCats *)
  let nles =
    List.concat_map
      (fun x ->
        let fx = f x in
        match fx with
        | NOp (LstCat, les) -> les
        | _ -> [ fx ])
      les
  in
  (* L.verbose (fun fmt -> fmt "nles, v1: %a" Expr.pp (NOp (LstCat, nles))); *)
  (* Bring lists of literals together, part 1 *)
  let lsts, nles =
    List.fold_left
      (fun (lsts, ac) nle ->
        match (nle : Expr.t) with
        | Lit (LList lst) -> (lsts @ List.map (fun x -> Expr.Lit x) lst, ac)
        | EList lst -> (lsts @ lst, ac)
        | _ -> ([], ac @ [ Expr.EList lsts ] @ [ nle ]))
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
        | _ -> true)
      nles
  in
  let result : Expr.t =
    match nles with
    | [] -> EList []
    | [ x ] -> x
    | _ -> NOp (LstCat, nles)
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
    let exn msg = ReductionException (le, msg) in
    match (le : Expr.t) with
    (* Literals **)
    | Lit (LList lst) -> Expr.from_lit_list (LList lst)
    (* Literals, variables, alocs *)
    | Lit _ | LVar _ | ALoc _ | PVar _ -> le
    (* Binary Operators **)
    | NOp (LstCat, les) -> normalise_cat f les
    | BinOp (le, LstNth, n) -> (
        match (f le, f n) with
        | EList lst, Lit (Int n) -> (
            match List.nth_opt lst (Z.to_int n) with
            | Some k -> k
            | None -> raise (exn "Invalid List Expression"))
        | NOp (LstCat, EList lst :: tl), Lit (Int n) -> (
            let n = Z.to_int n in
            match List_utils.nth_or_size lst n with
            | Left k -> (* The element is in the first list *) k
            | Right sz ->
                (* The element isn't in the first list, so we cut that part, and we got the size *)
                BinOp (NOp (LstCat, tl), LstNth, Expr.int (n - sz)))
        | NOp (LstCat, LstSub (_lst, _start, len) :: tl), idx
          when Expr.equal len idx -> Expr.list_nth (NOp (LstCat, tl)) 0
        | _, Lit (Num _) -> raise (exn "LstNth with float")
        | le, n -> BinOp (le, LstNth, n))
    | BinOp (le1, op, le2) -> BinOp (f le1, op, f le2)
    (* Unary Operators **)
    | UnOp (Car, lst) -> (
        match f lst with
        | EList lst -> (
            try List.hd lst
            with Failure _ ->
              raise
                (ReductionException
                   (UnOp (Car, EList lst), "Invalid List Expression")))
        | NOp (LstCat, EList lst_l :: _) -> (
            try List.hd lst_l
            with Failure _ ->
              raise
                (ReductionException (UnOp (Car, lst), "Invalid List Expression"))
            )
        | lst -> UnOp (Car, lst))
    | UnOp (Cdr, lst) -> (
        match f lst with
        | EList lst -> (
            try EList (List.tl lst)
            with Failure _ ->
              raise
                (ReductionException
                   (UnOp (Cdr, EList lst), "Invalid List Expression")))
        | NOp (LstCat, EList lst_l :: tl) -> (
            try NOp (LstCat, EList (List.tl lst_l) :: tl)
            with Failure _ ->
              raise
                (ReductionException (UnOp (Cdr, lst), "Invalid List Expression"))
            )
        | lst -> UnOp (Cdr, lst))
    | UnOp (LstLen, le) -> (
        match f le with
        | EList lst -> Expr.int (List.length lst)
        | NOp (LstCat, EList lst :: tl) ->
            BinOp
              ( Expr.int (List.length lst),
                IPlus,
                f (UnOp (LstLen, NOp (LstCat, tl))) )
        | le -> UnOp (LstLen, le))
    | UnOp (op, le) -> UnOp (op, f le)
    | NOp (op, les) -> NOp (op, List.map f les)
    (* Uninteresting cases **)
    | EList lst -> EList (List.map f lst)
    | ESet lst -> ESet (List.map f lst)
    | LstSub (le1, le2, le3) -> LstSub (f le1, f le2, f le3)
    | Exists (bt, le) -> Exists (bt, f le)
    | EForall (bt, le) -> EForall (bt, f le)
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

(* -----------------------------------------------------
   Resolving locations and lists
   -----------------------------------------------------
   _____________________________________________________
*)

let resolve_list (le : Expr.t) (pfs : Formula.t list) : Expr.t =
  let rec search x pfs =
    match (pfs : Formula.t list) with
    | [] -> None
    | Eq (LVar x', le) :: rest when String.equal x' x -> (
        let le' = normalise_list_expressions le in
        match le' with
        (* Weird things can happen where x reduces to e.g. `{{ l-nth(x, 0) }}`.
           We check absence of cycles *)
        | (EList _ | NOp (LstCat, _)) when not (SS.mem x (Expr.lvars le')) ->
            Some le'
        | Expr.BinOp (_, LstRepeat, _) as ret
          when not (SS.mem x (Expr.lvars ret)) -> Some ret
        | _ -> search x rest)
    | Eq (le, LVar x') :: rest when String.equal x' x -> (
        let le' = normalise_list_expressions le in
        match le' with
        | (EList _ | NOp (LstCat, _)) when not (SS.mem x (Expr.lvars le')) ->
            Some le'
        | _ -> search x rest)
    | _ :: rest -> search x rest
  in

  match normalise_list_expressions le with
  | LVar x -> (
      match search x pfs with
      | Some le -> le
      | None -> LVar x)
  | le -> le

(**********************************)
(* Pure formulae helper functions *)
(**********************************)

let find_equalities (pfs : PFS.t) (le : Expr.t) : Expr.t list =
  let lpfs = PFS.to_list pfs in
  let lpfs =
    List.find_all
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> Expr.equal x le || Expr.equal y le
        | _ -> false)
      lpfs
  in
  let result =
    List.map
      (fun x ->
        match x with
        | Formula.Eq (x, y) -> if Expr.equal x le then y else x
        | _ ->
            raise
              (Exceptions.Impossible
                 "find_equalities_in_pfs: guarantee by match/filter"))
      lpfs
  in
  result

(***************************)
(* TYPING HELPER FUNCTIONS *)
(***************************)

let typable (gamma : Type_env.t) (le : Expr.t) (target_type : Type.t) : bool =
  let t, success = Typing.type_lexpr gamma le in
  if success then
    Option.fold ~some:(fun t -> Type.equal t target_type) ~none:true t
  else
    let msg : string =
      Fmt.str "TYPE ERROR: %a not typable in typing environment %a" Expr.pp le
        Type_env.pp gamma
    in
    L.fail msg

(* Lists *)
let lexpr_is_list (gamma : Type_env.t) (le : Expr.t) : bool =
  typable gamma le ListType

(* Strings *)
let lexpr_is_string (gamma : Type_env.t) (le : Expr.t) : bool =
  typable gamma le StringType

(* Numbers *)
let lexpr_is_number ?(gamma = Type_env.init ()) (le : Expr.t) : bool =
  typable gamma le NumberType

let lexpr_is_int ?(gamma = Type_env.init ()) (le : Expr.t) : bool =
  typable gamma le IntType

(* Booleans *)
let lexpr_is_bool (gamma : Type_env.t) (le : Expr.t) : bool =
  typable gamma le BooleanType

(* Sets *)
let lexpr_is_set (gamma : Type_env.t) (le : Expr.t) : bool =
  typable gamma le SetType

let get_equal_expressions (pfs : PFS.t) nle =
  let res =
    List.rev
      (PFS.fold_left
         (fun ac a ->
           match (a : Formula.t) with
           | Eq (le1, le2) when Expr.equal le1 nle -> le2 :: ac
           | Eq (le2, le1) when Expr.equal le1 nle -> le2 :: ac
           | Eq (e, EList el) | Eq (EList el, e) -> (
               match List_utils.index_of nle el with
               | None -> ac
               | Some index -> Expr.list_nth e index :: ac)
           | _ -> ac)
         [] pfs)
  in
  L.tmi (fun m -> m "ALL POTENTIAL EQUALS: %a" (Fmt.Dump.list Expr.pp) res);
  res

(***********************************)
(* LIST REASONING HELPER FUNCTIONS *)
(***********************************)

(* Finding the length of a list *)
let rec get_length_of_list (lst : Expr.t) : int option =
  let f = get_length_of_list in

  match lst with
  | Lit (LList l) -> Some (List.length l)
  | EList l -> Some (List.length l)
  | LstSub (_, _, Lit (Int len)) -> Some (Z.to_int len)
  | NOp (LstCat, les) -> (
      match List_utils.flaky_map f les with
      | None -> None
      | Some lens ->
          let lens = List.fold_left Int.add 0 lens in
          Some lens)
  | _ -> None

(* Finding the nth element of a list *)
let rec get_nth_of_list (pfs : PFS.t) (lst : Expr.t) (idx : int) : Expr.t option
    =
  let f = get_nth_of_list pfs in

  let err_msg = "get_nth_of_list: index out of bounds." in

  (* If we can compute the length of the list, then the index needs to be compatible *)
  let olen = get_length_of_list lst in
  let _ =
    match olen with
    | None -> ()
    | Some len ->
        if len <= idx then raise (ReductionException (Lit Nono, err_msg))
  in

  match lst with
  (* Nothing can be done for variables *)
  | PVar _ | LVar _ ->
      let lst' = resolve_list lst (PFS.to_list pfs) in
      if Expr.equal lst lst' then None else f lst' idx
  (* Base lists of literals and logical expressions *)
  | Lit (LList l) ->
      assert (idx < List.length l);
      Some (Lit (List.nth l idx))
  | EList l ->
      assert (idx < List.length l);
      Some (List.nth l idx)
  | LstSub (lst, Lit (Int start), Lit (Int _)) -> (
      match lst with
      | EList l -> (
          match List.nth_opt l (Z.to_int start + idx) with
          | Some _ as e -> e
          | None -> raise (ReductionException (lst, "non-existent list-nth")))
      | Lit (LList l) -> (
          match List.nth_opt l (Z.to_int start + idx) with
          | Some e -> Some (Lit e)
          | None -> raise (ReductionException (lst, "non-existent list-nth")))
      | LVar x ->
          let eqs = find_equalities pfs (LVar x) in
          List.find_map (fun e -> f e (Z.to_int start + idx)) eqs
      | _ -> None)
  | LstSub _ -> None
  | NOp (LstCat, lel :: ler) ->
      Option.bind (get_length_of_list lel) @@ fun llen ->
      let lst, idx =
        if idx < llen then (lel, idx) else (NOp (LstCat, ler), idx - llen)
      in
      f lst idx
  | Expr.BinOp (x, LstRepeat, _) -> Some x
  | _ -> None

(* Finding the nth element of a list *)
let get_head_and_tail_of_list ~pfs lst =
  let rec loop (pfs : PFS.t) (unacceptable : Expr.Set.t) (lst : Expr.t) :
      (Expr.t * Expr.t) option =
    let loop = loop pfs (Expr.Set.add lst unacceptable) in
    match lst with
    (* Nothing can be done for variables *)
    (* FIXME: This function is suboptimal *)
    | PVar _ -> None
    | LVar _ -> (
        let ole = get_equal_expressions pfs lst in
        let ole =
          List.filter (fun x -> not (Expr.Set.mem x unacceptable)) ole
        in
        match ole with
        | [] -> None
        | le :: _ ->
            L.verbose (fun fmt -> fmt "LE: %a\n\n" Expr.pp le);
            loop le)
    (* Base lists of literals and logical expressions *)
    | Lit (LList l) ->
        if l = [] then None else Some (Lit (List.hd l), Lit (LList (List.tl l)))
    | EList l -> if l = [] then None else Some (List.hd l, EList (List.tl l))
    | NOp (LstCat, lel :: ler) ->
        Option.value ~default:None
          (Option.map
             (fun (hd, tl) -> Some (hd, Expr.NOp (LstCat, tl :: ler)))
             (loop lel))
    | _ -> None
  in
  loop pfs Expr.Set.empty lst

(*************************************)
(* STRING REASONING HELPER FUNCTIONS *)
(*************************************)

(* Finding the length of a string *)
let rec get_length_of_string (str : Expr.t) : int option =
  let f = get_length_of_string in

  match str with
  | PVar _ -> None
  | LVar _ -> None
  | Lit (String s) -> Some (String.length s)
  | BinOp (sl, StrCat, sr) ->
      Option.value ~default:None
        (Option.map (fun ll -> Option.map (fun lr -> ll + lr) (f sr)) (f sl))
  | _ ->
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
    | None -> ()
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
  | true -> Some false
  | false -> (
      match (li, lj) with
      | Expr.Lit x, Lit y when not (Literal.equal x y) -> Some true
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
          else None)

(* I dont understand this! *)
let rec set_member (pfs : Formula.t list) m s =
  let f = set_member pfs m in
  match s with
  | Expr.LVar _ -> m = s
  | Expr.ESet s -> List.mem m s
  | Expr.NOp (SetUnion, les) -> List.exists (fun x -> f x) les
  | Expr.NOp (SetInter, les) -> List.for_all (fun x -> f x) les
  | _ -> List.mem (Formula.SetMem (m, s)) pfs

let rec not_set_member pfs m s =
  let f = not_set_member pfs m in
  match s with
  | Expr.NOp (SetUnion, les) -> List.for_all (fun x -> f x) les
  | Expr.NOp (SetInter, les) -> List.exists (fun x -> f x) les
  | Expr.ESet les ->
      List.for_all (fun le -> is_different pfs m le = Some true) les
  | _ -> List.mem (Formula.Not (Formula.SetMem (m, s))) pfs

let rec set_subset pfs s s' =
  let f = set_subset pfs s in
  match s' with
  | Expr.LVar _ -> s = s'
  | Expr.NOp (SetUnion, les) -> List.exists f les
  | Expr.NOp (SetInter, les) -> List.for_all f les
  | _ -> (
      match s with
      | Expr.ESet les -> List.for_all (fun x -> set_member pfs x s') les
      | _ -> false)

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
      | _ :: rest -> contained_in_union rest le1 le2)
  | _ -> false

let all_different pfs les =
  let result = ref true in
  let len = List.length les in
  let les = Array.of_list les in
  let i = ref 0 in
  while !result && !i < len - 1 do
    let j = ref (!i + 1) in
    while !result && !j < len do
      let li, lj = (les.(!i), les.(!j)) in
      let () =
        match is_different pfs li lj with
        | Some true -> ()
        | _ -> result := false
      in
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
    | _ -> la
  in
  let lb =
    match la with
    | NOp (LstCat, [ x ]) -> x
    | _ -> lb
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
        | _ -> nono)
    | NOp (LstCat, x :: y), lb -> (
        match f x lb with
        | false, _ -> nono
        | true, suffix -> f (NOp (LstCat, y)) suffix)
    | LVar a, lb -> (
        let lvars_lb = Expr.lvars lb in
        let candidates = get_equal_expressions pfs la in
        let candidates =
          List.map (fun (x : Expr.t) -> (x, Expr.lvars x)) candidates
        in
        let candidates =
          List.filter
            (fun (_, lvars) -> not (SS.is_empty (SS.inter lvars_lb lvars)))
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
                (fun (_, lvars) -> Containers.SS.mem a lvars)
                candidates
            in
            match candidates with
            | [ (x, _) ] -> f (LVar a) x
            | _ -> nono))
    | _, _ -> nono

let prefix_catch pfs (x : Expr.t) (y : string) =
  match x with
  | NOp (LstCat, x) ->
      PFS.exists
        (fun pf ->
          match pf with
          | Eq (NOp (LstCat, lx), NOp (LstCat, LVar y' :: _)) when y = y' -> (
              match List_utils.list_sub lx 0 (List.length x) with
              | Some x' -> x' = x
              | _ -> false)
          | _ -> false)
        pfs
  | LVar x ->
      PFS.exists
        (fun pf ->
          match pf with
          | Eq (NOp (LstCat, LVar x' :: _), NOp (LstCat, LVar y' :: _)) ->
              (x' = x && y = y') || (y' = x && x' = y)
          | _ -> false)
        pfs
  | _ -> false

(*************)
(* REDUCTION *)
(*************)

module Canonical = struct
  module type P = sig
    type t

    val one : t
    val zero : t
    val neg : t -> t
    val add : t -> t -> t
    val mul : t -> t -> t
    val sub : t -> t -> t
    val equal : t -> t -> bool
    val is_integer : t -> bool
    val is_positive : t -> bool
    val of_expr : Expr.t -> t option
    val to_expr : t -> Expr.t
    val e_times : Expr.t -> Expr.t -> Expr.t
    val e_plus : Expr.t -> Expr.t -> Expr.t
    val binop_plus : BinOp.t
    val binop_times : BinOp.t
    val binop_minus : BinOp.t
    val unop_minus : UnOp.t
    val canonicalise : Expr.t -> Expr.t
  end

  module Make (P : P) = struct
    type t = { conc : P.t; symb : P.t Expr.Map.t }

    (** Use carefuly! *)
    let singleton (e : Expr.t) : t =
      match P.of_expr e with
      | Some n -> { conc = n; symb = Expr.Map.empty }
      | None -> { conc = P.zero; symb = Expr.Map.singleton e P.one }

    let to_expr (c : t) : Expr.t =
      if Expr.Map.is_empty c.symb then P.to_expr c.conc
      else
        let erest =
          Expr.Map.fold
            (fun e v erest ->
              let factor = P.e_times (P.to_expr v) e in
              match erest with
              | Some e -> Some (P.e_plus e factor)
              | None -> Some factor)
            c.symb None
        in
        let erest = Option.get erest in
        (* guaranteed to be not-none because c.symb isn't empty *)
        if P.equal c.conc P.zero then erest
        else P.e_plus (P.to_expr c.conc) erest

    let uminus (c : t) : t =
      { conc = P.neg c.conc; symb = Expr.Map.map P.neg c.symb }

    let plus (c1 : t) (c2 : t) =
      let conc = P.add c1.conc c2.conc in
      let symb =
        Expr.Map.fold
          (fun e v sum ->
            match Expr.Map.find_opt e sum with
            | None -> Expr.Map.add e v sum
            | Some v' ->
                let v = P.add v v' in
                if P.equal v P.zero then Expr.Map.remove e sum
                else Expr.Map.add e v sum)
          c2.symb c1.symb
      in
      { conc; symb }

    let minus (c1 : t) (c2 : t) : t = plus c1 (uminus c2)

    let const_mult (const : P.t) ({ conc; symb } : t) : t =
      { conc = P.mul conc const; symb = Expr.Map.map (P.mul const) symb }

    let is_const_posint (c : t) : bool =
      Expr.Map.is_empty c.symb && P.is_integer c.conc && P.is_positive c.conc

    let rec of_expr (e : Expr.t) : t =
      match e with
      (* Unary minus *)
      | UnOp (o, e) when UnOp.equal P.unop_minus o -> uminus (of_expr e)
      (* Addition *)
      | BinOp (e1, k, e2) when BinOp.equal k P.binop_plus ->
          plus (of_expr e1) (of_expr e2)
      | BinOp (e1, k, e2) when BinOp.equal k P.binop_minus ->
          minus (of_expr e1) (of_expr e2)
      | BinOp (e1, k, e2) when BinOp.equal k P.binop_times -> (
          match P.of_expr e1 with
          | Some n -> const_mult n (of_expr e2)
          | None -> (
              match P.of_expr e2 with
              | Some n -> const_mult n (of_expr e1)
              | None -> singleton e))
      | _ -> singleton e

    let cut (el : Expr.t) (er : Expr.t) : bool * Expr.t * Expr.t =
      (* L.verbose (fun fmt -> fmt "In cut: %a, %a" Expr.pp el Expr.pp er); *)
      let success = ref false in
      let cl = of_expr (P.canonicalise el) in
      let cr = of_expr (P.canonicalise er) in
      let nl, nr =
        if P.equal cl.conc P.zero || P.equal cr.conc P.zero then
          (cl.conc, cr.conc)
        else (
          success := true;
          if cl.conc > cr.conc then (P.sub cl.conc cr.conc, P.zero)
          else (P.zero, P.sub cr.conc cl.conc))
      in
      let restl, restr =
        Expr.Map.fold
          (fun e vr (restl, restr) ->
            match Expr.Map.find_opt e restl with
            | None -> (restl, restr)
            | Some vl -> (
                match vl = vr with
                | true -> (Expr.Map.remove e restl, Expr.Map.remove e restr)
                | false ->
                    if vl > vr then
                      ( Expr.Map.add e (P.sub vl vr) restl,
                        Expr.Map.remove e restr )
                    else
                      ( Expr.Map.remove e restl,
                        Expr.Map.add e (P.sub vr vl) restr )))
          cr.symb (cl.symb, cr.symb)
      in
      let cl, cr = ({ conc = nl; symb = restl }, { conc = nr; symb = restr }) in
      (!success, to_expr cl, to_expr cr)

    let canonicalise (e : Expr.t) : Expr.t = to_expr (of_expr e)
  end
end

module Cint = Canonical.Make (struct
  include Z

  let is_integer _ = true
  let is_positive x = Z.geq x Z.zero

  let of_expr (e : Expr.t) : t option =
    match e with
    | Lit (Int n) -> Some n
    | _ -> None

  let to_expr = Expr.int_z
  let e_times = Expr.Infix.( * )
  let e_plus = Expr.Infix.( + )

  let binop_plus, binop_minus, binop_times, unop_minus =
    (BinOp.IPlus, BinOp.IMinus, BinOp.ITimes, UnOp.IUnaryMinus)

  let rec canonicalise (e : Expr.t) : Expr.t =
    let sort (e1 : Expr.t) (e2 : Expr.t) : Expr.t * Expr.t =
      if Stdlib.compare e1 e2 > 0 then (e2, e1) else (e1, e2)
    in
    let ineg e = Expr.UnOp (IUnaryMinus, e) in

    let f = canonicalise in
    let open Expr.Infix in
    match e with
    | BinOp (e1, IPlus, e2) ->
        let fe1, fe2 = sort (f e1) (f e2) in
        fe1 + fe2
    (* Binary minus to unary minus *)
    | BinOp (e1, IMinus, e2) -> f (e1 + ineg e2)
    | UnOp (IUnaryMinus, e) -> (
        match f e with
        | BinOp (e1, IPlus, e2) -> f (ineg e1) + f (ineg e2)
        | ce -> UnOp (IUnaryMinus, ce))
    | BinOp (e1, ITimes, e2) -> (
        match (f e1, f e2) with
        | BinOp (e1, IPlus, e2), fe2 -> f ((e1 * fe2) + (e2 * fe2))
        | fe1, BinOp (e1, IPlus, e2) -> f ((fe1 * e1) + (fe1 * e2))
        | fe1, fe2 ->
            let fe1, fe2 = sort fe1 fe2 in
            fe1 * fe2)
    | _ -> e
end)

module Cnum = Canonical.Make (struct
  include Float

  let is_integer = Float.is_integer
  let is_positive x = x >= 0.

  let of_expr (e : Expr.t) : t option =
    match e with
    | Lit (Num n) -> Some n
    | _ -> None

  let to_expr = Expr.num
  let e_times = Expr.Infix.( *. )
  let e_plus = Expr.Infix.( +. )

  let binop_plus, binop_minus, binop_times, unop_minus =
    (BinOp.FPlus, BinOp.FMinus, BinOp.FTimes, UnOp.FUnaryMinus)

  let rec canonicalise (e : Expr.t) : Expr.t =
    let sort (e1 : Expr.t) (e2 : Expr.t) : Expr.t * Expr.t =
      if Stdlib.compare e1 e2 > 0 then (e2, e1) else (e1, e2)
    in
    let fneg e = Expr.UnOp (FUnaryMinus, e) in

    let f = canonicalise in
    let open Expr.Infix in
    match e with
    | BinOp (e1, FPlus, e2) ->
        let fe1, fe2 = sort (f e1) (f e2) in
        fe1 +. fe2
    (* Binary minus to unary minus *)
    | BinOp (e1, FMinus, e2) -> f (e1 +. fneg e2)
    | UnOp (FUnaryMinus, e) -> (
        match f e with
        | BinOp (e1, FPlus, e2) -> f (fneg e1) +. f (fneg e2)
        | ce -> UnOp (FUnaryMinus, ce))
    | BinOp (e1, FTimes, e2) -> (
        match (f e1, f e2) with
        | BinOp (e1, FPlus, e2), fe2 -> f ((e1 *. fe2) +. (e2 *. fe2))
        | fe1, BinOp (e1, FPlus, e2) -> f ((fe1 *. e1) +. (fe1 *. e2))
        | fe1, fe2 ->
            let fe1, fe2 = sort fe1 fe2 in
            fe1 *. fe2)
    | _ -> e
end)

let find_list_length_eqs (pfs : PFS.t) (e : Expr.t) : Cint.t list =
  let llen_expr = Expr.UnOp (LstLen, e) in
  let found_lengths =
    PFS.fold_left
      (fun found pf ->
        match pf with
        | Eq (e1, e2) when e1 = llen_expr -> Cint.of_expr e2 :: found
        | Eq (e1, e2) when e2 = llen_expr -> Cint.of_expr e1 :: found
        | _ -> found)
      [] pfs
  in
  List.rev found_lengths

let rec reduce_binop_inttonum_const
    matching
    reduce_lvars
    pfs
    gamma
    (l : Expr.t)
    (r : Expr.t)
    (op : BinOp.t) : Expr.t option =
  let open Utils.Syntaxes.Option in
  let f = reduce_lexpr_loop ~matching ~reduce_lvars pfs gamma in
  match (l, r) with
  | Lit (Num x), UnOp (IntToNum, e) | UnOp (IntToNum, e), Lit (Num x) ->
      let* () = if snd (modf x) = 0.0 then Some () else None in
      let l = Expr.Lit (Int (Z.of_float x)) in
      let r = f e in
      let+ op =
        BinOp.(
          match op with
          | Equal -> Some Equal
          | FLessThan -> Some ILessThan
          | FLessThanEqual -> Some ILessThanEqual
          | FPlus -> Some IPlus
          | FMinus -> Some IMinus
          | FTimes -> Some ITimes
          | BitwiseAndF -> Some BitwiseAnd
          | BitwiseOrF -> Some BitwiseOr
          | BitwiseXorF -> Some BitwiseXor
          | LeftShiftF -> Some LeftShiftF
          | SignedRightShiftF -> Some SignedRightShift
          | UnsignedRightShiftF -> Some UnsignedRightShift
          | _ -> None)
      in
      Expr.BinOp (l, op, r)
  | _ -> None

(**
  Reduction of logical expressions
  - gamma is used for:
  - pfs  are used for: Car, Cdr, SetDiff
*)
and reduce_lexpr_loop
    ?(matching = false)
    ?(reduce_lvars = false)
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (le : Expr.t) =
  let f = reduce_lexpr_loop ~matching ~reduce_lvars pfs gamma in

  (* L.verbose (fun fmt -> fmt "Reducing Expr: %a" Expr.pp le); *)
  let rec find_lstsub_inn (lst : Expr.t) (start : Expr.t) =
    match lst with
    | LstSub (lst', start', _) -> find_lstsub_inn lst' start'
    | _ -> (lst, start)
  in

  let result : Expr.t =
    match le with
    | Lit _ -> le
    | BinOp (BinOp (a, FTimes, _), FMod, c)
      when Expr.equal a c || Expr.equal a c -> Expr.num 0.
    | BinOp (x, FTimes, BinOp (y, FDiv, z)) when x = z -> f y
    | BinOp (BinOp (x, FDiv, y), FTimes, z) when y = z -> f x
    | BinOp (Lit (LList ll), Equal, Lit (LList lr)) -> Lit (Bool (ll = lr))
    | BinOp (left, BImpl, right) -> (
        let left = f left in
        match Formula.lift_logic_expr left with
        | None -> BinOp (left, BImpl, f right)
        | Some (True, _) -> f right
        | Some (False, _) -> Lit (Bool true)
        | Some (left_f, _) ->
            let pfs_with_left =
              let copy = PFS.copy pfs in
              let () = PFS.extend copy left_f in
              copy
            in
            let right =
              reduce_lexpr_loop ~matching ~reduce_lvars pfs_with_left gamma
                right
            in
            BinOp (left, BImpl, right))
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
    | BinOp (ALoc x, Equal, ALoc y) when not matching -> Lit (Bool (x = y))
    | LVar x when reduce_lvars -> (
        let equals = get_equal_expressions pfs (LVar x) in
        let lit_equals =
          List.filter
            (fun eq ->
              match eq with
              | Expr.Lit _ -> true
              | _ -> false)
            equals
        in
        match lit_equals with
        | [] -> LVar x
        | Lit l :: _ -> Lit l
        | _ ->
            raise
              (Exceptions.Impossible
                 "reduce_lexpr: LVar x when reducing lvars: guaranteed by \
                  match/filter"))
    (* Base lists *)
    | EList les -> (
        let fles = List.map f les in
        let all_literals =
          let rec loop l =
            match l with
            | [] -> Some []
            | Expr.Lit l :: r -> Option.map (fun x -> l :: x) (loop r)
            | _ -> None
          in
          loop fles
        in
        match all_literals with
        | Some lits -> Expr.Lit (LList lits)
        | None -> EList fles)
    (* Base sets *)
    | ESet les -> ESet (Expr.Set.elements (Expr.Set.of_list (List.map f les)))
    | UnOp (NumToInt, UnOp (IntToNum, le)) -> f le
    | UnOp (IntToNum, UnOp (NumToInt, le)) when PFS.mem pfs (IsInt le) -> f le
    (* Number-to-string-to-number-to-string-to... *)
    | UnOp (ToNumberOp, UnOp (ToStringOp, le)) -> (
        let fle = f le in
        match fle with
        | Lit (Num _) -> fle
        | fle -> (
            let tfle, how = Typing.type_lexpr gamma fle in
            match (how, tfle) with
            | true, Some NumberType -> fle
            | _, _ -> UnOp (ToNumberOp, UnOp (ToStringOp, fle))))
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
    | BinOp
        (BinOp (LVar x, IPlus, UnOp (IUnaryMinus, LVar y)), Equal, Lit (Int z))
      when Z.equal z Z.zero -> BinOp (LVar x, Equal, LVar y)
    (* List indexing *)
    | BinOp (le, LstNth, idx) -> (
        let fle = f le in
        let fidx = f idx in
        match fidx with
        (* Index is a non-negative integer *)
        | Lit (Int n) when Z.leq Z.zero n ->
            if lexpr_is_list gamma fle then
              Option.value
                ~default:(Expr.BinOp (fle, LstNth, fidx))
                (get_nth_of_list pfs fle (Z.to_int n))
            else
              let err_msg =
                Fmt.str "LstNth(%a, %a): list is not a GIL list." Expr.pp fle
                  Expr.pp idx
              in
              L.normal (fun fmt -> fmt "%s" err_msg);
              raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
        (* Index is a number, but is either not an integer or is negative *)
        | Lit (Int _) | Lit (Num _) ->
            let err_msg =
              "LstNth(list, index): index is smaller than zero or a float."
            in
            raise (ReductionException (BinOp (fle, LstNth, fidx), err_msg))
        (* All other cases *)
        | _ -> BinOp (fle, LstNth, fidx))
    (* String indexing *)
    | BinOp (le, StrNth, idx) -> (
        let fle = f le in
        let fidx = f idx in
        match fidx with
        (* Index is a non-negative integer *)
        | Lit (Num n) when Arith_utils.is_int n && 0. <= n -> (
            match lexpr_is_string gamma fle with
            | true ->
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
        | Lit (Num _) ->
            let err_msg =
              "StrNth(str, index): index is non-integer or smaller than zero."
            in
            raise (ReductionException (Expr.BinOp (fle, StrNth, fidx), err_msg))
        (* All other cases *)
        | _ -> BinOp (fle, StrNth, fidx))
    | NOp (SetUnion, les) -> (
        let fles = List.map f les in
        (* Flatten unions *)
        let unions, rest =
          List.partition
            (fun x ->
              match x with
              | Expr.NOp (SetUnion, _) -> true
              | _ -> false)
            fles
        in
        let unions =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.NOp (SetUnion, ls) -> ls
                | _ ->
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
              | _ -> false)
            fles
        in
        let lesets =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.ESet ls -> ls
                | _ -> raise (Failure "LSetUnion: joining ESets: impossible.")
              in
              ac @ ls)
            [] lesets
        in
        let lesets = Expr.Set.elements (Expr.Set.of_list lesets) in
        let fles = Expr.ESet lesets :: rest in
        (* Remove empty sets *)
        let fles =
          List.filter
            (function
              | Expr.ESet [] -> false
              | _ -> true)
            fles
        in
        (* Remove duplicates *)
        let fles = Expr.Set.elements (Expr.Set.of_list fles) in
        match fles with
        | [] -> ESet []
        | [ x ] -> x
        | _ -> NOp (SetUnion, fles))
    | BinOp (x, LstRepeat, Lit (Int i)) when Z.lt i (Z.of_int 100) ->
        let fx = f x in
        let result = List.init (Z.to_int i) (fun _ -> fx) in
        EList result
    | NOp (LstCat, LstSub (x1, Lit (Int z), z1) :: LstSub (x2, y2, z3) :: rest)
      when Z.equal z Z.zero && Expr.equal x1 x2 && Expr.equal z1 y2 ->
        f
          (NOp (LstCat, LstSub (x1, Expr.zero_i, BinOp (z1, IPlus, z3)) :: rest))
    | NOp (LstCat, fst :: rest) when PFS.mem pfs (Eq (fst, EList [])) ->
        f (NOp (LstCat, rest))
    | NOp (LstCat, [ x; LstSub (LVar y, UnOp (LstLen, x'), len) ])
      when x = x'
           && Cint.canonicalise len
              = Cint.canonicalise
                  (BinOp (UnOp (LstLen, LVar y), IMinus, UnOp (LstLen, x)))
           && prefix_catch pfs x y -> LVar y
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
              | _ -> false)
            fles
        in
        let inters =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.NOp (SetInter, ls) -> ls
                | _ ->
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
              | _ -> false)
            fles
        in
        let lesets =
          List.fold_left
            (fun ac u ->
              let ls =
                match u with
                | Expr.ESet ls -> ls
                | _ -> raise (Failure "LSetUnion: joining ESets: impossible.")
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
          | [] -> ESet []
          | [ x ] -> x
          | _ -> NOp (SetInter, fles))
    | UnOp (FUnaryMinus, UnOp (FUnaryMinus, e)) -> f e
    | UnOp (LstLen, BinOp (_, LstRepeat, e)) -> f e
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
            | e -> raise e)
        | _ -> (
            match op with
            | UNot -> (
                match fle with
                | UnOp (UNot, ex) -> f ex
                | BinOp (ex, BAnd, ey) ->
                    f (BinOp (UnOp (UNot, ex), BOr, UnOp (UNot, ey)))
                | BinOp (ex, BOr, ey) ->
                    f (BinOp (UnOp (UNot, ex), BAnd, UnOp (UNot, ey)))
                | _ -> def)
            (* The TypeOf operator *)
            | TypeOf -> (
                let tfle, how = Typing.type_lexpr gamma fle in
                match how with
                | false ->
                    let err_msg = "LTypeOf(le): expression is not typable." in
                    raise (ReductionException (def, err_msg))
                | true -> (
                    match tfle with
                    | None -> def
                    | Some t -> Lit (Type t)))
            (* List head *)
            | Car -> (
                match lexpr_is_list gamma fle with
                | true ->
                    let ohdtl = get_head_and_tail_of_list ~pfs fle in
                    Option.fold ~some:(fun (hd, _) -> f hd) ~none:def ohdtl
                | false ->
                    let err_msg = "UnOp(Car, list): list is not a GIL list." in
                    raise (ReductionException (def, err_msg)))
            (* List tail *)
            | Cdr -> (
                match lexpr_is_list gamma fle with
                | true ->
                    let ohdtl = get_head_and_tail_of_list ~pfs fle in
                    Option.fold ~some:(fun (_, tl) -> f tl) ~none:def ohdtl
                | false ->
                    let err_msg = "UnOp(Cdr, list): list is not a GIL list." in
                    raise (ReductionException (def, err_msg)))
            (* List length *)
            | LstLen -> (
                match lexpr_is_list gamma fle with
                | true -> (
                    match fle with
                    | Lit (LList le) -> Expr.int (List.length le)
                    | EList le -> Expr.int (List.length le)
                    | NOp (LstCat, les) ->
                        let les = List.map Expr.list_length les in
                        let le =
                          List.fold_left Expr.Infix.( + ) (List.hd les)
                            (List.tl les)
                        in
                        f le
                    | LstSub (_, _, len) -> len
                    | _ -> def)
                | false ->
                    let err_msg =
                      "UnOp(LstLen, list): list is not a GIL list."
                    in
                    raise (ReductionException (def, err_msg)))
            (* List reverse *)
            | LstRev -> (
                match lexpr_is_list gamma fle with
                | true -> (
                    match fle with
                    | Lit (LList le) -> Lit (LList (List.rev le))
                    | EList le -> EList (List.rev le)
                    | NOp (LstCat, les) ->
                        NOp
                          ( LstCat,
                            List.rev
                              (List.map (fun x -> Expr.UnOp (LstRev, x)) les) )
                    | _ -> def)
                | false ->
                    let err_msg =
                      "UnOp(LstRev, list): list is not a GIL list."
                    in
                    raise (ReductionException (def, err_msg)))
            (* List reverse *)
            | SetToList -> (
                match fle with
                | ESet le -> EList (Expr.Set.elements (Expr.Set.of_list le))
                | _ -> def)
            (* String length *)
            | StrLen -> (
                match lexpr_is_string gamma fle with
                | true ->
                    let len = get_length_of_string fle in
                    Option.fold
                      ~some:(fun len -> Expr.Lit (Num (float_of_int len)))
                      ~none:def len
                | false ->
                    let err_msg =
                      "UnOp(StrLen, list): string is not a GIL string."
                    in
                    raise (ReductionException (def, err_msg)))
            | FUnaryMinus when lexpr_is_number ~gamma def ->
                simplify_num_arithmetic_lexpr pfs gamma def
            | IUnaryMinus when lexpr_is_int ~gamma def ->
                simplify_int_arithmetic_lexpr pfs gamma def
            | _ -> UnOp (op, fle)))
    (* Nested L-sub *)
    | LstSub (LstSub (ile1, ile2, ile3), fle2, fle3)
      when match find_lstsub_inn ile1 ile2 with
           | LVar x, _
             when List.exists
                    (fun x ->
                      match x with
                      | Expr.LVar _ -> false
                      | _ -> true)
                    (find_equalities pfs (LVar x)) -> true
           | _, LVar x
             when List.exists
                    (fun x ->
                      match x with
                      | Expr.LVar _ -> false
                      | _ -> true)
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
                   | _ -> true)
                 (find_equalities pfs (LVar x)) ->
            (* L.verbose (fun fmt ->
                fmt "Reducing: %a\n1st: Innermost list and start: %a and %a"
                  Expr.pp base_expr Expr.pp inn_lst Expr.pp inn_start); *)
            let eqs =
              List.filter
                (fun x ->
                  match x with
                  | Expr.LVar _ -> false
                  | _ -> true)
                (find_equalities pfs (LVar x))
            in
            let subst_expr = List.hd eqs in
            let att_exp =
              Expr.subst_expr_for_expr ~to_subst:(LVar x) ~subst_with:subst_expr
                base_expr
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
                   | _ -> true)
                 (find_equalities pfs (LVar x)) ->
            (* L.verbose (fun fmt ->
                fmt "Reducing: %a\n2nd: Innermost list and start: %a and %a"
                  Expr.pp base_expr Expr.pp inn_lst Expr.pp inn_start); *)
            let eqs =
              List.filter
                (fun x ->
                  match x with
                  | Expr.LVar _ -> false
                  | _ -> true)
                (find_equalities pfs (LVar x))
            in
            let subst_expr = List.hd eqs in
            let att_exp =
              Expr.subst_expr_for_expr ~to_subst:(LVar x) ~subst_with:subst_expr
                base_expr
            in
            let reduced_att_exp = f att_exp in
            (* L.verbose (fun fmt ->
                fmt "2nd: Attempted and reduced expr: %a and %a" Expr.pp att_exp
                  Expr.pp reduced_att_exp); *)
            if att_exp = reduced_att_exp then LstSub (fle1, fle2, fle3)
            else reduced_att_exp
        | _, _ -> LstSub (fle1, fle2, fle3))
    | LstSub (l, Lit (Int n), BinOp (UnOp (LstLen, l'), IMinus, Lit (Int n')))
      when l = l' && n == n'
           &&
           let eqs = get_equal_expressions pfs l in
           List.exists
             (function
               | Expr.NOp (LstCat, EList les :: r) ->
                   Int.equal (List.compare_length_with les (Z.to_int n)) 0
                   && not (List.length r == 1 && Expr.equal (List.hd r) le)
               (* return (List.length les == n), but efficiently *)
               | NOp (LstCat, Lit (LList les) :: r) ->
                   (* return (List.length les == n), but efficiently *)
                   Int.equal (List.compare_length_with les (Z.to_int n)) 0
                   && not (List.length r == 1 && Expr.equal (List.hd r) le)
               | _ -> false)
             eqs ->
        Logging.tmi (fun m -> m "REDUCTION: Case l-sub(l, n, (l-len l) - n)");
        let eqs = get_equal_expressions pfs l in
        let cat =
          List.filter_map
            (function
              | Expr.NOp (LstCat, EList les :: rest)
                when Int.equal (List.compare_length_with les (Z.to_int n)) 0 ->
                  Some (Expr.NOp (LstCat, rest))
              | NOp (LstCat, Lit (LList les) :: rest)
                when Int.equal (List.compare_length_with les (Z.to_int n)) 0 ->
                  Some (NOp (LstCat, rest))
              | _ -> None)
            eqs
        in
        f (List.hd cat)
    | LstSub (e1, Lit (Int z), e3)
      when Z.equal z Z.zero
           && List.mem (Cint.of_expr e3) (find_list_length_eqs pfs e1) -> f e1
    | LstSub (le1, le2, le3) -> (
        let fle1 = f le1 in
        let fle2 = substitute_for_list_length pfs (f le2) in
        let fle3 = substitute_for_list_length pfs (f le3) in
        L.tmi (fun fmt ->
            fmt "REDUCTION: LstSub(%a, %a, %a)" Expr.pp fle1 Expr.pp fle2
              Expr.pp fle3);
        match (fle1, fle2, fle3) with
        | _, _, Lit (Int z) when Z.equal z Z.zero ->
            L.tmi (fun fmt -> fmt "Case 1");
            EList []
        | flx, Lit (Int z), UnOp (LstLen, fle1)
          when Z.equal z Z.zero && Expr.equal flx fle1 ->
            L.tmi (fun fmt -> fmt "Case 2");
            fle1
        | NOp (LstCat, [ x ]), fle2, fle3 ->
            L.tmi (fun fmt -> fmt "Case 3");
            f (LstSub (x, fle2, fle3))
        | NOp (LstCat, flx :: _), Lit (Int z), UnOp (LstLen, fle1)
          when Z.equal z Z.zero && Expr.equal flx fle1 ->
            L.tmi (fun fmt -> fmt "Case 4");
            fle1
        | NOp (LstCat, flx :: _), Lit (Int z), Lit (Int n)
          when Z.equal z Z.zero
               &&
               let eqs = get_equal_expressions pfs flx in
               let first =
                 List.find_map
                   (fun e ->
                     match e with
                     | Expr.EList les
                       when List.compare_length_with les (Z.to_int n) >= 0 ->
                         Some e
                     | Lit (LList les)
                       when List.compare_length_with les (Z.to_int n) >= 0 ->
                         Some e
                     | NOp (LstCat, (EList les as e) :: _)
                       when List.compare_length_with les (Z.to_int n) >= 0 ->
                         Some e
                     | NOp (LstCat, (Lit (LList les) as e) :: _)
                       when List.compare_length_with les (Z.to_int n) >= 0 ->
                         Some e
                     | _ -> None)
                   eqs
               in
               match first with
               | None -> false
               | Some first ->
                   let res =
                     Expr.list_sub ~lst:first ~start:(Expr.int 0)
                       ~size:(Expr.int_z n)
                   in
                   not (Expr.equal res le) ->
            L.tmi (fun fmt -> fmt "Case 5");
            let eqs = get_equal_expressions pfs flx in
            let first =
              List.find_map
                (fun e ->
                  match e with
                  | Expr.EList les
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | Lit (LList les)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | NOp (LstCat, (EList les as e) :: _)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | NOp (LstCat, (Lit (LList les) as e) :: _)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | _ -> None)
                eqs
            in
            let res =
              Expr.list_sub ~lst:(Option.get first) ~start:(Expr.int 0)
                ~size:(Expr.int_z n)
            in
            Logging.tmi (fun m ->
                m "Case 5:\nRes: %a\nOriginal: %a" Expr.pp res Expr.pp le);
            f res
        | le, Lit (Int z), Lit (Int n)
          when Z.equal z Z.zero
               && (match le with
                  | EList _ | Lit (LList _) -> false
                  | _ -> true)
               &&
               let eqs = get_equal_expressions pfs le in
               L.tmi (fun fmt ->
                   fmt "le: %a, z: %s, n: %s" Expr.pp le (Z.to_string z)
                     (Z.to_string n));
               L.tmi (fun fmt -> fmt "PFS:\n%a" PFS.pp pfs);
               L.tmi (fun fmt ->
                   fmt "Found eqs: %a: %a" Expr.pp le
                     Fmt.(list ~sep:comma Expr.pp)
                     eqs);
               let eqs =
                 match le with
                 | LVar x ->
                     List.filter
                       (fun eq -> not (Containers.SS.mem x (Expr.lvars eq)))
                       eqs
                 | _ -> eqs
               in
               List.exists
                 (function
                   (* Returns true if length les >= n, but efficiently *)
                   | Expr.EList les ->
                       List.compare_length_with les (Z.to_int n) >= 0
                   | Lit (LList les) ->
                       List.compare_length_with les (Z.to_int n) >= 0
                   | NOp (LstCat, EList les :: _) ->
                       List.compare_length_with les (Z.to_int n) >= 0
                   | NOp (LstCat, Lit (LList les) :: _) ->
                       List.compare_length_with les (Z.to_int n) >= 0
                   | _ -> false)
                 eqs ->
            L.tmi (fun fmt -> fmt "Case 6");
            let eqs = get_equal_expressions pfs le in
            let first =
              List.find_map
                (fun e ->
                  match e with
                  | Expr.EList les
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | Lit (LList les)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | NOp (LstCat, (EList les as e) :: _)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | NOp (LstCat, (Lit (LList les) as e) :: _)
                    when List.compare_length_with les (Z.to_int n) >= 0 ->
                      Some e
                  | _ -> None)
                eqs
            in
            (* L.tmi (fun fmt ->
                fmt "EQs: %a" Fmt.(brackets (list ~sep:comma Expr.pp)) eqs); *)
            f
              (Expr.list_sub ~lst:(Option.get first) ~start:(Expr.int 0)
                 ~size:(Expr.int_z n))
        | fle1, UnOp (LstLen, lx), fle3 when fst (list_prefix pfs lx fle1) ->
            L.tmi (fun fmt -> fmt "Case 7");
            let _, suffix = list_prefix pfs lx fle1 in
            f (LstSub (suffix, Expr.zero_i, fle3))
        | fle1, Lit (Int z), UnOp (LstLen, LVar lx)
          when Z.equal z Z.zero
               && List.exists
                    (function
                      | (Expr.LVar lx' | NOp (LstCat, LVar lx' :: _))
                        when String.equal lx lx' -> true
                      | _ -> false)
                    (get_equal_expressions pfs fle1) ->
            L.tmi (fun fmt -> fmt "Case 8");
            let choice =
              List.find_map
                (fun e ->
                  match e with
                  | Expr.LVar ly when String.equal ly lx -> Some e
                  | NOp (LstCat, (LVar ly as e) :: _) when String.equal ly lx ->
                      Some e
                  | _ -> None)
                (get_equal_expressions pfs fle1)
            in
            (* Guaranteed to work because of the 'when' close *)
            Option.get choice
        | NOp (LstCat, EList les :: _), Lit (Int s), Lit (Int f)
          when List.compare_length_with les (Z.to_int s + Z.to_int f) >= 0 ->
            L.tmi (fun fmt -> fmt "Case 9");
            let result = Expr.LstSub (EList les, fle2, fle3) in
            L.verbose (fun fmt -> fmt "Very simple case: %a" Expr.pp result);
            result
        | EList lst, Lit (Int start), Lit (Int len) -> (
            L.tmi (fun fmt -> fmt "Case 10");
            L.verbose (fun fmt ->
                fmt "EList LSub: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
            match List_utils.list_sub lst (Z.to_int start) (Z.to_int len) with
            | Some result ->
                let result = Expr.EList result in
                L.verbose (fun fmt -> fmt "Resulting LSub: %a" Expr.pp result);
                result
            | None ->
                L.verbose (fun fmt ->
                    fmt "ILE: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
                raise
                  (ReductionException
                     (LstSub (fle1, fle2, fle3), "Invalid List Expression")))
        | Lit (LList lst), Lit (Int start), Lit (Int len) -> (
            L.tmi (fun fmt -> fmt "Case 11");
            L.verbose (fun fmt ->
                fmt "LList LSub: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
            match List_utils.list_sub lst (Z.to_int start) (Z.to_int len) with
            | Some result ->
                let result = Expr.Lit (LList result) in
                L.verbose (fun fmt -> fmt "Resulting LSub: %a" Expr.pp result);
                result
            | None ->
                L.verbose (fun fmt ->
                    fmt "ILE: %a" Expr.pp (LstSub (fle1, fle2, fle3)));
                raise
                  (ReductionException
                     (LstSub (fle1, fle2, fle3), "Invalid List Expression")))
        | NOp (LstCat, lel :: ler), fle2, fle3
        (* COMPLEX: LSTSUB AND LSTCAT *)
          when (* Sub starts after first cat *)
               let lel_len = Expr.UnOp (LstLen, lel) in
               let diff = f (BinOp (fle2, IMinus, lel_len)) in
               check_ge_zero_int ~top_level:true pfs diff = Some true ->
            L.tmi (fun fmt -> fmt "Case 12");
            L.verbose (fun fmt ->
                fmt "LSUB: Start after first: %a" Expr.pp
                  (LstSub (fle1, fle2, fle3)));
            let diff = f (BinOp (fle2, IMinus, Expr.UnOp (LstLen, lel))) in
            let new_lstsub = Expr.LstSub (NOp (LstCat, ler), diff, fle3) in
            L.verbose (fun fmt ->
                fmt "Recursively calling with: %a" Expr.pp new_lstsub);
            let result = f new_lstsub in
            L.verbose (fun fmt ->
                fmt "LSUB: Start after first result: %a" Expr.pp result);
            result
        | NOp (LstCat, EList lel :: ler), Lit (Int n), fle3 when Z.gt n Z.zero
          ->
            L.tmi (fun fmt -> fmt "Case 13");
            (* Sub starts inside first cat *)
            L.verbose (fun fmt ->
                fmt "LSUB: Start inside first: %a" Expr.pp
                  (LstSub (fle1, fle2, fle3)));
            let rest_of_lel =
              Expr.LstSub
                ( EList lel,
                  fle2,
                  BinOp (Lit (Int (Z.of_int (List.length lel))), IMinus, fle2)
                )
            in
            let result =
              f (LstSub (NOp (LstCat, rest_of_lel :: ler), Expr.zero_i, fle3))
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Start inside first result: %a" Expr.pp result);
            result
        | NOp (LstCat, lel :: ler), Lit (Int z), fle3
          when Z.equal z Z.zero
               &&
               (* Sub ends after first cat *)
               let lel_len = Expr.UnOp (LstLen, lel) in
               let diff = f (BinOp (fle3, IMinus, lel_len)) in
               check_ge_zero_int ~top_level:true pfs diff = Some true ->
            L.tmi (fun fmt -> fmt "Case 14");
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
                           Expr.zero_i,
                           BinOp (fle3, IMinus, UnOp (LstLen, lel)) );
                     ] ))
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Contains first result: %a" Expr.pp result);
            result
        | _ ->
            L.tmi (fun fmt -> fmt "Case 15");
            LstSub (fle1, fle2, fle3))
    (* CHECK: FTimes and Div are the same, how does the 'when' scope? *)
    | BinOp (lel, op, ler) -> (
        let op_is_or_and () =
          match op with
          | BOr | BAnd -> true
          | _ -> false
        in
        let flel, fler =
          (* If we're reducing A || B or A && B and either side have a reduction exception, it must be false *)
          let flel =
            try f lel with
            | ReductionException _ when op_is_or_and () -> Expr.bool false
            | exn -> raise exn
          in
          let fler =
            try f ler with
            | ReductionException _ when op_is_or_and () -> Expr.bool false
            | exn -> raise exn
          in
          (flel, fler)
        in
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
            | e -> raise e)
        | _ -> (
            match
              reduce_binop_inttonum_const matching reduce_lvars pfs gamma flel
                fler op
            with
            | Some e -> e
            | None -> (
                match op with
                | Equal -> (
                    if Expr.equal flel fler then Lit (Bool true)
                    else if
                      PFS.exists
                        (fun e ->
                          Formula.equal e (Eq (flel, fler))
                          || Formula.equal e (Eq (fler, flel)))
                        pfs
                    then Lit (Bool true)
                    else if
                      PFS.mem pfs (Not (Eq (flel, fler)))
                      || PFS.mem pfs (Not (Eq (fler, flel)))
                    then Lit (Bool false)
                    else
                      let t1, _ = Typing.type_lexpr gamma flel in
                      let t2, _ = Typing.type_lexpr gamma fler in
                      match (t1, t2) with
                      | Some t1, Some t2 ->
                          if Type.equal t1 t2 then def else Lit (Bool false)
                      | _, _ -> (
                          match (flel, fler) with
                          | UnOp (NumToInt, flel'), _ ->
                              BinOp (flel', op, UnOp (IntToNum, fler))
                          | _, UnOp (NumToInt, fler') ->
                              BinOp (UnOp (IntToNum, flel), op, fler')
                          | _, _ -> def))
                | (FPlus | FMinus) when lexpr_is_number ~gamma def ->
                    simplify_num_arithmetic_lexpr pfs gamma def
                | (IPlus | IMinus) when lexpr_is_int ~gamma def ->
                    simplify_int_arithmetic_lexpr pfs gamma def
                | FTimes when lexpr_is_number ~gamma def -> (
                    match (flel, fler) with
                    (* 1 is the neutral *)
                    | Lit (Num 1.), x | x, Lit (Num 1.) -> x
                    | Lit (Num x), _ when x == nan -> Lit (Num nan)
                    | _, Lit (Num x) when x == nan -> Lit (Num nan)
                    | BinOp (Lit (Num x), FTimes, y), Lit (Num z)
                    | Lit (Num z), BinOp (Lit (Num x), FTimes, y) ->
                        BinOp (Lit (Num (z *. x)), FTimes, y)
                    (* Rest *)
                    | _, _ -> def)
                | ITimes when lexpr_is_int ~gamma def -> (
                    match (flel, fler) with
                    | Lit (Int z), x when Z.equal z Z.one -> x
                    | x, Lit (Int z) when Z.equal z Z.one -> x
                    | (Lit (Int z) as zero), _ when Z.equal z Z.zero -> zero
                    | _, (Lit (Int z) as zero) when Z.equal z Z.zero -> zero
                    | BinOp (Lit (Int x), ITimes, y), Lit (Int z)
                    | Lit (Int z), BinOp (Lit (Int x), ITimes, y) ->
                        BinOp (Lit (Int (Z.mul z x)), ITimes, y)
                    | _, _ -> def)
                | FDiv when lexpr_is_number ~gamma def -> (
                    match (flel, fler) with
                    (* 1 is the neutral *)
                    | x, Lit (Num 1.) -> x
                    | _, _ -> def)
                | IDiv when lexpr_is_int ~gamma def -> (
                    match (flel, fler) with
                    | x, Lit (Int o) when Z.equal o Z.one -> x
                    | _, _ -> def)
                | BAnd when lexpr_is_bool gamma def -> (
                    match (flel, fler) with
                    (* 1 is the neutral *)
                    | Lit (Bool true), x | x, Lit (Bool true) -> x
                    | Lit (Bool false), _ | _, Lit (Bool false) ->
                        Lit (Bool false)
                    (* Rest *)
                    | _, _ ->
                        let fal, nfal =
                          Option.get (Formula.lift_logic_expr flel)
                        in
                        let far, nfar =
                          Option.get (Formula.lift_logic_expr fler)
                        in
                        if PFS.mem pfs nfal || PFS.mem pfs nfar then
                          Lit (Bool false)
                        else if PFS.mem pfs fal then f fler
                        else if PFS.mem pfs far then f flel
                        else BinOp (flel, BAnd, fler))
                | BOr when lexpr_is_bool gamma def -> (
                    match (flel, fler) with
                    (* 1 is the neutral *)
                    | Lit (Bool true), _ | _, Lit (Bool true) -> Lit (Bool true)
                    | Lit (Bool false), x | x, Lit (Bool false) -> x
                    (* Rest *)
                    | _, _ ->
                        let fal, nfal =
                          Option.get (Formula.lift_logic_expr flel)
                        in
                        let far, nfar =
                          Option.get (Formula.lift_logic_expr fler)
                        in
                        if PFS.mem pfs fal || PFS.mem pfs far then
                          Lit (Bool true)
                        else if PFS.mem pfs nfal then f fler
                        else if PFS.mem pfs nfar then f flel
                        else BinOp (flel, BOr, fler))
                | StrCat when lexpr_is_string gamma def -> (
                    match (flel, fler) with
                    (* Empty list is the neutral *)
                    | x, Lit (String "") | Lit (String ""), x -> x
                    (* Rest *)
                    | BinOp (el, StrCat, Lit (String s1)), Lit (String s2) ->
                        f (BinOp (el, StrCat, Lit (String (s1 ^ s2))))
                    | _, _ -> def)
                | SetDiff when lexpr_is_set gamma def -> (
                    let pfs = PFS.to_list pfs in
                    if contained_in_union pfs flel fler then ESet []
                    else
                      match (flel, fler) with
                      | x, y when x = y -> ESet []
                      | ESet [], _ -> ESet []
                      | x, ESet [] -> x
                      | ESet left, ESet right
                        when Expr.all_literals left && Expr.all_literals right
                        ->
                          ESet
                            (Expr.Set.elements
                               (Expr.Set.diff (Expr.Set.of_list left)
                                  (Expr.Set.of_list right)))
                      | ESet left, s when Expr.all_literals left ->
                          if List.for_all (fun x -> set_member pfs x s) left
                          then ESet []
                          else def
                      | ESet left, ESet right ->
                          L.verbose (fun fmt ->
                              fmt "Inside relevant SetDiff case.");
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
                            List.map
                              (fun le -> f (BinOp (le, SetDiff, fler)))
                              les
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
                          | true ->
                              let _, rest =
                                List.partition
                                  (fun x -> set_member pfs x fler)
                                  les
                              in
                              if
                                List.for_all
                                  (fun x -> not_set_member pfs x fler)
                                  rest
                              then ESet rest
                              else BinOp (ESet rest, SetDiff, fler))
                      | _, _ -> def)
                (* let hM = f (BinOp (flel, SetSub, fler)) in
                   (match hM with
                   | Lit (Bool true) -> ESet []
                   | _ -> def)) *)
                | BSetMem when lexpr_is_bool gamma def -> (
                    match (flel, fler) with
                    | _, ESet [] -> Lit (Bool false)
                    | _, ESet [ x ] -> BinOp (flel, Equal, x)
                    | le, ESet les -> (
                        match List.mem le les with
                        | true -> Lit (Bool true)
                        | false -> (
                            match le with
                            | Lit _ ->
                                if Expr.all_literals les then Lit (Bool false)
                                else def
                            | _ -> def))
                    | _, _ -> def)
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
                    | LVar _, NOp (SetUnion, les) ->
                        if List.mem flel les then Lit (Bool true) else def
                    | _, _ -> def)
                | FLessThan ->
                    let success, el, er = Cnum.cut flel fler in
                    let nexpr = Expr.BinOp (el, FLessThan, er) in
                    if success then f nexpr else nexpr
                | ILessThan -> (
                    match (flel, fler) with
                    | x, fler
                      when let fler_len = substitute_for_list_length pfs fler in
                           match fler_len with
                           | UnOp (LstLen, _) -> true
                           | _ -> false ->
                        f
                          (BinOp
                             ( BinOp (x, IPlus, Lit (Int Z.one)),
                               ILessThanEqual,
                               fler ))
                    | UnOp (LstLen, _), Lit (Int n) when Z.leq n Z.zero ->
                        Lit (Bool false)
                    | UnOp (LstLen, le), Lit (Int z) when Z.equal z Z.one ->
                        BinOp (le, Equal, EList [])
                    | _ ->
                        let success, el, er = Cint.cut flel fler in
                        let nexpr = Expr.BinOp (el, ILessThan, er) in
                        if success then f nexpr else nexpr
                        (* | _, _ ->
                            f
                              (BinOp
                                 (BinOp (flel, FMinus, fler), FLessThan, Lit (Num 0.))) *)
                    )
                | FLessThanEqual -> (
                    let success, el, er = Cnum.cut flel fler in
                    if success then f (BinOp (el, FLessThanEqual, er))
                    else
                      match
                        check_ge_zero_num ~top_level:true pfs
                          (f (BinOp (fler, FMinus, flel)))
                      with
                      | Some x -> Lit (Bool x)
                      | None -> def)
                | ILessThanEqual -> (
                    let success, el, er = Cint.cut flel fler in
                    if success then f (BinOp (el, ILessThanEqual, er))
                    else
                      match
                        check_ge_zero_int ~top_level:true pfs
                          (f (BinOp (fler, IMinus, flel)))
                      with
                      | Some x -> Lit (Bool x)
                      | None -> def)
                | _ -> def)))
    | Exists (bt, e) -> (
        (* We create a new pfs and gamma where:
           - All shadowed variables are substituted with a fresh variable
           - The gamma has been updated with the types given in the binder *)
        let new_gamma = Type_env.copy gamma in
        let new_pfs = PFS.copy pfs in
        let subst_bindings = List.map (fun (x, _) -> (x, LVar.alloc ())) bt in
        let subst =
          SVal.SESubst.init
            (List.map (fun (x, y) -> (Expr.LVar x, Expr.LVar y)) subst_bindings)
        in
        let () =
          List.iter
            (fun (x, t) ->
              let () =
                match Type_env.get new_gamma x with
                | Some t ->
                    let new_var = List.assoc x subst_bindings in
                    Type_env.update new_gamma new_var t
                | None -> ()
              in
              match t with
              | Some t -> Type_env.update new_gamma x t
              | None -> Type_env.remove new_gamma x)
            bt
        in
        let () = PFS.substitution subst new_pfs in
        (* We reduce using our new pfs and gamma *)
        let re =
          reduce_lexpr_loop ~matching ~reduce_lvars new_pfs new_gamma e
        in
        let vars = Expr.lvars re in
        let bt = List.filter (fun (b, _) -> Containers.SS.mem b vars) bt in
        (* We remove all quantifiers that aren't used anymore *)
        match bt with
        | [] -> re
        | _ -> Exists (bt, re))
    | EForall (bt, e) -> (
        (* We create a new pfs and gamma where:
           - All shadowed variables are substituted with a fresh variable
           - The gamma has been updated with the types given in the binder *)
        let new_gamma = Type_env.copy gamma in
        let new_pfs = PFS.copy pfs in
        let subst_bindings = List.map (fun (x, _) -> (x, LVar.alloc ())) bt in
        let subst =
          SVal.SESubst.init
            (List.map (fun (x, y) -> (Expr.LVar x, Expr.LVar y)) subst_bindings)
        in
        let () =
          List.iter
            (fun (x, t) ->
              let () =
                match Type_env.get new_gamma x with
                | Some t ->
                    let new_var = List.assoc x subst_bindings in
                    Type_env.update new_gamma new_var t
                | None -> ()
              in
              match t with
              | Some t -> Type_env.update new_gamma x t
              | None -> Type_env.remove new_gamma x)
            bt
        in
        let () = PFS.substitution subst new_pfs in
        (* We reduce using our new pfs and gamma *)
        let re =
          reduce_lexpr_loop ~matching ~reduce_lvars new_pfs new_gamma e
        in
        let vars = Expr.lvars re in
        let bt = List.filter (fun (b, _) -> Containers.SS.mem b vars) bt in
        (* We remove all quantifiers that aren't used anymore *)
        match bt with
        | [] -> re
        | _ -> EForall (bt, re))
    (* The remaining cases cannot be reduced *)
    | _ -> le
  in

  let result = normalise_list_expressions result in
  if not (Expr.equal le result) then (
    L.tmi (fun m -> m "\tReduce_lexpr: %a -> %a" Expr.pp le Expr.pp result);
    f result)
  else result

and reduce_lexpr
    ?(matching = false)
    ?(reduce_lvars = false)
    ?(pfs = PFS.init ())
    ?(gamma = Type_env.init ())
    (le : Expr.t) =
  (* let t = Sys.time () in *)
  let result = reduce_lexpr_loop ~matching ~reduce_lvars pfs gamma le in
  (* Utils.Statistics.update_statistics "Reduce Expression" (Sys.time () -. t); *)
  result

and simplify_num_arithmetic_lexpr
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (le : Expr.t) =
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
      | _ -> le)
  (* FPlus - we collect the positives and the negatives, see what we have and deal with them *)
  | BinOp (l, FPlus, r) ->
      let cl = Cnum.of_expr l in
      let cr = Cnum.of_expr r in
      Cnum.to_expr (Cnum.plus cl cr)
  | _ -> le

and simplify_int_arithmetic_lexpr
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (le : Expr.t) =
  let f = reduce_lexpr_loop pfs gamma in
  match le with
  | BinOp (l, IPlus, Lit (Int z)) when Z.equal z Z.zero -> l
  | BinOp (Lit (Int z), IPlus, l) when Z.equal z Z.zero -> l
  (* Binary minus to unary minus *)
  | BinOp (l, IMinus, r) -> f (BinOp (l, IPlus, UnOp (IUnaryMinus, r)))
  (* Unary minus distributes over + *)
  | UnOp (IUnaryMinus, e) -> (
      match e with
      | BinOp (l, IPlus, r) ->
          f (BinOp (UnOp (IUnaryMinus, l), IPlus, UnOp (IUnaryMinus, r)))
      | _ -> le)
  (* IPlus - we collect the positives and the negatives, see what we have and deal with them *)
  | BinOp (l, IPlus, r) ->
      let cl = Cint.of_expr l in
      let cr = Cint.of_expr r in
      Cint.to_expr (Cint.plus cl cr)
  | _ -> le

(** Checks if an int expression is greater than zero.
      
    @returns [Some true] if definitely > 0, [Some false] if definitely < 0,
      and [None] if both outcomes are satisfiable. *)
and check_ge_zero_int ?(top_level = false) (pfs : PFS.t) (e : Expr.t) :
    bool option =
  (* L.verbose (fun fmt -> fmt "Check >= 0: %a" Expr.pp e); *)
  let f = check_ge_zero_int pfs in
  match e with
  | Lit (Int n) -> Some (Z.geq n Z.zero)
  | UnOp (LstLen, _) | UnOp (StrLen, _) -> Some true
  | (LVar _ | PVar _) when not top_level ->
      if
        List.exists
          (fun pf -> PFS.mem pfs pf)
          [ Formula.ILessEq (Expr.zero_i, e); Formula.ILess (Expr.zero_i, e) ]
      then Some true
      else if PFS.mem pfs (Formula.ILess (e, Expr.zero_i)) then Some false
      else None
  | LVar _ | PVar _ -> None
  | UnOp (IUnaryMinus, _) -> None
  | _ ->
      let ce = Cint.of_expr e in
      if Z.geq ce.conc Z.zero then
        Expr.Map.fold
          (fun e' c result ->
            if
              match result with
              | Some true -> false
              | _ -> true
            then result
            else if Expr.equal e' e then None
            else if Z.gt c Z.zero then
              match f e' with
              | Some true -> Some true
              | _ -> None
            else
              match f (UnOp (IUnaryMinus, e')) with
              | Some true -> Some true
              | _ -> None)
          ce.symb (Some true)
      else None

(** Same as {!check_ge_zero_int}, but for real number expressions. *)
and check_ge_zero_num ?(top_level = false) (pfs : PFS.t) (e : Expr.t) :
    bool option =
  (* L.verbose (fun fmt -> fmt "Check >= 0: %a" Expr.pp e); *)
  let f = check_ge_zero_num pfs in
  match e with
  | Lit (Num n) -> Some (n >= 0.)
  | (LVar _ | PVar _) when not top_level ->
      if
        List.exists
          (fun pf -> PFS.mem pfs pf)
          [ Formula.FLessEq (Lit (Num 0.), e); Formula.FLess (Lit (Num 0.), e) ]
      then Some true
      else if PFS.mem pfs (Formula.FLess (e, Lit (Num 0.))) then Some false
      else None
  | LVar _ | PVar _ -> None
  | UnOp (FUnaryMinus, _) -> None
  | _ ->
      let ce = Cnum.of_expr e in
      if ce.conc >= 0. then
        Expr.Map.fold
          (fun e' c result ->
            if result <> Some true then result
            else if e' = e then None
            else if c > 0. then
              match f e' with
              | Some true -> Some true
              | _ -> None
            else
              match f (UnOp (FUnaryMinus, e')) with
              | Some true -> Some true
              | _ -> None)
          ce.symb (Some true)
      else None

and substitute_in_num_expr (le_to_find : Expr.t) (le_to_subst : Expr.t) le =
  match le_to_find with
  (* Understand if le_to_find appears with a precise coefficient, and if it does, substitute *)
  | le_tf when lexpr_is_number le_tf -> (
      let c_le_tf = Cnum.of_expr le_tf in
      let c_le_tf_symb = Expr.Map.bindings c_le_tf.symb in
      match c_le_tf_symb with
      | [] -> le
      | _ -> (
          let c_le = Cnum.of_expr le in
          let coeffs =
            List.map
              (fun (factor, _) -> Expr.Map.find_opt factor c_le.symb)
              c_le_tf_symb
          in
          match List.for_all Option.is_some coeffs with
          | false -> le
          | true -> (
              let scaled_coeffs =
                List.map2
                  (fun c (_, s) -> Option.get c /. s)
                  coeffs c_le_tf_symb
              in
              (* L.verbose (fun fmt ->
                     fmt "SINE :: letf: %a, le: %a" Expr.pp le_to_find Expr.pp le);
                 L.verbose (fun fmt ->
                     fmt "Coefficients in le    :: %a"
                       Fmt.(brackets (list ~sep:comma float))
                       (List.map (fun (_, v) -> v) c_le_tf_symb));
                 L.verbose (fun fmt ->
                     fmt "Coefficients in le_tf :: %a"
                       Fmt.(brackets (list ~sep:comma float))
                       scaled_coeffs); *)
              let coeff = List.hd scaled_coeffs in
              match List.for_all (fun x -> x = coeff) scaled_coeffs with
              | false -> le
              | true -> (
                  let base_diff = c_le.conc -. c_le_tf.conc in
                  match
                    (c_le.conc >= 0. && base_diff >= 0.)
                    || (c_le.conc < 0. && base_diff <= 0.)
                  with
                  | false -> le
                  | true ->
                      let c_le_tf = Cnum.const_mult coeff c_le_tf in
                      let c_diff = Cnum.minus c_le c_le_tf in
                      (* L.verbose (fun fmt ->
                          fmt "After subtraction: %a" Expr.pp
                            (cnum_to_expr c_diff)); *)
                      let c_to_subst =
                        Cnum.const_mult coeff (Cnum.of_expr le_to_subst)
                      in
                      let result = Cnum.to_expr (Cnum.plus c_diff c_to_subst) in
                      (* L.verbose (fun fmt ->
                          fmt "After re-addition: %a" Expr.pp result); *)
                      result))))
  (* Recursively for LstSub *)
  | _ -> le

and substitute_in_int_expr (le_to_find : Expr.t) (le_to_subst : Expr.t) le :
    Expr.t =
  let open Z in
  let f = substitute_in_num_expr le_to_find le_to_subst in
  match le_to_find with
  | LstSub (lst, start, sz) -> LstSub (lst, f start, f sz)
  | _ -> (
      if not (lexpr_is_int le_to_find) then le
      else
        (* Understand if le_to_find appears with a precise coefficient, and if it does, substitute *)
        let c_le_tf = Cint.of_expr le_to_find in
        let c_le_tf_symb = Expr.Map.bindings c_le_tf.symb in
        match c_le_tf_symb with
        | [] -> le
        | _ -> (
            let c_le = Cint.of_expr le in
            let coeffs =
              List.map
                (fun (factor, _) -> Expr.Map.find_opt factor c_le.symb)
                c_le_tf_symb
            in
            match List.for_all Option.is_some coeffs with
            | false -> le
            | true -> (
                let scaled_coeffs =
                  List.map2
                    (fun c (_, s) ->
                      let c = Option.get c in
                      let () =
                        if not (Z.equal (c mod s) Z.zero) then
                          failwith
                            "Reduction.substitute_in_int_expr, scaling with \
                             invalid number"
                      in
                      c / s)
                    coeffs c_le_tf_symb
                in
                (* L.verbose (fun fmt ->
                       fmt "SINE :: letf: %a, le: %a" Expr.pp le_to_find Expr.pp le);
                   L.verbose (fun fmt ->
                       fmt "Coefficients in le    :: %a"
                         Fmt.(brackets (list ~sep:comma float))
                         (List.map (fun (_, v) -> v) c_le_tf_symb));
                   L.verbose (fun fmt ->
                       fmt "Coefficients in le_tf :: %a"
                         Fmt.(brackets (list ~sep:comma float))
                         scaled_coeffs); *)
                let coeff = List.hd scaled_coeffs in
                match List.for_all (Z.equal coeff) scaled_coeffs with
                | false -> le
                | true -> (
                    let base_diff = c_le.conc - c_le_tf.conc in
                    match
                      (geq c_le.conc Z.zero && geq base_diff Z.zero)
                      || (lt c_le.conc Z.zero && leq base_diff Z.zero)
                    with
                    | false -> le
                    | true ->
                        let c_le_tf = Cint.const_mult coeff c_le_tf in
                        let c_diff = Cint.minus c_le c_le_tf in
                        (* L.verbose (fun fmt ->
                            fmt "After subtraction: %a" Expr.pp
                              (cnum_to_expr c_diff)); *)
                        let c_to_subst =
                          Cint.const_mult coeff (Cint.of_expr le_to_subst)
                        in
                        let result =
                          Cint.to_expr (Cint.plus c_diff c_to_subst)
                        in
                        (* L.verbose (fun fmt ->
                            fmt "After re-addition: %a" Expr.pp result); *)
                        result))))

and substitute_for_specific_length
    (pfs : PFS.t)
    (len_to_subst : Expr.t)
    (le : Expr.t) : Expr.t =
  let len_expr = Expr.UnOp (LstLen, len_to_subst) in
  let eqs = find_equalities pfs len_expr in
  let results =
    List.filter_map
      (fun eq ->
        let subst = substitute_in_int_expr eq len_expr le in
        if Expr.equal subst le then None else Some subst)
      eqs
  in
  let results = List.sort_uniq Stdlib.compare results in
  match results with
  | [] -> le
  | [ result ] -> result
  | oops ->
      L.verbose (fun fmt ->
          fmt "Multiple results... %a" Fmt.(list ~sep:comma Expr.pp) oops);
      le

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
    (fun le (len_expr, _lex) -> substitute_for_specific_length pfs len_expr le)
    le len_eqs

let resolve_expr_to_location (pfs : PFS.t) (gamma : Type_env.t) (e : Expr.t) :
    string option =
  let max_fuel = 5 in

  let rec resolve_expr_to_location_aux
      (fuel : int)
      (tried : Expr.Set.t)
      (to_try : Expr.t list) : string option =
    let f = resolve_expr_to_location_aux (fuel - 1) in
    match fuel = 0 with
    | true -> None
    | false -> (
        match to_try with
        | [] -> None
        | e :: _rest -> (
            match e with
            | Lit (Loc loc) | ALoc loc -> Some loc
            | _ -> (
                let equal_e = get_equal_expressions pfs e in
                let equal_e =
                  equal_e @ List.map (reduce_lexpr ~pfs ~gamma) equal_e
                in
                let ores =
                  List.find_opt
                    (fun x ->
                      match x with
                      | Expr.ALoc _ | Lit (Loc _) -> true
                      | _ -> false)
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
                          | [] -> None
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
                    | _ ->
                        let new_tried = Expr.Set.add e tried in
                        let new_to_try = equal_e @ [ subst_e ] in
                        let new_to_try =
                          List.filter
                            (fun e -> not (Expr.Set.mem e new_tried))
                            new_to_try
                        in
                        f new_tried new_to_try))))
  in

  resolve_expr_to_location_aux max_fuel Expr.Set.empty [ e ]

let rec reduce_formula_loop
    ?(top_level = false)
    ?(rpfs = false)
    (matching : bool)
    (pfs : PFS.t)
    (gamma : Type_env.t)
    ?(previous = Formula.True)
    (a : Formula.t) : Formula.t =
  Logging.tmi (fun m ->
      m "Reduce formula: %a -> %a"
        (fun ft f ->
          match f with
          | Formula.True ->
              Fmt.pf ft "STARTING TO REDUCE: matching %b, rpfs %b" matching rpfs
          | _ -> Formula.pp ft f)
        previous Formula.pp a);
  if Formula.equal a previous then
    let () =
      Logging.tmi (fun m -> m "Finished reducing, obtained: %a" Formula.pp a)
    in
    a
  else
    let f = reduce_formula_loop ~rpfs matching pfs gamma in
    let fe = reduce_lexpr_loop ~matching pfs gamma in
    let result : Formula.t =
      match a with
      | Eq (e1, e2) when Expr.equal e1 e2 -> True
      (* DEDICATED SIMPLIFICATIONS - this should probably be handled properly by Z3... *)
      | Eq (BinOp (Lit (Num x), FPlus, LVar y), LVar z)
        when x <> 0. && String.equal y z -> False
      | Eq (BinOp (Lit (Int x), IPlus, LVar y), LVar z)
        when (not (Z.equal x Z.zero)) && String.equal y z -> False
      | ForAll
          ( [ (x, Some IntType) ],
            Or
              ( Or (ILess (LVar a, Lit (Int z)), ILessEq (Lit (Int len), LVar b)),
                Eq (BinOp (EList c, LstNth, LVar d), e) ) )
        when Z.equal z Z.zero && String.equal x a && String.equal a b
             && String.equal b d
             && Int.equal (List.compare_length_with c (Z.to_int len)) 0 ->
          let rhs = Expr.EList (List_utils.make (Z.to_int len) e) in
          Eq (EList c, rhs)
      (* FIXME: INTEGER BYTE-BY-BYTE BREAKDOWN *)
      | Eq
          ( Lit (Int n),
            BinOp (BinOp (Lit (Int tfs), ITimes, LVar b1), IPlus, LVar b0) )
        when top_level && Z.equal tfs _256
             && PFS.mem pfs (ILessEq (Expr.zero_i, LVar b0))
             && PFS.mem pfs (ILessEq (Expr.zero_i, LVar b1))
             && PFS.mem pfs (ILess (LVar b0, Lit (Int _256)))
             && PFS.mem pfs (ILess (LVar b1, Lit (Int _256))) ->
          if Z.gt n _65535 then False
          else
            let vb1 = Z.div n _256 in
            let vb0 = Z.sub n vb1 in
            Formula.And
              (Eq (LVar b1, Lit (Int vb1)), Eq (LVar b0, Lit (Int vb0)))
      | Eq
          ( BinOp (BinOp (Lit (Int tfs), ITimes, LVar b1), IPlus, LVar b0),
            Lit (Int n) )
        when top_level && Z.equal tfs _256
             && PFS.mem pfs (ILessEq (Expr.zero_i, LVar b0))
             && PFS.mem pfs (ILessEq (Expr.zero_i, LVar b1))
             && PFS.mem pfs (ILess (LVar b0, Lit (Int _256)))
             && PFS.mem pfs (ILess (LVar b1, Lit (Int _256))) ->
          if Z.gt n _65535 then False
          else
            let vb1 = Z.div n _256 in
            let vb0 = Z.sub n vb1 in
            Formula.And
              (Eq (LVar b1, Lit (Int vb1)), Eq (LVar b0, Lit (Int vb0)))
      | Eq (BinOp (e, FTimes, Lit (Num x)), Lit (Num 0.)) when x <> 0. ->
          Eq (e, Lit (Num 0.))
      | Eq (BinOp (e, ITimes, Lit (Int x)), Lit (Int n))
        when Z.equal n Z.zero && not (Z.equal x Z.zero) -> Eq (e, Expr.zero_i)
      | Eq (BinOp (Lit (Num x), FTimes, e), Lit (Num 0.)) when x <> 0. ->
          Eq (e, Lit (Num 0.))
      | Eq (BinOp (Lit (Int x), ITimes, e), Lit (Int z))
        when Z.equal z Z.zero && not (Z.equal x Z.zero) -> Eq (e, Expr.zero_i)
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
            conj
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
            conj
      | Eq (left_list, right_list)
        when (match
                ( Typing.type_lexpr gamma left_list,
                  Typing.type_lexpr gamma right_list )
              with
             | (Some Type.ListType, _), (Some Type.ListType, _) -> true
             | _ -> false)
             &&
             match
               fe
                 (Expr.Infix.( - )
                    (Expr.list_length left_list)
                    (Expr.list_length right_list))
             with
             | Expr.Lit (Int k) when not (Z.equal k Z.zero) -> true
             | _ -> false ->
          (* If we have two lists but can reduce the equality of their lengths to false,
             then we know the lists cannot be equal*)
          False
      | Eq (NOp (LstCat, les), LVar x)
        when List.mem (Expr.LVar x) les
             && List.exists
                  (fun e ->
                    match e with
                    | Expr.EList (_ :: _) | Lit (LList (_ :: _)) -> true
                    | _ -> false)
                  les -> False
      | Eq (UnOp (NumToInt, le), re) -> Eq (le, UnOp (IntToNum, re))
      | Eq (le, UnOp (NumToInt, re)) -> Eq (UnOp (IntToNum, le), re)
      | And (a1, a2) -> (
          let fa1 = f a1 in
          let fa2 = f a2 in
          match (fa1, fa2) with
          | False, _ | _, False -> False
          | True, a | a, True -> a
          | _, _ -> And (fa1, fa2))
      | Or (a1, a2) -> (
          let fa1 = f a1 in
          let fa2 = f a2 in
          match (fa1, fa2) with
          | False, a | a, False -> a
          | True, _ | _, True -> True
          | _, _ ->
              if PFS.mem pfs fa1 || PFS.mem pfs fa2 then True
              else if PFS.mem pfs (Not fa1) then fa2
              else if PFS.mem pfs (Not fa2) then fa1
              else Or (fa1, fa2))
      (* JOSE: why the recursive call? *)
      | Not a -> (
          let fa = f a in
          match a with
          | True -> False
          | False -> True
          | Not a -> a
          | Or (a1, a2) -> And (Not a1, Not a2)
          | And (a1, a2) -> Or (Not a1, Not a2)
          | FLess (e1, e2) -> FLessEq (e2, e1)
          | FLessEq (e1, e2) -> FLess (e2, e1)
          | ILess (e1, e2) -> ILessEq (e2, e1)
          | ILessEq (e1, e2) -> ILess (e2, e1)
          | _ -> Not fa)
      | Eq (e1, e2) -> (
          let re1 = fe e1 in
          let re2 = fe e2 in
          (* Warning - NaNs, infinities, this and that, this is not good enough *)
          let eq = re1 = re2 in
          if eq then True
          else
            let t1, s1 = Typing.type_lexpr gamma re1 in
            let t2, s2 = Typing.type_lexpr gamma re2 in
            if
              s1 && s2
              &&
              match (t1, t2) with
              | Some t1, Some t2 -> t1 <> t2
              | _, _ -> false
            then False
            else
              let ite a b : Formula.t = if a = b then True else False in
              let default re1 re2 : Formula.t = Eq (re1, re2) in
              match (re1, re2) with
              (* DEDICATED RPFS REDUCTIONS *)
              | NOp (LstCat, _), LVar y when rpfs && prefix_catch pfs re1 y ->
                  Eq (UnOp (LstLen, re1), UnOp (LstLen, re2))
              | LVar x, NOp (LstCat, LstSub (y, UnOp (LstLen, z), len) :: t)
                when rpfs
                     && PFS.mem pfs
                          (Eq (NOp (LstCat, y :: t), NOp (LstCat, [ z; LVar x ])))
                     && Cint.canonicalise len
                        = Cint.canonicalise
                            (BinOp (UnOp (LstLen, y), IMinus, UnOp (LstLen, z)))
                -> True
              (* USUAL REDUCTIONS *)
              | ALoc _, Lit (Loc _) | Lit (Loc _), ALoc _ -> False
              | ALoc x, ALoc y when (not matching) && x <> y -> False
              | EList [], x
              | x, EList []
              | Lit (LList []), x
              | x, Lit (LList []) -> (
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
                  | _ -> Eq (re1, re2))
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
              | UnOp (LstRev, ll), UnOp (LstRev, rl) -> Eq (ll, rl)
              (* TODO: This is a specialised simplification, not sure for what, disabled for now
                 | UnOp  (LstRev, full_list), BinOp (UnOp (LstRev, plist_left), LstCat, plist_right)
                 | BinOp (UnOp (LstRev, plist_left), LstCat, plist_right), UnOp (LstRev, full_list)
                     ->
                     f (Eq (full_list, BinOp (UnOp (LstRev, plist_right), LstCat, plist_left))) *)
              | LstSub (e1, Lit (Int z), el), e2
                when Z.equal z Z.zero && Expr.equal e1 e2 ->
                  Eq (UnOp (LstLen, e1), el)
              | e2, LstSub (e1, Lit (Int z), el)
                when Z.equal z Z.zero && Expr.equal e1 e2 ->
                  Eq (UnOp (LstLen, e1), el)
              | e2, LstSub (NOp (LstCat, e1 :: _), Lit (Int z), el)
                when Z.equal z Z.zero && Expr.equal e1 e2 ->
                  Eq (UnOp (LstLen, e1), el)
              | LstSub (NOp (LstCat, e1 :: _), Lit (Int z), el), e2
                when Z.equal z Z.zero && Expr.equal e1 e2 ->
                  Eq (UnOp (LstLen, e1), el)
              | e2, LstSub (NOp (LstCat, e3 :: e1 :: _), ex, ey)
                when Expr.equal e1 e2 ->
                  And (Eq (UnOp (LstLen, e3), ex), Eq (UnOp (LstLen, e1), ey))
              | LstSub (NOp (LstCat, e3 :: e1 :: _), ex, ey), e2
                when Expr.equal e1 e2 ->
                  And (Eq (UnOp (LstLen, e3), ex), Eq (UnOp (LstLen, e1), ey))
              | NOp (LstCat, fl :: rl), NOp (LstCat, fr :: rr)
                when Expr.equal fl fr -> Eq (NOp (LstCat, rl), NOp (LstCat, rr))
              | NOp (LstCat, fl :: rl), NOp (LstCat, fr :: rr)
                when Expr.equal
                       (List.hd (List.rev (fl :: rl)))
                       (List.hd (List.rev (fr :: rr))) ->
                  f
                    (Eq
                       ( NOp (LstCat, List.rev (List.tl (List.rev (fl :: rl)))),
                         NOp (LstCat, List.rev (List.tl (List.rev (fr :: rr))))
                       ))
              | ( LVar lst,
                  NOp (LstCat, LstSub (LVar lst', Lit (Int z), split) :: _rest)
                )
                when Z.equal z Z.zero && String.equal lst lst'
                     && PFS.mem pfs (ILess (UnOp (LstLen, LVar lst), split)) ->
                  False
              | le1, le2
                when (match le1 with
                     | LVar _ -> false
                     | _ -> true)
                     && (match le2 with
                        | LVar _ -> false
                        | _ -> true)
                     && lexpr_is_list gamma le1 && lexpr_is_list gamma le2 -> (
                  let htl1, htl2 =
                    ( get_head_and_tail_of_list ~pfs le1,
                      get_head_and_tail_of_list ~pfs le2 )
                  in
                  match (htl1, htl2) with
                  | Some (hl1, tl1), Some (hl2, tl2) ->
                      And (Eq (hl1, hl2), Eq (tl1, tl2))
                  | None, Some _ -> (
                      match le1 with
                      | Lit (LList _) | EList _ -> False
                      | _ -> Eq (re1, re2))
                  | Some _, None -> (
                      match le2 with
                      | Lit (LList _) | EList _ -> False
                      | _ -> Eq (re1, re2))
                  | None, None -> Eq (re1, re2))
              (* Strings #1 *)
              | Lit (String ls), BinOp (Lit (String rs), StrCat, s)
              | BinOp (Lit (String rs), StrCat, s), Lit (String ls) -> (
                  let lls = String.length ls in
                  let lrs = String.length rs in
                  match Stdlib.compare lls lrs with
                  | -1 -> False
                  | 0 -> if ls <> rs then False else Eq (s, Lit (String ""))
                  | 1 ->
                      let sub = String.sub ls 0 lrs in
                      if sub <> rs then False
                      else Eq (s, Lit (String (String.sub ls lrs (lls - lrs))))
                  | _ ->
                      raise
                        (Exceptions.Impossible
                           "reduce_formula: string stuff: guaranteed by \
                            match/filter"))
              (* String #2 *)
              | BinOp (sl1, StrCat, sr1), BinOp (sl2, StrCat, sr2)
                when sl1 = sl2 -> Eq (sr1, sr2)
              | BinOp (sl1, StrCat, sr1), BinOp (sl2, StrCat, sr2)
                when sr1 = sr2 -> Eq (sl1, sl2)
              (* String #3 *)
              | BinOp (sl, StrCat, sr), s when sl = s -> Eq (sr, Lit (String ""))
              | BinOp (sl, StrCat, sr), s when sr = s -> Eq (sl, Lit (String ""))
              | s, BinOp (sl, StrCat, sr) when sl = s -> Eq (sr, Lit (String ""))
              | s, BinOp (sl, StrCat, sr) when sr = s -> Eq (sl, Lit (String ""))
              | BinOp (sl, StrCat, sr), Lit (String "") ->
                  And (Eq (sl, Lit (String "")), Eq (sr, Lit (String "")))
              (* Num-to-String injectivity *)
              | UnOp (ToStringOp, le1), UnOp (ToStringOp, le2) -> Eq (le1, le2)
              (* Num-to-String understanding *)
              | UnOp (ToStringOp, le1), Lit (String s)
              | Lit (String s), UnOp (ToStringOp, le1) -> (
                  match s with
                  | "" -> False
                  | "Infinity" | "-Infinity" | "NaN" -> default re1 re2
                  | _ -> (
                      let num = try Some (Float.of_string s) with _ -> None in
                      match num with
                      | Some num -> Eq (le1, Lit (Num num))
                      | None -> False))
              (* The empty business *)
              | _, Lit Empty -> (
                  match re1 with
                  | Lit l when l <> Empty -> False
                  | EList _ | ESet _ -> False
                  | _ -> default re1 re2)
              | Lit l1, Lit l2 -> ite l1 l2
              | Lit Nono, PVar _ | PVar _, Lit Nono -> default re1 re2
              (* JOSE: Why are we considering the case of a logical variable being bound to None? *)
              | Lit Nono, LVar x | LVar x, Lit Nono -> (
                  let tx = Type_env.get gamma x in
                  match tx with
                  | None | Some NoneType -> default re1 re2
                  | _ -> False)
              | Lit Nono, _ | _, Lit Nono -> False
              | Lit (Bool true), BinOp (e1, FLessThan, e2) -> FLess (e1, e2)
              | Lit (Bool false), BinOp (e1, FLessThan, e2) ->
                  Not (FLess (e1, e2))
              | Lit (Bool true), BinOp (e1, ILessThan, e2) -> ILess (e1, e2)
              | Lit (Bool false), BinOp (e1, ILessThan, e2) ->
                  Not (ILess (e1, e2))
              (* FPlus theory -> theory? I would not go that far *)
              | le1, le2 when lexpr_is_number le1 && lexpr_is_number le2 ->
                  let success, le1', le2' = Cnum.cut le1 le2 in
                  if success then Eq (le1', le2') else Eq (le1, le2)
              | le1, le2 when lexpr_is_int le1 && lexpr_is_int le2 ->
                  let success, le1', le2' = Cint.cut le1 le2 in
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
                  then Eq (ls, rs)
                  else default re1 re2
              | _, _ -> default re1 re2)
      | FLess (e1, e2) ->
          if PFS.mem pfs (FLessEq (e2, e1)) then False
          else if PFS.mem pfs (FLess (e2, e1)) then False
          else
            let le = Option.get (Formula.to_expr (FLess (e1, e2))) in
            let re = fe le in
            let result, _ = Option.get (Formula.lift_logic_expr re) in
            result
      | ILess (e1, e2) ->
          if PFS.mem pfs (ILessEq (e2, e1)) then False
          else if PFS.mem pfs (ILess (e2, e1)) then False
          else
            let le = Option.get (Formula.to_expr (ILess (e1, e2))) in
            let re = fe le in
            let result, _ = Option.get (Formula.lift_logic_expr re) in
            result
      | ILessEq (Lit (Int z), UnOp (LstLen, _)) when Z.equal z Z.zero -> True
      | FLessEq (e1, e2) ->
          if PFS.mem pfs (FLessEq (e2, e1)) then Eq (e1, e2)
          else if PFS.mem pfs (FLess (e1, e2)) then True
          else if PFS.mem pfs (FLess (e2, e1)) then False
          else
            let le = Option.get (Formula.to_expr (FLessEq (e1, e2))) in
            let re = fe le in
            let result, _ = Option.get (Formula.lift_logic_expr re) in
            result
      | ILessEq (e1, e2) ->
          if PFS.mem pfs (ILessEq (e2, e1)) then Eq (e1, e2)
          else if PFS.mem pfs (ILess (e1, e2)) then True
          else if PFS.mem pfs (ILess (e2, e1)) then False
          else
            let le = Option.get (Formula.to_expr (ILessEq (e1, e2))) in
            let re = fe le in
            let result, _ = Option.get (Formula.lift_logic_expr re) in
            result
      | SetMem (leb, NOp (SetUnion, lle)) ->
          let rleb = fe leb in
          let formula : Formula.t =
            match lle with
            | [] -> False
            | le :: lle ->
                let rle = fe le in
                List.fold_left
                  (fun ac le : Formula.t ->
                    let rle = fe le in
                    Or (ac, SetMem (rleb, rle)))
                  (SetMem (rleb, rle))
                  lle
          in
          formula
      | SetMem (leb, NOp (SetInter, lle)) ->
          let rleb = fe leb in
          let formula : Formula.t =
            match lle with
            | [] -> False
            | le :: lle ->
                let rle = fe le in
                List.fold_left
                  (fun ac le : Formula.t ->
                    let rle = fe le in
                    And (ac, SetMem (rleb, rle)))
                  (SetMem (rleb, rle))
                  lle
          in
          formula
      | SetMem (leb, BinOp (lel, SetDiff, ler)) ->
          let rleb = fe leb in
          let rlel = fe lel in
          let rler = fe ler in
          And (SetMem (rleb, rlel), Not (SetMem (rleb, rler)))
      | SetMem (leb, ESet les) ->
          let rleb = fe leb in
          let rles = List.map (fun le -> fe le) les in
          let result : Formula.t list =
            List.map (fun le : Formula.t -> Eq (rleb, le)) rles
          in
          List.fold_left
            (fun ac eq : Formula.t ->
              match (ac : Formula.t) with
              | False -> eq
              | _ -> Or (ac, eq))
            False result
      | IsInt e -> (
          match fe e with
          | UnOp (UnOp.IntToNum, e) -> (
              let t, _ = Typing.type_lexpr gamma e in
              match t with
              | Some IntType -> True
              | Some _ -> False
              | None -> f @@ Eq (UnOp (TypeOf, e), Lit (Type IntType)))
          | _ -> a)
      | Impl (left, right) -> (
          let pfs_with_left =
            let copy = PFS.copy pfs in
            let () = PFS.extend copy left in
            copy
          in
          let reduced_left =
            reduce_formula_loop ~rpfs:true matching pfs_with_left gamma left
          in
          match (reduced_left, f right) with
          | True, _ -> right
          | False, _ | _, True -> True
          | _, False -> f (Not left)
          | _ -> Impl (left, right))
      | ForAll
          ( [ (i, Some IntType) ],
            Impl
              ( And
                  ( ILessEq (Lit (Int z), LVar i'),
                    ILess (LVar i'', UnOp (LstLen, l)) ),
                Eq (BinOp (l', LstNth, LVar i'''), k) ) )
        when Z.(equal z zero)
             && i = i' && i' = i'' && i'' = i''' && Expr.equal l l'
             &&
             match l with
             | EList _ -> true
             | _ -> false ->
          let l =
            match l with
            | EList l -> l
            | _ -> failwith "unreachable"
          in
          List.map (fun x -> Formula.Infix.(x #== k)) l
          |> List.fold_left Formula.Infix.( #&& ) Formula.True
      | ForAll (bt, a) -> (
          (* We create a new pfs and gamma where:
             - All shadowed variables are substituted with a fresh variable
             - The gamma has been updated with the types given in the binder *)
          let new_gamma = Type_env.copy gamma in
          let new_pfs = PFS.copy pfs in
          let subst_bindings = List.map (fun (x, _) -> (x, LVar.alloc ())) bt in
          let subst =
            SVal.SESubst.init
              (List.map
                 (fun (x, y) -> (Expr.LVar x, Expr.LVar y))
                 subst_bindings)
          in
          let () =
            List.iter
              (fun (x, t) ->
                let () =
                  match Type_env.get new_gamma x with
                  | Some t ->
                      let new_var = List.assoc x subst_bindings in
                      Type_env.update new_gamma new_var t
                  | None -> ()
                in
                match t with
                | Some t -> Type_env.update new_gamma x t
                | None -> Type_env.remove new_gamma x)
              bt
          in
          let () = PFS.substitution subst new_pfs in
          (* We reduce using our new pfs and gamma *)
          let ra = reduce_formula_loop ~rpfs matching new_pfs new_gamma a in
          let vars = Formula.lvars ra in
          let bt = List.filter (fun (b, _) -> Containers.SS.mem b vars) bt in
          (* We remove all quantifiers that aren't used anymore *)
          match bt with
          | [] -> ra
          | _ -> ForAll (bt, ra))
      | _ -> a
    in

    f ~previous:a result

let reduce_formula
    ?(matching = false)
    ?(rpfs = false)
    ?time:_
    ?(pfs : PFS.t = PFS.init ())
    ?(gamma = Type_env.init ())
    (a : Formula.t) : Formula.t =
  reduce_formula_loop ~top_level:true ~rpfs matching pfs gamma a

let relate_llen
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (e : Expr.t)
    (lcat : Expr.t list) : (Formula.t * Containers.SS.t) option =
  (* Loop *)
  let rec relate_llen_loop
      (llen : Cint.t)
      (ac : Expr.t list)
      (lcat : Expr.t list) : (Expr.t list * bool * Expr.t) option =
    let decide () =
      L.verbose (fun fmt -> fmt "Deciding on: %a" Expr.pp (Cint.to_expr llen));
      L.verbose (fun fmt ->
          fmt "Generated so far: %a" Fmt.(list ~sep:comma Expr.pp) ac);
      if Cint.is_const_posint llen then Some (ac, true, Expr.Lit (Int llen.conc))
      else if
        ac <> []
        && check_ge_zero_int ~top_level:true pfs (Cint.to_expr llen) = Some true
      then Some (ac, false, Cint.to_expr llen)
      else None
    in
    match lcat with
    | [] -> decide ()
    | e :: rest -> (
        let e_llen = reduce_lexpr ~pfs ~gamma (Expr.UnOp (LstLen, e)) in
        let e_llen =
          let eqs = find_equalities pfs e_llen in
          match
            List.filter
              (fun e ->
                match e with
                | Expr.Lit (Int _) -> true
                | e
                  when Containers.SS.subset (Expr.lvars e)
                         (Expr.lvars (Cint.to_expr llen)) -> true
                | _ -> false)
              eqs
          with
          | [] -> e_llen
          | _ -> List.hd eqs
        in
        let new_llen = Cint.minus llen (Cint.of_expr e_llen) in
        L.verbose (fun fmt ->
            fmt "relate_llen_loop: %a consumed, %a left" Expr.pp e Expr.pp
              (Cint.to_expr new_llen));
        match e_llen with
        | Lit (Int _) ->
            if Z.geq new_llen.conc Z.zero then
              relate_llen_loop new_llen (ac @ [ e ]) rest
            else decide ()
        | _ -> (
            match Expr.Map.find_opt e_llen new_llen.symb with
            (* Subtracted too much, decide *)
            | Some n when Z.lt n Z.zero -> decide ()
            (* Otherwise, proceed *)
            | _ -> relate_llen_loop new_llen (ac @ [ e ]) rest))
  in

  (* Auxiliary function *)
  let relate_llen_aux (e : Expr.t) (llen : Cint.t) (lcat : Expr.t list) =
    L.tmi (fun fmt ->
        fmt "Relate llen aux: %a: %a, %a" Expr.pp e Expr.pp (Cint.to_expr llen)
          Fmt.(brackets (list ~sep:semi Expr.pp))
          lcat);
    Option.map
      (fun (les, is_concrete, exp) ->
        match (is_concrete, exp) with
        | true, Expr.Lit (Int n) ->
            let new_vars = List.init (Z.to_int n) (fun _ -> LVar.alloc ()) in
            let new_lvars = List.map (fun x -> Expr.LVar x) new_vars in
            let new_lvars =
              match new_lvars with
              | [] -> []
              | _ -> [ Expr.EList new_lvars ]
            in
            let pf = Formula.Eq (e, NOp (LstCat, les @ new_lvars)) in
            L.verbose (fun fmt -> fmt "Constructed equality: %a" Formula.pp pf);
            (pf, Containers.SS.of_list new_vars)
        | false, exp ->
            let rest_var = LVar.alloc () in
            let rest = Expr.LVar rest_var in
            let pfeq = Formula.Eq (e, NOp (LstCat, les @ [ rest ])) in
            let pflen = Formula.Eq (UnOp (LstLen, rest), exp) in
            (Formula.And (pfeq, pflen), Containers.SS.singleton rest_var)
        | _ -> failwith "Impossible by construction")
      (relate_llen_loop llen [] lcat)
  in

  L.tmi (fun fmt ->
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
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (lcat : Expr.t list)
    (rcat : Expr.t list) : (Formula.t * Containers.SS.t) option =
  L.tmi (fun fmt ->
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
  | el :: _, er :: _ -> (
      match relate_llen pfs gamma el rcat with
      | Some result -> Some result
      | None -> relate_llen pfs gamma er lcat)

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
    | Pure True -> ([], [])
    | Pure False -> raise PFSFalse
    | Pure (Eq (UnOp (TypeOf, e), Lit (Type t)))
    | Pure (Eq (Lit (Type t), UnOp (TypeOf, e))) -> ([], [ (e, t) ])
    | Star (a1, a2) ->
        let fa1, ft1 = separate a1 in
        let fa2, ft2 = separate a2 in
        (fa1 @ fa2, ft1 @ ft2)
    | Types ets -> ([], ets)
    | _ -> ([ a ], [])
  in

  try
    let others, ets = separate a in

    let ets = ETSet.elements (ETSet.of_list ets) in
    match (others, ets) with
    | [], [] -> Pure True
    | [], ets -> Types ets
    | a, ets ->
        let result = Asrt.star a in
        if ets = [] then result else Star (Types ets, result)
  with PFSFalse -> Pure False

(* Reduction of assertions *)
let rec reduce_assertion_loop
    (matching : bool)
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (a : Asrt.t) : Asrt.t =
  let f = reduce_assertion_loop matching pfs gamma in
  let fe = reduce_lexpr_loop ~matching pfs gamma in
  let result =
    match a with
    (* Empty heap *)
    | Emp -> Asrt.Emp
    (* Star *)
    | Star (a1, a2) -> (
        match (f a1, f a2) with
        | Emp, a | a, Emp -> a
        | Pure False, _ | _, Pure False -> Asrt.Pure False
        | Pure True, a | a, Pure True -> a
        | fa1, fa2 -> Star (fa1, fa2))
    | Wand { lhs = lname, largs; rhs = rname, rargs } ->
        Wand
          { lhs = (lname, List.map fe largs); rhs = (rname, List.map fe rargs) }
    (* Predicates *)
    | Pred (name, les) -> Pred (name, List.map fe les)
    (* Pure assertions *)
    | Pure True -> Emp
    | Pure f -> Pure (reduce_formula_loop ~top_level:true matching pfs gamma f)
    (* Types *)
    | Types lvt -> (
        try
          let lvt =
            List.fold_right
              (fun (e, t) ac ->
                match (e : Expr.t) with
                | Lit lit ->
                    if t <> Literal.type_of lit then raise WrongType else ac
                | _ -> (e, t) :: ac)
              lvt []
          in
          if lvt = [] then Emp else Types lvt
        with WrongType -> Pure False)
    (* General action *)
    | GA (act, l_ins, l_outs) -> GA (act, List.map fe l_ins, List.map fe l_outs)
  in

  if a <> result && not (a == result) then (
    L.(tmi (fun m -> m "Reduce_assertion: %a -> %a" Asrt.pp a Asrt.pp result));
    f result)
  else result

let rec extract_lvar_equalities (a : Asrt.t) =
  match a with
  | Pure (Eq (LVar x, v) | Eq (v, LVar x)) ->
      if Names.is_lvar_name x && not (Names.is_spec_var_name x) then [ (x, v) ]
      else []
  | Star (a1, a2) -> extract_lvar_equalities a1 @ extract_lvar_equalities a2
  | _ -> []

let reduce_assertion
    ?(matching = false)
    ?(pfs = PFS.init ())
    ?(gamma = Type_env.init ())
    (a : Asrt.t) : Asrt.t =
  let a = reduce_types a in

  let rec loop (a : Asrt.t) =
    let a' = reduce_assertion_loop matching pfs gamma a in
    let equalities = extract_lvar_equalities a' in
    let a' =
      List.fold_left
        (fun a (v, x) ->
          let subst =
            Asrt.subst_expr_for_expr ~to_subst:(LVar v) ~subst_with:x
          in
          match x with
          | Lit _ -> subst a
          | LVar w when v <> w -> subst a
          | EList lx when not (Var.Set.mem v (Expr.lvars (EList lx))) -> subst a
          | _ -> a)
        a' equalities
    in
    let a' = reduce_assertion_loop matching pfs gamma a' in
    if a' <> a && not (a' == a) then loop a' else a'
  in

  loop a

let is_tautology ?pfs ?gamma formula =
  match reduce_formula ?pfs ?gamma formula with
  | True -> true
  | _ -> false
