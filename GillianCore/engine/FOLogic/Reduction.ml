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
      (function
        | Expr.Lit (LList []) | EList [] -> false
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
    | ForAll (bt, le) -> ForAll (bt, f le)
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

let resolve_list (le : Expr.t) (pfs : Expr.t list) : Expr.t =
  let rec search x pfs =
    match (pfs : Expr.t list) with
    | [] -> None
    | BinOp (LVar x', Equal, le) :: rest when String.equal x' x -> (
        let le' = normalise_list_expressions le in
        match le' with
        (* Weird things can happen where x reduces to e.g. `{{ l-nth(x, 0) }}`.
           We check absence of cycles *)
        | (EList _ | NOp (LstCat, _)) when not (SS.mem x (Expr.lvars le')) ->
            Some le'
        | Expr.BinOp (_, LstRepeat, _) as ret
          when not (SS.mem x (Expr.lvars ret)) -> Some ret
        | _ -> search x rest)
    | BinOp (le, Equal, LVar x') :: rest when String.equal x' x -> (
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
  PFS.to_list pfs
  |> List.filter_map (function
       | Expr.BinOp (x, Equal, y) ->
           if Expr.equal x le then Some y
           else if Expr.equal y le then Some x
           else None
       | _ -> None)

(***************************)
(* TYPING HELPER FUNCTIONS *)
(***************************)

let typable (gamma : Type_env.t) (le : Expr.t) (target_type : Type.t) : bool =
  let t, success = Typing.type_lexpr gamma le in
  if success then Option.fold ~some:(Type.equal target_type) ~none:true t
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
           match (a : Expr.t) with
           | BinOp (le1, Equal, le2) when Expr.equal le1 nle -> le2 :: ac
           | BinOp (le2, Equal, le1) when Expr.equal le1 nle -> le2 :: ac
           | BinOp (e, Equal, EList el) | BinOp (EList el, Equal, e) -> (
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
  let rec loop (pfs : PFS.t) (checked : Expr.Set.t) (lst : Expr.t) :
      (Expr.t * Expr.t) option =
    let loop = loop pfs (Expr.Set.add lst checked) in
    match lst with
    (* Nothing can be done for variables *)
    (* FIXME: This function is suboptimal *)
    | PVar _ -> None
    | LVar _ -> (
        let ole =
          get_equal_expressions pfs lst
          |> List.filter (fun x -> not (Expr.Set.mem x checked))
        in
        match ole with
        | [] -> None
        | le :: _ ->
            L.verbose (fun fmt -> fmt "LE: %a\n\n" Expr.pp le);
            loop le)
    (* Base lists of literals and logical expressions *)
    | Lit (LList []) -> None
    | Lit (LList (hd :: tl)) -> Some (Lit hd, Lit (LList tl))
    | EList [] -> None
    | EList (hd :: tl) -> Some (hd, EList tl)
    | NOp (LstCat, lel :: ler) ->
        Option.map
          (fun (hd, tl) -> (hd, Expr.NOp (LstCat, tl :: ler)))
          (loop lel)
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
        (Option.map (fun ll -> Option.map (( + ) ll) (f sr)) (f sl))
  | _ ->
      Fmt.failwith "get_length_of_string: string equals %a, impossible" Expr.pp
        str

(* Finding the nth element of a list *)
let rec get_nth_of_string (str : Expr.t) (idx : int) : Expr.t option =
  let f = get_nth_of_string in

  let err_msg = "get_nth_of_string: index out of bounds." in

  (* If we can compute the length of the list, then the index needs to be compatible *)
  let olen = get_length_of_string str in
  let () =
    match olen with
    | Some len when len <= idx -> raise (ReductionException (Lit Nono, err_msg))
    | _ -> ()
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
        Option.bind (get_length_of_string ls) (fun llen ->
            let lst, idx = if idx < llen then (ls, idx) else (rs, idx - llen) in
            f lst idx)
    | _ ->
        Fmt.failwith "get_nth_of_string: string equals %a, impossible" Expr.pp
          str
  in
  result

(**********************************)
(* SET REASONING HELPER FUNCTIONS *)
(**********************************)

let is_different (pfs : Expr.t list) (li : Expr.t) (lj : Expr.t) : bool option =
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
            List.mem (Expr.UnOp (Not, BinOp (li, Equal, lj))) pfs
            || List.mem (Expr.UnOp (Not, BinOp (lj, Equal, li))) pfs
          then Some true
          else None)

(* I dont understand this! *)
let rec set_member (pfs : Expr.t list) m s =
  let f = set_member pfs m in
  match s with
  | Expr.LVar _ -> m = s
  | Expr.ESet s -> List.mem m s
  | Expr.NOp (SetUnion, les) -> List.exists f les
  | Expr.NOp (SetInter, les) -> List.for_all f les
  | _ -> List.mem (Expr.BinOp (m, SetMem, s)) pfs

let rec not_set_member pfs m s =
  let f = not_set_member pfs m in
  match s with
  | Expr.NOp (SetUnion, les) -> List.for_all f les
  | Expr.NOp (SetInter, les) -> List.exists f les
  | Expr.ESet les ->
      List.for_all (fun le -> is_different pfs m le = Some true) les
  | _ -> List.mem (Expr.UnOp (Not, BinOp (m, SetMem, s))) pfs

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

let rec contained_in_union (pfs : Expr.t list) (le1 : Expr.t) (le2 : Expr.t) =
  L.(
    tmi (fun m ->
        m "Contained in union: %s %s"
          ((Fmt.to_to_string Expr.pp) le1)
          ((Fmt.to_to_string Expr.pp) le2)));
  match (le2, pfs) with
  | LVar _, BinOp (le, Equal, NOp (SetUnion, les)) :: _
    when le = le2 && List.mem le1 les -> true
  | LVar _, _ :: rest -> contained_in_union rest le1 le2
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
        (function
          | BinOp (NOp (LstCat, lx), Equal, NOp (LstCat, LVar y' :: _))
            when y = y' -> List_utils.list_sub lx 0 (List.length x) = Some x
          | _ -> false)
        pfs
  | LVar x ->
      PFS.exists
        (function
          | BinOp (NOp (LstCat, LVar x' :: _), Equal, NOp (LstCat, LVar y' :: _))
            -> (x' = x && y = y') || (y' = x && x' = y)
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
            | Some vl ->
                if vl = vr then
                  (Expr.Map.remove e restl, Expr.Map.remove e restr)
                else if vl > vr then
                  (Expr.Map.add e (P.sub vl vr) restl, Expr.Map.remove e restr)
                else
                  (Expr.Map.remove e restl, Expr.Map.add e (P.sub vr vl) restr))
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
        | BinOp (e1, Equal, e2) when e1 = llen_expr -> Cint.of_expr e2 :: found
        | BinOp (e1, Equal, e2) when e2 = llen_expr -> Cint.of_expr e1 :: found
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
(* TODO: can this whole mess be removed since we did sth similar with formulae? *)
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
    (* -------------------------
              Base cases
       ------------------------- *)
    | Lit _ | PVar _ | ALoc _ -> le
    (* -------------------------
                 LVar
       ------------------------- *)
    | LVar _ when reduce_lvars ->
        get_equal_expressions pfs le
        |> List.find_opt (function
             | Expr.Lit _ -> true
             | _ -> false)
        |> Option.value ~default:le
    | LVar _ -> le
    (* -------------------------
                 EList
       ------------------------- *)
    | EList les -> List.map f les |> Expr.list
    (* -------------------------
                 ESet
       ------------------------- *)
    | ESet les -> ESet (Expr.Set.elements @@ Expr.Set.of_list @@ List.map f les)
    (* -------------------------
            ForAll + Exists
       ------------------------- *)
    (* Given:  l = [l0, l1, ..., ln]
       Before: ∀ i∈ℕ. i<0 ∨ len(l)<=i ∨ l[i]==e
       After:  l==[e, e, ..., e] *)
    | ForAll
        ( [ (x, Some IntType) ],
          BinOp
            ( BinOp
                ( BinOp (LVar a, ILessThan, Lit (Int z)),
                  Or,
                  BinOp (Lit (Int len), ILessThanEqual, LVar b) ),
              Or,
              BinOp (BinOp (EList c, LstNth, LVar d), Equal, e) ) )
      when Z.equal z Z.zero && String.equal x a && String.equal a b
           && String.equal b d
           && Int.equal (List.compare_length_with c (Z.to_int len)) 0 ->
        let rhs = Expr.EList (List_utils.make (Z.to_int len) e) in
        BinOp (EList c, Equal, rhs)
    (* Given:  l = [l0, l1, ..., ln]
       Before: ∀ i∈ℕ. (0<=i ∧ i<len(l)) => l[i]==k
       After:  l0=k ∧ l1=k ∧ ... ∧ ln=k *)
    | ForAll
        ( [ (i, Some IntType) ],
          BinOp
            ( BinOp
                ( BinOp (Lit (Int z), ILessThanEqual, LVar i'),
                  And,
                  BinOp (LVar i'', ILessThan, UnOp (LstLen, (EList ll as l))) ),
              Impl,
              BinOp (BinOp (l', LstNth, LVar i'''), Equal, k) ) )
      when Z.(equal z zero)
           && i = i' && i' = i'' && i'' = i''' && Expr.equal l l' ->
        List.map (Expr.Infix.( == ) k) ll |> Expr.conjunct
    | ForAll (bt, e) | Exists (bt, e) -> (
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
          bt;
        PFS.substitution subst new_pfs;
        (* We reduce using our new pfs and gamma *)
        let re =
          reduce_lexpr_loop ~matching ~reduce_lvars new_pfs new_gamma e
        in
        let vars = Expr.lvars re in
        let bt = List.filter (fun (b, _) -> Containers.SS.mem b vars) bt in
        (* We remove all quantifiers that aren't used anymore *)
        match (le, bt) with
        | _, [] -> re
        | ForAll _, _ -> ForAll (bt, re)
        | Exists _, _ -> Exists (bt, re)
        | _, _ -> failwith "Impossible.")
    (* -------------------------
                 LstSub
       ------------------------- *)
    | LstSub (LstSub (ile1, ile2, _), _, _)
      when match find_lstsub_inn ile1 ile2 with
           | (LVar _ as x), _
             when List.exists
                    (function
                      | Expr.LVar _ -> false
                      | _ -> true)
                    (find_equalities pfs x) -> true
           | _, (LVar _ as x)
             when List.exists
                    (function
                      | Expr.LVar _ -> false
                      | _ -> true)
                    (find_equalities pfs x) -> true
           | _ -> false ->
        (* We (painfully) found out that we can substitute something
           from an LVar to a non-LVar -- now we do it, avoiding getting
           results more than once. *)
        let inn_lst, inn_start = find_lstsub_inn ile1 ile2 in
        let to_subst, subst_with =
          let list_eqs =
            match inn_lst with
            | LVar _ ->
                find_equalities pfs inn_lst
                |> List.filter (function
                     | Expr.LVar _ -> false
                     | _ -> true)
            | _ -> []
          in
          match list_eqs with
          | x :: _ -> (inn_lst, x)
          | [] ->
              let x =
                find_equalities pfs inn_start
                |> List.find (function
                     | Expr.LVar _ -> false
                     | _ -> true)
              in
              (inn_start, x)
        in
        let att_exp = Expr.subst_expr_for_expr ~to_subst ~subst_with le in
        let reduced_att_exp = f att_exp in
        (* We can't reduce further, so useless - throw away *)
        if att_exp = reduced_att_exp then le else reduced_att_exp
    (* If:
       - l[n..(len(l))] with n a constant
       - l is of the form l1 ++ l2 ++ ... ++ lm
       - len(l1) = n and !(m = 2 && l2 = l) (ie. a recursive list??)
       Then we reduce to l2 ++ ... ++ lm *)
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
        get_equal_expressions pfs l
        |> List.find_map (function
             | Expr.NOp (LstCat, EList les :: rest)
               when Int.equal (List.compare_length_with les (Z.to_int n)) 0 ->
                 Some (Expr.NOp (LstCat, rest))
             | NOp (LstCat, Lit (LList les) :: rest)
               when Int.equal (List.compare_length_with les (Z.to_int n)) 0 ->
                 Some (NOp (LstCat, rest))
             | _ -> None)
        |> Option.get
    | LstSub (e1, Lit (Int z), e3)
      when Z.equal z Z.zero
           && List.mem (Cint.of_expr e3) (find_list_length_eqs pfs e1) -> e1
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
            res
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
            Expr.list_sub ~lst:(Option.get first) ~start:(Expr.int 0)
              ~size:(Expr.int_z n)
        | fle1, UnOp (LstLen, lx), fle3 when fst (list_prefix pfs lx fle1) ->
            L.tmi (fun fmt -> fmt "Case 7");
            let _, suffix = list_prefix pfs lx fle1 in
            LstSub (suffix, Expr.zero_i, fle3)
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
            new_lstsub
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
              Expr.LstSub (NOp (LstCat, rest_of_lel :: ler), Expr.zero_i, fle3)
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
              Expr.NOp
                ( LstCat,
                  [
                    lel;
                    LstSub
                      ( NOp (LstCat, ler),
                        Expr.zero_i,
                        BinOp (fle3, IMinus, UnOp (LstLen, lel)) );
                  ] )
            in
            L.verbose (fun fmt ->
                fmt "LSUB: Contains first result: %a" Expr.pp result);
            result
        | _ ->
            L.tmi (fun fmt -> fmt "Case 15");
            LstSub (fle1, fle2, fle3))
    (* -------------------------
                 UnOp
       ------------------------- *)
    (* Cancelling *)
    | UnOp (NumToInt, UnOp (IntToNum, e))
    | UnOp (FUnaryMinus, UnOp (FUnaryMinus, e))
    | UnOp (IUnaryMinus, UnOp (IUnaryMinus, e))
    | UnOp (LstLen, BinOp (_, LstRepeat, e))
    | UnOp (LstLen, LstSub (_, _, e)) -> e
    | UnOp (IntToNum, UnOp (NumToInt, le)) when PFS.mem pfs (UnOp (IsInt, le))
      -> le
    (* Number-to-string-to-number-to-string-to... *)
    | UnOp (ToNumberOp, UnOp (ToStringOp, le)) -> (
        let fle = f le in
        match fle with
        | Lit (Num _) -> fle
        | _ -> (
            let tfle, how = Typing.type_lexpr gamma fle in
            match (how, tfle) with
            | true, Some NumberType -> fle
            | _, _ -> UnOp (ToNumberOp, UnOp (ToStringOp, fle))))
    | UnOp (LstRev, UnOp (LstRev, le)) -> le
    (* Less than and lessthaneq *)
    | UnOp (Not, BinOp (le1, FLessThan, le2)) -> BinOp (le2, FLessThanEqual, le1)
    | UnOp (Not, BinOp (le1, FLessThanEqual, le2)) -> BinOp (le2, FLessThan, le1)
    | UnOp (Not, BinOp (le1, ILessThan, le2)) -> BinOp (le2, ILessThanEqual, le1)
    | UnOp (Not, BinOp (le1, ILessThanEqual, le2)) -> BinOp (le2, ILessThan, le1)
    | UnOp (op, le) -> (
        let fle = f le in
        let def = Expr.UnOp (op, fle) in
        match (op, fle) with
        | _, Lit lit -> (
            try Lit (CExprEval.evaluate_unop op lit) with
            | CExprEval.TypeError err_msg ->
                raise (ReductionException (def, err_msg))
            | CExprEval.EvaluationError err_msg ->
                raise (ReductionException (def, err_msg))
            | e -> raise e)
        (* Negation *)
        | Not, UnOp (Not, ex) -> ex
        | Not, BinOp (ex, And, ey) -> BinOp (UnOp (Not, ex), Or, UnOp (Not, ey))
        | Not, BinOp (ex, Or, ey) -> BinOp (UnOp (Not, ex), And, UnOp (Not, ey))
        | Not, _ -> def
        (* The TypeOf operator *)
        | TypeOf, _ -> (
            let tfle, how = Typing.type_lexpr gamma fle in
            match how with
            | false ->
                let err_msg = "LTypeOf(le): expression is not typable." in
                raise (ReductionException (def, err_msg))
            | true -> (
                match tfle with
                | None -> def
                | Some t -> Lit (Type t)))
        (* List operations: head, tail *)
        | Car, EList (hd :: _) -> hd
        | Car, _ when lexpr_is_list gamma fle ->
            let ohdtl = get_head_and_tail_of_list ~pfs fle in
            Option.fold ~some:(fun (hd, _) -> f hd) ~none:def ohdtl
        | Cdr, EList (_ :: tl) -> EList tl
        | Cdr, _ when lexpr_is_list gamma fle ->
            let ohdtl = get_head_and_tail_of_list ~pfs fle in
            Option.fold ~some:(fun (_, tl) -> f tl) ~none:def ohdtl
        (* List operations: length *)
        | LstLen, EList le -> Expr.int (List.length le)
        | LstLen, NOp (LstCat, []) -> Expr.zero_i
        | LstLen, NOp (LstCat, les) when lexpr_is_list gamma fle ->
            let les = List.map Expr.list_length les in
            List.fold_left Expr.Infix.( + ) (List.hd les) (List.tl les)
        | LstLen, LstSub (_, _, len) when lexpr_is_list gamma fle -> len
        | LstLen, _ when lexpr_is_list gamma fle -> def
        (* List operations: reverse *)
        | LstRev, EList le -> EList (List.rev le)
        | LstRev, NOp (LstCat, les) when lexpr_is_list gamma fle ->
            NOp (LstCat, List.rev_map (fun x -> Expr.UnOp (LstRev, x)) les)
        | LstRev, _ when lexpr_is_list gamma fle -> def
        (* List operations when not lists *)
        | Car, _ | Cdr, _ | LstLen, _ | LstRev, _ ->
            let err_msg =
              Fmt.str "UnOp(%s, list): list is not a GIL list." (UnOp.str op)
            in
            raise (ReductionException (def, err_msg))
        (* Set operation *)
        | SetToList, ESet le -> EList (Expr.Set.elements (Expr.Set.of_list le))
        (* String length *)
        | StrLen, _ when lexpr_is_string gamma fle ->
            let len = get_length_of_string fle in
            Option.fold
              ~some:(fun len -> Expr.Lit (Num (float_of_int len)))
              ~none:def len
        | StrLen, _ ->
            let err_msg = "UnOp(StrLen, list): string is not a GIL string." in
            raise (ReductionException (def, err_msg))
        (* Minus *)
        | FUnaryMinus, _ when lexpr_is_number ~gamma def ->
            simplify_num_arithmetic_lexpr pfs gamma def
        | IUnaryMinus, _ when lexpr_is_int ~gamma def ->
            simplify_int_arithmetic_lexpr pfs gamma def
        (* IsInt *)
        | IsInt, UnOp (IntToNum, e) -> (
            match Typing.type_lexpr gamma e with
            | Some IntType, _ -> Expr.true_
            | Some _, _ -> Expr.false_
            | None, _ -> BinOp (UnOp (TypeOf, e), Equal, Lit (Type IntType)))
        | _, _ -> def)
    (* -------------------------
                 NOp
       ------------------------- *)
    (* List concatenation *)
    (* l[0..n] ++ l[n..n+m] ++ rest <=> l[0..n+m] ++ rest *)
    | NOp (LstCat, LstSub (x1, Lit (Int z), z1) :: LstSub (x2, y2, z3) :: rest)
      when Z.equal z Z.zero && Expr.equal x1 x2 && Expr.equal z1 y2 ->
        NOp (LstCat, LstSub (x1, Expr.zero_i, Expr.Infix.( + ) z1 z3) :: rest)
    | NOp (LstCat, fst :: rest) when PFS.mem pfs (BinOp (fst, Equal, EList []))
      -> NOp (LstCat, rest)
    | NOp (LstCat, [ x; LstSub (LVar y, UnOp (LstLen, x'), len) ])
      when x = x'
           && Cint.canonicalise len
              = Cint.canonicalise
                  (BinOp (UnOp (LstLen, LVar y), IMinus, UnOp (LstLen, x)))
           && prefix_catch pfs x y -> LVar y
    | NOp (LstCat, les) -> normalise_cat f les
    (* Set union *)
    | NOp (SetUnion, les) -> (
        let fles = List.map f les in
        (* Flatten unions *)
        let fles =
          List.concat_map
            (function
              | Expr.NOp (SetUnion, les) -> les
              | le -> [ le ])
            fles
        in
        (* Join ESets *)
        let lesets, rest =
          List.partition_map
            (function
              | Expr.ESet es -> Left es
              | e -> Right e)
            fles
        in
        let lesets = List.concat lesets in
        (* Merge together, without duplicates *)
        let rest = rest |> Expr.Set.of_list |> Expr.Set.elements in
        let fles =
          match lesets with
          | [] -> rest
          | _ ->
              (* TODO: Check is List.sort_uniq is faster than Set *)
              let lesets = lesets |> Expr.Set.of_list |> Expr.Set.elements in
              Expr.ESet lesets :: rest
        in
        (* Remove duplicates *)
        match fles with
        | [] -> ESet []
        | [ x ] -> x
        | _ -> NOp (SetUnion, fles))
    (* Set intersection *)
    | NOp (SetInter, [ BinOp (le1, SetDiff, le2); ESet le3 ]) ->
        NOp (SetInter, [ le2; BinOp (ESet le3, SetDiff, le1) ])
    | NOp (SetInter, les) -> (
        let fles = List.map f les in
        (* Flatten intersections *)
        let fles =
          List.concat_map
            (function
              | Expr.NOp (SetInter, es) -> es
              | e -> [ e ])
            fles
        in
        (* Join ESets *)
        let lesets, rest =
          List.partition_map
            (function
              | Expr.ESet es -> Left es
              | e -> Right e)
            fles
        in
        let lesets = List.concat lesets in
        (* Merge together, without duplicates *)
        match (lesets, rest) with
        | [], _ -> ESet []
        | _, [] ->
            let lesets = lesets |> Expr.Set.of_list |> Expr.Set.elements in
            Expr.ESet lesets
        | _ ->
            let lesets = lesets |> Expr.Set.of_list |> Expr.Set.elements in
            let rest = rest |> Expr.Set.of_list |> Expr.Set.elements in
            let fles = Expr.ESet lesets :: rest in
            NOp (SetInter, fles))
    (* -------------------------
                BinOp
             (terrifying)
       ------------------------- *)
    (* BinOps: Equalities (basics) *)
    | BinOp (e1, Equal, e2) when Expr.equal e1 e2 -> Expr.true_
    (* BinOps: Equalities (injective unops) *)
    | BinOp (UnOp (IUnaryMinus, e1), Equal, UnOp (IUnaryMinus, e2))
    | BinOp (UnOp (FUnaryMinus, e1), Equal, UnOp (FUnaryMinus, e2))
    | BinOp (UnOp (BitwiseNot, e1), Equal, UnOp (BitwiseNot, e2))
    | BinOp (UnOp (Not, e1), Equal, UnOp (Not, e2))
    | BinOp (UnOp (LstRev, e1), Equal, UnOp (LstRev, e2))
    | BinOp (UnOp (IntToNum, e1), Equal, UnOp (IntToNum, e2))
    | BinOp (UnOp (ToStringOp, e1), Equal, UnOp (ToStringOp, e2)) ->
        BinOp (e1, Equal, e2)
    (* BinOps: Equalities (locations) *)
    (* This line is the central mechanism to "matching": *)
    | BinOp (ALoc x, Equal, ALoc y) when not matching -> Lit (Bool (x = y))
    | BinOp (ALoc _, Equal, Lit (Loc _)) | BinOp (Lit (Loc _), Equal, ALoc _) ->
        Expr.false_
    (* BinOps: Equalities (lists) *)
    | BinOp (Lit (LList ll), Equal, Lit (LList lr)) -> Expr.bool (ll = lr)
    | BinOp (EList le, Equal, Lit (LList ll))
    | BinOp (Lit (LList ll), Equal, EList le) ->
        if List.length ll <> List.length le then Expr.false_
        else if ll = [] then Expr.true_
        else
          List.map2 (fun x y -> Expr.Infix.( == ) x (Lit y)) le ll
          |> Expr.conjunct
    (* Z3 edge case: A list can't contain itself *)
    | BinOp (EList [ x ], Equal, y) when Expr.equal x y -> Expr.false_
    | BinOp (x, Equal, EList [ y ]) when Expr.equal x y -> Expr.false_
    | BinOp (EList ll, Equal, EList lr) ->
        if List.length ll <> List.length lr then Expr.(false_)
        else if ll = [] then Expr.(true_)
        else List.map2 Expr.Infix.( == ) ll lr |> Expr.conjunct
    (* x = l1 ++ ... ++ ln when x = li and there is a non empty list => false *)
    | BinOp (NOp (LstCat, les), Equal, (LVar _ as x))
      when List.mem x les
           && List.exists
                (function
                  | Expr.EList (_ :: _) | Lit (LList (_ :: _)) -> true
                  | _ -> false)
                les -> Expr.false_
    (* l[0..n] = l <=> n = len(l)  *)
    | BinOp (LstSub (e1, Lit (Int z), el), Equal, e2)
      when Z.equal z Z.zero && Expr.equal e1 e2 ->
        BinOp (UnOp (LstLen, e1), Equal, el)
    | BinOp (e2, Equal, LstSub (e1, Lit (Int z), el))
      when Z.equal z Z.zero && Expr.equal e1 e2 ->
        BinOp (UnOp (LstLen, e1), Equal, el)
    (* (l ++ ...)[0..n] = l  <==> n = len(l) *)
    | BinOp (e2, Equal, LstSub (NOp (LstCat, e1 :: _), Lit (Int z), el))
      when Z.equal z Z.zero && Expr.equal e1 e2 ->
        BinOp (UnOp (LstLen, e1), Equal, el)
    | BinOp (LstSub (NOp (LstCat, e1 :: _), Lit (Int z), el), Equal, e2)
      when Z.equal z Z.zero && Expr.equal e1 e2 ->
        BinOp (UnOp (LstLen, e1), Equal, el)
    (* l = (l1 ++ l ++ ...)[n..m] <=> n = len(l1) /\ m = len(l) *)
    | BinOp (e2, Equal, LstSub (NOp (LstCat, e3 :: e1 :: _), ex, ey))
      when Expr.equal e1 e2 ->
        BinOp
          ( BinOp (UnOp (LstLen, e3), Equal, ex),
            And,
            BinOp (UnOp (LstLen, e1), Equal, ey) )
    | BinOp (LstSub (NOp (LstCat, e3 :: e1 :: _), ex, ey), Equal, e2)
      when Expr.equal e1 e2 ->
        BinOp
          ( BinOp (UnOp (LstLen, e3), Equal, ex),
            And,
            BinOp (UnOp (LstLen, e1), Equal, ey) )
    (* l ++ l1 = l ++ l2 <=> l1 = l2 *)
    | BinOp (NOp (LstCat, fl :: rl), Equal, NOp (LstCat, fr :: rr))
      when Expr.equal fl fr -> BinOp (NOp (LstCat, rl), Equal, NOp (LstCat, rr))
    (* la ++ ... ++ l = lb ++ ... ++ l <=> la ++ ... = lb ++ ... *)
    | BinOp
        ( NOp (LstCat, (_ :: (_ :: _ as rl) as fl)),
          Equal,
          NOp (LstCat, (_ :: (_ :: _ as rr) as fr)) )
      when let last l = List.hd @@ List.rev l in
           Expr.equal (last rl) (last rr) ->
        let rem_last l = List.rev @@ List.tl @@ List.rev l in
        BinOp (NOp (LstCat, rem_last fl), Equal, NOp (LstCat, rem_last fr))
    (* l = l[0..s] ++ ... /\ len(l) < s <=> false *)
    | BinOp
        ( LVar lst,
          Equal,
          NOp (LstCat, LstSub (LVar lst', Lit (Int z), split) :: _) )
      when Z.equal z Z.zero && String.equal lst lst'
           && PFS.mem pfs (BinOp (UnOp (LstLen, LVar lst), ILessThan, split)) ->
        Expr.false_
    (* l U {x} = l' U {x} /\ x ∉ l /\ x ∉ l' <=> l = l' *)
    | BinOp
        ( NOp (SetUnion, [ ls; ESet [ lx ] ]),
          Equal,
          NOp (SetUnion, [ rs; ESet [ rx ] ]) )
      when lx = rx
           && PFS.mem pfs (UnOp (Not, BinOp (lx, SetMem, ls)))
           && PFS.mem pfs (UnOp (Not, BinOp (lx, SetMem, rs))) ->
        BinOp (ls, Equal, rs)
    (* BinOps: Equalities (maths) *)
    (* These always map to a boolean so no need for =.
       Could be simplified with an fn that maps BinOp -> ret type *)
    | BinOp
        ( Lit (Bool true),
          Equal,
          (BinOp
             ( _,
               ( Equal
               | ILessThan
               | ILessThanEqual
               | FLessThan
               | FLessThanEqual
               | StrLess
               | And
               | Or
               | Impl
               | SetMem
               | SetSub ),
               _ ) as e) ) -> e
    | BinOp (Lit (Bool false), Equal, BinOp (e1, ILessThan, e2)) ->
        BinOp (e2, ILessThanEqual, e1)
    | BinOp (Lit (Bool false), Equal, BinOp (e1, ILessThanEqual, e2)) ->
        BinOp (e2, ILessThan, e1)
    | BinOp (Lit (Bool false), Equal, BinOp (e1, FLessThan, e2)) ->
        BinOp (e2, FLessThanEqual, e1)
    | BinOp (Lit (Bool false), Equal, BinOp (e1, FLessThanEqual, e2)) ->
        BinOp (e2, FLessThan, e1)
    | BinOp (* x + (-y) = 0f <=> x = y *)
        (BinOp (LVar x, FPlus, UnOp (FUnaryMinus, LVar y)), Equal, Lit (Num 0.))
      -> BinOp (LVar x, Equal, LVar y)
    | BinOp (* x + (-y) = 0i <=> x = y *)
        (BinOp (LVar x, IPlus, UnOp (IUnaryMinus, LVar y)), Equal, Lit (Int z))
      when Z.equal z Z.zero -> BinOp (LVar x, Equal, LVar y)
    | BinOp (BinOp (Lit (Num x), FPlus, LVar y), Equal, LVar z)
      when x <> 0. && String.equal y z -> Expr.false_
    | BinOp (BinOp (Lit (Int x), IPlus, LVar y), Equal, LVar z)
      when (not (Z.equal x Z.zero)) && String.equal y z -> Expr.false_
    (* FIXME: INTEGER BYTE-BY-BYTE BREAKDOWN *)
    (* 256 * b1 + b0 = n /\ b0,b1 ∈ [0;256[ <==> b1 = n/256 /\ b0 = n-b1
       Opale: The b0 = n-b1 bit is weird?? Why not mod? *)
    | BinOp
        ( Lit (Int n),
          Equal,
          BinOp
            ( BinOp (Lit (Int tfs), ITimes, (LVar _ as b1)),
              IPlus,
              (LVar _ as b0) ) )
      when (*top_level &&*)
           Z.equal tfs _256
           && PFS.mem pfs (BinOp (Expr.zero_i, ILessThanEqual, b0))
           && PFS.mem pfs (BinOp (Expr.zero_i, ILessThanEqual, b1))
           && PFS.mem pfs (BinOp (b0, ILessThan, Lit (Int _256)))
           && PFS.mem pfs (BinOp (b1, ILessThan, Lit (Int _256))) ->
        if Z.gt n _65535 then Expr.false_
        else
          let vb1 = Z.div n _256 in
          let vb0 = Z.sub n vb1 in
          BinOp
            ( BinOp (b1, Equal, Lit (Int vb1)),
              And,
              BinOp (b0, Equal, Lit (Int vb0)) )
    | BinOp
        ( BinOp
            ( BinOp (Lit (Int tfs), ITimes, (LVar _ as b1)),
              IPlus,
              (LVar _ as b0) ),
          Equal,
          Lit (Int n) )
      when (*top_level &&*)
           Z.equal tfs _256
           && PFS.mem pfs (BinOp (Expr.zero_i, ILessThanEqual, b0))
           && PFS.mem pfs (BinOp (Expr.zero_i, ILessThanEqual, b1))
           && PFS.mem pfs (BinOp (b0, ILessThan, Lit (Int _256)))
           && PFS.mem pfs (BinOp (b1, ILessThan, Lit (Int _256))) ->
        if Z.gt n _65535 then Expr.false_
        else
          let vb1 = Z.div n _256 in
          let vb0 = Z.sub n vb1 in
          BinOp
            ( BinOp (b1, Equal, Lit (Int vb1)),
              And,
              BinOp (b0, Equal, Lit (Int vb0)) )
    | BinOp (BinOp (e, FTimes, Lit (Num x)), Equal, Lit (Num 0.)) when x <> 0.
      -> BinOp (e, Equal, Lit (Num 0.))
    | BinOp (BinOp (Lit (Num x), FTimes, e), Equal, Lit (Num 0.)) when x <> 0.
      -> BinOp (e, Equal, Lit (Num 0.))
    | BinOp (BinOp (e, ITimes, Lit (Int x)), Equal, Lit (Int n))
      when Z.equal n Z.zero && not (Z.equal x Z.zero) ->
        BinOp (e, Equal, Expr.zero_i)
    | BinOp (BinOp (Lit (Int x), ITimes, e), Equal, Lit (Int n))
      when Z.equal n Z.zero && not (Z.equal x Z.zero) ->
        BinOp (e, Equal, Expr.zero_i)
    | BinOp (BinOp (a, FTimes, b), FMod, c)
      when Expr.equal a c || Expr.equal b c -> Expr.num 0.
    | BinOp (x, FTimes, BinOp (y, FDiv, z)) when x = z -> y
    | BinOp (BinOp (x, FDiv, y), FTimes, z) when y = z -> x
    | BinOp (UnOp (NumToInt, x), Equal, y) | BinOp (y, Equal, UnOp (NumToInt, x))
      -> BinOp (UnOp (IntToNum, y), Equal, x)
    (* BinOps: Equalities (strings) *)
    (* x = y ++ z
          /\ |x| < |y| => false
          /\ |x| = |y| => x = y /\ z = ""
          /\ |x| > |y| => x[0..|y|] = y /\ x[|y|..] = z *)
    | BinOp (Lit (String ls), Equal, BinOp (Lit (String rs), StrCat, s))
    | BinOp (BinOp (Lit (String rs), StrCat, s), Equal, Lit (String ls)) -> (
        let lls = String.length ls in
        let lrs = String.length rs in
        match Stdlib.compare lls lrs with
        | -1 -> Expr.false_
        | 0 when ls <> rs -> Expr.false_
        | 0 -> BinOp (s, Equal, Lit (String ""))
        | 1 when not (String.starts_with ~prefix:rs ls) -> Expr.false_
        | 1 -> BinOp (s, Equal, Lit (String (String.sub ls lrs (lls - lrs))))
        | _ -> raise (Exceptions.Impossible "int comparison not in {-1, 0, 1}"))
    (* a ++ b = a ++ c <=> b = c  *)
    | BinOp (BinOp (sl1, StrCat, sr1), Equal, BinOp (sl2, StrCat, sr2))
      when sl1 = sl2 -> BinOp (sr1, Equal, sr2)
    | BinOp (BinOp (sl1, StrCat, sr1), Equal, BinOp (sl2, StrCat, sr2))
      when sr1 = sr2 -> BinOp (sl1, Equal, sl2)
    (* a ++ b = a <=> b = "" *)
    | BinOp (BinOp (sl, StrCat, sr), Equal, s) when sl = s ->
        BinOp (sr, Equal, Lit (String ""))
    | BinOp (BinOp (sl, StrCat, sr), Equal, s) when sr = s ->
        BinOp (sl, Equal, Lit (String ""))
    | BinOp (s, Equal, BinOp (sl, StrCat, sr)) when sl = s ->
        BinOp (sr, Equal, Lit (String ""))
    | BinOp (s, Equal, BinOp (sl, StrCat, sr)) when sr = s ->
        BinOp (sl, Equal, Lit (String ""))
    | BinOp (BinOp (sl, StrCat, sr), Equal, Lit (String "")) ->
        BinOp
          ( BinOp (sl, Equal, Lit (String "")),
            And,
            BinOp (sr, Equal, Lit (String "")) )
    (* by injectivity *)
    | BinOp (UnOp (ToStringOp, le1), Equal, Lit (String s))
    | BinOp (Lit (String s), Equal, UnOp (ToStringOp, le1)) -> (
        match s with
        | "" -> Expr.false_
        | "Infinity" | "-Infinity" | "NaN" -> le
        | _ -> (
            try Expr.BinOp (le1, Equal, Lit (Num (Float.of_string s)))
            with _ -> Expr.false_))
    (* BinOps: Equalities (Empty?) *)
    | BinOp (Lit Empty, Equal, e) | BinOp (e, Equal, Lit Empty) -> (
        match e with
        | Lit l when l <> Empty -> Expr.false_
        | EList _ | ESet _ -> Expr.false_
        | _ -> le)
    | BinOp (Lit l1, Equal, Lit l2) -> Expr.bool (l1 = l2)
    | BinOp (Lit Nono, Equal, PVar _) | BinOp (PVar _, Equal, Lit Nono) -> le
    (* JOSE: Why are we considering the case of a logical variable being bound to None? *)
    | BinOp (Lit Nono, Equal, LVar x) | BinOp (LVar x, Equal, Lit Nono) -> (
        match Type_env.get gamma x with
        | None | Some NoneType -> le
        | _ -> Expr.false_)
    | BinOp (Lit Nono, Equal, e) | BinOp (e, Equal, Lit Nono) -> (
        let fe = f e in
        match fe with
        | Lit Nono -> Expr.true_
        | Lit _ -> Expr.false_
        | LVar x when Type_env.get gamma x = Some NoneType ->
            BinOp (Lit Nono, Equal, fe)
        | PVar _ -> BinOp (Lit Nono, Equal, fe)
        | LVar _ -> Expr.false_
        | _ -> Expr.false_)
    (* BinOps: Equalities (typing) *)
    (* Can this be generalised? add an fn to typing, that maps BinOp -> ret type *)
    | BinOp (UnOp (TypeOf, BinOp (_, StrCat, _)), Equal, Lit (Type t))
      when t <> StringType -> Expr.false_
    | BinOp (UnOp (TypeOf, BinOp (_, SetMem, _)), Equal, Lit (Type t))
      when t <> BooleanType -> Expr.false_
    (* BinOps: Logic *)
    | BinOp (Lit (Bool true), And, e)
    | BinOp (e, And, Lit (Bool true))
    | BinOp (Lit (Bool false), Or, e)
    | BinOp (e, Or, Lit (Bool false))
    | BinOp (Lit (Bool true), Impl, e) -> e
    | BinOp (Lit (Bool false), And, _) | BinOp (_, And, Lit (Bool false)) ->
        Expr.false_
    | BinOp (Lit (Bool true), Or, _)
    | BinOp (_, Or, Lit (Bool true))
    | BinOp (Lit (Bool false), Impl, _)
    | BinOp (_, Impl, Lit (Bool true)) -> Expr.true_
    | BinOp (left, Impl, Lit (Bool false)) -> UnOp (Not, left)
    | BinOp (left, Impl, right) -> (
        match f left with
        | Lit (Bool true) -> right
        | Lit (Bool false) -> Expr.true_
        | left ->
            if not @@ Expr.is_boolean_expr left then BinOp (left, Impl, f right)
            else
              let pfs_with_left = PFS.copy pfs in
              PFS.extend pfs_with_left left;
              let right =
                reduce_lexpr_loop ~matching ~reduce_lvars pfs_with_left gamma
                  right
              in
              BinOp (left, Impl, right))
    (* BinOps: List indexing *)
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
    | BinOp (x, LstRepeat, Lit (Int i)) when Z.lt i (Z.of_int 100) ->
        let fx = f x in
        let result = List.init (Z.to_int i) (fun _ -> fx) in
        EList result
    (* BinOps: String indexing *)
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
    (* BinOps: Maths *)
    | BinOp (Lit (Int z), ILessThanEqual, UnOp (LstLen, _))
      when Z.equal z Z.zero -> Expr.true_
    (* x < y /\ (y <= x \/ y < x) <=> false *)
    | BinOp (e1, FLessThan, e2)
      when PFS.mem pfs (BinOp (e2, FLessThanEqual, e1))
           || PFS.mem pfs (BinOp (e2, FLessThan, e1)) -> Expr.false_
    | BinOp (e1, ILessThan, e2)
      when PFS.mem pfs (BinOp (e2, ILessThanEqual, e1))
           || PFS.mem pfs (BinOp (e2, ILessThan, e1)) -> Expr.false_
    (* x <= y /\ y < x <=> false *)
    | BinOp (e1, FLessThanEqual, e2)
      when PFS.mem pfs (BinOp (e2, FLessThan, e1)) -> Expr.false_
    | BinOp (e1, ILessThanEqual, e2)
      when PFS.mem pfs (BinOp (e2, ILessThan, e1)) -> Expr.false_
    (* x <= y /\ x < y <=> true *)
    | BinOp (e1, FLessThanEqual, e2)
      when PFS.mem pfs (BinOp (e1, FLessThan, e2)) -> Expr.true_
    | BinOp (e1, ILessThanEqual, e2)
      when PFS.mem pfs (BinOp (e1, ILessThan, e2)) -> Expr.true_
    (* x <= y /\ y <= x <=> x = y *)
    | BinOp (e1, ((FLessThanEqual | ILessThanEqual) as op), e2)
      when PFS.mem pfs (BinOp (e2, op, e1)) -> BinOp (e1, Equal, e2)
    (* BinOps: set operations *)
    | BinOp (_, SetMem, NOp ((SetUnion | SetInter), [])) -> Expr.false_
    | BinOp (leb, SetMem, NOp (((SetUnion | SetInter) as op), le :: lle)) ->
        let bop : BinOp.t = if op = SetUnion then Or else And in
        let rle = f le in
        let rleb = f leb in
        List.fold_left
          (fun ac le ->
            let rle = f le in
            Expr.BinOp (ac, bop, BinOp (rleb, SetMem, rle)))
          (Expr.BinOp (rleb, SetMem, rle))
          lle
    | BinOp (leb, SetMem, BinOp (lel, SetDiff, ler)) ->
        let rleb = f leb in
        let rlel = f lel in
        let rler = f ler in
        BinOp
          ( BinOp (rleb, SetMem, rlel),
            And,
            UnOp (Not, BinOp (rleb, SetMem, rler)) )
    | BinOp (leb, SetMem, ESet les) ->
        let rleb = f leb in
        let rles = List.map f les in
        let result = List.map (fun le -> Expr.BinOp (rleb, Equal, le)) rles in
        Expr.disjunct result
    (* CHECK: FTimes and Div are the same, how does the 'when' scope? *)
    | BinOp (lel, op, ler) -> (
        let open Syntaxes.Option in
        (* If we're reducing A || B or A && B and either side have a reduction exception, it must be false *)
        let flel, fler, exn =
          try (f lel, f ler, false) with
          | ReductionException _ when op = Or || op = And ->
              (Expr.false_, Expr.false_, true)
          | exn -> raise exn
        in
        let- () = if exn then Some Expr.false_ else None in
        let def = Expr.BinOp (flel, op, fler) in
        let- () =
          match (flel, fler) with
          | (Lit _ as ll), (Lit _ as lr) -> (
              try
                let lit = CExprEval.evaluate_binop (CStore.init []) op ll lr in
                Some (Expr.Lit lit)
              with
              | CExprEval.TypeError err_msg ->
                  raise (ReductionException (def, err_msg))
              | CExprEval.EvaluationError err_msg ->
                  raise (ReductionException (def, err_msg))
              | e -> raise e)
          | _ -> None
        in
        let- () =
          reduce_binop_inttonum_const matching reduce_lvars pfs gamma flel fler
            op
        in
        match op with
        | Equal when Expr.equal flel fler -> Expr.true_
        | Equal -> (
            (* TODO: Here we don't use the 2nd param, is that ok? *)
            let t1, _ = Typing.type_lexpr gamma flel in
            let t2, _ = Typing.type_lexpr gamma fler in
            let is_type typ = Option.fold ~none:true ~some:(Type.equal typ) in
            let- () =
              match (t1, t2) with
              | Some t1, Some t2 when t1 <> t2 -> Some Expr.false_
              | _, _ -> None
            in
            match (flel, fler) with
            (* Lists *)
            (* Removed for now since we removed the `rpfs` parameter. Was important for AWS-JS,
               so might need to be re-added later.
               | NOp (LstCat, _), LVar y when rpfs && prefix_catch pfs flel y ->
                                 BinOp (UnOp (LstLen, flel), Equal, UnOp (LstLen, fler)) *)
            | EList [], x | x, EList [] | Lit (LList []), x | x, Lit (LList [])
              -> (
                match x with
                | Lit (LList (_ :: _)) | EList (_ :: _) -> Expr.false_
                | NOp (LstCat, les)
                  when List.exists
                         (function
                           | Expr.EList (_ :: _) | Lit (LList (_ :: _)) -> true
                           | _ -> false)
                         les -> Expr.false_
                | _ -> def)
            (* Booleans *)
            (* Need this case, otherwise we end up with lvars as assertions, which the matcher
               doesn't enjoy much. *)
            | Lit (Bool true), LVar _ | LVar _, Lit (Bool true) ->
                BinOp (flel, Equal, fler)
            | Lit (Bool true), _ when t2 = Some Type.BooleanType -> fler
            | _, Lit (Bool true) when t1 = Some Type.BooleanType -> flel
            (* Nested equalities *)
            | Lit (Bool b), (BinOp (_, Equal, _) as e)
            | (BinOp (_, Equal, _) as e), Lit (Bool b)
            | Lit (Bool b), UnOp (Not, (BinOp (_, Equal, _) as e))
            | UnOp (Not, (BinOp (_, Equal, _) as e)), Lit (Bool b) ->
                if b then e else UnOp (Not, e)
            (* For two non-LVar lists l1 = h1::tl1, l2 = h2::tl2
               l1 = l2 <=> h1 = h2 /\ tl1 = tl2
               The two 'false' cases are if we can't get the head/tail of a list
               while it is a LList/EList, meaning it's definitely empty.
               These two cases are likely to have been caught before but who knows. *)
            | _, _
              when (match (flel, fler) with
                   | LVar _, _ | _, LVar _ -> false
                   | _ -> true)
                   && is_type Type.ListType t1 && is_type Type.ListType t2 -> (
                let htl1, htl2 =
                  ( get_head_and_tail_of_list ~pfs flel,
                    get_head_and_tail_of_list ~pfs fler )
                in
                match (htl1, htl2, flel, fler) with
                | Some (hl1, tl1), Some (hl2, tl2), _, _ ->
                    BinOp (BinOp (hl1, Equal, hl2), And, BinOp (tl1, Equal, tl2))
                | None, Some _, (Lit (LList _) | EList _), _ -> Expr.false_
                | Some _, None, _, (Lit (LList _) | EList _) -> Expr.false_
                | _ -> def)
            (* FPlus theory -> theory? I would not go that far *)
            | _, _ when is_type Type.NumberType t1 && is_type Type.NumberType t2
              ->
                let success, le1', le2' = Cnum.cut flel fler in
                if success then BinOp (le1', Equal, le2') else def
            | le1, le2 when is_type Type.IntType t1 && is_type Type.IntType t2
              ->
                let success, le1', le2' = Cint.cut le1 le2 in
                if success then BinOp (le1', Equal, le2') else def
            | _, _ -> def)
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
        | And when lexpr_is_bool gamma def -> (
            match (flel, fler) with
            (* 1 is the neutral *)
            | Lit (Bool true), x | x, Lit (Bool true) -> x
            | Lit (Bool false), _ | _, Lit (Bool false) -> Lit (Bool false)
            (* Rest *)
            | _, _ ->
                if
                  (PFS.mem pfs @@ Expr.negate flel)
                  || (PFS.mem pfs @@ Expr.negate fler)
                then Lit (Bool false)
                else if PFS.mem pfs flel then fler
                else if PFS.mem pfs fler then flel
                else BinOp (flel, And, fler))
        | Or when lexpr_is_bool gamma def -> (
            match (flel, fler) with
            (* 1 is the neutral *)
            | Lit (Bool true), _ | _, Lit (Bool true) -> Lit (Bool true)
            | Lit (Bool false), x | x, Lit (Bool false) -> x
            (* Rest *)
            | _, _ ->
                if PFS.mem pfs flel || PFS.mem pfs fler then Lit (Bool true)
                else if PFS.mem pfs @@ Expr.negate flel then fler
                else if PFS.mem pfs @@ Expr.negate fler then flel
                else BinOp (flel, Or, fler))
        | StrCat when lexpr_is_string gamma def -> (
            match (flel, fler) with
            (* Empty list is the neutral *)
            | x, Lit (String "") | Lit (String ""), x -> x
            (* Rest *)
            | BinOp (el, StrCat, Lit (String s1)), Lit (String s2) ->
                BinOp (el, StrCat, Lit (String (s1 ^ s2)))
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
                when Expr.all_literals left && Expr.all_literals right ->
                  ESet Expr.Set.(elements (diff (of_list left) (of_list right)))
              | ESet left, s when Expr.all_literals left ->
                  if List.for_all (fun x -> set_member pfs x s) left then
                    ESet []
                  else def
              | ESet left, ESet right ->
                  L.verbose (fun fmt -> fmt "Inside relevant SetDiff case.");
                  let candidate_result =
                    Expr.Set.(elements (diff (of_list left) (of_list right)))
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
                  L.verbose (fun fmt -> fmt "Actual result: %a" Expr.pp result);
                  result
              | NOp (SetUnion, les), _ ->
                  let diffs =
                    List.map (fun le -> f (BinOp (le, SetDiff, fler))) les
                  in
                  NOp (SetUnion, diffs)
              | _, NOp (SetUnion, les) ->
                  NOp
                    ( SetInter,
                      List.map (fun le -> Expr.BinOp (flel, SetDiff, le)) les )
              | x, ESet [ el ]
                when List.mem (Expr.UnOp (Not, BinOp (el, SetMem, x))) pfs -> x
              | LVar _, _ -> if set_subset pfs flel fler then ESet [] else def
              | ESet les, fler when all_different pfs les ->
                  (* We must know that the elements of les are all different, and for that we need the pure formulae *)
                  let _, rest =
                    List.partition (fun x -> set_member pfs x fler) les
                  in
                  if List.for_all (fun x -> not_set_member pfs x fler) rest then
                    ESet rest
                  else BinOp (ESet rest, SetDiff, fler)
              | _, _ -> def)
        (* let hM = f (BinOp (flel, SetSub, fler)) in
           (match hM with
           | Lit (Bool true) -> ESet []
           | _ -> def)) *)
        | SetMem when lexpr_is_bool gamma def -> (
            match (flel, fler) with
            | _, ESet [] -> Lit (Bool false)
            | _, ESet [ x ] -> BinOp (flel, Equal, x)
            | le, ESet les -> (
                match List.mem le les with
                | true -> Lit (Bool true)
                | false -> (
                    match le with
                    | Lit _ ->
                        if Expr.all_literals les then Lit (Bool false) else def
                    | _ -> def))
            | _, _ -> def)
        | SetSub when lexpr_is_bool gamma def -> (
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
            if success then Expr.BinOp (el, FLessThan, er) else def
        | ILessThan -> (
            match (flel, fler) with
            | x, fler
              when let fler_len = substitute_for_list_length pfs fler in
                   match fler_len with
                   | UnOp (LstLen, _) -> true
                   | _ -> false ->
                BinOp (BinOp (x, IPlus, Lit (Int Z.one)), ILessThanEqual, fler)
            | UnOp (LstLen, _), Lit (Int n) when Z.leq n Z.zero ->
                Lit (Bool false)
            | UnOp (LstLen, le), Lit (Int z) when Z.equal z Z.one ->
                BinOp (le, Equal, EList [])
            | _ ->
                let success, el, er = Cint.cut flel fler in
                if success then Expr.BinOp (el, ILessThan, er) else def)
        | FLessThanEqual -> (
            let success, el, er = Cnum.cut flel fler in
            if success then BinOp (el, FLessThanEqual, er)
            else
              match
                check_ge_zero_num ~top_level:true pfs
                  (f (BinOp (fler, FMinus, flel)))
              with
              | Some x -> Lit (Bool x)
              | None -> def)
        | ILessThanEqual -> (
            let success, el, er = Cint.cut flel fler in
            if success then BinOp (el, ILessThanEqual, er)
            else
              match
                check_ge_zero_int ~top_level:true pfs
                  (f (BinOp (fler, IMinus, flel)))
              with
              | Some x -> Lit (Bool x)
              | None -> def)
        | _ -> def)
  in
  let result = normalise_list_expressions result in
  if Expr.equal le result then result
  else (
    L.tmi (fun m -> m "\tReduce_lexpr: %a -> %a" Expr.pp le Expr.pp result);
    f result)

and reduce_lexpr
    ?(matching = false)
    ?(reduce_lvars = false)
    ?(pfs = PFS.init ())
    ?(gamma = Type_env.init ())
    (le : Expr.t) =
  (* let t = Sys.time () in *)
  let result = reduce_lexpr_loop ~matching ~reduce_lvars pfs gamma le in
  (* Utils.Statistics.update_statistics "Reduce Expression" (Sys.time () -. t); *)
  Logging.normal (fun f ->
      f "reduce_lexpr: @[%a -> %a@]" Expr.pp le Expr.pp result);
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
  (* Opale: how is this any better? *)
  | BinOp (l, IMinus, r) -> f (BinOp (l, IPlus, UnOp (IUnaryMinus, r)))
  (* Unary minus distributes over + *)
  | UnOp (IUnaryMinus, BinOp (l, IPlus, r)) ->
      f (BinOp (UnOp (IUnaryMinus, l), IPlus, UnOp (IUnaryMinus, r)))
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
        List.exists (PFS.mem pfs)
          [
            Expr.BinOp (Expr.zero_i, ILessThanEqual, e);
            Expr.BinOp (Expr.zero_i, ILessThan, e);
          ]
      then Some true
      else if PFS.mem pfs (Expr.BinOp (e, ILessThan, Expr.zero_i)) then
        Some false
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
          [
            Expr.BinOp (Lit (Num 0.), FLessThanEqual, e);
            BinOp (Lit (Num 0.), FLessThan, e);
          ]
      then Some true
      else if PFS.mem pfs (Expr.BinOp (e, FLessThan, Lit (Num 0.))) then
        Some false
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
        | Expr.BinOp (UnOp (LstLen, LVar x), Equal, lex)
          when match lex with
               | UnOp (LstLen, LVar _) | Lit _ -> false
               | _ -> true -> Some (Expr.LVar x, lex)
        | BinOp (lex, Equal, UnOp (LstLen, LVar x))
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
  let max_fuel = 10 in

  let loc_name = function
    | Expr.ALoc loc | Lit (Loc loc) -> Some loc
    | _ -> None
  in

  let rec resolve_expr_to_location_aux
      (fuel : int)
      (tried : Expr.Set.t)
      (to_try : Expr.t list) : string option =
    let open Syntaxes.Option in
    L.tmi (fun m -> m "to_try: %a" (Fmt.Dump.list Expr.pp) to_try);
    let* () = if fuel <= 0 then None else Some () in
    let* e, rest =
      match to_try with
      | [] -> None
      | e :: rest -> Some (e, rest)
    in
    let f = resolve_expr_to_location_aux (fuel - 1) in
    (* If e is a loc name, we return it *)
    let/ () = loc_name e in
    let equal_e = get_equal_expressions pfs e in
    let equal_e = equal_e @ List.map (reduce_lexpr ~pfs ~gamma) equal_e in
    (* If we find a loc in there, we return it *)
    let/ () = List.find_map loc_name equal_e in
    (* We actually want to try all possible substs! *)
    let all_lvars = Containers.SS.elements (Expr.lvars e) in
    let subst_for_each_lvar =
      List.map
        (fun x ->
          let e = Expr.LVar x in
          let with_eq =
            List.map (fun eq -> (e, eq)) (get_equal_expressions pfs e)
          in
          (e, e) :: with_eq)
        all_lvars
    in
    L.tmi (fun m ->
        m "subst_for_each_lvar: %a"
          (Fmt.Dump.list (Fmt.Dump.list (Fmt.Dump.pair Expr.pp Expr.pp)))
          subst_for_each_lvar);
    let found_substs =
      List.fold_left
        (fun l1 l2 -> List_utils.cross_product l1 l2 (fun l x -> x :: l))
        [ [] ] subst_for_each_lvar
    in
    L.tmi (fun m ->
        m "found_substs: %a"
          (Fmt.Dump.list (Fmt.Dump.list (Fmt.Dump.pair Expr.pp Expr.pp)))
          found_substs);
    (* lvar and substs is a list [ (ei, esi) ] where for each ei, esi is a list of equal expressions.
       We are going to build the product of each esi to obtain *)
    let subst_es =
      List.map
        (List.fold_left
           (fun (e : Expr.t) (e_to, e_with) ->
             Expr.subst_expr_for_expr ~to_subst:e_to ~subst_with:e_with e)
           e)
        found_substs
    in
    L.tmi (fun m -> m "subst_es: %a" (Fmt.Dump.list Expr.pp) subst_es);
    let subst_es = List.map (reduce_lexpr ~pfs ~gamma) subst_es in
    let/ () = List.find_map loc_name subst_es in
    let new_tried = Expr.Set.add e tried in
    let new_to_try = rest @ equal_e @ subst_es in
    let new_to_try =
      List.filter (fun e -> not (Expr.Set.mem e new_tried)) new_to_try
    in
    f new_tried new_to_try
  in
  resolve_expr_to_location_aux max_fuel Expr.Set.empty [ e ]

let relate_llen
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (e : Expr.t)
    (lcat : Expr.t list) : (Expr.t * Containers.SS.t) option =
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
            let pf = Expr.BinOp (e, Equal, NOp (LstCat, les @ new_lvars)) in
            L.verbose (fun fmt -> fmt "Constructed equality: %a" Expr.pp pf);
            (pf, Containers.SS.of_list new_vars)
        | false, exp ->
            let rest_var = LVar.alloc () in
            let rest = Expr.LVar rest_var in
            let pfeq = Expr.BinOp (e, Equal, NOp (LstCat, les @ [ rest ])) in
            let pflen = Expr.BinOp (UnOp (LstLen, rest), Equal, exp) in
            (Expr.BinOp (pfeq, And, pflen), Containers.SS.singleton rest_var)
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
    (rcat : Expr.t list) : (Expr.t * Containers.SS.t) option =
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
  try
    let others, ets =
      List.fold_left
        (fun (others, ets) -> function
          | Asrt.Pure (Lit (Bool true)) -> (others, ets)
          | Asrt.Pure (Lit (Bool false)) -> raise PFSFalse
          | Asrt.Pure (BinOp (UnOp (TypeOf, e), Equal, Lit (Type t)))
          | Asrt.Pure (BinOp (Lit (Type t), Equal, UnOp (TypeOf, e))) ->
              (others, (e, t) :: ets)
          | Asrt.Types ets' -> (others, ets' @ ets)
          | a -> (a :: others, ets))
        ([], []) a
    in

    let ets = ETSet.elements (ETSet.of_list ets) in
    match (others, ets) with
    | [], [] -> [ Asrt.Pure (Lit (Bool true)) ] (* Could this be []? *)
    | [], ets -> [ Asrt.Types ets ]
    | others, [] -> others
    | others, ets -> Asrt.Types ets :: others
  with PFSFalse -> [ Asrt.Pure (Lit (Bool false)) ]

(* Reduction of assertions *)
let reduce_assertion_loop
    (matching : bool)
    (pfs : PFS.t)
    (gamma : Type_env.t)
    (a : Asrt.t) : Asrt.t =
  let fe = reduce_lexpr_loop ~matching pfs gamma in
  let f : Asrt.atom -> Asrt.t = function
    (* Empty heap *)
    | Asrt.Emp -> []
    (* Star *)
    | Wand { lhs = lname, largs; rhs = rname, rargs } ->
        [
          Wand
            {
              lhs = (lname, List.map fe largs);
              rhs = (rname, List.map fe rargs);
            };
        ]
    (* Predicates *)
    | Pred (name, les) -> [ Pred (name, List.map fe les) ]
    (* Pure assertions *)
    | Pure (Lit (Bool true)) -> []
    | Pure f -> [ Pure (reduce_lexpr ~matching ~pfs ~gamma f) ]
    (* Types *)
    | Types lvt -> (
        try
          let lvt =
            List.fold_right
              (fun (e, t) ac ->
                match (e : Expr.t) with
                | Lit lit when t <> Literal.type_of lit -> raise WrongType
                | Lit _ -> ac
                | _ -> (e, t) :: ac)
              lvt []
          in
          if lvt = [] then [] else [ Types lvt ]
        with WrongType -> [ Pure (Lit (Bool false)) ])
    (* General action *)
    | CorePred (act, l_ins, l_outs) ->
        [ CorePred (act, List.map fe l_ins, List.map fe l_outs) ]
  in
  let result = List.concat_map f a in
  let result =
    if List.mem (Asrt.Pure (Lit (Bool false))) result then
      [ Asrt.Pure (Lit (Bool false)) ]
    else result
  in

  (if a <> result && not (a == result) then
     L.(tmi (fun m -> m "Reduce_assertion: %a -> %a" Asrt.pp a Asrt.pp result)));
  result

let extract_lvar_equalities : Asrt.t -> (string * Expr.t) list =
  List.filter_map @@ function
  | Asrt.Pure (BinOp (LVar x, Equal, v) | BinOp (v, Equal, LVar x)) ->
      if Names.is_lvar_name x && not (Names.is_spec_var_name x) then Some (x, v)
      else None
  | _ -> None

let clean_double_equalities (a : Asrt.t) : Asrt.t =
  let pures = Asrt.pure_asrts a in
  let true_vars =
    pures
    |> List.filter_map @@ function
       | Expr.BinOp (LVar x, Equal, Lit (Bool true))
       | BinOp (Lit (Bool true), Equal, LVar x) -> Some x
       | _ -> None
  in
  let is_bool_binop = function
    | BinOp.Equal
    | ILessThan
    | ILessThanEqual
    | FLessThan
    | FLessThanEqual
    | StrLess
    | And
    | Or
    | Impl
    | SetMem
    | SetSub -> true
    | _ -> false
  in
  let pures', vars_to_remove =
    List.fold_left
      (fun (pures, to_remove) v ->
        let found = ref false in
        let pures' =
          pures
          |> List.map @@ function
             | (Expr.BinOp (LVar x, Equal, (BinOp (_, b, _) as e)) as a)
             | (Expr.BinOp ((BinOp (_, b, _) as e), Equal, LVar x) as a) ->
                 if List.mem x true_vars && is_bool_binop b then (
                   found := true;
                   e)
                 else a
             | a -> a
        in
        if !found then (pures', v :: to_remove) else (pures', to_remove))
      (pures, []) true_vars
  in
  let pures' =
    pures'
    |> List.filter @@ function
       | Expr.BinOp (LVar x, Equal, Lit (Bool true))
       | Expr.BinOp (Lit (Bool true), Equal, LVar x)
         when List.mem x vars_to_remove -> false
       | _ -> true
  in
  let non_pures =
    a
    |> List.filter @@ function
       | Asrt.Pure _ -> false
       | _ -> true
  in
  let pures = pures' |> List.map @@ fun x -> Asrt.Pure x in
  pures @ non_pures

let reduce_assertion
    ?(matching = false)
    ?(pfs = PFS.init ())
    ?(gamma = Type_env.init ())
    (a : Asrt.t) : Asrt.t =
  let a = reduce_types a in
  let a = clean_double_equalities a in

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
  reduce_lexpr ?pfs ?gamma formula = Lit (Bool true)
