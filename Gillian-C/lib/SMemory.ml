module GUtils = Gillian.Utils
module Result = Stdlib.Result

let ( let* ) = Option.bind

let ( let+ ) o f = Option.map f o

open Gillian.Symbolic
open Gillian.Gil_syntax
module Logging = Gillian.Logging
module SS = GUtils.Containers.SS

(* Some utils first *)

let get_loc_name pfs gamma = Gillian.Logic.FOSolver.resolve_loc_name ~pfs ~gamma

let resolve_or_create_loc_name pfs gamma lvar_loc =
  match get_loc_name pfs gamma lvar_loc with
  | None   ->
      let new_loc_name = ALoc.alloc () in
      let new_pf = Formula.Eq (ALoc new_loc_name, lvar_loc) in
      ([ new_pf ], new_loc_name)
  | Some l -> ([], l)

type vt = Values.t

type st = Subst.t

type fill_typ = PTR | INT | FLOAT32 | FLOAT64

type i_fix_t = IFSvalMem of (vt * int * fill_typ)

type err_t = {
  failing_constraint : Formula.t;
  recovery_values : vt list;
  fixes : i_fix_t list list;
}

let make_err ~fixes ?(fc = Formula.False) ?(rvs = []) () =
  { failing_constraint = fc; recovery_values = rvs; fixes }

let ( let** ) = Result.bind

let ( let++ ) r f = Result.map f r

module Mem = struct
  type permission = Compcert.Memtype.permission =
    | Freeable
    | Writable
    | Readable
    | Nonempty

  type memory_chunk = Compcert.AST.memory_chunk =
    | Mint8signed
    | Mint8unsigned
    | Mint16signed
    | Mint16unsigned
    | Mint32
    | Mint64
    | Mfloat32
    | Mfloat64
    | Many32
    | Many64

  let fill_typ_of_chunk chunk =
    match chunk with
    | Mint64        -> PTR
    | Mint32        -> INT
    | Mint8signed   -> INT
    | Mint8unsigned -> INT
    | Mfloat32      -> FLOAT32
    | Mfloat64      -> FLOAT64
    | _             ->
        failwith
          (Printf.sprintf "Unsuported chunk type for bi_abduction : %s"
             (ValueTranslation.string_of_chunk chunk))

  let type_of_chunk = Compcert.AST.type_of_chunk

  let size_chunk chunk =
    let open Compcert in
    Camlcoq.Z.to_int (Memdata.size_chunk chunk)

  let align_chunk chunk =
    let open Compcert in
    Camlcoq.Z.to_int (Memdata.align_chunk chunk)

  let perm_to_int = function
    | Freeable -> 4
    | Writable -> 3
    | Readable -> 2
    | Nonempty -> 1

  let perm_leq pa pb = perm_to_int pa <= perm_to_int pb

  let perm_leq_opt pa pb =
    match pb with
    | Some p -> perm_leq pa p
    | None   -> false

  type smemval = SymMV of SVal.t * int * int [@unboxed]

  let rec mem_data_from_val ?(c = 0) size value =
    if c >= size then []
    else SymMV (value, c, size) :: mem_data_from_val ~c:(c + 1) size value

  let encode_val chunk v =
    let open SVal in
    let make = mem_data_from_val in
    match (v, chunk) with
    | SVint _, (Mint8signed | Mint8unsigned) -> make 1 v
    | SVint _, (Mint16signed | Mint16unsigned) -> make 2 v
    | SVint _, Mint32 -> make 4 v
    | SVlong _, Mint64 -> make 8 v
    | Sptr _, Mint64 ->
        let v = if Compcert.Archi.ptr64 then v else SUndefined in
        make 8 v
    | Sptr _, Mint32 ->
        let v = if Compcert.Archi.ptr64 then SUndefined else v in
        make 4 v
    | SVsingle _, Mfloat32 -> make 4 v
    | SVfloat _, Mfloat64 -> make 8 v
    | _ ->
        Logging.verbose (fun fmt ->
            fmt
              "\n\
               Value %a cannot be put into the chunk %s, becoming undefined.\n"
              SVal.pp v
              (ValueTranslation.string_of_chunk chunk));
        make (size_chunk chunk) SUndefined

  let val_from_mem_data size data =
    let rec check v_opt ts_opt togo datap =
      match (togo, datap) with
      | 0, [] -> true
      | 0, _ | _, [] -> false
      | togo, SymMV (v, k, ts) :: r ->
          Option.fold ~some:(fun x -> x = v) ~none:true v_opt
          && Option.fold ~some:(fun x -> x = ts) ~none:true ts_opt
          && togo = ts - k
          && check (Some v) (Some ts) (togo - 1) r
    in
    match data with
    | [] -> failwith "Empty data when it can never happen"
    | SymMV (v, _, _) :: _ when check None None size data -> v
    | _ -> SUndefined

  let rec is_zero data size =
    match (size, data) with
    | 0, _                    -> true
    | _, []                   -> false
    | n, SymMV (v, _, _) :: r -> SVal.is_zero v && is_zero r (n - 1)

  let decode_val chunk data =
    let decode_symbolic chunk data =
      let open SVal in
      let check_type v typ =
        match (typ, v) with
        | Tint, SVint _ -> v
        | Tfloat, SVfloat _ -> v
        | Tlong, SVlong _ -> v
        | Tsingle, SVsingle _ -> v
        | typ, Sptr _ when typ = tptr -> v
        | _, _ -> SUndefined
      in
      match chunk with
      | Many32 | Many64 -> SUndefined
      | _               ->
          check_type
            (val_from_mem_data (size_chunk chunk) data)
            (type_of_chunk chunk)
    in
    if is_zero data (size_chunk chunk) then
      match type_of_chunk chunk with
      | Tint    -> SVal.SVint (Lit (Num 0.))
      | Tfloat  -> SVfloat (Lit (Num 0.))
      | Tlong   -> SVlong (Lit (Num 0.))
      | Tsingle -> SVsingle (Lit (Num 0.))
      | _       -> SUndefined
    else decode_symbolic chunk data

  type t = {
    bounds : (string, int * int) PMap.t;
    contents : (string, smemval array) PMap.t;
    perms : (string, permission option array) PMap.t;
  }

  let copy m =
    let cop_map_arr ma = PMap.map (fun a -> Array.copy a) ma in
    let { bounds; contents; perms } = m in
    let boundsp = bounds in
    let contentsp = cop_map_arr contents in
    let permsp = cop_map_arr perms in
    { bounds = boundsp; contents = contentsp; perms = permsp }

  (* Pretty printing *)

  module PrettyPrint = struct
    (* Pretty printing this memory is not easy *)
    type pp_info = {
      functions : int;
      (* How many non-printed functions in memory (not printed) *)
      unimplemented : int;
      (* How many unimplemented functions in memory *)
      lowest_bound : int;
      max_size_md : int;
      max_size_loc : int;
      contents : (string * int * string array * char array) list;
          (* loc, low_bound, str_memdata, char_perm *)
    }

    let empty_pp_info =
      {
        functions = 0;
        unimplemented = 0;
        lowest_bound = 10000;
        max_size_md = -1;
        max_size_loc = -1;
        contents = [];
      }

    let unimplemented genv loc =
      try
        match GEnv.find_def genv loc with
        | FunDef f
          when String.equal f CConstants.Internal_Functions.not_implemented ->
            true
        | _ -> false
      with Not_found -> false

    let is_fun genv loc =
      try
        match GEnv.find_def genv loc with
        | FunDef _ -> true
        | _        -> false
      with Not_found -> false

    let char_perm = function
      | None          -> 'X'
      | Some Nonempty -> 'N'
      | Some Readable -> 'R'
      | Some Writable -> 'W'
      | Some Freeable -> 'F'

    let str_sval sv =
      let open SVal in
      let se = Expr.pp in
      match sv with
      | SUndefined    -> "undef"
      | SVint v       -> Format.asprintf "i(%a)" se v
      | SVlong v      -> Format.asprintf "L(%a)" se v
      | SVsingle v    -> Format.asprintf "s(%a)" se v
      | SVfloat v     -> Format.asprintf "f(%a)" se v
      | Sptr (loc, v) -> Format.asprintf "P(%s,%a)" loc se v

    let str_memdata (SymMV (sv, i, k)) =
      Printf.sprintf "{%s,%i,%i}" (str_sval sv) i k

    let max_size str_arr =
      Array.fold_left (fun c e -> max (String.length e) c) (-1) str_arr

    let add_to_pi genv (mem : t) (loc : string) (low, _) pi =
      if not (is_fun genv loc) then
        let contents = PMap.find loc mem.contents in
        let perms = PMap.find loc mem.perms in
        let c_str = Array.map str_memdata contents in
        let p_char = Array.map char_perm perms in
        let m_size = max_size c_str in
        {
          functions = pi.functions;
          unimplemented = pi.unimplemented;
          lowest_bound = min low pi.lowest_bound;
          max_size_md = max pi.max_size_md m_size;
          max_size_loc = max pi.max_size_loc (String.length loc);
          contents = (loc, low, c_str, p_char) :: pi.contents;
        }
      else if unimplemented genv loc then
        { pi with unimplemented = pi.unimplemented + 1 }
      else { pi with functions = pi.functions + 1 }

    let print_white fmt k =
      for _ = 1 to k do
        Format.fprintf fmt " "
      done

    let rec print_padding fmt size lowest low c =
      let curr_pos = lowest + c in
      if curr_pos < low then (
        if curr_pos = 0 then Format.fprintf fmt "|";
        Format.fprintf fmt "%a|" print_white size;
        print_padding fmt size lowest low (c + 1) )
      else ()

    let print_centered fmt max_size lowest low i str =
      let () =
        if lowest < low && i = 0 then print_padding fmt max_size lowest low 0
      in
      let curr_pos = low + i in
      let () = if curr_pos = 0 then Format.fprintf fmt "|" in
      let str_len = String.length str in
      let padding = (max_size - str_len) / 2 in
      Format.fprintf fmt "%a%s%a|" print_white padding str print_white
        (max_size - padding - str_len)

    let print_full fmt sz lowest low i chr =
      let () =
        if lowest < low && i = 0 then print_padding fmt sz lowest low 0
      in
      let curr_pos = low + i in
      let () = if curr_pos = 0 then Format.fprintf fmt "|" in
      Format.fprintf fmt "%s|" (String.make sz chr)

    let pp ?(genv = GEnv.empty) fmt mem =
      let pp_info = PMap.foldi (add_to_pi genv mem) mem.bounds empty_pp_info in
      let print_content (loc, low, data, perms) =
        Format.fprintf fmt "%s %a|-> @[|" loc print_white
          (pp_info.max_size_loc - String.length loc);
        Array.iteri
          (print_centered fmt pp_info.max_size_md pp_info.lowest_bound low)
          data;
        Format.fprintf fmt "@\n|";
        Array.iteri
          (print_full fmt pp_info.max_size_md pp_info.lowest_bound low)
          perms;
        Format.fprintf fmt "@]@\n@\n"
      in
      let () = Format.fprintf fmt "{@[<v 2>@\n" in
      List.iter print_content pp_info.contents;
      Format.fprintf fmt "There are %i allocated functions@\n" pp_info.functions;
      Format.fprintf fmt
        "There are %i allocated unimplemented external functions@]@\n}"
        pp_info.unimplemented
  end

  let pp = PrettyPrint.pp

  (* Low-level read/write utils *)

  let find_opt a pmap = try Some (PMap.find a pmap) with Not_found -> None

  let nth_opt a arr = try arr.(a) with Invalid_argument _ -> None

  let setN lis n arr =
    let rec aux l k a =
      match l with
      | []     -> a
      | b :: r ->
          a.(k) <- b;
          aux r (k + 1) a
    in
    aux lis n (Array.copy arr)

  let rec getN ?(k = 0) n ofs arr =
    if n = k then [] else arr.(ofs + k) :: getN ~k:(k + 1) n ofs arr

  let empty =
    let compare = String.compare in
    {
      bounds = PMap.create compare;
      contents = PMap.create compare;
      perms = PMap.create compare;
    }

  let get_low_bound_opt mem loc =
    let+ low, _ = find_opt loc mem.bounds in
    low

  let get_low_bound_exn mem loc =
    let low, _ = PMap.find loc mem.bounds in
    low

  let alloc mem low high =
    let loc = ALoc.alloc () in
    let bounds = PMap.add loc (low, high) mem.bounds in
    let contents =
      PMap.add loc
        (Array.make (high - low) (SymMV (SUndefined, 0, 1)))
        mem.contents
    in
    let perms =
      PMap.add loc (Array.make (high - low) (Some Freeable)) mem.perms
    in
    ({ bounds; contents; perms }, loc)

  let getcurperm_concrete mem loc_name offset =
    let* ps = find_opt loc_name mem.perms in
    let* low = get_low_bound_opt mem loc_name in
    nth_opt (offset - low) ps

  let getcurperm mem ~pfs ~gamma loc offset =
    let* loc_name = get_loc_name pfs gamma loc in
    getcurperm_concrete mem loc_name offset

  let perm mem loc ofs p =
    let cp = getcurperm_concrete mem loc ofs in
    perm_leq_opt p cp

  let range_perm mem loc low high p =
    let rec forall cur high perms_loc =
      if cur < high then perm mem loc cur p && forall (cur + 1) high perms_loc
      else true
    in
    try forall low high (PMap.find loc mem.perms) with Not_found -> false

  (* Permission is None everywhere *)

  let valid_access mem chunk loc ofs perm =
    range_perm mem loc ofs (ofs + size_chunk chunk) perm
    && ofs mod align_chunk chunk = 0

  let store
      chunk
      mem
      ?(pfs = PureContext.init ())
      ?(gamma = TypEnv.init ())
      (loc : vt)
      (ofs : int)
      (value : SVal.t) : (t, err_t list) Result.t =
    let** loc_name =
      match get_loc_name pfs gamma loc with
      | None    ->
          Result.error
            [
              make_err ~rvs:[ loc ]
                ~fixes:[ [ IFSvalMem (loc, ofs, fill_typ_of_chunk chunk) ] ]
                ();
            ]
      | Some ln -> Result.ok ln
    in
    if not (valid_access mem chunk loc_name ofs Writable) then
      failwith
        (Format.asprintf
           "Writing Access is invalid for the chunk %s and pointer (%a, %i)"
           (ValueTranslation.string_of_chunk chunk)
           Expr.pp loc ofs)
    else
      let low = get_low_bound_exn mem loc_name in
      let new_contents_loc =
        setN (encode_val chunk value) (ofs - low)
          (PMap.find loc_name mem.contents)
      in
      let contents = PMap.add loc_name new_contents_loc mem.contents in
      Ok { mem with contents }

  let load chunk mem ~pfs ~gamma loc ofs =
    let load_error =
      make_err ~rvs:[ loc ]
        ~fixes:[ [ IFSvalMem (loc, ofs, fill_typ_of_chunk chunk) ] ]
        ()
    in
    let** loc_name =
      match get_loc_name pfs gamma loc with
      | None    -> Error [ load_error ]
      | Some ln -> Ok ln
    in
    if not (valid_access mem chunk loc_name ofs Readable) then
      Error [ load_error ]
    else
      let low = get_low_bound_exn mem loc_name in
      let data =
        getN (size_chunk chunk) (ofs - low) (PMap.find loc_name mem.contents)
      in
      Ok (decode_val chunk data)

  let rec get_uniform_perm ?current data =
    let check a =
      match current with
      | None -> a
      | Some k when k = a -> a
      | _ -> failwith "non-uniform permissions"
    in
    match data with
    | []     -> failwith "empty memdata array, cannot retrieve permissions"
    | [ a ]  -> check a
    | a :: r -> get_uniform_perm ~current:(check a) r

  let get mem pfs gamma loc low high =
    let* loc_name = get_loc_name pfs gamma loc in
    let* lb, hb = find_opt loc_name mem.bounds in
    if lb <= low && low <= high && high <= hb then
      let* cont_loc = find_opt loc_name mem.contents in
      let* perm_loc = find_opt loc_name mem.perms in
      let size = high - low in
      let sval = val_from_mem_data size (getN size (low - lb) cont_loc) in
      let perm = get_uniform_perm (getN size (low - lb) perm_loc) in
      Some (loc_name, sval, perm)
    else None

  let set_existing_loc mem loc low high sval perm_opt =
    let rec list_make k p = if k = 0 then [] else p :: list_make (k - 1) p in
    let extend st_cop sz el arr =
      let sz_arr = Array.length arr in
      let n_arr = Array.make (sz + sz_arr) el in
      let () = Array.blit arr 0 n_arr st_cop sz_arr in
      n_arr
    in
    let extend_left sz = extend sz sz in
    let extend_right sz = extend 0 sz in
    let undef_mem_data = SymMV (SUndefined, 0, 1) in
    let ccontent = PMap.find loc mem.contents in
    let cperms = PMap.find loc mem.perms in
    let* lb, hb = find_opt loc mem.bounds in
    let nlb, content, perms =
      if low < lb then
        let nlb = low in
        let ncontent = extend_left (lb - low) undef_mem_data ccontent in
        let nperms = extend_left (lb - low) None cperms in
        (nlb, ncontent, nperms)
      else (lb, ccontent, cperms)
    in
    let nhb, content, perms =
      if high > hb then
        let nhb = high in
        let ncontent = extend_right (high - hb) undef_mem_data content in
        let nperms = extend_right (high - hb) None perms in
        (nhb, ncontent, nperms)
      else (hb, content, perms)
    in
    let new_content =
      setN (mem_data_from_val (high - low) sval) (low - nlb) content
    in
    let new_perm = setN (list_make (high - low) perm_opt) (low - nlb) perms in
    let new_bounds = (nlb, nhb) in
    let new_mem =
      {
        bounds = PMap.add loc new_bounds mem.bounds;
        perms = PMap.add loc new_perm mem.perms;
        contents = PMap.add loc new_content mem.contents;
      }
    in
    Some new_mem

  let set_new_loc mem aloc low high sval perm_opt =
    let bounds = PMap.add aloc (low, high) mem.bounds in
    let contents =
      PMap.add aloc
        (Array.of_list (mem_data_from_val (high - low) sval))
        mem.contents
    in
    let perms = PMap.add aloc (Array.make (high - low) perm_opt) mem.perms in
    { bounds; contents; perms }

  let set mem pfs gamma loc low high sval perm_opt =
    match get_loc_name pfs gamma loc with
    | None          ->
        let aloc = ALoc.alloc () in
        let pf = Formula.Eq (Expr.ALoc aloc, loc) in
        let new_mem = set_new_loc mem aloc low high sval perm_opt in
        Some (new_mem, [ pf ])
    | Some loc_name ->
        Logging.verbose (fun fmt -> fmt "Found loc_name %s" loc_name);
        let exists = Option.is_some (find_opt loc_name mem.bounds) in
        let+ new_mem =
          if exists then set_existing_loc mem loc_name low high sval perm_opt
          else Some (set_new_loc mem loc_name low high sval perm_opt)
        in
        (new_mem, [])

  let rem_loc mem loc =
    {
      perms = PMap.remove loc mem.perms;
      contents = PMap.remove loc mem.contents;
      bounds = PMap.remove loc mem.bounds;
    }

  let rem mem loc low high =
    let () = ALoc.dealloc loc in
    let* lb, hb = find_opt loc mem.bounds in
    if low < lb || high > hb || not (range_perm mem loc low high Nonempty) then
      None
    else
      let* memp = set_existing_loc mem loc low high SUndefined None in
      (* then we clean the memory, by looking at what has no permissions *)
      let* content = find_opt loc memp.contents in
      let* perms = find_opt loc memp.perms in
      let rec none_left k =
        if k = hb || Option.is_some perms.(k - lb) then k else none_left (k + 1)
      in
      let hn = none_left lb in
      if hn = hb then Some (rem_loc mem loc)
      else
        let rec non_right k =
          if Option.is_some perms.(k - lb - 1) then k else non_right (k - 1)
        in
        let ln = non_right hb in
        let resize t d u a =
          let ap = Array.make (u - d) t in
          let () = Array.blit a d ap 0 (u - d) in
          ap
        in
        Some
          {
            perms =
              PMap.add loc (resize None (hn - lb) (ln - lb) perms) memp.perms;
            contents =
              PMap.add loc
                (resize (SymMV (SUndefined, 0, 1)) (hn - lb) (ln - lb) content)
                memp.contents;
            bounds = PMap.add loc (hn, ln) memp.bounds;
          }

  let free mem loc low high =
    if not (range_perm mem loc low high Freeable) then
      failwith
        (Format.asprintf
           "Range between %i and %i at location %s is not freeable" low high
           loc)
    else rem mem loc low high

  let loadbytes mem loc_name ofs size =
    if range_perm mem loc_name ofs (ofs + size) Readable then
      let* low = get_low_bound_opt mem loc_name in
      Some (getN size (ofs - low) (PMap.find loc_name mem.contents))
    else None

  let storebytes mem loc_name ofs bytes =
    if range_perm mem loc_name ofs (ofs + List.length bytes) Writable then
      let* low = get_low_bound_opt mem loc_name in
      let arr = setN bytes (ofs - low) (PMap.find loc_name mem.contents) in
      let contents = PMap.add loc_name arr mem.contents in
      Some { mem with contents }
    else None

  let move mem ~pfs ~gamma loc_1 ofs_1 loc_2 ofs_2 size =
    let* loc_name_1 = get_loc_name pfs gamma loc_1 in
    let* loc_name_2 = get_loc_name pfs gamma loc_2 in
    let* memvals = loadbytes mem loc_name_2 ofs_2 size in
    let* mem' = storebytes mem loc_name_1 ofs_1 memvals in
    Some (mem', loc_name_1, ofs_1)

  let drop_perm mem loc ~pfs ~gamma low high perm =
    let loc_name =
      match get_loc_name pfs gamma loc with
      | None    ->
          failwith
            (Format.asprintf "Drop_Perm : %a is not a location" Expr.pp loc)
      | Some ln -> ln
    in
    let drop_one i p =
      if low <= i && i < high && perm_leq_opt perm p then Some perm
      else failwith "Invalid drop_perm"
    in
    let old_perms_for_loc = PMap.find loc_name mem.perms in
    let new_perms_for_loc = Array.mapi drop_one old_perms_for_loc in
    let perms = PMap.add loc_name new_perms_for_loc mem.perms in
    { mem with perms }

  let merge_locs old_loc new_loc mem =
    let ret_ops =
      match
        ( find_opt new_loc mem.bounds,
          find_opt new_loc mem.contents,
          find_opt new_loc mem.perms )
      with
      | None, None, None ->
          Logging.verbose (fun fmt -> fmt "New location does not exist");
          let* old_low, old_high = find_opt old_loc mem.bounds in
          let* old_contents = find_opt old_loc mem.contents in
          let* old_perms = find_opt old_loc mem.perms in
          Some (old_low, old_high, old_contents, old_perms)
      | Some (new_low, new_high), Some new_content, Some new_perms ->
          let* old_low, old_high = find_opt old_loc mem.bounds in
          let* old_contents = find_opt old_loc mem.contents in
          let* old_perms = find_opt old_loc mem.perms in
          let def_low = min old_low new_low in
          let def_high = max old_high new_high in
          let def_contents =
            Array.make (def_high - def_low) (SymMV (SUndefined, 0, 1))
          in
          let def_perms = Array.make (def_high - def_low) None in
          let () =
            Array.blit old_contents 0 def_contents (old_low - def_low)
              (old_high - old_low)
          in
          let () =
            Array.blit old_perms 0 def_perms (old_low - def_low)
              (old_high - old_low)
          in
          let () =
            Array.blit new_content 0 def_contents (new_low - def_low)
              (new_high - new_low)
          in
          let () =
            Array.blit new_perms 0 def_perms (new_low - def_low)
              (new_high - new_low)
          in
          Some (def_low, def_high, def_contents, def_perms)
      | _ -> None
    in
    match ret_ops with
    | None              ->
        Logging.verbose (fun fmt -> fmt "Warning: Unable to merge");
        mem (* The old location or new location wasn't found *)
    | Some (l, h, c, p) ->
        {
          bounds = PMap.add new_loc (l, h) (PMap.remove old_loc mem.bounds);
          contents = PMap.add new_loc c (PMap.remove old_loc mem.contents);
          perms = PMap.add new_loc p (PMap.remove old_loc mem.perms);
        }

  let substitution subst mem =
    if not (Subst.domain subst None = SS.empty) then (
      (* The substitution is not empty *)
      let aloc_subst =
        Subst.filter subst (fun var _ -> GUtils.Names.is_aloc_name var)
      in
      Logging.verbose (fun fmt -> fmt "Aloc subst:\n%a" Subst.pp aloc_subst);
      let le_subst = Subst.subst_in_expr subst ~partial:true in
      let sval_subst sv =
        let open SVal in
        match sv with
        | SVint v          -> SVint (le_subst v)
        | SVfloat v        -> SVfloat (le_subst v)
        | SVlong v         -> SVlong (le_subst v)
        | SVsingle v       -> SVsingle (le_subst v)
        | SUndefined       -> SUndefined
        | Sptr (loc, offs) -> (
            match Subst.get aloc_subst loc with
            | Some (ALoc nloc) | Some (Lit (Loc nloc)) ->
                Sptr (nloc, le_subst offs)
            | Some nloc ->
                failwith
                  (Format.asprintf "Heap substitution fail for loc: %a" Expr.pp
                     nloc)
            | None -> Sptr (loc, le_subst offs) )
      in
      let md_subst (SymMV (v, i, k)) = SymMV (sval_subst v, i, k) in
      (* we first substitute the values *)
      let substitute_array _ arr =
        Array.iteri (fun i v -> arr.(i) <- md_subst v) arr
      in
      let () = PMap.iter substitute_array mem.contents in
      (* Then we substitute the locations *)
      Subst.fold aloc_subst
        (fun old_loc new_loc cmem ->
          Logging.verbose (fun fmt ->
              fmt "Merge locs: %s --> %a" old_loc Expr.pp new_loc);
          let new_loc =
            match new_loc with
            | Lit (Loc loc) | ALoc loc -> loc
            | _                        ->
                failwith
                  (Format.asprintf "Heap substitution failed for loc : %a"
                     Expr.pp new_loc)
          in
          merge_locs old_loc new_loc cmem)
        mem )
    else mem

  (** Other utils *)
  let lvars mem =
    let lvar_sm (SymMV (sval, _, _)) =
      match sval with
      | Sptr (_, e) | SVint e | SVlong e | SVsingle e | SVfloat e ->
          Expr.lvars e
      | _ -> SS.empty
    in
    let lvars_sm_a sm_a =
      Array.fold_left (fun s sm -> SS.union (lvar_sm sm) s) SS.empty sm_a
    in
    PMap.fold (fun sma s -> SS.union (lvars_sm_a sma) s) mem.contents SS.empty

  let assertions ?(exclude = []) mem =
    let build_asrt loc_name low high sval perm =
      let e_loc =
        if GUtils.Names.is_aloc_name loc_name then Expr.ALoc loc_name
        else Expr.Lit (Loc loc_name)
      in
      let ga_name = LActions.(str_ga (GMem SVal)) in
      let num k = Expr.Lit (Num (float_of_int k)) in
      let p_e =
        Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
      in
      let sv_e, typs = SVal.to_gil_expr sval in
      let t_as = Asrt.Types typs in
      Asrt.Star
        (Asrt.GA (ga_name, [ e_loc; num low; num high ], [ sv_e; p_e ]), t_as)
    in
    let assertions_of_one_loc loc_name (low, high) =
      let* content_of_loc = find_opt loc_name mem.contents in
      let* perms_of_loc = find_opt loc_name mem.perms in
      let rec aux being_built n =
        if n = high then []
        else
          let curr_index = n - low in
          match
            (being_built, content_of_loc.(curr_index), perms_of_loc.(curr_index))
          with
          | None, SymMV (sv, 0, 1), p ->
              build_asrt loc_name n (n + 1) sv p :: aux None (n + 1)
          | None, SymMV (sv, 0, m), p -> aux (Some (sv, m, p)) (n + 1)
          | Some (svb, mb, pb), SymMV (svc, k, mc), pc
            when svb = svc && mb = mc && pb = pc ->
              if k = mc - 1 then
                if Option.is_some pc then
                  build_asrt loc_name (n - k) (n + 1) svc pc :: aux None (n + 1)
                else aux None (n + 1)
              else aux (Some (svb, mb, pb)) (n + 1)
          | _ ->
              (* This might happen while the state is incorrect : an element is cut, and the it is just undefined,
                 or an element is cut but its permissions are None -> it's been just dealocated we don't really care.
                 I'll handle that *)
              failwith
                "Cannot serialize state as assertion, it is not well formed"
      in
      Some (aux None low)
    in
    PMap.foldi
      (fun loc lh ac ->
        if not (List.mem loc exclude) then
          match assertions_of_one_loc loc lh with
          | Some a -> a @ ac
          | None   -> failwith "Error with the memory, should never happen"
        else ac)
      mem.bounds []
end

type t' = { genv : GEnv.t; mem : Mem.t }

type t = t' ref

type action_ret =
  | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
  | AFail of err_t list

let lift_res res =
  match res with
  | Ok a    -> ASucc a
  | Error e -> AFail e

let make_branch ~heap ~rets ?(new_pfs = []) ?(new_gamma = []) () =
  (ref heap, rets, new_pfs, new_gamma)

(* Init *)

let init () = ref { genv = GEnv.empty; mem = Mem.empty }

let copy h = ref { genv = !h.genv; mem = Mem.copy !h.mem }

(* let subst_spec_vars _ _ = () *)

let rec concretize e =
  let open Expr in
  match e with
  | Lit l   -> l
  | EList l -> LList (List.map concretize l)
  | _       ->
      failwith
        (Format.asprintf "param %a should be concrete but isn't" Expr.pp e)

let rec expr_type_binding_to_pfs_gamma etb =
  let f = expr_type_binding_to_pfs_gamma in
  match etb with
  | [] -> ([], [])
  | (Expr.PVar s, t) :: r | (Expr.LVar s, t) :: r ->
      let pfs, gamma = f r in
      (pfs, (s, t) :: gamma)
  | (e, t) :: r ->
      let pfs, gamma = f r in
      let pf =
        FOLogic.Reduction.reduce_formula
          (Formula.Eq (Expr.UnOp (UnOp.TypeOf, e), Lit (Type t)))
      in
      let pfs =
        if pf = Formula.True then pfs
        else if pf = Formula.False then [ Formula.False ]
        else pf :: pfs
      in
      (pfs, gamma)

(* Ungraceful failure *)

let pp_params fmt params =
  let rec aux fmtp = function
    | []     -> ()
    | [ a ]  -> Format.fprintf fmt "%a" Expr.pp a
    | a :: r ->
        Format.fprintf fmt "%a, " Expr.pp a;
        aux fmtp r
  in
  Format.fprintf fmt "[%a]" aux params

let fail_ungracefully act_name params =
  failwith (Format.asprintf "Invalid call to %s : %a" act_name pp_params params)

(* Action execution *)

let execute_alloc heap _pfs _gamma params =
  match params with
  | [ Expr.Lit (Num low); Lit (Num high) ] ->
      let int_low, int_high = (int_of_float low, int_of_float high) in
      let mem, loc = Mem.alloc heap.mem int_low int_high in
      let branch =
        make_branch ~heap:{ heap with mem } ~rets:[ Expr.ALoc loc ] ()
      in
      ASucc [ branch ]
  | _ ->
      failwith "Alloc doesn't support anything other than concrete values yet"

let execute_getcurperm heap pfs gamma params =
  match params with
  | [ loc; Expr.Lit (Num offs) ] ->
      let int_offs = int_of_float offs in
      let perm = Mem.getcurperm heap.mem ~pfs ~gamma loc int_offs in
      let perm_string =
        Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
      in
      let branch = make_branch ~heap ~rets:[ perm_string ] () in
      ASucc [ branch ]
  | _ -> failwith "invalid call to getcurperm"

let execute_drop_perm heap pfs gamma params =
  match params with
  | [ loc; Expr.Lit (Num low); Lit (Num high); Lit (String perm_string) ] ->
      let int_low, int_high = (int_of_float low, int_of_float high) in
      let perm = ValueTranslation.permission_of_string perm_string in
      let mem = Mem.drop_perm heap.mem ~pfs ~gamma loc int_low int_high perm in
      ASucc [ make_branch ~heap:{ heap with mem } ~rets:[] () ]
  | _ -> failwith "invalid call to drop_perm"

let execute_store heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String chunk_name); loc; Lit (Num ofs); value ] ->
      let sym_val, new_pfs = SVal.of_gil_expr_exn ~pfs ~gamma value in
      let chunk = ValueTranslation.chunk_of_string chunk_name in
      let int_ofs = int_of_float ofs in
      let branches =
        let++ mem = Mem.store chunk heap.mem loc int_ofs sym_val in
        [ make_branch ~heap:{ heap with mem } ~rets:[] ~new_pfs () ]
      in
      lift_res branches
  | _ -> failwith "wrong call to execute_store"

let execute_load heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String chunk_name); loc; Lit (Num ofs) ] ->
      let chunk = ValueTranslation.chunk_of_string chunk_name in
      let int_ofs = int_of_float ofs in
      let res =
        let** value = Mem.load chunk heap.mem ~pfs ~gamma loc int_ofs in
        let gil_value, typs = SVal.to_gil_expr value in
        let new_pfs, new_gamma = expr_type_binding_to_pfs_gamma typs in
        Ok [ make_branch ~heap ~rets:[ gil_value ] ~new_pfs ~new_gamma () ]
      in
      lift_res res
  | _ -> failwith "wrong call to execute_load"

let execute_free heap _pfs _gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (Loc loc); Lit (Num low); Lit (Num high) ]
  | [ ALoc loc; Lit (Num low); Lit (Num high) ] -> (
      let int_low, int_high = (int_of_float low, int_of_float high) in
      match Mem.free heap.mem loc int_low int_high with
      | Some mem -> ASucc [ make_branch ~heap:{ heap with mem } ~rets:[] () ]
      | None     -> AFail [] )
  | _ -> failwith "wrong call to execute_free"

let execute_move heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ loc_1; Lit (Num ofs_1); loc_2; Lit (Num ofs_2); Lit (Num size) ] -> (
      let int_ofs_1, int_ofs_2, int_size =
        (int_of_float ofs_1, int_of_float ofs_2, int_of_float size)
      in
      let res =
        Mem.move heap.mem ~pfs ~gamma loc_1 int_ofs_1 loc_2 int_ofs_2 int_size
      in
      match res with
      | Some (mem, res_loc, ofs) ->
          let ofs_float = float_of_int ofs in
          let loc =
            if GUtils.Names.is_aloc_name res_loc then Expr.ALoc res_loc
            else Lit (Loc res_loc)
          in
          ASucc
            [
              make_branch ~heap:{ heap with mem }
                ~rets:[ loc; Lit (Num ofs_float) ]
                ();
            ]
      | None                     -> AFail [] )
  | _ -> failwith "wrong call to execute_move"

let execute_mem_get heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ loc; Lit (Num low); Lit (Num high) ] -> (
      let int_low, int_high = (int_of_float low, int_of_float high) in
      let res = Mem.get heap.mem pfs gamma loc int_low int_high in
      match res with
      | Some (loc_name, sval, perm) ->
          let loc_e =
            if GUtils.Names.is_aloc_name loc_name then ALoc loc_name
            else Lit (Loc loc_name)
          in
          let sval_e, typs = SVal.to_gil_expr sval in
          let new_pfs, new_gamma = expr_type_binding_to_pfs_gamma typs in
          let perm_e =
            Lit (String (ValueTranslation.string_of_permission_opt perm))
          in
          ASucc
            [
              make_branch ~heap
                ~rets:[ loc_e; Lit (Num low); Lit (Num high); sval_e; perm_e ]
                ~new_pfs ~new_gamma ();
            ]
      | None -> AFail [] )
  | _ -> failwith "wrong call to execute_mem_get"

let execute_mem_set heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match List.map (FOLogic.Reduction.reduce_lexpr ~gamma ~pfs) params with
  | [ loc; Lit (Num low); Lit (Num high); sval_e; Lit (String perm_string) ]
    -> (
      let int_low, int_high = (int_of_float low, int_of_float high) in
      let sval, new_pfs_sv = SVal.of_gil_expr_exn ~pfs ~gamma sval_e in
      let perm_opt = ValueTranslation.permission_opt_of_string perm_string in
      let res = Mem.set heap.mem pfs gamma loc int_low int_high sval perm_opt in
      match res with
      | Some (memp, new_pfs) ->
          ASucc
            [
              make_branch ~heap:{ heap with mem = memp } ~rets:[]
                ~new_pfs:(new_pfs @ new_pfs_sv) ();
            ]
      | None                 -> AFail [] )
  | l -> fail_ungracefully "execute_mem_set" l

let execute_mem_rem heap _pfs _gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (Loc loc); Lit (Num low); Lit (Num high) ]
  | [ ALoc loc; Lit (Num low); Lit (Num high) ] -> (
      let int_low, int_high = (int_of_float low, int_of_float high) in
      let res = Mem.rem heap.mem loc int_low int_high in
      match res with
      | Some memp ->
          ASucc [ make_branch ~heap:{ heap with mem = memp } ~rets:[] () ]
      | None      -> AFail [] )
  | _ -> failwith "wrong call to execute_mem_rem"

let execute_genvgetsymbol heap _pfs _gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String symbol) ] ->
      ASucc
        [
          make_branch ~heap
            ~rets:
              [
                Lit (String symbol);
                loc_from_loc_name (GEnv.find_symbol heap.genv symbol);
              ]
            ();
        ]
  | _                       -> failwith "invalid call to genvgetsymbol"

let execute_genvsetsymbol heap pfs gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (String symbol); lvar_loc ] ->
      let new_pfs, loc_name = resolve_or_create_loc_name pfs gamma lvar_loc in
      let genv = GEnv.set_symbol heap.genv symbol loc_name in
      ASucc [ make_branch ~heap:{ heap with genv } ~new_pfs ~rets:[] () ]
  | _ -> failwith "invalid call to genvsetsymbol"

let execute_genvremsymbol heap _pfs _gamma params =
  match params with
  | [ _symbolc ] -> ASucc [ make_branch ~heap ~rets:[] () ]
  | _            -> failwith "invalid call genvremsymbol"

let execute_genvgetdef heap _pfs _gamma params =
  let open Gillian.Gil_syntax.Expr in
  match params with
  | [ Lit (Loc loc) ] | [ ALoc loc ] ->
      let def = GEnv.find_def heap.genv loc in
      let v = GEnv.serialize_def def in
      ASucc [ make_branch ~heap ~rets:[ Lit (Loc loc); Lit v ] () ]
  | _ -> failwith "invalid call to genvgetdef"

let execute_genvsetdef heap pfs gamma params =
  match params with
  | [ lvar_loc; v_def ] ->
      let new_pfs, loc_name = resolve_or_create_loc_name pfs gamma lvar_loc in
      let concrete_def = concretize v_def in
      let def = GEnv.deserialize_def concrete_def in
      let genv = GEnv.set_def heap.genv loc_name def in
      ASucc [ make_branch ~heap:{ heap with genv } ~new_pfs ~rets:[] () ]
  | _                   -> failwith "invalid call to genvsetdef"

let execute_genvremdef heap _pfs _gamma params =
  match params with
  | [ _loc ] -> ASucc [ make_branch ~heap ~rets:[] () ]
  | _        -> failwith "invalid call to genvremdef"

(* Complete fixes  *)

type c_fix_t = CFSValMem of (string * int * SVal.t * int)

(* Pretty printing utils *)

let str_fill_type ft =
  match ft with
  | PTR     -> "PTR"
  | INT     -> "INT"
  | FLOAT32 -> "FLOAT32"
  | FLOAT64 -> "FLOAT64"

let pp_i_fix fmt i_fix =
  match i_fix with
  | IFSvalMem (loc, ofs, fillt) ->
      Format.fprintf fmt "IFSvalMem(%a, %i, %s)" Expr.pp loc ofs
        (str_fill_type fillt)

(* let str_of_i_fix i_f = Format.asprintf "%a" pp_i_fix i_f *)

let pp_c_fix fmt c_fix =
  match c_fix with
  | CFSValMem (loc, ofs, sv, size) ->
      Format.fprintf fmt "CFSvalMem(%s, %i, %a, %i)" loc ofs SVal.pp sv size

(* let str_of_c_fix c_f = Format.asprintf "%a" pp_c_fix c_f *)

let pp_err fmt e =
  let open Pretty_utils in
  let { failing_constraint; recovery_values; fixes } = e in
  Format.fprintf fmt "@[<v 2>{ fc : %a;@\nrvs : [ %a ];@\nfixes : @[%a@]@] }"
    Formula.pp failing_constraint (pp_list Expr.pp) recovery_values
    (pp_list ~sep:(format_of_string ",@ ") ~pre:(format_of_string "@[[")
       ~suf:(format_of_string "]@]")
       (pp_list ~pre:(format_of_string "@[[") ~suf:(format_of_string "]@]")
          pp_i_fix))
    fixes

(* let str_of_err e = Format.asprintf "%a" pp_err e *)

let pp_a_ret fmt a_ret =
  let open Pretty_utils in
  let pp_succ_branch fmtp (_, vs, _, _) =
    Format.fprintf fmtp "Returning Values : [ %a ]" (pp_list Expr.pp) vs
  in
  match a_ret with
  | ASucc branches ->
      Format.fprintf fmt "@[<v 2>SUCCESS with %i branches :@\n%a@]"
        (List.length branches)
        (pp_list ~sep:(format_of_string ";;@\n- ") pp_succ_branch)
        branches
  | AFail errors   ->
      Format.fprintf fmt "@[<v 2>FAILURE with %i errors :@\n%a@]"
        (List.length errors)
        (pp_list ~sep:(format_of_string ";;@\n- ") pp_err)
        errors

let pp fmt h =
  Format.fprintf fmt "GEnv : @[%a@]@\nMem  : @[%a@]" GEnv.pp !h.genv
    (Mem.pp ~genv:!h.genv) !h.mem

(* let str_noheap _ = "NO HEAP PRINTED" *)

(* Actual action execution *)

let execute_action ac_name heap pfs gamma params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" ac_name pp_params params);
  let open LActions in
  let a_ret =
    match ac_from_str ac_name with
    | AMem Alloc      -> execute_alloc !heap pfs gamma params
    | AMem GetCurPerm -> execute_getcurperm !heap pfs gamma params
    | AMem DropPerm   -> execute_drop_perm !heap pfs gamma params
    | AMem Store      -> execute_store !heap pfs gamma params
    | AMem Load       -> execute_load !heap pfs gamma params
    | AMem Free       -> execute_free !heap pfs gamma params
    | AMem Move       -> execute_move !heap pfs gamma params
    | AMem MGet       -> execute_mem_get !heap pfs gamma params
    | AMem MSet       -> execute_mem_set !heap pfs gamma params
    | AMem MRem       -> execute_mem_rem !heap pfs gamma params
    | AGEnv GetSymbol -> execute_genvgetsymbol !heap pfs gamma params
    | AGEnv SetSymbol -> execute_genvsetsymbol !heap pfs gamma params
    | AGEnv RemSymbol -> execute_genvremsymbol !heap pfs gamma params
    | AGEnv GetDef    -> execute_genvgetdef !heap pfs gamma params
    | AGEnv SetDef    -> execute_genvsetdef !heap pfs gamma params
    | AGEnv RemDef    -> execute_genvremdef !heap pfs gamma params
  in
  let () =
    Logging.verbose (fun fmt ->
        fmt
          "--------------------@\n\
           RETURNING FROM ACTION@\n\
           %a@\n\
           --------------------@\n"
          pp_a_ret a_ret)
  in
  a_ret

(* LActions static *)

let ga_to_setter = LActions.ga_to_setter_str

let ga_to_getter = LActions.ga_to_getter_str

let ga_to_deleter = LActions.ga_to_deleter_str

(* Serialization and operations *)
let substitution_in_place subst heap =
  let { mem; genv } = !heap in
  let nmem = Mem.substitution subst mem in
  let ngenv = GEnv.substitution subst genv in
  heap := { mem = nmem; genv = ngenv }

let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up _ = ()

let lvars heap = Mem.lvars !heap.mem

let assertions ?to_keep:_ heap =
  let genv_locs, genv_asrts = GEnv.assertions !heap.genv in
  let mem_asrts = Mem.assertions ~exclude:genv_locs !heap.mem in
  genv_asrts @ mem_asrts

let mem_constraints _heap = []

let is_overlapping_asrt = LActions.is_overlapping_asrt_str

let ga_loc_indexes = LActions.ga_loc_indexes_str

(** Things defined for BiAbduction *)

let get_recovery_vals e = e.recovery_values

let get_failing_constraint e = e.failing_constraint

let get_fixes ?simple_fix:_ _heap _pfs _gamma err =
  let fixes = err.fixes in
  match fixes with
  | [ [ IFSvalMem (loc, ofs, typ) ] ] ->
      let aloc, new_pfs, new_spec_vars =
        match loc with
        | LVar loclv ->
            let aloc = ALoc.alloc () in
            let new_pf = Formula.Eq (LVar loclv, Expr.ALoc aloc) in
            (aloc, [ new_pf ], SS.singleton loclv)
        | ALoc aloc  -> (aloc, [], SS.empty)
        | _          ->
            failwith
              "Invalid fix, location should be a logic variable or abstract \
               location"
      in
      let sval_possibilities =
        match typ with
        | FLOAT32 ->
            let value = LVar.alloc () in
            let type_value = Asrt.Types [ (LVar value, NumberType) ] in
            let sval = SVal.SVfloat (LVar value) in
            [ ([ type_value ], SS.singleton value, sval, 4) ]
        | FLOAT64 ->
            let value = LVar.alloc () in
            let type_value = Asrt.Types [ (LVar value, NumberType) ] in
            let sval = SVal.SVfloat (LVar value) in
            [ ([ type_value ], SS.singleton value, sval, 8) ]
        | INT     ->
            let value = LVar.alloc () in
            let type_value = Asrt.Types [ (LVar value, NumberType) ] in
            let sval = SVal.SVint (LVar value) in
            [ ([ type_value ], SS.singleton value, sval, 4) ]
        | PTR     ->
            let offset_target = LVar.alloc () in
            let aloc_target = ALoc.alloc () in
            let type_offset = Asrt.Types [ (LVar offset_target, NumberType) ] in
            let sval_ptr = SVal.Sptr (aloc_target, LVar offset_target) in
            let size_ptr =
              if Compcert.Archi.ptr64 then 8
              else failwith "Bi-abduction is only implemented in Archi 64"
            in
            [
              ([ type_offset ], SS.singleton offset_target, sval_ptr, size_ptr);
              ([], SS.empty, SVal.SVlong (Lit (Num 0.)), size_ptr);
            ]
      in
      let build_with_sval (asrts, vars, sv, size) =
        ( [ CFSValMem (aloc, ofs, sv, size) ],
          new_pfs,
          SS.union new_spec_vars vars,
          asrts )
      in
      List.map build_with_sval sval_possibilities
  | _ -> failwith "unhandled amount of fixes"

let apply_fix heap pfs gamma fix =
  match fix with
  | CFSValMem (loc_name, ofs, svl, size) -> (
      let loc = Expr.loc_from_loc_name loc_name in
      let res_mem =
        Mem.set !heap.mem pfs gamma loc ofs (ofs + size) svl (Some Freeable)
      in
      match res_mem with
      | Some (memp, []) ->
          heap := { !heap with mem = memp };
          heap
      | _               -> failwith
                             (Format.asprintf "Invalid complete fix : %a"
                                pp_c_fix fix) )
