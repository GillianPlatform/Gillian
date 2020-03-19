module GUtils = Gillian.Utils
module PMap = GUtils.PMap
module Result = Stdlib.Result

let ( let* ) = Option.bind

let ( let+ ) o f = Option.map f o

module LActions = struct
  (* First, type definitions *)

  type mem_ac =
    | Alloc
    | DropPerm
    | GetCurPerm
    | Store
    | Load
    | Free
    | Move
    | MGet
    | MSet
    | MRem

  type genv_ac = GetSymbol | SetSymbol | RemSymbol | GetDef | SetDef | RemDef

  type glob_ac = GetFun | SetFun | RemFun | SetVar

  type ac = AGEnv of genv_ac | AMem of mem_ac | AGlob of glob_ac

  type mem_ga = SVal

  type glob_ga = Fun

  type genv_ga = Symbol | Definition

  type ga = GMem of mem_ga | GGlob of glob_ga | GGenv of genv_ga

  (* Some things about the semantics of these Actions *)

  let is_overlapping_asrt = function
    | GGenv _ -> true
    | _       -> false

  let mem_ga_to_setter = function
    | SVal -> MSet

  let mem_ga_to_getter = function
    | SVal -> MGet

  let mem_ga_to_deleter = function
    | SVal -> MRem

  let glob_ga_to_setter = function
    | Fun -> SetFun

  let glob_ga_to_getter = function
    | Fun -> GetFun

  let glob_ga_to_deleter = function
    | Fun -> RemFun

  let genv_ga_to_getter = function
    | Definition -> GetDef
    | Symbol     -> GetSymbol

  let genv_ga_to_setter = function
    | Definition -> SetDef
    | Symbol     -> SetSymbol

  let genv_ga_to_deleter = function
    | Definition -> RemDef
    | Symbol     -> RemSymbol

  let make_map_act tr_mem tr_glob tr_genv = function
    | GMem mga  -> AMem (tr_mem mga)
    | GGlob gga -> AGlob (tr_glob gga)
    | GGenv gge -> AGEnv (tr_genv gge)

  let ga_to_getter =
    make_map_act mem_ga_to_getter glob_ga_to_getter genv_ga_to_getter

  let ga_to_setter =
    make_map_act mem_ga_to_setter glob_ga_to_setter genv_ga_to_setter

  let ga_to_deleter =
    make_map_act mem_ga_to_deleter glob_ga_to_deleter genv_ga_to_deleter

  (* Then serialization and deserialization functions *)

  let mem_prefix = "mem"

  let genv_prefix = "genv"

  let glob_prefix = "glob"

  let str_mem_ac = function
    | Alloc      -> "alloc"
    | DropPerm   -> "dropperm"
    | GetCurPerm -> "getperm"
    | Store      -> "store"
    | Load       -> "load"
    | Move       -> "move"
    | Free       -> "free"
    | MGet       -> "get"
    | MSet       -> "set"
    | MRem       -> "rem"

  let mem_ac_from_str = function
    | "alloc"      -> Alloc
    | "dropperm"   -> DropPerm
    | "getcurperm" -> GetCurPerm
    | "store"      -> Store
    | "load"       -> Load
    | "free"       -> Free
    | "move"       -> Move
    | "get"        -> MGet
    | "set"        -> MSet
    | "rem"        -> MRem
    | s            -> failwith ("Unkown Memory Action : " ^ s)

  let str_genv_ac = function
    | GetSymbol -> "getsymbol"
    | SetSymbol -> "setsymbol"
    | RemSymbol -> "remsymbol"
    | GetDef    -> "getdef"
    | SetDef    -> "setdef"
    | RemDef    -> "remdef"

  let genv_ac_from_str = function
    | "getsymbol" -> GetSymbol
    | "setsymbol" -> SetSymbol
    | "remsymbol" -> RemSymbol
    | "getdef"    -> GetDef
    | "setdef"    -> SetDef
    | "remdef"    -> RemDef
    | s           -> failwith ("Unkown Global Env Action : " ^ s)

  let str_glob_ac = function
    | GetFun -> "getfun"
    | SetFun -> "setfun"
    | RemFun -> "remfun"
    | SetVar -> "setvar"

  let glob_ac_from_str = function
    | "getfun" -> GetFun
    | "setfun" -> SetFun
    | "remfun" -> RemFun
    | "setvar" -> SetVar
    | s        -> failwith ("Unkown Global Action : " ^ s)

  let separator_char = '_'

  let separator_string = String.make 1 separator_char

  let str_ac = function
    | AMem mem_ac   -> mem_prefix ^ separator_string ^ str_mem_ac mem_ac
    | AGEnv genv_ac -> genv_prefix ^ separator_string ^ str_genv_ac genv_ac
    | AGlob glob_ac -> glob_prefix ^ separator_string ^ str_glob_ac glob_ac

  let ac_from_str str =
    match String.split_on_char separator_char str with
    | [ pref; ac ] when String.equal pref mem_prefix ->
        AMem (mem_ac_from_str ac)
    | [ pref; ac ] when String.equal pref genv_prefix ->
        AGEnv (genv_ac_from_str ac)
    | [ pref; ac ] when String.equal pref glob_prefix ->
        AGlob (glob_ac_from_str ac)
    | _ -> failwith ("Unkown action : " ^ str)

  let str_mem_ga = function
    | SVal -> "sval"

  let str_glob_ga = function
    | Fun -> "fun"

  let str_genv_ga = function
    | Definition -> "def"
    | Symbol     -> "symb"

  let mem_ga_from_str = function
    | "sval" -> SVal
    | str    -> failwith ("Unkown memory assertion : " ^ str)

  let glob_ga_from_str = function
    | "fun" -> Fun
    | str   -> failwith ("Unkown global assertion : " ^ str)

  let genv_ga_from_str = function
    | "symb" -> Symbol
    | "def"  -> Definition
    | str    -> failwith ("Unkown global assertion : " ^ str)

  let str_ga = function
    | GMem mem_ga   -> mem_prefix ^ separator_string ^ str_mem_ga mem_ga
    | GGlob glob_ga -> glob_prefix ^ separator_string ^ str_glob_ga glob_ga
    | GGenv genv_ga -> genv_prefix ^ separator_string ^ str_genv_ga genv_ga

  let ga_from_str str =
    match String.split_on_char separator_char str with
    | [ pref; ga ] when String.equal pref mem_prefix ->
        GMem (mem_ga_from_str ga)
    | [ pref; ga ] when String.equal pref glob_prefix ->
        GGlob (glob_ga_from_str ga)
    | [ pref; ga ] when String.equal pref genv_prefix ->
        GGenv (genv_ga_from_str ga)
    | _ -> failwith ("Unkown GA : " ^ str)

  let ga_to_action_str action str = ga_from_str str |> action |> str_ac

  let ga_to_setter_str = ga_to_action_str ga_to_setter

  let ga_to_getter_str = ga_to_action_str ga_to_getter

  let ga_to_deleter_str = ga_to_action_str ga_to_deleter

  (** Additional stuff *)

  let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt

  let ga_loc_indexes ga =
    match ga with
    | GGlob Fun        -> []
    | GMem SVal        -> [ 0 ]
    | GGenv Definition -> [ 0 ]
    | GGenv Symbol     -> []

  let ga_loc_indexes_str ga_str = ga_from_str ga_str |> ga_loc_indexes
end

module GEnv = struct
  open Gillian.Concrete

  type init_data =
    | Init_int8    of int
    | Init_int16   of int
    | Init_int32   of int
    | Init_int64   of int
    | Init_float32 of float
    | Init_float64 of float
    | Init_space   of int
    | Init_addrof  of string * int

  let init_data_size = function
    | Init_int8 _    -> 1
    | Init_int16 _   -> 2
    | Init_int32 _   -> 4
    | Init_int64 _   -> 8
    | Init_float32 _ -> 4
    | Init_float64 _ -> 8
    | Init_addrof _  -> if Compcert.Archi.ptr64 then 8 else 4
    | Init_space n   -> max 0 n

  let init_data_of_gil init_data =
    let open Gillian.Gil_syntax.Literal in
    match init_data with
    | LList [ String "int8"; Num n ] -> Init_int8 (int_of_float n)
    | LList [ String "int16"; Num n ] -> Init_int16 (int_of_float n)
    | LList [ String "int32"; Num n ] -> Init_int32 (int_of_float n)
    | LList [ String "int64"; Num n ] -> Init_int64 (int_of_float n)
    | LList [ String "float32"; Num n ] -> Init_float32 n
    | LList [ String "float64"; Num n ] -> Init_float64 n
    | LList [ String "space"; Num n ] -> Init_space (int_of_float n)
    | LList [ String "addrof"; String sym; Num ofs ] ->
        Init_addrof (sym, int_of_float ofs)
    | _ ->
        failwith
          (Format.asprintf "Invalid init_data, can't be parsed : %a" pp
             init_data)

  type def = FunDef of string | GlobVar of string

  type t = {
    symb : (string, string) PMap.t;  (** maps symbols to loc names *)
    defs : (string, def) PMap.t;  (** maps loc names to definitions *)
  }

  let find_opt x s = try Some (PMap.find x s) with Not_found -> None

  let find_symbol genv sym =
    try PMap.find sym genv.symb
    with Not_found -> failwith ("Can't find symbol " ^ sym ^ " !")

  let set_symbol genv sym block =
    let symb = PMap.add sym block genv.symb in
    { genv with symb }

  let find_def genv block = PMap.find block genv.defs

  let set_def genv block def =
    let defs = PMap.add block def genv.defs in
    { genv with defs }

  let find_def_from_symbol genv sym = find_def genv (find_symbol genv sym)

  let rem_symbol_and_def genv sym =
    let loc_name = find_symbol genv sym in
    let symb = PMap.remove sym genv.symb in
    let defs = PMap.remove loc_name genv.defs in
    { symb; defs }

  let empty = { symb = PMap.empty; defs = PMap.empty }

  (** Serialization of definitions *)
  let serialize_def def =
    let open Gillian.Gil_syntax.Literal in
    match def with
    | FunDef fname  -> LList [ String "function"; String fname ]
    | GlobVar vname -> LList [ String "variable"; String vname ]

  let deserialize_def sdef =
    let open Gillian.Gil_syntax.Literal in
    match sdef with
    | LList [ String "function"; String fname ] -> FunDef fname
    | LList [ String "variable"; String vname ] -> GlobVar vname
    | _ ->
        failwith
          (Format.asprintf "Invalid global definition : %a" Values.pp sdef)

  (* Pretty printing *)

  let pp_def fmt def =
    match def with
    | FunDef f  -> Format.fprintf fmt "(Function %s)" f
    | GlobVar v -> Format.fprintf fmt "(Variable %s)" v

  let pp fmt genv =
    let rec aux not_printed vlist =
      match vlist with
      | []     ->
          Format.fprintf fmt
            "There are %i unimplemented external functions@]@\n" not_printed
      | s :: r ->
          let new_not_printed =
            try
              let l = find_symbol genv s in
              let d = find_def genv l in
              match d with
              | FunDef f
                when String.equal f
                       CConstants.Internal_Functions.not_implemented ->
                  not_printed + 1
              | _ ->
                  let () =
                    Format.fprintf fmt "'%s' -> %s -> %a@\n" s l pp_def d
                  in
                  not_printed
            with Not_found ->
              let () = Format.fprintf fmt "Error unkown symbol %s@\n" s in
              not_printed
          in
          aux new_not_printed r
    in
    if !Config.hide_genv then Format.fprintf fmt "{@[<v 2>@\nHIDDEN@]@\n}"
    else
      let () = Format.fprintf fmt "{@[<v 2>@\n" in
      let keys = List.rev (PMap.foldi (fun k _ l -> k :: l) genv.symb []) in
      let () = aux 0 keys in
      Format.fprintf fmt "}"

  let substitution subst genv =
    let open Gillian.Gil_syntax in
    let open Gillian.Symbolic in
    let aloc_subst =
      Subst.filter subst (fun var _ -> GUtils.Names.is_aloc_name var)
    in
    let rename old_loc new_loc pmap =
      match find_opt old_loc pmap with
      | Some target -> PMap.add new_loc target (PMap.remove old_loc pmap)
      | None        -> pmap
    in
    (* Then we substitute the locations *)
    Subst.fold aloc_subst
      (fun old_loc new_loc cgenv ->
        let new_loc =
          match new_loc with
          | Lit (Loc loc) | ALoc loc -> loc
          | _                        ->
              failwith
                (Format.asprintf "Heap substitution failed for loc : %a" Expr.pp
                   new_loc)
        in
        {
          symb = rename old_loc new_loc cgenv.symb;
          defs = rename old_loc new_loc cgenv.defs;
        })
      genv

  let assertions genv =
    let open Gillian.Gil_syntax in
    let build_asrt s def =
      match def with
      | FunDef _ as d ->
          let d_ser = Expr.Lit (serialize_def d) in
          let s_ser = Expr.Lit (String s) in
          let ga_name = LActions.(str_ga (GGlob Fun)) in
          Asrt.GA (ga_name, [ s_ser ], [ d_ser ])
      | GlobVar _     -> failwith "CANNOT MAKE ASSERTION OF GLOBAL VAR YET"
    in
    let assert_symb symb loc =
      let def = find_def genv loc in
      let asrt = build_asrt symb def in
      asrt
    in
    let list_of_loc_asrt_pairs =
      PMap.foldi
        (fun sym loc lis -> (loc, assert_symb sym loc) :: lis)
        genv.symb []
    in
    List.split list_of_loc_asrt_pairs
end

module CMemory = struct
  open Gillian.Concrete
  module Mem = Compcert.Memory.Mem
  module Literal = Gillian.Gil_syntax.Literal

  type vt = Values.t

  type st = Subst.t

  type fix_t = unit

  type err_t = unit

  let pp_err _ () = ()

  type t = { mem : Compcert.Memory.Mem.mem; genv : GEnv.t }

  let empty = { mem = Mem.empty; genv = GEnv.empty }

  type action_ret = ASucc of (t * vt list) | AFail of err_t list

  let init () = empty

  (** No need to copy immutable data *)
  let copy x = x

  let pp_mem fmt mem =
    let int_of_p = Compcert.Camlcoq.P.to_int in
    let open Compcert.Memory.Mem in
    Format.fprintf fmt "{@[<v 2>@\nnext block: %i@]@\n}"
      (int_of_p mem.nextblock)

  let pp fmt h =
    Format.fprintf fmt "GEnv : @[%a@]@\nMem: @[%a@]" GEnv.pp h.genv pp_mem h.mem

  let ga_to_setter = LActions.ga_to_setter_str

  let ga_to_getter = LActions.ga_to_getter_str

  let ga_to_deleter = LActions.ga_to_deleter_str

  let ga_loc_indexes _ = [ 0 ]

  let store_init_data genv mem loc ofs init_data =
    let block = ValueTranslation.block_of_loc_name loc in
    let i_to_z = Compcert.Camlcoq.Z.of_sint in
    let z_ofs = i_to_z ofs in
    let f32_to_bf = Compcert.Camlcoq.coqfloat32_of_camlfloat in
    let f_to_bf = Compcert.Camlcoq.coqfloat_of_camlfloat in
    let open Compcert.AST in
    let open Compcert.Values in
    match init_data with
    | GEnv.Init_int8 n        ->
        Mem.store Mint8unsigned mem block z_ofs (Vint (i_to_z n))
    | Init_int16 n            -> Mem.store Mint16unsigned mem block z_ofs
                                   (Vint (i_to_z n))
    | Init_int32 n            -> Mem.store Mint32 mem block z_ofs
                                   (Vint (i_to_z n))
    | Init_int64 n            -> Mem.store Mint64 mem block z_ofs
                                   (Vlong (i_to_z n))
    | Init_float32 n          ->
        Mem.store Mfloat32 mem block z_ofs (Vfloat (f32_to_bf n))
    | Init_float64 n          -> Mem.store Mfloat32 mem block z_ofs
                                   (Vfloat (f_to_bf n))
    | Init_space _            -> Some mem
    | Init_addrof (sym, ofsp) ->
        let locp = GEnv.find_symbol genv sym in
        let blockp = ValueTranslation.block_of_loc_name locp in
        let z_ofsp = i_to_z ofsp in
        Mem.store coq_Mptr mem block z_ofs (Vptr (blockp, z_ofsp))

  let rec store_init_data_list genv mem loc ofs id_list =
    match id_list with
    | []             -> Some mem
    | init_data :: r ->
        let* memp = store_init_data genv mem loc ofs init_data in
        store_init_data_list genv memp loc
          (ofs + GEnv.init_data_size init_data)
          r

  let execute_store heap params =
    let open Gillian.Gil_syntax.Literal in
    match params with
    | [ String chunk_name; Loc loc; Num ofs; value ] -> (
        let compcert_val = ValueTranslation.compcert_of_gil value in
        let chunk = ValueTranslation.chunk_of_string chunk_name in
        let block = ValueTranslation.block_of_loc_name loc in
        let z_ofs = Compcert.Camlcoq.Z.of_sint (int_of_float ofs) in
        let res = Mem.store chunk heap.mem block z_ofs compcert_val in
        match res with
        | Some mem -> ASucc ({ heap with mem }, [])
        | None     -> AFail [] )
    | _ -> failwith "wrong call to execute_store"

  let execute_load heap params =
    match params with
    | [ Literal.String chunk_name; Loc loc_name; Num offset ] -> (
        let compcert_chunk = ValueTranslation.chunk_of_string chunk_name in
        let z_offset = ValueTranslation.z_of_float offset in
        let block = ValueTranslation.block_of_loc_name loc_name in
        let res = Mem.load compcert_chunk heap.mem block z_offset in
        match res with
        | Some ret ->
            let ocaml_ret = ValueTranslation.gil_of_compcert ret in
            ASucc (heap, [ ocaml_ret ])
        | None     -> AFail [] )
    | _ -> failwith "invalid call to load"

  let execute_move heap params =
    match params with
    | [ Literal.Loc loc_1; Num ofs_1; Loc loc_2; Num ofs_2; Num size ] -> (
        let block_1, block_2 =
          ValueTranslation.(block_of_loc_name loc_1, block_of_loc_name loc_2)
        in
        let z_ofs_1, z_ofs_2 =
          ValueTranslation.(z_of_float ofs_1, z_of_float ofs_2)
        in
        let z_size = ValueTranslation.z_of_float size in
        match Mem.loadbytes heap.mem block_2 z_ofs_2 z_size with
        | None         -> AFail []
        | Some lmemval -> (
            match Mem.storebytes heap.mem block_1 z_ofs_1 lmemval with
            | None     -> AFail []
            | Some mem -> ASucc ({ heap with mem }, [ Loc loc_1; Num ofs_1 ]) )
        )
    | _ -> failwith "invalid call to move"

  let execute_free heap params =
    match params with
    | [ Literal.Loc loc_name; Num low; Num high ] -> (
        let z_low, z_high =
          ValueTranslation.(z_of_float low, z_of_float high)
        in
        let block = ValueTranslation.block_of_loc_name loc_name in
        let res = Mem.free heap.mem block z_low z_high in
        match res with
        | Some mem -> ASucc ({ heap with mem }, [])
        | None     -> AFail [] )
    | _ -> failwith "invalid call to free"

  let execute_alloc heap params =
    let mem = heap.mem in
    match params with
    | [ Literal.Num low; Literal.Num high ] ->
        let z_low, z_high =
          ValueTranslation.(z_of_float low, z_of_float high)
        in
        let memout, block = Mem.alloc mem z_low z_high in
        let ocaml_block = ValueTranslation.loc_name_of_block block in
        ASucc ({ heap with mem = memout }, [ Literal.Loc ocaml_block ])
    | _ -> failwith "invalid call to alloc"

  let execute_getcurperm heap params =
    let open Gillian.Gil_syntax.Literal in
    match params with
    | [ Loc loc_name; Num offs ] ->
        let z_offs = ValueTranslation.z_of_float offs in
        let block = ValueTranslation.block_of_loc_name loc_name in
        let perm_f = Compcert.Maps.PMap.get block heap.mem.mem_access in
        let perm_opt = perm_f z_offs Compcert.Memtype.Cur in
        let res_ocaml = ValueTranslation.string_of_permission_opt perm_opt in
        ASucc (heap, [ String res_ocaml ])
    | _                          -> failwith "invalid call to getcurperm"

  let execute_drop_perm heap params =
    let open Gillian.Gil_syntax.Literal in
    match params with
    | [ Loc loc_name; Num low; Num high; String perm ] -> (
        let z_low, z_high =
          ValueTranslation.(z_of_float low, z_of_float high)
        in
        let compcert_perm = ValueTranslation.permission_of_string perm in
        let block = ValueTranslation.block_of_loc_name loc_name in
        let res = Mem.drop_perm heap.mem block z_low z_high compcert_perm in
        match res with
        | Some mem -> ASucc ({ heap with mem }, [])
        | None     -> AFail [] )
    | _ -> failwith "invalid call to drop_perm"

  let execute_genvgetsymbol heap params =
    match params with
    | [ Literal.String symbol ] ->
        ASucc (heap, [ String symbol; Loc (GEnv.find_symbol heap.genv symbol) ])
    | _                         -> failwith "invalid call to genvgetsymbol"

  let execute_genvsetsymbol heap params =
    match params with
    | [ Literal.String symbol; Literal.Loc loc ] ->
        let genv = GEnv.set_symbol heap.genv symbol loc in
        ASucc ({ heap with genv }, [])
    | _ -> failwith "invalid call to genvsetsymbol"

  let execute_genvsetdef heap params =
    match params with
    | [ Literal.Loc loc; v_def ] ->
        let def = GEnv.deserialize_def v_def in
        let genv = GEnv.set_def heap.genv loc def in
        ASucc ({ heap with genv }, [])
    | _                          -> failwith "invalid call to genvsetdef"

  let execute_genvgetdef heap params =
    match params with
    | [ Literal.Loc loc ] ->
        let def = GEnv.find_def heap.genv loc in
        let v = GEnv.serialize_def def in
        ASucc (heap, [ Loc loc; v ])
    | _                   -> failwith "invalid call to genvgetdef"

  let execute_globsetfun heap params =
    match params with
    | [ Literal.String symbol; v_def ] -> (
        (* First we allocate in memory *)
        let zero, one = Compcert.Camlcoq.Z.(zero, one) in
        let memp, block = Mem.alloc heap.mem zero one in
        let res_memf =
          Mem.drop_perm memp block zero one Compcert.Memtype.Nonempty
        in
        (* First we set it in the env *)
        let def = GEnv.deserialize_def v_def in
        let loc_name = ValueTranslation.loc_name_of_block block in
        let genvp = GEnv.set_symbol heap.genv symbol loc_name in
        let genvf = GEnv.set_def genvp loc_name def in
        match res_memf with
        | Some memf -> ASucc ({ mem = memf; genv = genvf }, [])
        | None      -> AFail [] )
    | _ -> failwith "invalid call to execute_globsetfun"

  let execute_globsetvar heap params =
    match params with
    | [
     Literal.String symbol;
     v_def;
     Num sz;
     LList init_data_list;
     String permission;
    ] -> (
        (* First we allocate in memory *)
        let zero = Compcert.Camlcoq.Z.zero in
        let comcert_perm = ValueTranslation.permission_of_string permission in
        let init_data_list_des =
          List.map GEnv.init_data_of_gil init_data_list
        in
        let sz_z = ValueTranslation.z_of_float sz in
        let memp, block = Mem.alloc heap.mem zero sz_z in
        let loc_name = ValueTranslation.loc_name_of_block block in
        let res_mempp = Compcert.Globalenvs.store_zeros memp block zero sz_z in
        let res_memppp =
          match res_mempp with
          | Some mempp ->
              store_init_data_list heap.genv mempp loc_name 0 init_data_list_des
          | None       -> None
        in
        let res_memf =
          match res_memppp with
          | Some memppp -> Mem.drop_perm memppp block zero sz_z comcert_perm
          | None        -> None
        in
        (* Then we set it in the env *)
        let def = GEnv.deserialize_def v_def in
        let genvp = GEnv.set_symbol heap.genv symbol loc_name in
        let genvf = GEnv.set_def genvp loc_name def in
        match res_memf with
        | Some memf -> ASucc ({ mem = memf; genv = genvf }, [])
        | None      -> AFail [] )
    | _ -> failwith "invalid call to execute_globsetfun"

  let execute_action name heap params =
    let open LActions in
    let action = ac_from_str name in
    match action with
    | AMem Alloc -> execute_alloc heap params
    | AMem DropPerm -> execute_drop_perm heap params
    | AMem GetCurPerm -> execute_getcurperm heap params
    | AMem Store -> execute_store heap params
    | AMem Load -> execute_load heap params
    | AMem Free -> execute_free heap params
    | AMem Move -> execute_move heap params
    | AGEnv GetSymbol -> execute_genvgetsymbol heap params
    | AGEnv SetSymbol -> execute_genvsetsymbol heap params
    | AGEnv SetDef -> execute_genvsetdef heap params
    | AGEnv GetDef -> execute_genvgetdef heap params
    | AGlob SetFun -> execute_globsetfun heap params
    | AGlob SetVar -> execute_globsetvar heap params
    | AGlob (GetFun | RemFun)
    | AMem (MGet | MSet | MRem)
    | AGEnv (RemDef | RemSymbol) ->
        failwith
          (Printf.sprintf
             "%s is an action related to a General Assertion, it should never \
              be called during a concrete execution"
             name)

  (** Non-implemented functions *)
  let assertions ?to_keep:_ _ =
    raise (Failure "ERROR: to_assertions called for concrete executions")

  let lvars _ =
    raise (Failure "ERROR: get_lvars called for concrete executions")

  let clean_up _ = raise (Failure "Cleanup of concrete state.")

  let fresh_val _ =
    raise (Failure "fresh_val not implemented in concrete state")

  let substitution_in_place _ _ =
    raise (Failure "substitution_in_place not implemented in concrete state")

  let is_overlapping_asrt _ = false
end

module SMemory = struct
  open Gillian.Symbolic
  open Gillian.Gil_syntax
  module Logging = Gillian.Logging
  module SS = GUtils.Containers.SS

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

  module SVal = struct
    type t =
      | SUndefined
      | Sptr       of string * vt
      | SVint      of vt
      | SVlong     of vt
      | SVsingle   of vt
      | SVfloat    of vt

    type typ = Compcert.AST.typ =
      | Tint
      | Tfloat
      | Tlong
      | Tsingle
      | Tany32
      | Tany64

    let tptr = Compcert.AST.coq_Tptr

    let is_loc gamma loc =
      let r_opt =
        let* loc_t = TypEnv.get gamma loc in
        match loc_t with
        | Type.ObjectType -> Some true
        | _               -> Some false
      in
      Option.value ~default:false r_opt

    let is_zero = function
      | SVint (Lit (Num 0.))
      | SVlong (Lit (Num 0.))
      | SVsingle (Lit (Num 0.))
      | SVfloat (Lit (Num 0.)) -> true
      | _ -> false

    let is_loc_ofs gamma loc ofs =
      let r_opt =
        let* loc_t = TypEnv.get gamma loc in
        let* ofs_t = TypEnv.get gamma ofs in
        match (loc_t, ofs_t) with
        | Type.ObjectType, Type.NumberType -> Some true
        | _ -> Some false
      in
      Option.value ~default:false r_opt

    let of_gil_expr_almost_concrete ?(gamma = TypEnv.init ()) gexpr =
      let open Expr in
      let open CConstants.VTypes in
      match gexpr with
      | Lit Undefined -> Some (SUndefined, [])
      | EList [ ALoc loc; offset ] | EList [ Lit (Loc loc); offset ] ->
          Some (Sptr (loc, offset), [])
      | EList [ LVar loc; Lit (Num k) ] when is_loc gamma loc ->
          let aloc = ALoc.alloc () in
          let new_pf = Formula.Eq (LVar loc, Expr.ALoc aloc) in
          Some (Sptr (aloc, Lit (Num k)), [ new_pf ])
      | EList [ LVar loc; LVar ofs ] when is_loc_ofs gamma loc ofs ->
          let aloc = ALoc.alloc () in
          let new_pf = Formula.Eq (LVar loc, Expr.ALoc aloc) in
          Some (Sptr (aloc, LVar ofs), [ new_pf ])
      | EList [ Lit (String typ); value ] when String.equal typ int_type ->
          Some (SVint value, [])
      | EList [ Lit (String typ); value ] when String.equal typ float_type ->
          Some (SVfloat value, [])
      | EList [ Lit (String typ); value ] when String.equal typ single_type ->
          Some (SVsingle value, [])
      | EList [ Lit (String typ); value ] when String.equal typ long_type ->
          Some (SVlong value, [])
      | _ -> None

    let of_gil_expr
        ?(pfs = PureContext.init ()) ?(gamma = TypEnv.init ()) sval_e =
      Logging.verboser (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
      let possible_exprs =
        sval_e :: FOLogic.Reduction.get_equal_expressions pfs sval_e
      in
      List.fold_left
        (fun ac exp ->
          Logging.verboser (fun fmt ->
              fmt "TRYING SUBSTITUTE EXPR : %a" Expr.pp exp);
          match ac with
          | None -> of_gil_expr_almost_concrete ~gamma exp
          | _    -> ac)
        None possible_exprs

    let of_gil_expr_exn
        ?(pfs = PureContext.init ()) ?(gamma = TypEnv.init ()) gexp =
      match of_gil_expr ~pfs ~gamma gexp with
      | Some s -> s
      | None   ->
          failwith
            (Format.asprintf
               "The following expression does not seem to correspond to any \
                compcert value : %a"
               Expr.pp gexp)

    let to_gil_expr gexpr =
      let open Expr in
      let open CConstants.VTypes in
      match gexpr with
      | SUndefined              -> (Lit Undefined, [])
      | Sptr (loc_name, offset) ->
          let loc = loc_from_loc_name loc_name in
          ( EList [ loc; offset ],
            [ (loc, Type.ObjectType); (offset, Type.NumberType) ] )
      | SVint n                 -> ( EList [ Lit (String int_type); n ],
                                     [ (n, Type.NumberType) ] )
      | SVlong n                ->
          (EList [ Lit (String long_type); n ], [ (n, Type.NumberType) ])
      | SVfloat n               ->
          (EList [ Lit (String float_type); n ], [ (n, Type.NumberType) ])
      | SVsingle n              ->
          (EList [ Lit (String single_type); n ], [ (n, Type.NumberType) ])

    let pp fmt v =
      let se = Expr.pp in
      let f = Format.fprintf in
      match v with
      | SUndefined    -> f fmt "undefined"
      | Sptr (l, ofs) -> f fmt "Ptr(%s, %a)" l se ofs
      | SVint i       -> f fmt "Int(%a)" se i
      | SVlong i      -> f fmt "Long(%a)" se i
      | SVfloat i     -> f fmt "Float(%a)" se i
      | SVsingle i    -> f fmt "Single(%a)" se i

    (* let str v = Format.asprintf "%a" pp v *)
  end

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
          Logging.verboser (fun fmt ->
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
        let pp_info =
          PMap.foldi (add_to_pi genv mem) mem.bounds empty_pp_info
        in
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
        Format.fprintf fmt "There are %i allocated functions@\n"
          pp_info.functions;
        Format.fprintf fmt
          "There are %i allocated unimplemented external functions@]@\n}"
          pp_info.unimplemented
    end

    let pp = PrettyPrint.pp

    (* Location resolving *)

    let get_loc_name pfs gamma loc =
      let open FOLogic in
      Logging.tmi (fun fmt -> fmt "get_loc_name: %a" Expr.pp loc);
      let lpfs = PureContext.to_list pfs in
      match Reduction.reduce_lexpr ~pfs ~gamma loc with
      | Lit (Loc loc) | ALoc loc -> Some loc
      | LVar x                   -> (
          match Reduction.resolve_expr_to_location lpfs (LVar x) with
          | Some (loc_name, _) -> Some loc_name
          | _                  -> None )
      | loc'                     -> (
          match Reduction.resolve_expr_to_location lpfs loc' with
          | Some (loc_name, _) -> Some loc_name
          | None               ->
              let msg =
                Format.asprintf "Unsupported location: %a with pfs:\n%a" Expr.pp
                  loc' PureContext.pp pfs
              in
              Logging.verboser (fun fmt -> fmt "%s" msg);
              raise (Failure msg) )

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
          Logging.verboser (fun fmt -> fmt "Found loc_name %s" loc_name);
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
      if low < lb || high > hb || not (range_perm mem loc low high Nonempty)
      then None
      else
        let* memp = set_existing_loc mem loc low high SUndefined None in
        (* then we clean the memory, by looking at what has no permissions *)
        let* content = find_opt loc memp.contents in
        let* perms = find_opt loc memp.perms in
        let rec none_left k =
          if k = hb || Option.is_some perms.(k - lb) then k
          else none_left (k + 1)
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
                  (resize
                     (SymMV (SUndefined, 0, 1))
                     (hn - lb) (ln - lb) content)
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

    let rec store_zeros mem loc p n =
      if n <= 0 then Ok mem
      else
        let** memp =
          store Mint8unsigned mem loc p (SVal.SVint (Lit (Num 0.)))
        in
        store_zeros memp loc (p + 1) (n - 1)

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
            Logging.verboser (fun fmt -> fmt "New location does not exist");
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
          Logging.verboser (fun fmt -> fmt "Warning: Unable to merge");
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
        Logging.verboser (fun fmt -> fmt "Aloc subst:\n%a" Subst.pp aloc_subst);
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
                    (Format.asprintf "Heap substitution fail for loc: %a"
                       Expr.pp nloc)
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
            Logging.verboser (fun fmt ->
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
              ( being_built,
                content_of_loc.(curr_index),
                perms_of_loc.(curr_index) )
            with
            | None, SymMV (sv, 0, 1), p ->
                build_asrt loc_name n (n + 1) sv p :: aux None (n + 1)
            | None, SymMV (sv, 0, m), p -> aux (Some (sv, m, p)) (n + 1)
            | Some (svb, mb, pb), SymMV (svc, k, mc), pc
              when svb = svc && mb = mc && pb = pc ->
                if k = mc - 1 then
                  if Option.is_some pc then
                    build_asrt loc_name (n - k) (n + 1) svc pc
                    :: aux None (n + 1)
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

  (* Memory utils *)

  let store_init_data genv mem loc ofs init_data =
    let num_i i = Expr.Lit (Num (float_of_int i)) in
    let num_f f = Expr.Lit (Num f) in
    match init_data with
    | GEnv.Init_int8 n -> Mem.store Mint8unsigned mem loc ofs (SVint (num_i n))
    | GEnv.Init_int16 n ->
        Mem.store Mint16unsigned mem loc ofs (SVint (num_i n))
    | GEnv.Init_int32 n -> Mem.store Mint32 mem loc ofs (SVint (num_i n))
    | GEnv.Init_int64 n -> Mem.store Mint64 mem loc ofs (SVlong (num_i n))
    | GEnv.Init_float32 n -> Mem.store Mfloat32 mem loc ofs (SVfloat (num_f n))
    | GEnv.Init_float64 n -> Mem.store Mfloat64 mem loc ofs (SVfloat (num_f n))
    | GEnv.Init_space _ -> Ok mem
    | GEnv.Init_addrof (sym, ofsp) ->
        let locp = GEnv.find_symbol genv sym in
        let i_ofsp = num_i ofsp in
        Mem.store Compcert.AST.coq_Mptr mem loc ofs (Sptr (locp, i_ofsp))

  let rec store_init_data_list genv mem loc ofs id_list =
    match id_list with
    | []             -> Ok mem
    | init_data :: r ->
        let** memp = store_init_data genv mem loc ofs init_data in
        store_init_data_list genv memp loc
          (ofs + GEnv.init_data_size init_data)
          r

  (* Gillian utils *)

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
    failwith
      (Format.asprintf "Invalid call to %s : %a" act_name pp_params params)

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
        let mem =
          Mem.drop_perm heap.mem ~pfs ~gamma loc int_low int_high perm
        in
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
        let res =
          Mem.set heap.mem pfs gamma loc int_low int_high sval perm_opt
        in
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

  let execute_genvsetsymbol heap _pfs _gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [ Lit (String symbol); Lit (Loc loc) ] | [ Lit (String symbol); ALoc loc ]
      ->
        let genv = GEnv.set_symbol heap.genv symbol loc in
        ASucc [ make_branch ~heap:{ heap with genv } ~rets:[] () ]
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

  let execute_genvsetdef heap _pfs _gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [ Lit (Loc loc); Expr.Lit v_def ] | [ ALoc loc; Expr.Lit v_def ] ->
        let def = GEnv.deserialize_def v_def in
        let genv = GEnv.set_def heap.genv loc def in
        ASucc [ make_branch ~heap:{ heap with genv } ~rets:[] () ]
    | _ -> failwith "invalid call to genvsetdef"

  let execute_genvremdef heap _pfs _gamma params =
    match params with
    | [ _loc ] -> ASucc [ make_branch ~heap ~rets:[] () ]
    | _        -> failwith "invalid call to genvremdef"

  let execute_globsetfun heap pfs gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [ Lit (String symbol); v_def_e ] ->
        let v_def = concretize v_def_e in
        (* First we allocate in memory *)
        let memp, loc_name = Mem.alloc heap.mem 0 1 in
        let loc = loc_from_loc_name loc_name in
        let memf =
          Mem.drop_perm memp ~pfs ~gamma loc 0 1 Compcert.Memtype.Nonempty
        in
        (* First we set it in the env *)
        let def = GEnv.deserialize_def v_def in
        let genvp = GEnv.set_symbol heap.genv symbol loc_name in
        let genvf = GEnv.set_def genvp loc_name def in
        ASucc [ make_branch ~heap:{ mem = memf; genv = genvf } ~rets:[] () ]
    | _ -> failwith "invalid call to execute_globsetfun"

  let execute_globgetfun heap _pfs _gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [ Lit (String symbol) ] -> (
        try
          let def = GEnv.find_def_from_symbol heap.genv symbol in
          let def_e = Lit (GEnv.serialize_def def) in
          ASucc [ make_branch ~heap ~rets:[ Lit (String symbol); def_e ] () ]
        with Not_found -> AFail [] )
    | _                       -> failwith "invalid call to execute_globgetfun"

  let execute_globremfun heap _pfs _gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [ Lit (String symbol) ] -> (
        try
          let loc_name = GEnv.find_symbol heap.genv symbol in
          let genv = GEnv.rem_symbol_and_def heap.genv symbol in
          let mem_opt = Mem.rem heap.mem loc_name 0 1 in
          match mem_opt with
          | Some mem -> ASucc [ make_branch ~heap:{ mem; genv } ~rets:[] () ]
          | None     -> AFail []
        with Not_found -> AFail [] )
    | _                       -> failwith "invalid call to execute_globremfun"

  let execute_globsetvar heap pfs gamma params =
    let open Gillian.Gil_syntax.Expr in
    match params with
    | [
     Lit (String symbol);
     v_def_e;
     Lit (Num sz);
     init_data_list_e;
     Lit (String permission);
    ] ->
        let v_def, init_data_list =
          (concretize v_def_e, concretize init_data_list_e)
        in
        let init_data_list =
          match init_data_list with
          | Literal.LList l -> l
          | _               ->
              failwith
                (Format.asprintf "init_data_list %a isn't a list !" Literal.pp
                   init_data_list)
        in
        (* First we allocate in memory *)
        let comcert_perm = ValueTranslation.permission_of_string permission in
        let init_data_list_des =
          List.map GEnv.init_data_of_gil init_data_list
        in
        let sz_int = int_of_float sz in
        let memp, loc_name = Mem.alloc heap.mem 0 sz_int in
        let loc = loc_from_loc_name loc_name in
        let res =
          let** mempp = Mem.store_zeros memp loc 0 sz_int in
          let** memppp =
            store_init_data_list heap.genv mempp loc 0 init_data_list_des
          in
          let memf =
            Mem.drop_perm memppp ~pfs ~gamma loc 0 sz_int comcert_perm
          in
          (* Then we set it in the env *)
          let def = GEnv.deserialize_def v_def in
          let genvp = GEnv.set_symbol heap.genv symbol loc_name in
          let genvf = GEnv.set_def genvp loc_name def in
          Ok [ make_branch ~heap:{ mem = memf; genv = genvf } ~rets:[] () ]
        in
        lift_res res
    | _ -> failwith "invalid call to execute_globsetvar"

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
    Logging.verboser (fun fmt ->
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
      | AGlob SetFun    -> execute_globsetfun !heap pfs gamma params
      | AGlob GetFun    -> execute_globgetfun !heap pfs gamma params
      | AGlob RemFun    -> execute_globremfun !heap pfs gamma params
      | AGlob SetVar    -> execute_globsetvar !heap pfs gamma params
    in
    let () =
      Logging.verboser (fun fmt ->
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
              let type_offset =
                Asrt.Types [ (LVar offset_target, NumberType) ]
              in
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
        | _               ->
            failwith (Format.asprintf "Invalid complete fix : %a" pp_c_fix fix)
        )
end
