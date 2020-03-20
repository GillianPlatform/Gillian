open Gillian.Concrete
module GUtils = Gillian.Utils
module PMap = GUtils.PMap

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
        (Format.asprintf "Invalid init_data, can't be parsed : %a" pp init_data)

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
      failwith (Format.asprintf "Invalid global definition : %a" Values.pp sdef)

(* Pretty printing *)

let pp_def fmt def =
  match def with
  | FunDef f  -> Format.fprintf fmt "(Function %s)" f
  | GlobVar v -> Format.fprintf fmt "(Variable %s)" v

let pp fmt genv =
  let rec aux not_printed vlist =
    match vlist with
    | []     ->
        Format.fprintf fmt "There are %i unimplemented external functions@]@\n"
          not_printed
    | s :: r ->
        let new_not_printed =
          try
            let l = find_symbol genv s in
            let d = find_def genv l in
            match d with
            | FunDef f
              when String.equal f CConstants.Internal_Functions.not_implemented
              -> not_printed + 1
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
