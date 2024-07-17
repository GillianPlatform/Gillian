open Gillian.Concrete
module Mem = Compcert.Memory.Mem
module Literal = Gillian.Gil_syntax.Literal
module Expr = Gillian.Gil_syntax.Expr

type init_data = Global_env.t
type vt = Values.t
type st = Subst.t
type err_t = unit [@@deriving yojson, show]

let pp_err _ () = ()

type t = { mem : Compcert.Memory.Mem.mem; genv : Global_env.t }
type action_ret = (t * vt list, err_t) result

let init genv = { mem = Mem.empty; genv }
let copy x = x

let pp_mem fmt mem =
  let int_of_p = Compcert.Camlcoq.P.to_int in
  let open Compcert.Memory.Mem in
  Format.fprintf fmt "{@[<v 2>@\nnext block: %i@]@\n}" (int_of_p mem.nextblock)

let pp fmt h = Format.fprintf fmt "@[%a@]" pp_mem h.mem

let execute_store heap params =
  let open Gillian.Gil_syntax.Literal in
  match params with
  | [ String chunk_name; Loc loc; Int ofs; value ] -> (
      let compcert_val = ValueTranslation.compcert_of_gil value in
      let compcert_chunk =
        Chunk.to_compcert (ValueTranslation.chunk_of_string chunk_name)
      in
      let block = ValueTranslation.block_of_loc_name loc in
      let z_ofs = Compcert.Camlcoq.Z.of_sint (Z.to_int ofs) in
      let res = Mem.store compcert_chunk heap.mem block z_ofs compcert_val in
      match res with
      | Some mem -> Ok ({ heap with mem }, [])
      | None -> Error ())
  | _ -> failwith "wrong call to execute_store"

let execute_load heap params =
  match params with
  | [ Literal.String chunk_name; Loc loc_name; Int offset ] -> (
      let compcert_chunk =
        Chunk.to_compcert (ValueTranslation.chunk_of_string chunk_name)
      in
      let z_offset = ValueTranslation.z_of_int offset in
      let block = ValueTranslation.block_of_loc_name loc_name in
      let res = Mem.load compcert_chunk heap.mem block z_offset in
      match res with
      | Some ret ->
          let ocaml_ret = ValueTranslation.gil_of_compcert ret in
          Ok (heap, [ ocaml_ret ])
      | None -> Error ())
  | _ -> failwith "invalid call to load"

let execute_move heap params =
  match params with
  | [ Literal.Loc loc_1; Int ofs_1; Loc loc_2; Int ofs_2; Int size ] -> (
      let block_1, block_2 =
        ValueTranslation.(block_of_loc_name loc_1, block_of_loc_name loc_2)
      in
      let z_ofs_1, z_ofs_2 =
        ValueTranslation.(z_of_int ofs_1, z_of_int ofs_2)
      in
      let z_size = ValueTranslation.z_of_int size in
      match Mem.loadbytes heap.mem block_2 z_ofs_2 z_size with
      | None -> Error ()
      | Some lmemval -> (
          match Mem.storebytes heap.mem block_1 z_ofs_1 lmemval with
          | None -> Error ()
          | Some mem -> Ok ({ heap with mem }, [ Literal.Loc loc_1; Int ofs_1 ])
          ))
  | _ -> failwith "invalid call to move"

let execute_free heap params =
  match params with
  | [ Literal.Loc loc_name; Int low; Int high ] -> (
      let z_low, z_high = ValueTranslation.(z_of_int low, z_of_int high) in
      let block = ValueTranslation.block_of_loc_name loc_name in
      let res = Mem.free heap.mem block z_low z_high in
      match res with
      | Some mem -> Ok ({ heap with mem }, [])
      | None -> Error ())
  | _ -> failwith "invalid call to free"

let execute_alloc heap params =
  let mem = heap.mem in
  match params with
  | [ Literal.Int low; Literal.Int high ] ->
      let z_low, z_high = ValueTranslation.(z_of_int low, z_of_int high) in
      let memout, block = Mem.alloc mem z_low z_high in
      let ocaml_block = ValueTranslation.loc_name_of_block block in
      Ok ({ heap with mem = memout }, [ Literal.Loc ocaml_block ])
  | _ -> failwith "invalid call to alloc"

let execute_weak_valid_pointer heap params =
  let open Gillian.Gil_syntax.Literal in
  match params with
  | [ Loc loc_name; Int offs ] ->
      let z_offs = ValueTranslation.z_of_int offs in
      let block = ValueTranslation.block_of_loc_name loc_name in
      let res = Mem.weak_valid_pointer heap.mem block z_offs in
      Ok (heap, [ Bool res ])
  | _ -> failwith "invalid call to weak_valid_pointer"

let execute_getcurperm heap params =
  let open Gillian.Gil_syntax.Literal in
  match params with
  | [ Loc loc_name; Int offs ] ->
      let z_offs = ValueTranslation.z_of_int offs in
      let block = ValueTranslation.block_of_loc_name loc_name in
      let perm_f = Compcert.Maps.PMap.get block heap.mem.mem_access in
      let perm_opt = perm_f z_offs Compcert.Memtype.Cur in
      let res_ocaml = ValueTranslation.string_of_permission_opt perm_opt in
      Ok (heap, [ String res_ocaml ])
  | _ -> failwith "invalid call to getcurperm"

let execute_drop_perm heap params =
  let open Gillian.Gil_syntax.Literal in
  match params with
  | [ Loc loc_name; Int low; Int high; String perm ] -> (
      let z_low, z_high = ValueTranslation.(z_of_int low, z_of_int high) in
      let compcert_perm = ValueTranslation.permission_of_string perm in
      let block = ValueTranslation.block_of_loc_name loc_name in
      let res = Mem.drop_perm heap.mem block z_low z_high compcert_perm in
      match res with
      | Some mem -> Ok ({ heap with mem }, [])
      | None -> Error ())
  | _ -> failwith "invalid call to drop_perm"

let execute_genvgetdef heap params =
  match params with
  | [ Literal.Loc loc ] ->
      let def = Global_env.find_def heap.genv loc in
      let v = Global_env.serialize_def def in
      Ok (heap, [ Literal.Loc loc; v ])
  | _ -> failwith "invalid call to genvgetdef"

let execute_action name heap params =
  let open LActions in
  let action = ac_from_str name in
  match action with
  | AMem Alloc -> execute_alloc heap params
  | AMem DropPerm -> execute_drop_perm heap params
  | AMem GetCurPerm -> execute_getcurperm heap params
  | AMem WeakValidPointer -> execute_weak_valid_pointer heap params
  | AMem Store -> execute_store heap params
  | AMem Load -> execute_load heap params
  | AMem Free -> execute_free heap params
  | AMem Move -> execute_move heap params
  | AGEnv GetDef -> execute_genvgetdef heap params
  | AMem SetZeros -> failwith "cannot execute set_zeros in concrete memory"
