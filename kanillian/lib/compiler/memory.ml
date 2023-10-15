open Gil_syntax
module GType = Goto_lib.Type
module Interface = Memory_model.Interface

let rec chunk_for_type ~(ctx : Ctx.t) (t : GType.t) : Chunk.t option =
  let res =
    match t with
    | CInteger I_bool ->
        Chunk.of_int_type ~signed:false ~size:ctx.machine.bool_width
    | CInteger I_char ->
        Chunk.of_int_type
          ~signed:(not ctx.machine.char_is_unsigned)
          ~size:ctx.machine.char_width
    | CInteger I_int ->
        Chunk.of_int_type ~signed:true ~size:ctx.machine.int_width
    | CInteger I_size_t ->
        Chunk.of_int_type ~signed:false ~size:ctx.machine.pointer_width
    | CInteger I_ssize_t ->
        Chunk.of_int_type ~signed:true ~size:ctx.machine.pointer_width
    | Signedbv { width } -> Chunk.of_int_type ~signed:true ~size:width
    | Unsignedbv { width } -> Chunk.of_int_type ~signed:false ~size:width
    | Float -> Some F32
    | Double -> Some F64
    | Pointer _ ->
        Chunk.of_int_type ~signed:false ~size:ctx.machine.pointer_width
    | StructTag t -> chunk_for_type ~ctx (Ctx.tag_lookup ctx t)
    | Struct { components; _ } ->
        Option.bind (Ctx.one_representable_field ctx components) (fun (_, ty) ->
            chunk_for_type ~ctx ty)
    | _ -> None
  in
  res

let ptr_add_e p e =
  let loc = Expr.list_nth p 0 in
  let offset = Expr.list_nth p 1 in
  let open Expr.Infix in
  Expr.EList [ loc; offset + e ]

let ptr_add p i = ptr_add_e p (Expr.int i)

let ptr_offset ~ctx ~ty p e =
  let sty = Expr.int (Ctx.size_of ctx ty) in
  ptr_add_e p (Expr.Infix.( * ) e sty)

(** Allocates the memory with the right size, and
   returns a location expression, addressing the block *)
let alloc ~loc_var ~size : Expr.t * string Cmd.t =
  if size == 0 then Fmt.failwith "OK ALLOCATING SOMETHING OF SIZE ZERO!";
  let alloc = Interface.(str_ac (AMem Alloc)) in
  let cmd = Cmd.LAction (loc_var, alloc, [ Expr.zero_i; Expr.int size ]) in
  let loc = Expr.list_nth (PVar loc_var) 0 in
  (loc, cmd)

(** Allocates the memory with the right size, and
   returns a pointer expression pointing to the
   beginning of the allocated block. *)
let alloc_ptr ~ctx ty : Expr.t * string Cmd.t =
  let size = Ctx.size_of ctx ty in
  let loc, cmd = alloc ~loc_var:(Ctx.fresh_v ctx) ~size in
  let ptr = Expr.EList [ loc; Expr.zero_i ] in
  (ptr, cmd)

let alloc_temp ~ctx ~location ty : Expr.t Cs.with_cmds =
  if Ctx.size_of ctx ty == 0 then
    Fmt.failwith "OK ALLOCATING A TEMP OF SIZE ZERO: %a" GType.pp ty;
  let ptr, alloc_cmd = alloc_ptr ~ctx ty in
  let temp = Ctx.fresh_v ctx in
  let assign = Cmd.Assignment (temp, ptr) in
  let () = Ctx.register_allocated_temp ctx ~name:temp ~type_:ty ~location in
  Cs.return ~app:[ alloc_cmd; assign ] (Expr.PVar temp)

(** Should only be called for a local that is in memory*)
let dealloc_local ~ctx (l : Ctx.Local.t) : Body_item.t =
  if not (Ctx.in_memory ctx l.symbol) then
    Error.code_error "dealloc_local: local is not in memory";
  let free = Interface.(str_ac (AMem Free)) in
  let size = Ctx.size_of ctx l.type_ |> Expr.int in
  let var = Ctx.fresh_v ctx in
  let cmd =
    Cmd.LAction
      (var, free, [ Expr.list_nth (Expr.PVar l.symbol) 0; Expr.zero_i; size ])
  in
  let loc = Body_item.compile_location l.location in
  Body_item.make ~loc cmd

(** Loads a value into the given variable.
    If no variable is given, one is created.
    In any case, the variable containing the value, as well as
    the load command is returned.
    If a variable is given, the returned variable
    is always equal to it *)
let load_scalar ~ctx ?var (e : Expr.t) (t : GType.t) : string Cs.with_cmds =
  match chunk_for_type ~ctx t with
  | None ->
      Error.code_error (Fmt.str "load_scalar - unexpected type: %a" GType.pp t)
  | Some chunk ->
      let chunk = Expr.Lit (String (Chunk.to_string chunk)) in
      let var =
        match var with
        | Some var -> var
        | None -> Ctx.fresh_v ctx
      in
      let loadv = Constants.Internal_functions.loadv in
      let load_cmd =
        Cmd.Call (var, Lit (String loadv), [ chunk; e ], None, None)
      in
      (var, [ load_cmd ])

let store_scalar ~ctx ?var (p : Expr.t) (v : Expr.t) (t : GType.t) :
    string Cmd.t =
  match chunk_for_type ~ctx t with
  | None ->
      Error.code_error (Fmt.str "store_scalar - unexpected type: %a" GType.pp t)
  | Some chunk ->
      let chunk = Expr.Lit (String (Chunk.to_string chunk)) in
      let var =
        match var with
        | Some var -> var
        | None -> Ctx.fresh_v ctx
      in
      let storev = Constants.Internal_functions.storev in
      let store_cmd =
        Cmd.Call (var, Lit (String storev), [ chunk; p; v ], None, None)
      in
      store_cmd

let memcpy ~ctx ~(type_ : GType.t) ~(dst : Expr.t) ~(src : Expr.t) =
  let temp = Ctx.fresh_v ctx in
  let size = Ctx.size_of ctx type_ in
  let memcpy = Constants.Internal_functions.ef_memcpy in
  (* TODO: emit a signal that alignment check is not performed correctly *)
  Cmd.Call
    ( temp,
      Lit (String memcpy),
      [ Expr.int size; Expr.zero_i; dst; src ],
      None,
      None )

let poison ~ctx ~(dst : Expr.t) byte_width =
  let temp = Ctx.fresh_v ctx in
  let size = Expr.int byte_width in
  let loc = Expr.list_nth dst 0 in
  let offset = Expr.list_nth dst 1 in
  let poison = Interface.(str_ac (AMem Poison)) in
  Cmd.LAction (temp, poison, [ loc; offset; size ])

let write_composit
    ~ctx
    ~annot
    ~dst
    (writes : (int * Val_repr.composit_write) Seq.t) =
  let loc = Expr.list_nth dst 0 in
  let base_ofs = Expr.list_nth dst 1 in
  let at_ofs i = Expr.EList [ loc; Expr.Infix.( + ) base_ofs (Expr.int i) ] in
  let rec aux start_ofs writes =
    match writes () with
    | Seq.Nil -> []
    | Cons ((i, Val_repr.Poison { byte_width }), rest) ->
        let curr_ofs = start_ofs + i in
        let cmds = poison ~ctx ~dst:(at_ofs curr_ofs) byte_width in
        annot cmds :: aux start_ofs rest
    | Cons ((i, V { type_ = ty; value = v, body }), rest) ->
        let curr_ofs = start_ofs + i in
        let dst = at_ofs curr_ofs in
        let cmds =
          match v with
          | Val_repr.ByCopy { ptr = src; type_ } ->
              [ annot (memcpy ~ctx ~dst ~src ~type_) ]
          | Val_repr.ByValue e -> [ annot (store_scalar ~ctx dst e ty) ]
          | Val_repr.ByCompositValue { writes; _ } -> aux curr_ofs writes
          | Val_repr.Procedure _ ->
              Error.code_error "Writing a procedure in a composit"
        in
        body @ cmds @ aux start_ofs rest
  in
  aux 0 writes

let write ~ctx ~(type_ : GType.t) ~annot ~(dst : Expr.t) ~(src : Val_repr.t) :
    Body_item.t list =
  if Ctx.is_zst_access ctx type_ then []
  else
    match src with
    | Val_repr.ByCopy { ptr = src; type_ } ->
        [ annot (memcpy ~ctx ~dst ~src ~type_) ]
    | Val_repr.ByValue e -> [ annot (store_scalar ~ctx dst e type_) ]
    | Val_repr.ByCompositValue { writes; _ } ->
        write_composit ~ctx ~annot ~dst writes
    | Val_repr.Procedure _ -> Error.code_error "Writing a procedure"

let object_size ~ctx ~ptr_ty ptr : Expr.t Cs.with_cmds =
  match (ptr_ty : GType.t) with
  | Pointer ty when Ctx.is_zst_access ctx ty -> Cs.return Expr.zero_i
  | Pointer _ ->
      let temp = Ctx.fresh_v ctx in
      let fct = Constants.Unop_functions.object_size in
      let call = Cmd.Call (temp, Lit (String fct), [ ptr ], None, None) in
      Cs.return ~app:[ call ] (Expr.PVar temp)
  | _ -> Error.unexpected "ObjectSize of something that is not a pointer"
