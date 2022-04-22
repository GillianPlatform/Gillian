(** {3 Variable names} *)

val true_name : Compcert.Camlcoq.P.t -> string

(** {3 Z and floats} *)

val z_of_int : Z.t -> Compcert.Camlcoq.Z.t
val int_of_z : Compcert.Camlcoq.Z.t -> Z.t

(** {3 Memory chunk serialization} *)

val string_of_chunk : Compcert.AST.memory_chunk -> string
val chunk_of_string : string -> Compcert.AST.memory_chunk

(** {3 Value serialization} *)

val compcert_of_gil : Gillian.Gil_syntax.Literal.t -> Compcert.Values.coq_val
val gil_of_compcert : Compcert.Values.coq_val -> Gillian.Gil_syntax.Literal.t

(** {3 Size serialization} *)

val compcert_size_of_gil : Z.t -> Compcert.Camlcoq.Z.t
val gil_size_of_compcert : Compcert.Camlcoq.Z.t -> Z.t

(** {3 Block serialization} *)

val loc_name_of_block : Compcert.Camlcoq.P.t -> string
val block_of_loc_name : string -> Compcert.Camlcoq.P.t

(** {3 Permission serialization} *)

val string_of_permission : Compcert.Memtype.permission -> string
val permission_of_string : string -> Compcert.Memtype.permission
val permission_opt_of_string : string -> Compcert.Memtype.permission option
val string_of_permission_opt : Compcert.Memtype.permission option -> string

(** {3 Freeable blocks} *)

val compcert_block_of_gil :
  Gillian.Gil_syntax.Literal.t ->
  (Compcert.Camlcoq.P.t * Compcert.Camlcoq.Z.t) * Compcert.Camlcoq.Z.t

(** {3 Initialization data} *)

val gil_init_data : Compcert.AST.init_data -> Gillian.Gil_syntax.Literal.t
