open Gil_syntax

let setup () = failwith "TODO"
 
type typenv = (string, Type.t) Hashtbl.t
type model = unit  (* TODO *)

let check_sat_core (fs : Formula.Set.t) (gamma : typenv) : model =
  ignore (fs, gamma);
  failwith "TODO"

