type t = {
  type_ : Irep.t;
  value : Irep.t;
  location : Irep.t;
  name : string;
  module_ : string;
  base_name : string;
  pretty_name : string;
  mode : string;
  is_type : bool;
  is_macro : bool;
  is_exported : bool;
  is_input : bool;
  is_output : bool;
  is_state_var : bool;
  is_property : bool;
  is_static_lifetime : bool;
  is_thread_local : bool;
  is_lvalue : bool;
  is_file_local : bool;
  is_extern : bool;
  is_volatile : bool;
  is_parameter : bool;
  is_auxiliary : bool;
  is_weak : bool;
}

let of_yojson json =
  let open Kutils in
  let ( $ ) = J.( $ ) in
  let ( $$ ) x y = x $ Id.to_string y in
  let type_ = Irep.of_yojson (json $$ Type) in
  let value = Irep.of_yojson (json $$ Value) in
  let location = Irep.of_yojson (json $ "location") in
  let name = J.to_string (json $$ Name) in
  let module_ = J.to_string (json $$ Module) in
  let base_name = J.to_string (json $ "baseName") in
  let pretty_name = J.to_string (json $ "prettyName") in
  let mode = J.to_string (json $$ Mode) in
  let is_type = J.to_bool (json $ "isType") in
  let is_macro = J.to_bool (json $ "isType") in
  let is_exported = J.to_bool (json $ "isExported") in
  let is_input = J.to_bool (json $ "isInput") in
  let is_output = J.to_bool (json $ "isOutput") in
  let is_state_var = J.to_bool (json $ "isStateVar") in
  let is_property = J.to_bool (json $ "isProperty") in
  let is_static_lifetime = J.to_bool (json $ "isStaticLifetime") in
  let is_thread_local = J.to_bool (json $ "isThreadLocal") in
  let is_lvalue = J.to_bool (json $ "isLvalue") in
  let is_file_local = J.to_bool (json $ "isFileLocal") in
  let is_extern = J.to_bool (json $ "isExtern") in
  let is_volatile = J.to_bool (json $ "isVolatile") in
  let is_parameter = J.to_bool (json $ "isParameter") in
  let is_auxiliary = J.to_bool (json $ "isAuxiliary") in
  let is_weak = J.to_bool (json $ "isWeak") in
  {
    type_;
    value;
    location;
    name;
    module_;
    base_name;
    pretty_name;
    mode;
    is_type;
    is_macro;
    is_exported;
    is_input;
    is_output;
    is_state_var;
    is_property;
    is_static_lifetime;
    is_thread_local;
    is_lvalue;
    is_file_local;
    is_extern;
    is_volatile;
    is_parameter;
    is_auxiliary;
    is_weak;
  }
