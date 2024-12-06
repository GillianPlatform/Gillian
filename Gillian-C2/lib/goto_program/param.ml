type t = Typedefs__.param = {
  type_ : Typedefs__.type_;
  identifier : string option;
  base_name : string option;
}

let pp fmt t = Typedefs__.pp_param fmt t
let of_irep ~machine irep = Type.param_of_irep ~machine irep
