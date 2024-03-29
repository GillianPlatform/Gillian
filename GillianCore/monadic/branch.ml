type 'a t = { pc : Pc.t; value : 'a }

let make ~pc ~value = { pc; value }

let pp pp_value =
  Fmt.braces
    (Fmt.record ~sep:Fmt.semi
       [
         Fmt.field "pc" (fun x -> x.pc) Pc.pp;
         Fmt.field "value" (fun x -> x.value) pp_value;
       ])

let value t = t.value
let learned t = t.pc.learned
let learned_types t = t.pc.learned_types
let pc t = t.pc
