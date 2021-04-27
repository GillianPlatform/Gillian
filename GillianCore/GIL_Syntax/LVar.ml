open Allocators

include
  Make_with_prefix
    (Basic
       ())
       (struct
         let prefix = Names.lvar_prefix
       end)
