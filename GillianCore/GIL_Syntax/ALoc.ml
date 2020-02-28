open Allocators

include Make_with_prefix
          (Basic
             ())
             (struct
               let prefix = Names.aloc_prefix
             end)
