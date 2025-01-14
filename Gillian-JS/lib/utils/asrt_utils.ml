open JSILNames
open Gillian.Gil_syntax.Asrt

let points_to ~loc ~field ~value = CorePred (aCell, [ loc; field ], [ value ])
let metadata ~loc ~metadata = CorePred (aMetadata, [ loc ], [ metadata ])
let empty_fields ~loc ~domain = CorePred (aProps, [ loc; domain ], [])
