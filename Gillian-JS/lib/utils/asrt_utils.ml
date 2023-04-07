open JSILNames
open Gillian.Gil_syntax.Asrt

let points_to ~loc ~field ~value = GA (aCell, [ loc; field ], [ value ])
let metadata ~loc ~metadata = GA (aMetadata, [ loc ], [ metadata ])
let empty_fields ~loc ~domain = GA (aProps, [ loc; domain ], [])
