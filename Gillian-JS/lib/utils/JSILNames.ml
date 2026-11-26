(** Local Actions *)

let aCell = "Cell"
let getCell = "GetCell"
let setCell = "SetCell"
let delCell = "DeleteCell"
let alloc = "Alloc"
let delObj = "DeleteObject"
let getAllProps = "GetAllProps"
let aMetadata = "Metadata"
let getMetadata = "GetMetadata"
let setMetadata = "SetMetadata"
let delMetadata = "DelMetadata"
let aProps = "Props"
let getProps = "getProps"
let setProps = "setProps"
let delProps = "delProps"

type action =
  | GetCell
  | SetCell
  | DelCell
  | Alloc
  | DelObj
  | GetAllProps
  | GetMetadata
  | SetMetadata
  | DelMetadata
  | GetProps
  | SetProps
  | DelProps

let actions =
  [
    (getCell, GetCell);
    (setCell, SetCell);
    (delCell, DelCell);
    (alloc, Alloc);
    (delObj, DelObj);
    (getAllProps, GetAllProps);
    (getMetadata, GetMetadata);
    (setMetadata, SetMetadata);
    (delMetadata, DelMetadata);
    (getProps, GetProps);
    (setProps, SetProps);
    (delProps, DelProps);
  ]

let action_from_str name = List.assoc_opt name actions
let resourceError = "ResourceError"
