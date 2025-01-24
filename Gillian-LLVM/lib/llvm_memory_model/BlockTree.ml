open SHeapTree

module M = struct
  open LActions

  type err_t = err [@@deriving show, yojson]

  type t = SHeapTree.t
  [@@deriving show, yojson]

  type action = ac
  type pred = ga

  let action_to_str = str_ac
  let action_from_str s = try Some (ac_from_str s) with _ -> None
  let pred_to_str = str_ga
  let pred_from_str s = try Some (ga_from_str s) with _ -> None

  let list_actions _ =
    [
      (DropPerm, [ "?" ], [ "?" ]);
      (GetCurPerm, [ "?" ], [ "?" ]);
      (WeakValidPointer, [ "?" ], [ "?" ]);
      (Store, [ "?" ], [ "?" ]);
      (Load, [ "?" ], [ "?" ]);
    ]

  let list_preds _ =
    [
      (LActions.Single, [ "?" ], [ "?" ]);
      (LActions.Array, [ "?" ], [ "?" ]);
      (LActions.Hole, [ "?" ], [ "?" ]);
      (LActions.Zeros, [ "?" ], [ "?" ]);
      (LActions.Bounds, [ "?" ], [ "?" ]);
    ]

end