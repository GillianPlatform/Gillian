type 'a t = { try_fold : 'a list option; try_unfold : 'a list option }

let pp pp_val ft t =
  let open Fmt in
  let open Dump in
  record
    [
      field "try_fold" (fun x -> x.try_fold) (option @@ list pp_val);
      field "try_unfold" (fun x -> x.try_unfold) (option @@ list pp_val);
    ]
    ft t

let try_fold x = { try_fold = Some x; try_unfold = None }
let try_unfold x = { try_fold = None; try_unfold = Some x }
let none = { try_fold = None; try_unfold = None }

let merge a b =
  let merge_opt a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some a, Some b ->
        Some (List.rev_append a b |> List.sort_uniq Stdlib.compare)
  in
  {
    try_fold = merge_opt a.try_fold b.try_fold;
    try_unfold = merge_opt a.try_unfold b.try_unfold;
  }

let is_none x = Option.is_none x.try_fold && Option.is_none x.try_unfold
