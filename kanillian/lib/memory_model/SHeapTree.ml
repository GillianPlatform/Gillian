open Gil_syntax
open Monadic
module DR = Delayed_result
module DO = Delayed_option
module SS = Utils.Containers.SS
module SVArr = SVal.SVArray
module CoreP = Predicates.Core
open SVal

let log_string s = Logging.verbose (fun fmt -> fmt "SHEAPTREE CHECKING: %s" s)

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled of string
  | WrongMemVal
  | MemoryNotFreed
  | LoadingPoison

exception FatalErr of err

let pp_err fmt = function
  | UseAfterFree -> Fmt.pf fmt "Use After Free"
  | BufferOverrun -> Fmt.pf fmt "Buffer Overrun"
  | InsufficientPermission { required; actual } ->
      Fmt.pf fmt "Insufficient Permision: Got %s but required %s"
        (Perm.to_string required) (Perm.to_string actual)
  | InvalidAlignment { alignment; offset } ->
      Fmt.pf fmt "Invalid alignment: %d should divide %a" alignment Expr.pp
        offset
  | MissingResource -> Fmt.pf fmt "MissingResource"
  | Unhandled e -> Fmt.pf fmt "Unhandled error with message : %s" e
  | WrongMemVal -> Fmt.pf fmt "WrongMemVal"
  | MemoryNotFreed -> Fmt.pf fmt "MemoryNotFreed"
  | LoadingPoison -> Fmt.pf fmt "LoadingPoison"

let err_equal a b =
  match (a, b) with
  | MissingResource, MissingResource -> true
  | UseAfterFree, UseAfterFree -> true
  | BufferOverrun, BufferOverrun -> true
  | ( InsufficientPermission { required = ra; actual = aa },
      InsufficientPermission { required = rb; actual = ab } ) ->
      let open Perm.Infix in
      ra =% rb && aa =% ab
  | Unhandled a, Unhandled b -> String.equal a b
  | _ -> false

type 'a or_error = ('a, err) Result.t
type 'a d_or_error = ('a, err) DR.t

module PathTaken = struct
  (** Going through a tree can become quite expensive.
      Remover is always called right after the getter, so we take note of the last path taken and on remove we just go for it directly.  *)

  type t = Left | Right | Here [@@deriving yojson]
end

module Range = struct
  type t = Expr.t * Expr.t [@@deriving yojson]

  let pp fmt (a, b) = Fmt.pf fmt "@[<h>[%a; %a[@]" Expr.pp a Expr.pp b
  let make low high = (low, high)

  module Lift = struct
    open Gillian.Debugger.Utils

    let as_variables
        ~(make_node :
           name:string ->
           value:string ->
           ?children:Variable.t list ->
           unit ->
           Variable.t)
        (low, high) =
      let str = Fmt.to_to_string (Fmt.hbox Expr.pp) in
      let from = make_node ~name:"From" ~value:(str low) () in
      let to_ = make_node ~name:"To" ~value:(str high) () in
      [ from; to_ ]
  end

  let of_low_and_size low size =
    let open Expr.Infix in
    (low, low + size)

  let of_low_and_chunk low chunk =
    let open Expr.Infix in
    let len = Expr.int (Chunk.size chunk) in
    (low, low + len)

  let of_low_chunk_and_size low chunk size =
    let open Expr.Infix in
    let sz_chunk = Expr.int (Chunk.size chunk) in
    (low, low + (sz_chunk * size))

  let is_equal (la, ha) (lb, hb) =
    let open Formula.Infix in
    la #== lb #&& (ha #== hb)

  let is_inside (la, ha) (lb, hb) =
    let open Formula.Infix in
    lb #<= la #&& (ha #<= hb)

  let size (a, b) = Expr.Infix.( - ) b a

  let point_strictly_inside x (l, h) =
    let open Formula.Infix in
    l #< x #&& (x #< h)

  let split_at (l, h) x = ((l, x), (x, h))
  let lvars (a, b) = SS.union (Expr.lvars a) (Expr.lvars b)
  let alocs (a, b) = SS.union (Expr.alocs a) (Expr.alocs b)
  let substitution ~le_subst (a, b) = (le_subst a, le_subst b)
end

module Node = struct
  type qty = Totally | Partially [@@deriving yojson]

  let str_qty = function
    | Totally -> "TOTALLY"
    | Partially -> "PARTIALLY"

  type mem_val =
    | Zeros
    | Poisoned of qty
    | Single of SVal.t
    | Array of SVArr.t
    | LazyValue
  [@@deriving yojson]

  type t =
    | NotOwned of qty
    | MemVal of {
        min_perm : Perm.t;
        exact_perm : Perm.t option;
        mem_val : mem_val;
      }
  [@@deriving yojson]

  let make_owned ~mem_val ~perm =
    MemVal { mem_val; min_perm = perm; exact_perm = Some perm }

  let drop_perm_exn ~perm = function
    | NotOwned _ ->
        raise (FatalErr (Unhandled "Inconsistent permissions in the tree"))
    | MemVal { mem_val; _ } ->
        MemVal { min_perm = perm; exact_perm = Some perm; mem_val }

  let update_parent_perm t ~left ~right =
    match (t, left, right) with
    | ( MemVal { mem_val; _ },
        MemVal { exact_perm = epl; min_perm = mpl; _ },
        MemVal { exact_perm = epr; min_perm = mpr; _ } ) ->
        let exact_perm =
          match (epr, epl) with
          | Some r, Some l when r == l -> Some r
          | _ -> None
        in
        let min_perm = Perm.min mpl mpr in
        MemVal { mem_val; exact_perm; min_perm }
    | _ -> t

  let poisoned ~perm = make_owned ~perm ~mem_val:(Poisoned Totally)
  let not_owned = NotOwned Totally

  let pp fmt = function
    | NotOwned qty -> Fmt.pf fmt "%s NOT OWNED" (str_qty qty)
    | MemVal { exact_perm; mem_val; _ } -> (
        match mem_val with
        | Zeros -> Fmt.pf fmt "ZEROS (%a)" (Fmt.Dump.option Perm.pp) exact_perm
        | Poisoned qty ->
            Fmt.pf fmt "%s POISONED (%a)" (str_qty qty)
              (Fmt.Dump.option Perm.pp) exact_perm
        | Single sval ->
            Fmt.pf fmt "%a (%a)" SVal.pp sval (Fmt.Dump.option Perm.pp)
              exact_perm
        | Array svarr ->
            Fmt.pf fmt "%a (%a)" SVArr.pp svarr (Fmt.Dump.option Perm.pp)
              exact_perm
        | LazyValue -> Fmt.pf fmt "NOT EVALUATED YET")

  let check_perm required node =
    match required with
    | None -> Ok ()
    | Some required -> (
        match node with
        | NotOwned _ -> Error MissingResource
        | MemVal { min_perm = actual; _ } ->
            let open Perm.Infix in
            if actual >=% required then Ok ()
            else Error (InsufficientPermission { actual; required }))

  let exact_perm = function
    | NotOwned Partially -> `KeepLooking
    | NotOwned Totally -> `StopLooking (Error MissingResource)
    | MemVal { exact_perm = None; _ } -> `KeepLooking
    | MemVal { exact_perm = Some x; _ } -> `StopLooking (Ok x)

  let split ~span:(low, high) ~at node =
    Logging.tmi (fun m ->
        m "ABOUT TO SPLIT NODE THAT HAS SPAN %a AT %a" Range.pp (low, high)
          Expr.pp at);
    let open Delayed.Syntax in
    match node with
    | NotOwned Totally -> Delayed.return (NotOwned Totally, NotOwned Totally)
    | NotOwned Partially -> failwith "Should never split a partially owned node"
    | MemVal { exact_perm; min_perm; mem_val } -> (
        let mk mem_val = MemVal { min_perm; exact_perm; mem_val } in
        let make_pair left right = Delayed.return (mk left, mk right) in
        match mem_val with
        | Zeros -> make_pair Zeros Zeros
        | Poisoned Totally -> make_pair (Poisoned Totally) (Poisoned Totally)
        | Single sval ->
            (* The sound approach right now is to transform the value into an array
               of byte and then split that in two *)
            let* svarr = SVArr.byte_array_of_sval sval in
            let at = Expr.Infix.(at - low) in
            let left, right = SVArr.split_at_offset ~at svarr in
            make_pair (Array left) (Array right)
        | Array arr ->
            let at = Expr.Infix.(at - low) in
            let* left, right = SVArr.split_at_byte ~at arr in
            make_pair (Array left) (Array right)
        | Poisoned Partially | LazyValue ->
            failwith "Intermediate node, should never be split")

  let rec merge ~left ~right =
    let ret = Delayed.return in
    let open Delayed.Syntax in
    let a, size_left = left in
    let b, size_right = right in
    match (a, b) with
    | NotOwned Totally, NotOwned Totally -> ret (NotOwned Totally)
    | NotOwned _, _ | _, NotOwned _ -> ret (NotOwned Partially)
    | ( MemVal { exact_perm = ex_perma; min_perm = min_perma; mem_val = vala },
        MemVal { exact_perm = ex_permb; min_perm = min_permb; mem_val = valb } )
      -> (
        let min_perm = Perm.min min_perma min_permb in
        let exact_perm =
          match (ex_perma, ex_permb) with
          | Some pa, Some pb when pa == pb -> Some pa
          | _, _ -> None
        in
        let mk mem_val = MemVal { min_perm; mem_val; exact_perm } in
        let array arr = ret (mk (Array arr)) in
        let lazy_value = ret (mk LazyValue) in
        let zeros = ret (mk Zeros) in
        match (vala, valb) with
        | Zeros, Zeros -> zeros
        | Poisoned Totally, Poisoned Totally -> ret (mk (Poisoned Totally))
        | Poisoned _, _ | _, Poisoned _ -> ret (mk (Poisoned Partially))
        | Single sv, Zeros ->
            if SVal.sure_is_zero sv then zeros
            else
              let chunk = SVal.leak_chunk sv in
              let chunk_size = Expr.int (Chunk.size chunk) in
              let zeros_can_be_converted_to_same_chunk =
                let open Formula.Infix in
                (Expr.imod size_right chunk_size) #== Expr.zero_i
              in
              if%ent zeros_can_be_converted_to_same_chunk then
                let+ zero_array =
                  SVArr.make_zeros
                    ~size:Expr.Infix.(size_right / chunk_size)
                    ~chunk
                in
                let result =
                  SVArr.cons_same_chunk sv zero_array |> Option.get
                  (* Option.get is always safe here *)
                in
                mk (Array result)
              else
                let* byte_zeros = SVArr.make_zeros ~size:size_left ~chunk:U8 in
                let+ byte_sv = SVArr.byte_array_of_sval sv in
                let result =
                  SVArr.concat_same_chunk byte_sv byte_zeros |> Option.get
                  (* Option.get is always safe here *)
                in
                mk (Array result)
        | Zeros, Single sv ->
            if SVal.sure_is_zero sv then zeros
            else
              let chunk = SVal.leak_chunk sv in
              let chunk_size = Expr.int (Chunk.size chunk) in
              let zeros_can_be_converted_to_same_chunk =
                let open Formula.Infix in
                (Expr.imod size_left chunk_size) #== Expr.zero_i
              in
              if%ent zeros_can_be_converted_to_same_chunk then
                let+ zero_array =
                  SVArr.make_zeros
                    ~size:Expr.Infix.(size_left / chunk_size)
                    ~chunk
                in
                let result =
                  SVArr.append_same_chunk zero_array sv |> Option.get
                in
                (* Garanteed to work *)
                mk (Array result)
              else
                let* byte_zeros = SVArr.make_zeros ~size:size_left ~chunk:U8 in
                let+ byte_sv = SVArr.byte_array_of_sval sv in
                let result =
                  SVArr.concat_same_chunk byte_zeros byte_sv |> Option.get
                in
                (* Garanteed to work *)
                mk (Array result)
        | (Array arr, Zeros | Zeros, Array arr) when SVArr.sure_is_all_zeros arr
          -> zeros
        | Zeros, Array arr ->
            if SVArr.sure_is_all_zeros arr then zeros
            else
              let chunk = SVArr.leak_chunk arr in
              let chunk_size = Expr.int (Chunk.size chunk) in
              let zeros_can_be_converted_to_same_chunk =
                let open Formula.Infix in
                (Expr.imod size_left chunk_size) #== Expr.zero_i
              in
              if%ent zeros_can_be_converted_to_same_chunk then
                let+ zero_array =
                  SVArr.make_zeros
                    ~size:Expr.Infix.(size_left / chunk_size)
                    ~chunk
                in
                let result =
                  SVArr.concat_same_chunk zero_array arr |> Option.get
                in
                (* Garanteed to work *)
                mk (Array result)
              else ret (mk LazyValue)
        | Array arr, Zeros ->
            if SVArr.sure_is_all_zeros arr then zeros
            else
              let chunk = SVArr.leak_chunk arr in
              let chunk_size = Expr.int (Chunk.size chunk) in
              let zeros_can_be_converted_to_same_chunk =
                let open Formula.Infix in
                (Expr.imod size_right chunk_size) #== Expr.zero_i
              in
              if%ent zeros_can_be_converted_to_same_chunk then
                let+ zero_array =
                  SVArr.make_zeros
                    ~size:Expr.Infix.(size_right / chunk_size)
                    ~chunk
                in
                let result =
                  SVArr.concat_same_chunk arr zero_array |> Option.get
                in
                (* Garanteed to work *)
                mk (Array result)
              else ret (mk LazyValue)
        | LazyValue, _ | _, LazyValue -> lazy_value
        | Single sva, Single svb -> (
            match SVArr.of_two_svals_same_chunk sva svb with
            | Some arr -> array arr
            | None -> lazy_value)
        | Single sv, Array arr -> (
            match SVArr.cons_same_chunk sv arr with
            | Some arr -> array arr
            | None -> lazy_value)
        | Array arr, Single sv -> (
            match SVArr.append_same_chunk arr sv with
            | Some arr -> array arr
            | None -> lazy_value)
        | Array arra, Array arrb -> (
            match SVArr.concat_same_chunk arra arrb with
            | Some arr -> array arr
            | None -> lazy_value))

  let decode ~chunk t =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match t with
    | NotOwned _ -> DR.error MissingResource
    | MemVal { mem_val = Poisoned _; exact_perm; _ } -> DR.error LoadingPoison
    | MemVal { mem_val = Zeros; exact_perm; _ } ->
        DR.ok (SVal.zero_of_chunk chunk, exact_perm)
    | MemVal { mem_val = Single sval; exact_perm; _ } ->
        let+ sval = SVal.reencode ~chunk sval in
        Ok (sval, exact_perm)
    | MemVal { mem_val = Array arr; exact_perm; _ } ->
        let+ sval = SVArr.decode_as_sval ~chunk arr in
        Ok (sval, exact_perm)
    | MemVal { mem_val = LazyValue; exact_perm; _ } ->
        failwith "unimplmented: decoding lazy value"

  let decode_array ~size ~chunk t =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match t with
    | NotOwned _ -> DR.error MissingResource
    | MemVal { mem_val = Poisoned _; exact_perm; _ } -> DR.error LoadingPoison
    | MemVal { mem_val = Zeros; exact_perm; _ } ->
        let+ arr = SVArr.make_zeros ~chunk ~size in
        Ok (arr, exact_perm)
    | MemVal { mem_val = Single sval; exact_perm; _ } ->
        let+ arr = SVArr.decode_sval_into ~chunk sval in
        Ok (arr, exact_perm)
    | MemVal { mem_val = Array svarr; exact_perm; _ } ->
        let+ arr = SVArr.reencode ~chunk svarr in
        Ok (arr, exact_perm)
    | MemVal { mem_val = LazyValue; _ } ->
        failwith "unimplmented: decoding lazy value"

  let single ~(perm : Perm.t) ~chunk (sval : SVal.t) : t =
    let mem_val = Single sval in
    MemVal { exact_perm = Some perm; min_perm = perm; mem_val }

  let array ~(perm : Perm.t) ~(chunk : Chunk.t) (sarr : SVArr.t) =
    let mem_val = Array sarr in
    MemVal { exact_perm = Some perm; min_perm = perm; mem_val }

  let lvars = function
    | MemVal { mem_val = Single sval; _ } -> SVal.lvars sval
    | MemVal { mem_val = Array sarr; _ } -> SVArr.lvars sarr
    | _ -> SS.empty

  let alocs = function
    | MemVal { mem_val = Single sval; _ } -> SVal.alocs sval
    | MemVal { mem_val = Array svarr; _ } -> SVArr.alocs svarr
    | _ -> SS.empty

  let substitution ~sval_subst ~svarr_subst n =
    let smv = function
      | Single s -> Single (sval_subst s)
      | Array a -> Array (svarr_subst a)
      | u -> u
    in
    match n with
    | MemVal mv -> MemVal { mv with mem_val = smv mv.mem_val }
    | no -> no
end

module Tree = struct
  type t = {
    node : Node.t;
    span : Range.t;
    children : (t * t) option;
    last_path : PathTaken.t option; [@ignore]
  }
  [@@deriving yojson]

  module Lift = struct
    open Gillian.Debugger.Utils

    let rec as_variable
        ~(make_node :
           name:string ->
           value:string ->
           ?children:Variable.t list ->
           unit ->
           Variable.t)
        (tree : t) : Variable.t =
      let as_variable = as_variable ~make_node in
      let str pp = Fmt.to_to_string (Fmt.hbox pp) in
      let name = (str Range.pp) tree.span in
      let value = (str Node.pp) tree.node in
      let children =
        Option.map
          (fun (a, b) -> [ as_variable a; as_variable b ])
          tree.children
      in
      make_node ~name ~value ?children ()
  end

  let box_range_and_node span node =
    let open PrintBox in
    frame
    @@ hlist
         [
           hpad 2 @@ text (Fmt.to_to_string Range.pp @@ span);
           hpad 1 @@ text (Fmt.to_to_string Node.pp @@ node);
         ]

  let box_full t =
    let open PrintBox in
    let make { node; span; children; _ } =
      let node = box_range_and_node span node in
      let children =
        match children with
        | None -> []
        | Some (a, b) -> [ a; b ]
      in
      (node, children)
    in
    mk_tree make t

  let pp_full fmt t = PrintBox_text.pp fmt (box_full t)

  let is_empty { node; _ } =
    match node with
    | NotOwned Totally -> true
    | _ -> false

  let make ~node ~span ?children () = { node; span; children; last_path = None }

  (* Used to change the position of a tree. The start of the tree is going to be [start], but the spans don't change. *)
  let rec realign t start =
    let open Expr.Infix in
    let reduce e = Engine.Reduction.reduce_lexpr e in
    let l, h = t.span in
    let span = (start, reduce (start + h - l)) in
    let children =
      Option.map
        (fun (left, right) ->
          let left = realign left start in
          let _, m = left.span in
          let right = realign right m in
          (left, right))
        t.children
    in
    make ~node:t.node ~span ?children ()

  let with_children t ~left ~right =
    Delayed.return { t with children = Some (left, right); last_path = None }

  let of_children_s ~left ~right =
    let open Delayed.Syntax in
    let span = (fst left.span, snd right.span) in
    let+ node =
      Node.merge
        ~left:(left.node, Range.size left.span)
        ~right:(right.node, Range.size right.span)
    in
    let children =
      match node with
      | Node.NotOwned Totally
      | MemVal { exact_perm = Some _; mem_val = Zeros | Poisoned Totally; _ } ->
          None
      | _ -> Some (left, right)
    in
    { span; children; node; last_path = None }

  let of_children _ ~left ~right = of_children_s ~left ~right

  let update_parent_perm t ~left ~right =
    let { node; span; _ } = t in
    let new_node =
      Node.update_parent_perm node ~left:left.node ~right:right.node
    in
    Delayed.return
      { node = new_node; span; children = Some (left, right); last_path = None }

  let sval_leaf ~low ~perm ~value ~chunk =
    let open Delayed.Syntax in
    let node = Node.single ~perm ~chunk value in
    let span = Range.of_low_and_chunk low chunk in
    make ~node ~span ()

  let sarr_leaf ~low ~perm ~size ~array ~chunk =
    let node = Node.array ~perm ~chunk array in
    let span = Range.of_low_chunk_and_size low chunk size in
    make ~node ~span ()

  let poisoned ?(perm = Perm.Freeable) span =
    make ~node:(Node.poisoned ~perm) ~span ()

  let zeros ?(perm = Perm.Freeable) span =
    make ~node:(Node.make_owned ~mem_val:Zeros ~perm) ~span ()

  let create_root range =
    {
      children = None;
      span = range;
      node = NotOwned Totally;
      last_path = Some Here;
    }

  let rec split ~range t : (Node.t * t * t) Delayed.t =
    (* this function splits a tree and returns the node in the given range *)
    (* We're assuming that range is inside old_span *)
    let open Formula.Infix in
    let open Delayed.Syntax in
    let old_span = t.span in
    let ol, oh = old_span in
    let nl, nh = range in
    if%sat
      log_string "ol #== nl";
      ol #== nl
    then
      let at = nh in
      let+ left_node, right_node = Node.split ~span:old_span ~at t.node in
      let left_span, right_span = Range.split_at old_span at in
      let left = make ~node:left_node ~span:left_span () in
      let right = make ~node:right_node ~span:right_span () in
      (left_node, left, right)
    else
      if%sat
        log_string "oh #== nh";
        oh #== nh
      then
        let at = nl in
        let+ left_node, right_node = Node.split ~span:old_span ~at t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let right = make ~node:right_node ~span:right_span () in
        (right_node, left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let* left_node, right_node = Node.split ~span:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let full_right = make ~node:right_node ~span:right_span () in
        let* node, right_left, right_right = split ~range full_right in
        let+ right =
          with_children full_right ~left:right_left ~right:right_right
        in
        (node, left, right)

  let extend_if_needed t range =
    let open Formula.Infix in
    let open Delayed.Syntax in
    let rl, rh = range in
    let sl, sh = t.span in
    let* t_with_left =
      if%sat rl #< sl then
        let new_left_tree = make ~node:(NotOwned Totally) ~span:(rl, sl) () in
        let children = (new_left_tree, t) in
        Delayed.return
          (make ~node:(NotOwned Partially) ~span:(rl, sh) ~children ())
      else Delayed.return t
    in
    let sl, _ = t_with_left.span in
    let* result =
      if%sat rh #> sh then
        let new_right_tree = make ~node:(NotOwned Totally) ~span:(sh, rh) () in
        let children = (t_with_left, new_right_tree) in
        Delayed.return
          (make ~node:(NotOwned Partially) ~span:(sl, rh) ~children ())
      else Delayed.return t_with_left
    in
    Delayed.return result

  let frame_range
      (t : t)
      ~(replace_node : t -> (t, err) DR.t)
      ~rebuild_parent
      (range : Range.t) : (t * t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let rec extract (t : t) (range : Range.t) : (t * t option) Delayed.t =
      (* First result is the extracted tree, second is the remain *)
      let open Delayed in
      let open Syntax in
      if%sat
        log_string "EXTRACT range is equal span";
        Range.is_equal range t.span
      then return (t, None)
      else
        let left, right = Option.get t.children in
        if%sat
          log_string "EXTRACT range inside left";
          Range.is_inside range left.span
        then
          let* extracted, new_left = extract left range in
          let+ new_self =
            match new_left with
            | Some left -> of_children_s ~right ~left
            | None -> Delayed.return right
          in
          (extracted, Some new_self)
        else
          let* extracted, new_right = extract right range in
          let+ new_self =
            match new_right with
            | Some right -> of_children_s ~right ~left
            | None -> Delayed.return left
          in
          (extracted, Some new_self)
    in
    let rec add_to_the_right t addition : t Delayed.t =
      match t.children with
      | None -> of_children_s ~left:t ~right:addition
      | Some (left, right) ->
          let* new_right = add_to_the_right right addition in
          of_children_s ~left ~right:new_right
    in
    let rec add_to_the_left t addition : t Delayed.t =
      match t.children with
      | None -> of_children_s ~left:addition ~right:t
      | Some (left, right) ->
          let* new_right = add_to_the_left left addition in
          of_children_s ~left ~right:new_right
    in
    let rec frame_inside
        ~(replace_node : t -> (t, err) DR.t)
        ~rebuild_parent
        (t : t)
        (range : Range.t) =
      Logging.verbose (fun fmt ->
          fmt "STARTING FRAME INSIDE WITH: %a" pp_full t);
      if%sat
        log_string "range equals span";
        Range.is_equal range t.span
      then (
        log_string "Range does equal span, replacing.";
        let++ new_tree = replace_node t in
        (t, { new_tree with last_path = Some Here }))
      else
        match t.children with
        | Some (left, right) ->
            let _, mid = left.span in
            if%sat
              log_string
                (Fmt.str "mid strictly in range: %a in %a" Expr.pp mid Range.pp
                   range);
              Range.point_strictly_inside mid range
            then
              let l, h = range in
              let upper_range = (mid, h) in
              let dont_replace_node t = DR.ok t in
              if%sat
                (* High-range already good *)
                Range.is_equal upper_range right.span
              then
                (* Rearrange left*)
                let lower_range = (l, mid) in
                let** _, left =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children left lower_range
                in
                let* extracted, left_opt = extract left lower_range in
                let* right = add_to_the_left right extracted in
                let* new_self =
                  match left_opt with
                  | Some left -> of_children_s ~left ~right
                  | None -> Delayed.return right
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
              else
                let** _, right =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children right upper_range
                in
                let* extracted, right_opt = extract right upper_range in
                let* left = add_to_the_right left extracted in
                let* new_self =
                  match right_opt with
                  | Some right -> of_children_s ~left ~right
                  | None -> Delayed.return left
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
            else
              if%sat
                log_string "range inside left";
                Range.is_inside range left.span
              then
                let** node, left =
                  frame_inside ~replace_node ~rebuild_parent left range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                Ok (node, { new_parent with last_path = Some Left })
              else
                if%sat
                  log_string "range inside right";
                  Range.is_inside range right.span
                then
                  let** node, right =
                    frame_inside ~replace_node ~rebuild_parent right range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  Ok (node, { new_parent with last_path = Some Right })
                else (
                  Logging.verbose (fun fmt ->
                      fmt
                        "ABOUT TO SAY PRECUT:\nLEFT: %a\nRIGHT: %a\n RANGE: %a"
                        Range.pp left.span Range.pp right.span Range.pp range);
                  DR.error (Unhandled "wrong pre-cut"))
        | None ->
            let open Delayed.Syntax in
            let* _, left, right = split ~range t in
            let* new_self = with_children t ~left ~right in
            Logging.verbose (fun fmt ->
                fmt "AFTER SPLITTING FOR %a: %a" Range.pp range pp_full new_self);
            frame_inside ~replace_node ~rebuild_parent new_self range
    in
    let open Delayed.Syntax in
    let* root = extend_if_needed t range in
    frame_inside ~replace_node ~rebuild_parent root range

  let get_node (t : t) range : (Node.t * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = DR.ok x in
    let rebuild_parent = with_children in
    let++ framed, rest = frame_range t ~replace_node ~rebuild_parent range in
    (framed.node, rest)

  let set_node (t : t) range node : (t, err) DR.t =
    let open DR.Syntax in
    let replace_node _ = DR.ok (make ~node ~span:range ()) in
    let rebuild_parent = of_children in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let rem_last_get (t : t) : t =
    let rebuild
        ({ span = lspan; node = lnode; _ } as left)
        ({ span = rspan; node = rnode; _ } as right) =
      let span = (fst lspan, snd rspan) in
      match (lnode, rnode) with
      | NotOwned Totally, NotOwned Totally ->
          make ~node:(NotOwned Totally) ~span ()
      | _ -> make ~node:(NotOwned Partially) ~span ~children:(left, right) ()
    in
    let rec loop { span; children; last_path; _ } =
      match last_path with
      | None -> failwith "Imposible, removing last get, but can't follow a path"
      | Some Here -> make ~node:Node.not_owned ~span ()
      | Some Left ->
          let left, right = Option.get children in
          let left = loop left in
          rebuild left right
      | Some Right ->
          let left, right = Option.get children in
          let right = loop right in
          rebuild left right
    in
    loop t

  let get_array (t : t) (low : Expr.t) (chunk : Chunk.t) (size : Expr.t) :
      (SVArr.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let* size = Delayed.reduce size in
    let replace_node x = DR.ok x in
    let rebuild_parent = with_children in
    let range = Range.of_low_chunk_and_size low chunk size in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let+* arr, perm = Node.decode_array ~size ~chunk framed.node in
    Ok (arr, perm, tree)

  let set_array
      (t : t)
      (low : Expr.t)
      (size : Expr.t)
      (chunk : Chunk.t)
      (array : SVArr.t)
      (perm : Perm.t) : (t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let replace_node _ = DR.ok (sarr_leaf ~low ~chunk ~array ~size ~perm) in
    let rebuild_parent = of_children in
    let range = Range.of_low_chunk_and_size low chunk size in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    (* let+ () = SVArr.learn_chunk ~chunk ~size array in *)
    t

  let get_single (t : t) (low : Expr.t) (chunk : Chunk.t) :
      (SVal.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = DR.ok x in
    let rebuild_parent = with_children in
    let range = Range.of_low_and_chunk low chunk in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let node = framed.node in
    Logging.tmi (fun m ->
        m "GET_SINGLE GOT THE FOLLOWING NODE: %a" Node.pp node);
    let++ sval, perm = Node.decode ~chunk node in
    (sval, perm, tree)

  let set_single
      (t : t)
      (low : Expr.t)
      (chunk : Chunk.t)
      (sval : SVal.t)
      (perm : Perm.t) : (t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let replace_node _ =
      let leaf = sval_leaf ~low ~chunk ~value:sval ~perm in
      DR.ok leaf
    in
    let rebuild_parent = of_children in
    let range = Range.of_low_and_chunk low chunk in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let load (t : t) (low : Expr.t) (chunk : Chunk.t) : (SVal.t * t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | Node.NotOwned _ -> DR.error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Readable then DR.ok node
          else
            DR.error
              (InsufficientPermission { required = Readable; actual = min_perm })
    in
    let rebuild_parent = with_children in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ sval, _ = Node.decode ~chunk framed.node in
    (sval, tree)

  let store (t : t) (low : Expr.t) (chunk : Chunk.t) (sval : SVal.t) :
      (t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | NotOwned _ -> DR.error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Writable then
            let leaf = sval_leaf ~low ~chunk ~value:sval ~perm:min_perm in
            DR.ok leaf
          else
            DR.error
              (InsufficientPermission { required = Writable; actual = min_perm })
    in
    let rebuild_parent = of_children in
    let++ _, tree = frame_range t ~replace_node ~rebuild_parent range in
    tree

  let zero_init (t : t) (range : Range.t) : (t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let replace_node node =
      match node.node with
      | NotOwned _ -> DR.error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Writable then DR.ok (zeros ~perm:min_perm range)
          else
            DR.error
              (InsufficientPermission { required = Writable; actual = min_perm })
    in
    let rebuild_parent = of_children in
    let++ _, tree = frame_range t ~replace_node ~rebuild_parent range in
    tree

  let poison (t : t) (range : Range.t) : (t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let replace_node node =
      match node.node with
      | NotOwned _ -> DR.error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Writable then DR.ok (poisoned ~perm:min_perm range)
          else
            DR.error
              (InsufficientPermission { required = Writable; actual = min_perm })
    in
    let rebuild_parent = of_children in
    let++ _, tree = frame_range t ~replace_node ~rebuild_parent range in
    tree

  let get_perm_at (tree : t) (ofs : Expr.t) : (Perm.t, err) DR.t =
    let range =
      let open Expr.Infix in
      (ofs, ofs + Expr.int 1)
    in
    let { span; _ } = tree in
    let rec rec_call treep =
      match Node.exact_perm treep.node with
      | `StopLooking r -> DR.of_result r
      | `KeepLooking ->
          let left, right = Option.get treep.children in
          if%sat Range.is_inside range left.span then rec_call left
          else rec_call right
    in
    if%sat Range.is_inside range span then rec_call tree
    else DR.error MissingResource

  let weak_valid_pointer (tree : t) (ofs : Expr.t) : (bool, err) DR.t =
    let open Delayed.Syntax in
    let open Perm.Infix in
    let open Expr.Infix in
    let* at_ofs = get_perm_at tree ofs in
    match at_ofs with
    | Ok p when p >=% Nonempty -> DR.ok true
    | _ ->
        let+ at_ofs_minus_one = get_perm_at tree (ofs - Expr.int 1) in
        at_ofs_minus_one |> Result.map (fun p -> p >=% Nonempty)

  let drop_perm (t : t) (low : Expr.t) (high : Expr.t) (perm : Perm.t) :
      (t, err) DR.t =
    let rec rec_set_perm { node; span; children; last_path } =
      let node = Node.drop_perm_exn ~perm node in
      let children =
        Option.map (fun (a, b) -> (rec_set_perm a, rec_set_perm b)) children
      in
      { node; span; children; last_path }
    in
    let open DR.Syntax in
    let range = Range.make low high in
    let replace_node node =
      match node.node with
      | NotOwned _ -> DR.error MissingResource
      | MemVal { min_perm = Freeable; _ } -> DR.ok (rec_set_perm node)
      | MemVal { min_perm; _ } ->
          DR.error
            (InsufficientPermission { required = Freeable; actual = min_perm })
    in
    let rebuild_parent = update_parent_perm in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let rec lvars { node; span; children; _ } =
    let node_lvars = Node.lvars node in
    let span_lvars = Range.lvars span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (lvars a) (lvars b)
      | None -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars

  let rec alocs { node; span; children; _ } =
    let node_lvars = Node.alocs node in
    let span_lvars = Range.alocs span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (alocs a) (alocs b)
      | None -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars

  let rec assertions ~loc { node; span; children; _ } =
    let low, high = span in
    match node with
    | NotOwned Totally -> []
    | NotOwned Partially
    | MemVal { mem_val = Poisoned Partially; _ }
    | MemVal { mem_val = LazyValue; _ } ->
        let left, right = Option.get children in
        assertions ~loc left @ assertions ~loc right
    | MemVal { mem_val = Poisoned Totally; exact_perm = perm; _ } ->
        [ CoreP.hole ~loc ~low ~high ~perm ]
    | MemVal { mem_val = Zeros; exact_perm = perm; _ } ->
        [ CoreP.zeros ~loc ~low ~high ~perm ]
    | MemVal { mem_val = Single sval; exact_perm = perm; _ } ->
        let chunk, sval = SVal.leak sval in
        [ CoreP.single ~loc ~ofs:low ~chunk ~sval ~perm ]
    | MemVal { mem_val = Array sarr; exact_perm = perm; _ } ->
        let chunk, sval_arr = SVArr.leak sarr in
        let chksize = Expr.int (Chunk.size chunk) in
        let total_size =
          let open Expr.Infix in
          (high - low) / chksize
        in
        [ CoreP.array ~loc ~ofs:low ~perm ~chunk ~size:total_size ~sval_arr ]

  let rec substitution
      ~svarr_subst
      ~sval_subst
      ~le_subst
      { node; span; children; last_path } =
    let node = Node.substitution ~sval_subst ~svarr_subst node in
    let span = Range.substitution ~le_subst span in
    let children =
      Option.map
        (fun (left, right) ->
          let f = substitution ~sval_subst ~le_subst ~svarr_subst in
          (f left, f right))
        children
    in
    { node; span; children; last_path }

  let box t =
    let rec flatten_tree { node; span; children; _ } =
      match node with
      | NotOwned Partially
      | MemVal { mem_val = Poisoned Partially | LazyValue; _ } ->
          let left, right = Option.get children in
          flatten_tree left @ flatten_tree right
      | node -> [ (span, node) ]
    in
    let open PrintBox in
    frame @@ vlist_map (fun (x, y) -> box_range_and_node x y) (flatten_tree t)

  let pp fmt tree = PrintBox_text.pp fmt (box tree)
end

type t = Freed | Tree of { bounds : Range.t option; root : Tree.t option }
[@@deriving yojson]

let pp_full fmt = function
  | Freed -> Fmt.pf fmt "FREED"
  | Tree { bounds; root } ->
      let pp_aux fmt (bounds, root) =
        Fmt.pf fmt "%a@ %a"
          (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
          bounds
          (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp_full)
          root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (bounds, root)

let pp fmt t =
  match t with
  | Freed -> Fmt.pf fmt "FREED"
  | Tree { bounds; root } ->
      Fmt.pf fmt "%a@ %a"
        (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
        bounds
        (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp)
        root

let empty =
  let bounds = None in
  let root = None in
  Tree { bounds; root }

let is_empty t =
  match t with
  | Freed -> false
  | Tree { bounds; root } ->
      Option.is_none bounds
      && Option.fold ~none:true ~some:(fun root -> Tree.is_empty root) root

let freed = Freed

let lvars = function
  | Freed -> SS.empty
  | Tree { bounds; root } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.lvars bounds)
        (Option.fold ~none:SS.empty ~some:Tree.lvars root)

let alocs = function
  | Freed -> SS.empty
  | Tree { bounds; root } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.alocs bounds)
        (Option.fold ~none:SS.empty ~some:Tree.alocs root)

let get_root = function
  | Freed -> Error UseAfterFree
  | Tree x -> Ok x.root

let is_in_bounds range bounds =
  match bounds with
  | None -> Formula.True
  | Some bounds -> Range.is_inside range bounds

let get_perm_at t ofs =
  let open DR.Syntax in
  match t with
  | Freed -> DR.ok None
  | Tree { bounds; root } ->
      let is_in_bounds =
        let open Expr.Infix in
        is_in_bounds (ofs, ofs + Expr.int 1) bounds
      in
      if%sat is_in_bounds then
        match root with
        | None -> DR.error MissingResource
        | Some root ->
            let++ perm = Tree.get_perm_at root ofs in
            Some perm
      else DR.ok None

let weak_valid_pointer (t : t) (ofs : Expr.t) : (bool, err) DR.t =
  let is_sure_false bounds ofs =
    let open Formula.Infix in
    match bounds with
    | None -> Formula.False
    | Some (low, high) -> ofs #< low #|| (ofs #> high)
  in
  match t with
  | Freed -> DR.ok false
  | Tree { bounds; root } -> (
      if%sat is_sure_false bounds ofs then DR.ok false
      else
        match root with
        | None -> DR.error MissingResource
        | Some root -> Tree.weak_valid_pointer root ofs)

let get_bounds = function
  | Freed -> Error UseAfterFree
  | Tree x -> Ok x.bounds

let set_bounds t bounds =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds })

let rem_bounds t =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds = None })

let with_root_opt t root =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with root })

let with_root t root = with_root_opt t (Some root)

let alloc low high =
  let bounds = Range.make low high in
  Tree { root = Some (Tree.poisoned bounds); bounds = Some bounds }

let drop_perm t low high new_perm =
  let open DR.Syntax in
  match t with
  | Freed -> DR.error UseAfterFree
  | Tree { bounds; root } -> (
      match root with
      | None -> DR.error MissingResource
      | Some tree ->
          let++ new_root = Tree.drop_perm tree low high new_perm in
          Tree { bounds; root = Some new_root })

let free t low high =
  let open DR.Syntax in
  let** bounds = DR.of_result (get_bounds t) in
  match t with
  (* Can't free something already freed *)
  | Freed -> DR.error UseAfterFree
  | Tree tree -> (
      (* Can only free if entirely freeable *)
      match bounds with
      | None -> DR.error MissingResource
      | Some bounds ->
          if%ent Range.is_equal (low, high) bounds then
            match tree.root with
            | None -> DR.error MissingResource
            | Some root ->
                let+* node, _ = Tree.get_node root (low, high) in
                Result.map
                  (fun () -> Freed)
                  (Node.check_perm (Some Freeable) node)
          else
            DR.error
              (Unhandled
                 "Freeing only part of an object (this might need fixing in \
                  the MM)"))

let get_single t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** value, perm, root_framed = Tree.get_single root low chunk in
        let++ wroot = DR.of_result (with_root t root_framed) in
        (value, perm, wroot)
  else DR.error BufferOverrun

let set_single t low chunk sval perm =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** root = DR.of_result (get_root t) in
  let root = Option.value root ~default:(Tree.create_root range) in
  let** root_set = Tree.set_single root low chunk sval perm in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let rem_last_get t =
  match t with
  | Tree { root = Some root; bounds } ->
      Tree { bounds; root = Some (Tree.rem_last_get root) }
  | _ -> failwith "Impossible, removing last get with something absurd"

let get_array t low size chunk =
  let open DR.Syntax in
  let range = Range.of_low_chunk_and_size low chunk size in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** array, perm, root_framed = Tree.get_array root low chunk size in
        let++ wroot = DR.of_result (with_root t root_framed) in
        (array, perm, wroot)
  else DR.error BufferOverrun

let set_array t low size chunk array perm =
  let open DR.Syntax in
  let range = Range.of_low_chunk_and_size low chunk size in
  let** root = DR.of_result (get_root t) in
  let root = Option.value root ~default:(Tree.create_root range) in
  let** root_set = Tree.set_array root low size chunk array perm in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let get_simple_mem_val ~expected_mem_val t low high =
  let open DR.Syntax in
  let range = (low, high) in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** node, root_framed = Tree.get_node root range in
        let res =
          match node with
          | MemVal { mem_val; exact_perm = perm; _ }
            when expected_mem_val mem_val -> Ok perm
          | NotOwned _ -> Error MissingResource
          | _ -> Error WrongMemVal
        in
        let++ wroot =
          DR.of_result
            (Result.bind res (fun perm ->
                 Result.map (fun mem -> (mem, perm)) (with_root t root_framed)))
        in
        wroot
  else DR.error BufferOverrun

let set_simple_mem_val ~mem_val t low high perm =
  let open DR.Syntax in
  let range = (low, high) in
  let** root = DR.of_result (get_root t) in
  let root = Option.value ~default:(Tree.create_root range) root in
  let** root_set = Tree.set_node root range (Node.make_owned ~perm ~mem_val) in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let get_hole =
  get_simple_mem_val ~expected_mem_val:(function
    | Poisoned Totally -> true
    | _ -> false)

let set_hole = set_simple_mem_val ~mem_val:(Poisoned Totally)

let get_zeros =
  get_simple_mem_val ~expected_mem_val:(function
    | Zeros -> true
    | _ -> false)

let set_zeros = set_simple_mem_val ~mem_val:Zeros

let get_freed t =
  match t with
  | Freed -> Ok ()
  | _ -> Error MemoryNotFreed

let _check_valid_alignment chunk ofs =
  let al = Chunk.align chunk in
  let al_expr = Expr.int al in
  let divides x y =
    let open Formula.Infix in
    Expr.(y #== (int 0)) #|| ((Expr.imod y x) #== (Expr.int 0))
  in
  if%sat divides al_expr ofs then DR.ok ()
  else DR.error (InvalidAlignment { offset = ofs; alignment = al })

let load t chunk ofs =
  let open DR.Syntax in
  (* FIXME: this should be reestablished asap *)
  (* let** () = check_valid_alignment chunk ofs in *)
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** value, root = Tree.load root ofs chunk in
        let++ wroot = DR.of_result (with_root t root) in
        (value, wroot)
  else DR.error BufferOverrun

let store t chunk ofs value =
  let open DR.Syntax in
  (* let** () = check_valid_alignment chunk ofs in *)
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** root = Tree.store root ofs chunk value in
        DR.of_result (with_root t root)
  else DR.error BufferOverrun

let zero_init t ofs size =
  let open DR.Syntax in
  let range = Range.of_low_and_size ofs size in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** root = Tree.zero_init root range in
        DR.of_result (with_root t root)
  else DR.error BufferOverrun

let poison t ofs size =
  let open DR.Syntax in
  let range = Range.of_low_and_size ofs size in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** root = Tree.poison root range in
        DR.of_result (with_root t root)
  else DR.error BufferOverrun

let move dst_tree dst_ofs src_tree src_ofs size =
  let open DR.Syntax in
  let dst_range, src_range =
    let open Expr.Infix in
    ((dst_ofs, dst_ofs + size), (src_ofs, src_ofs + size))
  in
  let** src_span = DR.of_result (get_bounds src_tree) in
  if%sat is_in_bounds src_range src_span then
    let** src_root = DR.of_result (get_root src_tree) in
    match src_root with
    | None -> DR.error MissingResource
    | Some src_root ->
        let** framed, _ =
          Tree.frame_range src_root
            ~replace_node:(fun x -> DR.ok x)
            ~rebuild_parent:(fun t ~left:_ ~right:_ -> Delayed.return t)
            src_range
        in
        let** () =
          match framed.node with
          | NotOwned _ -> DR.error MissingResource
          | _ -> DR.ok ()
        in
        let** dst_span = DR.of_result (get_bounds dst_tree) in
        if%sat is_in_bounds dst_range dst_span then
          let** dst_root = DR.of_result (get_root dst_tree) in
          match dst_root with
          | None -> DR.error MissingResource
          | Some dst_root ->
              let** _, new_dst_root =
                Tree.frame_range dst_root
                  ~replace_node:(fun current ->
                    match current.node with
                    | NotOwned _ -> DR.error MissingResource
                    | _ -> DR.ok (Tree.realign framed dst_ofs))
                  ~rebuild_parent:Tree.of_children dst_range
              in
              DR.of_result (with_root dst_tree new_dst_root)
        else DR.error BufferOverrun
  else DR.error BufferOverrun

let assertions ~loc t =
  let loc = Expr.loc_from_loc_name loc in
  match t with
  | Freed -> [ CoreP.freed ~loc ]
  | Tree x ->
      let bounds =
        Option.fold ~none:[]
          ~some:(fun (low, high) -> [ CoreP.bounds ~loc ~low ~high ])
          x.bounds
      in
      let tree =
        match x.root with
        | None -> []
        | Some root -> Tree.assertions ~loc root
      in
      bounds @ tree

let merge ~old_tree ~new_tree =
  let open DR.Syntax in
  Logging.verbose (fun m -> m "OLD TREE:@\n%a" pp old_tree);
  Logging.verbose (fun m -> m "NEW TREE:@\n%a" pp new_tree);
  if is_empty old_tree then DR.ok new_tree
  else if is_empty new_tree then DR.ok old_tree
  else
    match (old_tree, new_tree) with
    | Freed, _ | _, Freed ->
        failwith "merging a non-empty tree with a freed block"
    | Tree new_tree, Tree old_tree ->
        let def_bounds =
          match new_tree.bounds with
          | Some bounds -> Some bounds
          | None -> old_tree.bounds
        in
        let rec get_owned_nodes (t : Tree.t) : Tree.t list =
          match t.node with
          | NotOwned Totally -> []
          | NotOwned Partially ->
              let left, right = Option.get t.children in
              get_owned_nodes left @ get_owned_nodes right
          | _ -> [ t ]
        in
        let++ def_root =
          match (old_tree.root, new_tree.root) with
          | None, None -> DR.ok None
          | None, Some d | Some d, None -> DR.ok (Some d)
          | Some d, Some o when Tree.is_empty o -> DR.ok (Some d)
          | Some o, Some d when Tree.is_empty o -> DR.ok (Some d)
          | Some old_root, Some new_root ->
              let new_owned_nodes = get_owned_nodes new_root in
              Logging.verbose (fun fmt ->
                  fmt "There are %d new owned nodes"
                    (List.length new_owned_nodes));
              let++ tree =
                List.fold_left
                  (fun acc (tree_node : Tree.t) ->
                    let** acc = acc in
                    let replace_node _ = DR.ok tree_node in
                    let rebuild_parent = Tree.of_children in
                    let++ _, tree =
                      Tree.frame_range acc ~replace_node ~rebuild_parent
                        tree_node.span
                    in
                    tree)
                  (DR.ok old_root) new_owned_nodes
              in
              Some tree
        in
        Logging.verbose (fun m ->
            m "TREE AFTER MERGE:@\n%a" (Fmt.Dump.option Tree.pp) def_root);
        Tree { bounds = def_bounds; root = def_root }

let substitution ~le_subst ~sval_subst ~svarr_subst t =
  match t with
  | Freed -> Freed
  | Tree { bounds; root } ->
      let bounds = Option.map (Range.substitution ~le_subst) bounds in
      let root =
        Option.map (Tree.substitution ~sval_subst ~le_subst ~svarr_subst) root
      in
      Tree { bounds; root }

module Lift = struct
  open Gillian.Debugger.Utils

  let get_variable
      ~(make_node :
         name:string ->
         value:string ->
         ?children:Variable.t list ->
         unit ->
         Variable.t)
      ~loc
      t : Variable.t =
    match t with
    | Freed -> make_node ~name:loc ~value:"Freed" ()
    | Tree { bounds; root } ->
        let bounds =
          match bounds with
          | None -> make_node ~name:"Bounds" ~value:"Not owned" ()
          | Some bounds ->
              make_node ~name:"Bounds" ~value:""
                ~children:(Range.Lift.as_variables ~make_node bounds)
                ()
        in
        let root =
          match root with
          | None -> make_node ~name:"Tree" ~value:"Not owned" ()
          | Some root -> Tree.Lift.as_variable ~make_node root
        in
        make_node ~name:loc ~value:"Allocated" ~children:[ bounds; root ] ()
end
