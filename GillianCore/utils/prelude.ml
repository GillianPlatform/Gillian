(** @canonical Gillian.Utils.Prelude
  
  Most-used helper functions, [Stdlib] extensions *)

(** [Option] pretty-printer *)
let pp_option pp = Fmt.option ~none:(Fmt.any "None") pp

(** [List] pretty-printer *)
let pp_list ?(sep = Fmt.any ", ") = Fmt.list ~sep

(** Given an [of_yojson] function, converts a string to the desired type *)
let of_yojson_string of_yojson s =
  s |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

(** Extension of Hashtbl with functions to serialize to and deserialize
  from yojson, along with some other helpers
  
  A Hashtbl is a represented as a list of key-value pairs,
  where a key-value pair is list of two elements*)
module Hashtbl = struct
  include Hashtbl

  let singleton k v =
    let tbl = create 1 in
    add tbl k v;
    tbl

  let find_or_else_add tbl k f =
    match find_opt tbl k with
    | Some x -> x
    | None ->
        let v = f () in
        add tbl k v;
        v

  (** Analog to {!List.map} *)
  let map f tbl =
    let tbl' = create (length tbl) in
    iter
      (fun k v ->
        let k', v' = f k v in
        replace tbl' k' v')
      tbl;
    tbl'

  (** Analog to {!List.find_map} *)
  let find_map f tbl =
    let exception Found in
    let result = ref None in
    let aux () =
      Hashtbl.iter
        (fun k v ->
          match f k v with
          | None -> ()
          | x ->
              result := x;
              raise Found)
        tbl
    in
    try
      aux ();
      None
    with Found -> !result

  let rec remove_all tbl e =
    if mem tbl e then (
      remove tbl e;
      remove_all tbl e)
    else ()

  let of_yojson
      (key_of_yojson : Yojson.Safe.t -> ('a, string) result)
      (val_of_yojson : Yojson.Safe.t -> ('b, string) result)
      (yojson : Yojson.Safe.t) : (('a, 'b) t, string) result =
    match yojson with
    | `List lst ->
        let kv_of_yojson kv_yojson =
          match kv_yojson with
          | `List [ k_yojson; v_yojson ] ->
              Result.bind (key_of_yojson k_yojson) (fun k ->
                  val_of_yojson v_yojson |> Result.map (fun v -> (k, v)))
          | _ -> Error "hashtbl_of_yojson: tuple list needed"
        in
        let hashtbl = create 0 in
        List.fold_left
          (fun hashtbl kv_yojson ->
            Result.bind hashtbl (fun hashtbl ->
                kv_of_yojson kv_yojson
                |> Result.map (fun (k, v) ->
                       let () = add hashtbl k v in
                       hashtbl)))
          (Ok hashtbl) lst
    | _ -> Error "hashtbl_of_yojson: list needed"

  let to_yojson
      (key_to_yojson : 'a -> Yojson.Safe.t)
      (val_to_yojson : 'b -> Yojson.Safe.t)
      (hashtbl : ('a, 'b) t) : Yojson.Safe.t =
    let kv_to_yojson kv =
      let k, v = kv in
      `List [ key_to_yojson k; val_to_yojson v ]
    in
    `List (hashtbl |> to_seq |> Seq.map kv_to_yojson |> List.of_seq)
end

(** Extension of Map with functions to serialize to and deserialize from yojson
  
  A Map is a represented as a list of key-value pairs, where a
  key-value pair is list of two elements *)
module Map = struct
  module type OrderedType = sig
    type t [@@deriving yojson]

    include Map.OrderedType with type t := t
  end

  module type S = sig
    include Map.S

    val of_yojson :
      (Yojson.Safe.t -> ('a, string) result) ->
      Yojson.Safe.t ->
      ('a t, string) result

    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  end

  module Make (Ord : OrderedType) = struct
    include Map.Make (Ord)

    let of_yojson
        (val_of_yojson : Yojson.Safe.t -> ('a, string) result)
        (yojson : Yojson.Safe.t) : ('a t, string) result =
      match yojson with
      | `List lst ->
          let kv_of_yojson kv_yojson =
            match kv_yojson with
            | `List [ k_yojson; v_yojson ] ->
                Result.bind (Ord.of_yojson k_yojson) (fun k ->
                    val_of_yojson v_yojson |> Result.map (fun v -> (k, v)))
            | _ -> Error "map_of_yojson: tuple list needed"
          in

          List.fold_left
            (fun map kv_yojson : ('a t, string) result ->
              Result.bind map (fun map ->
                  kv_of_yojson kv_yojson
                  |> Result.map (fun (k, v) -> add k v map)))
            (Ok empty) lst
      | _ -> Error "map_of_yojson: list needed"

    let to_yojson (val_to_yojson : 'a -> Yojson.Safe.t) (map : 'a t) :
        Yojson.Safe.t =
      let kv_to_yojson kv =
        let k, v = kv in
        `List [ Ord.to_yojson k; val_to_yojson v ]
      in
      `List (map |> to_seq |> Seq.map kv_to_yojson |> List.of_seq)
  end
end

(** Represents a set of values, with no duplicates *)
module Hashset = struct
  type 'a t = ('a, unit) Hashtbl.t

  (** Makes an empty [Hashset] *)
  let empty ?(size = 1) () : 'a t = Hashtbl.create size

  (** Checks if an element is in the set *)
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x

  (** Adds an element to the set *)
  let add (h : 'a t) (x : 'a) = Hashtbl.replace h x ()

  (** Removes an element from the set *)
  let remove (h : 'a t) (x : 'a) = Hashtbl.remove h x

  (** Returns the size of the set *)
  let length (h : 'a t) = Hashtbl.length h

  (** Applies [f] to each element of the set *)
  let iter f set = Hashtbl.iter (fun x () -> f x) set

  (** Filters the set in-place *)
  let filter_in_place (h : 'a t) (f : 'a -> bool) =
    Hashtbl.filter_map_inplace (fun x () -> if f x then Some () else None) h

  (** Clones the set *)
  let copy (h : 'a t) = Hashtbl.copy h

  (** Gives a sequence of all items in the set *)
  let to_seq (h : 'a t) : 'a Seq.t = Hashtbl.to_seq_keys h
end

(** Extension of Stack with functions to serialize to and deserialize from yojson
  
  A Stack is a last-in-first-out collection of elements *)
module Stack = struct
  include Stack

  let to_yojson elem_to_yojson (s : 'a t) : Yojson.Safe.t =
    `List (to_seq s |> Seq.map elem_to_yojson |> List.of_seq)
end

(** @canonical Gillian.Utils.Containers.SS *)
module SS = Containers.SS

(** @canonical Gillian.Utils.Containers.SI *)
module SI = Containers.SI

(** @canonical Gillian.Utils.Containers.SN *)
module SN = Containers.SN

(** @canonical Gillian.Utils.Syntaxes *)
module Syntaxes = Syntaxes

(** Converts an [Option] to yojson *)
let opt_to_yojson (to_yojson : 'a -> Yojson.Safe.t) = function
  | None -> `Null
  | Some x -> to_yojson x

(** Converts a list of values to yojson *)
let list_to_yojson (to_yojson : 'a -> Yojson.Safe.t) xs =
  `List (xs |> List.map to_yojson)

let disable_stdout () =
  let outfile = Out_channel.open_bin Filename.null in
  Format.set_formatter_out_channel outfile

let map_fst f (a, b) = (f a, b)
let map_snd f (a, b) = (a, f b)
let concat_map_fst f (a, b) = List.map (fun a' -> (a', b)) (f a)
let concat_map_snd f (a, b) = List.map (fun b' -> (a, b')) (f b)
