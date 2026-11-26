(** JSIL Symbolic Heap *)

open Gillian.Gil_syntax
open Javert_utils
module SSubst = Gillian.Symbolic.Subst
module SMap = Gillian.Utils.Prelude.Map.Make (Id.Any_loc)
module Locs = Id.Any_loc.Set

type s_object = (SFVL.t * Expr.t option) * Expr.t option [@@deriving yojson]

(** A symbolic heap is an immutable map from location names to objects. An
    object is a field-value list, an optional domain (an over-approximation of
    the set of fields the object may have) and an optional metadata location. *)
type t = s_object SMap.t [@@deriving yojson]

(*************************************)
(** Symbolic heap functions **)

(*************************************)

(** Returns an empty symbolic heap *)
let init () : t = SMap.empty

(** Symbolic heap read heap(loc) *)
let get (heap : t) (loc : Id.Any_loc.t) : s_object option =
  SMap.find_opt loc heap

(** Symbolic heap read heap(loc) with the normal new obj default *)
let get_with_default (heap : t) (loc : Id.Any_loc.t) : s_object =
  Option.value ~default:((SFVL.empty, None), None) (get heap loc)

(** Symbolic heap set heap(loc) is assigned to fv_list *)
let set
    (heap : t)
    (loc : Id.Any_loc.t)
    (fv_list : SFVL.t)
    (dom : Expr.t option)
    (metadata : Expr.t option) : t =
  SMap.add loc ((fv_list, dom), metadata) heap

(** Symbolic heap put heap(loc, field) is assigned to value *)
let set_fv_pair
    (heap : t)
    (loc : Id.Any_loc.t)
    (field : Expr.t)
    (value : Expr.t) : t =
  let (fv_list, dom), metadata = get_with_default heap loc in
  set heap loc (SFVL.add field value fv_list) dom metadata

let init_object
    (heap : t)
    (loc : Id.Any_loc.t)
    ?is_empty:(ie = false)
    (mtdt : Expr.t option) : t =
  if SMap.mem loc heap then raise (Failure "Illegal init_object")
  else
    let dom : Expr.t option = if ie then None else Some (ESet []) in
    set heap loc SFVL.empty dom mtdt

let has_loc (heap : t) (loc : Id.Any_loc.t) : bool = SMap.mem loc heap

(** Removes the object associated with --loc-- in --heap-- *)
let remove (heap : t) (loc : Id.Any_loc.t) : t = SMap.remove loc heap

(** Retrieves the domain of --heap-- *)
let domain (heap : t) : Locs.t =
  SMap.fold (fun loc _ acc -> Locs.add loc acc) heap Locs.empty

let merge_loc (heap : t) (new_loc : Id.Any_loc.t) (old_loc : Id.Any_loc.t) : t =
  let (old_fvl, old_dom), old_met = get_with_default heap old_loc in
  let merged =
    match get heap new_loc with
    | None -> ((old_fvl, old_dom), old_met)
    | Some ((new_fvl, new_dom), new_met) ->
        (* Merge field-value lists, with the new location taking precedence *)
        let fvl = SFVL.union new_fvl old_fvl in
        let dom =
          match (old_dom, new_dom) with
          | None, None -> None
          | None, Some dom | Some dom, None -> Some dom
          | Some dom1, Some dom2 -> Some (Expr.NOp (SetUnion, [ dom1; dom2 ]))
        in
        let met =
          match (old_met, new_met) with
          | None, None -> None
          | None, Some met | Some met, None -> Some met
          | Some met1, Some _ -> Some met1
        in
        ((fvl, dom), met)
  in
  SMap.add new_loc merged (SMap.remove old_loc heap)

(** Returns subst(heap) *)
let substitution (subst : SSubst.t) (heap : t) : t =
  (* If the substitution is empty, there is nothing to be done *)
  if SSubst.domain subst None = Expr.Set.empty then heap
  else
    let le_subst = SSubst.subst_in_expr subst ~partial:true in
    let heap =
      SMap.map
        (fun ((fv_list, dom), met) ->
          ( (SFVL.substitution subst true fv_list, Option.map le_subst dom),
            Option.map le_subst met ))
        heap
    in
    (* Now we need to deal with any substitutions in the locations themselves *)
    let aloc_subst =
      SSubst.filter subst (fun var _ ->
          match var with
          | ALoc _ -> true
          | _ -> false)
    in
    SSubst.fold aloc_subst
      (fun aloc new_loc heap ->
        let aloc =
          match aloc with
          | Expr.ALoc loc -> (loc :> Id.Any_loc.t)
          | _ -> raise (Failure "Impossible by construction")
        in
        let new_loc =
          match (new_loc : Expr.t) with
          | Lit (Loc loc) -> (loc :> Id.Any_loc.t)
          | ALoc loc -> (loc :> Id.Any_loc.t)
          | _ ->
              raise
                (Failure
                   (Printf.sprintf "Heap substitution fail for loc: %s"
                      ((Fmt.to_to_string Expr.pp) new_loc)))
        in
        merge_loc heap new_loc aloc)
      heap

(** Returns the serialization of --heap-- as a list *)
let to_list (heap : t) : (Id.Any_loc.t * s_object) list =
  SMap.fold (fun loc obj ac -> (loc, obj) :: ac) heap []

(** converts a symbolic heap to a list of assertions *)
let assertions (heap : t) : Asrt.t =
  let assertions_of_object (loc, ((fv_list, domain), metadata)) =
    let le_loc = Expr.loc_from_loc_name loc in
    let fv_assertions = SFVL.assertions le_loc fv_list in
    let domain =
      Option.fold
        ~some:(fun domain -> [ Asrt_utils.empty_fields ~loc:le_loc ~domain ])
        ~none:[] domain
    in
    let metadata =
      match metadata with
      | Some metadata -> [ Asrt_utils.metadata ~loc:le_loc ~metadata ]
      | None -> []
    in
    fv_assertions @ domain @ metadata
  in

  to_list heap |> List.concat_map assertions_of_object |> List.sort Asrt.compare

let wf_assertions_of_obj (heap : t) (loc : Id.Any_loc.t) : Expr.t list =
  let (fv_list, _), _ = get_with_default heap loc in
  let cfvl, sfvl =
    SFVL.partition
      (fun prop value -> Expr.is_concrete value && Expr.is_concrete prop)
      fv_list
  in
  let cpps = SFVL.field_names cfvl in
  let spps = SFVL.field_names sfvl in
  let props = List_utils.cross_product spps (cpps @ spps) (fun x y -> (x, y)) in
  let props = List.filter (fun (x, y) -> x <> y) props in
  List.map (fun (x, y) : Expr.t -> UnOp (Not, BinOp (x, Equal, y))) props

let wf_assertions (heap : t) : Expr.t list =
  SMap.fold (fun loc _ ac -> wf_assertions_of_obj heap loc @ ac) heap []

let pp ft heap =
  let open Fmt in
  let pp_one ft (loc, ((fv_pairs, domain), metadata)) =
    pf ft "@[%a |-> [ @[%a@] | @[%a@] ] with metadata %a@]" Id.pp loc SFVL.pp
      fv_pairs (option Expr.pp) domain
      (option ~none:(any "unknown") Expr.pp)
      metadata
  in
  (list ~sep:(any "@\n") pp_one) ft (List.rev (to_list heap))

let get_print_info locs heap =
  let domain = domain heap in
  let metadata_locs =
    Locs.fold
      (fun loc locs ->
        let add x = if Locs.mem x domain then Locs.add x locs else locs in
        match get heap loc with
        | Some (_, Some (Lit (Loc x))) -> add (x :> Id.Any_loc.t)
        | Some (_, Some (ALoc x)) -> add (x :> Id.Any_loc.t)
        | _ -> locs)
      locs Locs.empty
  in
  (* TODO: Traverse locations and collect info about other locations and lvars *)
  (LVar.Set.empty, metadata_locs)

let pp_by_need locs ft heap =
  let existent_locs = Locs.inter locs (domain heap) in
  let sorted_locs_with_vals =
    List.map
      (fun loc -> (loc, Option.get (get heap loc)))
      (Locs.elements existent_locs)
  in
  let open Fmt in
  let pp_one ft (loc, ((fv_pairs, domain), metadata)) =
    pf ft "@[%a |-> [ @[%a@] | @[%a@] ] with metadata %a@]" Id.pp loc SFVL.pp
      fv_pairs (option Expr.pp) domain
      (option ~none:(any "unknown") Expr.pp)
      metadata
  in
  (list ~sep:(any "@\n") pp_one) ft sorted_locs_with_vals

(** Maps metadata expressions back to the locations they are the metadata of *)
let get_inv_metadata (heap : t) : Expr.t Expr.Map.t =
  SMap.fold
    (fun loc (_, met) inv_metadata ->
      match met with
      | None -> inv_metadata
      | Some e_metadata ->
          Expr.Map.add e_metadata (Expr.loc_from_loc_name loc) inv_metadata)
    heap Expr.Map.empty

let lvars (heap : t) : LVar.Set.t =
  let of_opt oe = Option.fold ~some:Expr.lvars ~none:LVar.Set.empty oe in
  SMap.fold
    (fun _ ((fv_list, dom), met) acc ->
      LVar.Set.union acc
        (LVar.Set.union (SFVL.lvars fv_list)
           (LVar.Set.union (of_opt dom) (of_opt met))))
    heap LVar.Set.empty

let alocs (heap : t) : ALoc.Set.t =
  let of_opt oe = Option.fold ~some:Expr.alocs ~none:ALoc.Set.empty oe in
  SMap.fold
    (fun _ ((fv_list, dom), met) acc ->
      ALoc.Set.union acc
        (ALoc.Set.union (SFVL.alocs fv_list)
           (ALoc.Set.union (of_opt dom) (of_opt met))))
    heap ALoc.Set.empty
