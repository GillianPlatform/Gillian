class ['s] endo =
  object
    inherit ['s] TypeDef__.endo
    method visit_'annot _ x = x
    method visit_'label _ x = x
  end

class virtual ['s] reduce =
  object (self)
    inherit ['s] TypeDef__.reduce
    method visit_'annot _ _ = self#zero
    method visit_'label _ _ = self#zero
  end

class ['s] iter =
  object
    inherit ['s] TypeDef__.iter
    method visit_'annot _ _ = ()
    method visit_'label _ _ = ()
  end

module Utils = struct
  module SS = Containers.SS

  class list_monoid =
    object
      method private zero = []
      method private plus = ( @ )
    end

  (** Same as list_monoid but uses [rev_append] as [plus]. Will break any
      order-conservation *)
  class non_ordered_list_monoid =
    object
      method private zero = []
      method private plus = List.rev_append
    end

  class ss_monoid =
    object
      method private zero = SS.empty
      method private plus = SS.union
    end

  class two_list_monoid =
    object
      method private zero = ([], [])
      method private plus (a, b) (c, d) = (a @ c, b @ d)
    end
end

module Collectors = struct
  let pvar_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.Var.Set.empty
      method private plus = Id.Var.Set.union
      method! visit_PVar () x = Id.Var.Set.singleton x
      method! visit_'label () (_ : int) = self#zero
      method! visit_'annot () () = self#zero
    end

  let lvar_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.LVar.Set.empty
      method private plus = Id.LVar.Set.union

      method! visit_ForAll exclude binders e =
        (* Quantified variables need to be excluded *)
        let univ_quant = List.to_seq binders |> Seq.map fst in
        let exclude = Id.LVar.Set.add_seq univ_quant exclude in
        self#visit_expr exclude e

      method! visit_Exists exclude binders e =
        let exist_quants = List.to_seq binders |> Seq.map fst in
        let exclude = Id.LVar.Set.add_seq exist_quants exclude in
        self#visit_expr exclude e

      method! visit_LVar exclude x =
        if not (Id.LVar.Set.mem x exclude) then Id.LVar.Set.singleton x
        else Id.LVar.Set.empty

      method! visit_'label _ (_ : int) = self#zero
      method! visit_'annot _ () = self#zero
    end

  let cloc_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.Loc.Set.empty
      method private plus = Id.Loc.Set.union
      method! visit_Loc () x = Id.Loc.Set.singleton x
      method! visit_'label () (_ : int) = self#zero
      method! visit_'annot () () = self#zero
    end

  let aloc_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.ALoc.Set.empty
      method private plus = Id.ALoc.Set.union
      method! visit_ALoc () x = Id.ALoc.Set.singleton x
      method! visit_'label () (_ : int) = self#zero
      method! visit_'annot () () = self#zero
    end

  let loc_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.Any_loc.Set.empty
      method private plus = Id.Any_loc.Set.union
      method! visit_ALoc () x = Id.Any_loc.Set.singleton (x :> Id.Any_loc.t)
      method! visit_Loc () x = Id.Any_loc.Set.singleton (x :> Id.Any_loc.t)
      method! visit_'label _ (_ : int) = self#zero
      method! visit_'annot _ () = self#zero
    end

  let substitutable_collector =
    object (self)
      inherit [_] reduce
      method private zero = Id.Substable.Set.empty
      method private plus = Id.Substable.Set.union
      method! visit_ALoc () x = Id.Substable.Set.singleton (x :> Id.Substable.t)
      method! visit_LVar () x = Id.Substable.Set.singleton (x :> Id.Substable.t)
      method! visit_'label () (_ : int) = self#zero
      method! visit_'annot () () = self#zero
    end

  let list_collector =
    object (self)
      inherit [_] reduce
      inherit Utils.non_ordered_list_monoid
      method! visit_'label () (_ : int) = self#zero
      method! visit_'annot () () = self#zero

      method! visit_LList () ls =
        [ TypeDef__.EList (List.map (fun x -> TypeDef__.Lit x) ls) ]

      method! visit_EList () le = [ TypeDef__.EList le ]

      method! visit_NOp () nop les =
        match nop with
        | LstCat -> les
        | _ -> []
    end
end

module Substs = struct
  class subst_clocs subst =
    object
      inherit [_] endo as super

      method! visit_expr () e =
        match e with
        | Lit (Loc loc) -> subst loc
        | _ -> super#visit_expr () e

      method! visit_'annot () () = ()
      method! visit_'label () (x : int) = x
    end
end
