class ['s] map =
  object
    inherit ['s] TypeDef__.map

    method visit_'annot _ x = x

    method visit_'label _ x = x
  end

class virtual ['s] reduce =
  object (self)
    inherit ['s] TypeDef__.reduce

    method visit_'annot _ _ = self#zero

    method visit_'label _ _ = self#zero
  end

module Utils = struct
  module SS = Containers.SS

  class list_monoid =
    object
      method private zero = []

      method private plus = ( @ )
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
