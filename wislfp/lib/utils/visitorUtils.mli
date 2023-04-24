val ( |>> ) : ([> `None ] as 'a) -> ('b -> 'a) * 'b -> 'a

val list_visitor_builder :
  (int -> 'a -> ([> `None ] as 'b)) -> int -> 'a list -> 'b

val list_visitor_builder2 :
  (int -> 'a -> ([> `None ] as 'b)) -> int -> ('c * 'a) list -> 'b
