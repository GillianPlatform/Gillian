type ac =
  | SetCell
      (** The [SetCell] action takes three parameters :
      {ul
        {- A location of type [Obj]}
        {- An offset of type [Num]}
        {- A value of any type}
      } 
      It does not return anything, (or rather, it returns an empty list)
  *)
  | GetCell
      (** The [GetCell] action takes two parameters :
      {ul
        {- A location of type [Obj]}
        {- An offset of type [Num]}
      }
      It returns three things :
      {ul
        {- The resolved accessed location (type [Obj])}
        {- The resolved accessed offset at that location (type [Num])}
        {- A value, which is the value at that place in the memory}
      }
  *)
  | RemCell
      (** The [RemCell] action takes two parameters :
      {ul
        {- A location of type [Obj]}
        {- An offset of type [Num]}
      }
      It returns nothing (or rather, it returns an empty list)
  *)
  | Alloc
      (** The [Alloc] action takes one parameter : a size of type [Num]
      It returns two things :
      {ul
        {- The generated location that contains all the created offsets (type [Obj])}
        {- The first offset (usually 0) (type [Num])}
      }
      Note that, the return list in itself is already a pointer from the WISL point of view
  *)
  | Dispose
      (** The [Dispose] action takes on parameter : the location of the block
          It deletes the entire block, and returns nothing (empty list) *)

type ga = Cell

val str_ac : ac -> string

val ac_from_str : string -> ac

val str_ga : ga -> string

val ga_from_str : string -> ga

val ga_to_setter_str : string -> string

val ga_to_getter_str : string -> string

val ga_to_deleter_str : string -> string
