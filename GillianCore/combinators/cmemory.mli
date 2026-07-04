module Make (ID : Engine.General.Init_data.S) :
  Engine.Concrete.Memory_S with type init_data = ID.t
