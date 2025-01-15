module Make (ID : Gillian.General.Init_data.S) :
  Gillian.Concrete.Memory_S with type init_data = ID.t
