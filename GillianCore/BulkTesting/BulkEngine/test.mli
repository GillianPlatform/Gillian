type ('info, 'category) t = {
  name : string;
  path : string;
  category : 'category;
  info : 'info;
}

val pp : Format.formatter -> ('a, 'b) t -> unit

val make :
  name:string ->
  path:string ->
  category:'category ->
  info:'info ->
  ('info, 'category) t
