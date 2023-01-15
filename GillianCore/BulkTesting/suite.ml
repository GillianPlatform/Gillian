module type S = sig
  type info

  val pp_info : Format.formatter -> info -> unit

  type category

  val pp_category : Format.formatter -> category -> unit
  val skip_category : category -> bool
  val filter_source : string -> bool
  val create_tests : string -> string -> (string * info * category) list
  val beforeTest : info -> string -> unit
  val beforeEach : unit -> unit
  val init_suite : string list -> unit
  val cmd_name : string
  val exec_mode : Exec_mode.t
end

module Dummy = struct
  let fail () = failwith "Test Suite non implemented"
  let cmd_name = ""

  type info = unit

  let pp_info _ _ = fail ()

  type category = unit

  let pp_category _ _ = fail ()
  let skip_category _ = fail ()
  let create_tests _ _ = fail ()
  let beforeEach _ = fail ()
  let init_suite _ = fail ()
  let filter_source _ = fail ()
  let beforeTest _ _ = fail ()
  let exec_mode = Exec_mode.Concrete
end

module ByFolder (P : sig
  val max_depth : int
  val cmd_name : string
  val exec_mode : Exec_mode.t
end) =
struct
  (** This defines a very simple way of handling tests.
      There is no information associated to tests, and tests are simply
      categorized by the folder they're in.
      Depth of the categorizing folder is parametrisable *)

  (** This contains an extremely simple way of handling test information.
        There is no info associated to tests, and the category is simply the name of the folder that contains the tests. 
        Depth of the category is parametrisable. *)

  let common_root list_string =
    let common_root2 a b =
      (* This can only cut the dirname, the basename has to stay full *)
      let curr = ref 0 in
      let continue = ref true in
      while !continue do
        try
          let letter_a = a.[!curr] in
          let letter_b = b.[!curr] in
          if Char.equal letter_a letter_b then curr := !curr + 1
          else continue := false
        with Invalid_argument _ -> continue := false
      done;
      String.sub a 0 !curr
    in
    match list_string with
    | [] -> ""
    | a :: _ -> List.fold_left common_root2 a list_string

  let suffix i str = String.sub str i (String.length str - i)

  type info = unit

  let pp_info _ _ = ()

  type category = string

  let pp_category fmt s = Fmt.string fmt s
  let skip_category _ = false
  let max_depth = P.max_depth
  let common_root_length = ref 0

  let init_suite file_list =
    common_root_length := String.length (common_root file_list)

  let prefix_of_depth depth str =
    let reg = Str.regexp_string Filename.dir_sep in
    let len_sep = String.length Filename.dir_sep in
    let curr = ref 0 in
    let count = ref 0 in
    let continue = ref true in
    while !continue do
      try
        let next = Str.search_forward reg str !curr in
        curr := next + len_sep;
        count := !count + 1;
        if !count = depth then continue := false
      with Not_found ->
        curr := String.length str;
        continue := false
    done;
    String.sub str 0 !curr

  let create_tests source _ =
    let remove_common_root = suffix !common_root_length in
    let get_cat = prefix_of_depth max_depth in
    let cut_source = remove_common_root source in
    let dname = Filename.dirname cut_source in
    let cat = get_cat dname in
    [ (cut_source, (), cat) ]

  let filter_source _ = true
  let beforeEach _ = ()
  let beforeTest _ _ = ()
  let cmd_name = P.cmd_name
  let exec_mode = P.exec_mode
end
