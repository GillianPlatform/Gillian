module ByFolder = Bulk.Suite.ByFolder (struct
  let max_depth = 2
  let cmd_name = "test262"
  let exec_mode = Utils.Exec_mode.Concrete
end)

type test_type = Positive | Negative [@@deriving show]
type test_mode = NoStrict | OnlyStrict | Both | Raw [@@deriving show]
type run_mode = NonStrict | Strict | Raw

let pp_run_mode fmt = function
  | NonStrict -> Fmt.pf fmt "NonStrict"
  | Strict -> Fmt.pf fmt "Strict"
  | Raw -> Fmt.pf fmt "Raw"

type error_phase = Parse | Early | Resolution | Runtime [@@deriving show]

type possible_err = SyntaxError | ReferenceError | CompileError | Test262Error
[@@deriving show]

type info = {
  tt : test_type;
  tm : test_mode;
  ept : (error_phase * possible_err) option;
  rm : run_mode;
}
[@@deriving show]

type category = run_mode * ByFolder.category

let pp_category fmt (r, c) =
  Fmt.pf fmt "%a - %a" ByFolder.pp_category c pp_run_mode r

let init_suite = ByFolder.init_suite

let harness =
  (* Only load harness on first call *)
  let loaded_harness = ref None in
  fun () ->
    match !loaded_harness with
    | Some s -> s
    | None ->
        let harness = Io_utils.load_file (Io_utils.harness_path ()) ^ "\n\n" in
        loaded_harness := Some harness;
        harness

(* let parse_and_compile code =
    let ( let* ) = Stdlib.Result.bind in
    let* parsed =
      match JS_Parser.parse_string code with
      | Ok p -> Ok p
      | Error (JS_Parser.Error.FlowParser (_, "ReferenceError")) ->
          Error ReferenceError
      | _ -> Error SyntaxError
    in
    try
      let offset_converter = JS_Utils.memoized_offsetchar_to_offsetline code in
      let jsil_prog, _, _ =
        JS2JSIL_Compiler2jsil parsed offset_converter false
      in
      Ok (JSIL2GILil2core_prog jsil_prog)
    with _ -> Error CompileError *)

let beforeEach () =
  ByFolder.beforeEach ();
  let open Js2jsil_lib in
  Hashtbl.reset JS2JSIL_Compiler.cc_tbl;
  Hashtbl.reset JS2JSIL_Compiler.fun_tbl;
  Hashtbl.reset JS2JSIL_Compiler.old_fun_tbl;
  Hashtbl.reset JS2JSIL_Compiler.vis_tbl;
  JS2JSIL_Helpers.reset_generators ();
  JSIL2GIL.reset_generators ();
  Js_generators.reset ()

let search_forward_safe r s : int option =
  try Some (Str.search_forward r s 0) with Not_found -> None

let contains str code =
  Option.is_some (search_forward_safe (Str.regexp_string str) code)

let create_tests source code =
  let tm =
    if contains "[noStrict]" code then NoStrict
    else if contains "[onlyStrict]" code then OnlyStrict
    else if contains "[raw]" code then Raw
    else Both
  in
  let tt = if contains "negative:" code then Negative else Positive in
  let ep =
    match tt with
    | Positive -> None
    | Negative -> (
        assert (
          Option.is_some
            (search_forward_safe
               (Str.regexp {|phase:[ \t]*\(.*\)[ \t]*$|})
               code));
        try
          Some
            (match Str.matched_group 1 code with
            | "parse" -> Parse
            | "early" -> Early
            | "resolution" -> Resolution
            | "runtime" -> Runtime
            | _ -> raise Not_found)
        with _ ->
          failwith
            ("Test262: Malformed test, negative test without error phase at : "
           ^ source))
  in
  let et =
    match tt with
    | Positive -> None
    | Negative -> (
        assert (
          Option.is_some
            (search_forward_safe (Str.regexp {|type:[ \t]*\(.*\)[ \t]*$|}) code));
        try
          let et = Str.matched_group 1 code in
          Some
            (match et with
            | "SyntaxError" -> SyntaxError
            | "ReferenceError" -> ReferenceError
            | "Test262Error" -> Test262Error
            | _ -> raise Not_found)
        with _ ->
          failwith "Test262: Malformed test: Negative test without error type")
  in
  let no_strict_test : info =
    {
      tt;
      tm;
      ept =
        (match tt with
        | Positive -> None
        | Negative -> (
            match (ep, et) with
            | Some ep, Some et -> Some (ep, et)
            | _, _ -> failwith "Test262: Impossible"));
      rm = NonStrict;
    }
  in
  let strict_test = { no_strict_test with rm = Strict } in
  let raw_test = { no_strict_test with rm = Raw } in
  let infos =
    match tm with
    | NoStrict -> [ no_strict_test ]
    | OnlyStrict -> [ strict_test ]
    | Both -> [ no_strict_test; strict_test ]
    | Raw -> [ raw_test ]
  in
  let fold_cat = ByFolder.create_tests source code in
  match fold_cat with
  | [ (_, _, fold) ] -> List.map (fun x -> (source, x, (x.rm, fold))) infos
  | _ -> failwith "Impossible"

let skip_category = function
  | NonStrict, _ -> true
  | Raw, _ -> false
  | Strict, _ -> false

let beforeTest info _ =
  Js_config.use_strict := info.rm = Strict;
  Js_config.js2jsil_harnessing := info.tm != Raw

let does_not_contain_substring s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    false
  with Not_found -> true

let filter_source fp =
  Filename.check_suffix fp "js"
  && List.for_all
       (fun sub -> does_not_contain_substring fp sub)
       (Test262_filtering.tests_to_filter_out ())

let cmd_name = ByFolder.cmd_name
let exec_mode = ByFolder.exec_mode
