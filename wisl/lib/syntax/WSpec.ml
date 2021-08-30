open VisitorUtils

type rt = RNormal | RError

type kind = Correctness | Incorrectness

type t = {
  pre : WLAssert.t;
  posts : WLAssert.t list;
  return_mode : rt;
  existentials : (string * string list) option;
  spid : int;
  fname : string;
  (* name of the function *)
  fparams : string list;
  (* parameters of the function *)
  sploc : CodeLoc.t;
  kind : kind;
}

let get_id spec = spec.spid

let get_pre spec = spec.pre

let get_posts spec = spec.posts

let get_loc spec = spec.sploc

let get_by_id id spec =
  let lassert_getter = WLAssert.get_by_id id in
  let stmt_list_visitor = list_visitor_builder WLAssert.get_by_id id in
  let self_or_none = if get_id spec = id then `WSpec spec else `None in
  self_or_none |>> (lassert_getter, spec.pre) |>> (stmt_list_visitor, spec.posts)

let make ~kind ?existentials pre posts return_mode fname fparams loc =
  {
    pre;
    posts;
    return_mode;
    spid = Generators.gen_id ();
    sploc = loc;
    fname;
    fparams;
    existentials;
    kind;
  }
