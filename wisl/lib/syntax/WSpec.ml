open VisitorUtils

type t = {
  pre : WLAssert.t;
  post : WLAssert.t;
  existentials : (string * string list) option;
  spid : int;
  fname : string;
  (* name of the function *)
  fparams : string list;
  (* parameters of the function *)
  sploc : CodeLoc.t;
}

let get_id spec = spec.spid
let get_pre spec = spec.pre
let get_post spec = spec.post
let get_loc spec = spec.sploc

let get_by_id id spec =
  let lassert_getter = WLAssert.get_by_id id in
  let self_or_none = if get_id spec = id then `WSpec spec else `None in
  self_or_none |>> (lassert_getter, spec.pre) |>> (lassert_getter, spec.post)

let make ?existentials pre post fname fparams loc =
  {
    pre;
    post;
    spid = Generators.gen_id ();
    sploc = loc;
    fname;
    fparams;
    existentials;
  }
