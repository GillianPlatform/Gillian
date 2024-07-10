open Simple_smt

type t = {
  solver : solver;
  mutable decls : SS.t list;
  mutable cmd_log : sexp list list;
}

let z3_config =
  [
    ("model", "true");
    ("proof", "false");
    ("unsat_core", "false");
    ("auto_config", "true");
    ("timeout", "30000");
  ]

let cmd ctx s =
  let () = ctx.cmd_log <- (s :: List.hd ctx.cmd_log) :: List.tl ctx.cmd_log in
  ack_command ctx.solver s

let declare_sort' ctx f arity = cmd ctx (declare_sort f arity)

let declare_sort ctx f arity =
  let () = declare_sort' ctx f arity in
  atom f

let declare_fun' ctx f ps r = cmd ctx (declare_fun f ps r)

let declare_fun ctx f ps r =
  let () = declare_fun' ctx f ps r in
  atom f

let declare' ctx f t =
  let d, ds =
    match ctx.decls with
    | [] -> raise Not_found
    | d :: ds -> (d, ds)
  in
  if not (SS.mem f d) then
    let () = ctx.decls <- SS.add f d :: ds in
    cmd ctx (declare f t)

let declare ctx f t =
  let () = declare' ctx f t in
  atom f

let define_fun' ctx f ps r d = cmd ctx (define_fun f ps r d)

let define_fun ctx f ps r d =
  let () = define_fun' ctx f ps r d in
  atom f

let define' ctx f t d = cmd ctx (define f t d)

let define ctx f t d =
  let () = define' ctx f t d in
  atom f

let declare_datatype' ctx name type_params cons =
  cmd ctx (declare_datatype name type_params cons)

let declare_datatype ctx name type_params cons =
  let () = declare_datatype' ctx name type_params cons in
  atom name

let declare_datatypes' ctx s = cmd ctx (declare_datatypes s)

let declare_datatypes ctx s =
  let () = declare_datatypes' ctx s in
  s |> List.map (fun (name, _, _) -> atom name)

let assume ctx s = cmd ctx (assume s)

let push ctx =
  let () = ctx.decls <- SS.empty :: ctx.decls in
  let () = ctx.cmd_log <- [] :: ctx.cmd_log in
  cmd ctx (push 1)

let pop ctx =
  let () = ctx.decls <- List.tl ctx.decls in
  let () = ctx.cmd_log <- List.tl ctx.cmd_log in
  cmd ctx (pop 1)

let dump { cmd_log; _ } =
  cmd_log |> List.rev |> List.concat
  |> Fmt.str "%a" (Fmt.list ~sep:(Fmt.any "\n") Sexplib.Sexp.pp_hum)

let make () =
  let solver = new_solver z3 in
  let ctx = { solver; decls = [ SS.empty ]; cmd_log = [ [] ] } in
  let () =
    z3_config |> List.iter (fun (k, v) -> cmd ctx (set_option (":" ^ k) v))
  in
  ctx
