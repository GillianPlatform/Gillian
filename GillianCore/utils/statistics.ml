(*************************
 * Timing and Statistics *
 *************************)

open Containers

let active () = !Config.stats || Logging.Mode.enabled ()

let time () = if not (active ()) then 0. else Sys.time ()

let exec_cmds = ref 0

(* Performance statistics *)
let statistics = Hashtbl.create 511

(* Update the value of the fname statistic in the table, or add it if not present *)
let update_statistics (fname : string) (time : float) =
  if not (active ()) then ()
  else if Hashtbl.mem statistics fname then
    let stat = Hashtbl.find statistics fname in
    Hashtbl.replace statistics fname (time :: stat)
  else Hashtbl.add statistics fname [ time ]

let print_statistics () =
  Logging.print_to_all "\n STATISTICS \n ========== \n";

  let keys : SS.t =
    SS.of_list (Hashtbl.fold (fun k _ ac -> k :: ac) statistics [])
  in

  Logging.print_to_all (Printf.sprintf "Executed commands: %d" !exec_cmds);

  (* Process each item in statistics table *)
  SS.iter
    (fun f ->
      let lt = Hashtbl.find statistics f in
      (* Calculate average, min, max *)
      let min = ref infinity in
      let max = ref 0. in
      let tot = ref 0. in
      let avg = ref 0. in
      let std = ref 0. in
      let len = float_of_int (List.length lt) in
      tot :=
        List.fold_left
          (fun ac t ->
            if t < !min then min := t;
            if t > !max then max := t;
            ac +. t)
          0. lt;
      avg := !tot /. len;
      std :=
        (List.fold_left (fun ac t -> ac +. ((!avg -. t) ** 2.)) 0. lt /. len)
        ** 0.5;
      Logging.print_to_all (Printf.sprintf "\t%s" f);
      Logging.print_to_all
        (Printf.sprintf "Tot: %f\tCll: %d\nMin: %f\tMax: %f\nAvg: %f\tStd: %f\n"
           !tot (int_of_float len) !min !max !avg !std))
    keys
