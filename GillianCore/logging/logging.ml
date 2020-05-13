module Mode = Mode
module Report = Report

let wrap_up = Reporter.wrap_up

let log lvl ?title ?severity msgf =
  if Mode.should_log lvl then
    let report = ReportBuilder.make ?title ~content:(Debug msgf) ?severity () in
    Reporter.log report

let normal ?title ?severity msgf = log Normal ?title ?severity msgf

let verbose ?title ?severity msgf = log Verbose ?title ?severity msgf

let tmi ?title ?severity msgf = log TMI ?title ?severity msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal ~severity:Error (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)

let normal_phase = ReportBuilder.start_phase Normal

let verbose_phase = ReportBuilder.start_phase Verbose

let tmi_phase = ReportBuilder.start_phase TMI

let end_phase = ReportBuilder.end_phase

(*$
  let () =
    print_newline ();
    Printf.printf
      "let phase_before level ?title ?severity phase () = \
       ReportBuilder.start_phase level ?severity ?title phase";
    print_newline ();
    Printf.printf "let phase_after phase () = ReportBuilder.end_phase phase";
    print_newline ();
    let dec i =
      let args =
        let rec aux = function
          | j when j == i -> Printf.sprintf "a%d" j
          | j -> Printf.sprintf "a%d %s" j @@ aux @@ (j + 1)
        in
        aux 1
      in
      Printf.printf
        "let dec%d ~before ~after f = let wrapper %s = before (); let res = f \
         %s in after (); res in wrapper"
        i args args;
      print_newline ();
      Printf.printf
        "let with_phase%d level ?title ?severity phase f = dec%d \
         ~before:(phase_before level ?title ?severity phase) \
         ~after:(phase_after phase) f"
        i i;
      print_newline ()
    in
    for i = 1 to 5 do
      dec i
    done
*)
let phase_before level ?title ?severity phase () =
  ReportBuilder.start_phase level ?severity ?title phase

let phase_after phase () = ReportBuilder.end_phase phase

let dec1 ~before ~after f =
  let wrapper a1 =
    before ();
    let res = f a1 in
    after ();
    res
  in
  wrapper

let with_phase1 level ?title ?severity phase f =
  dec1
    ~before:(phase_before level ?title ?severity phase)
    ~after:(phase_after phase) f

let dec2 ~before ~after f =
  let wrapper a1 a2 =
    before ();
    let res = f a1 a2 in
    after ();
    res
  in
  wrapper

let with_phase2 level ?title ?severity phase f =
  dec2
    ~before:(phase_before level ?title ?severity phase)
    ~after:(phase_after phase) f

let dec3 ~before ~after f =
  let wrapper a1 a2 a3 =
    before ();
    let res = f a1 a2 a3 in
    after ();
    res
  in
  wrapper

let with_phase3 level ?title ?severity phase f =
  dec3
    ~before:(phase_before level ?title ?severity phase)
    ~after:(phase_after phase) f

let dec4 ~before ~after f =
  let wrapper a1 a2 a3 a4 =
    before ();
    let res = f a1 a2 a3 a4 in
    after ();
    res
  in
  wrapper

let with_phase4 level ?title ?severity phase f =
  dec4
    ~before:(phase_before level ?title ?severity phase)
    ~after:(phase_after phase) f

let dec5 ~before ~after f =
  let wrapper a1 a2 a3 a4 a5 =
    before ();
    let res = f a1 a2 a3 a4 a5 in
    after ();
    res
  in
  wrapper

let with_phase5 level ?title ?severity phase f =
  dec5
    ~before:(phase_before level ?title ?severity phase)
    ~after:(phase_after phase) f

(*$*)
