module Mode = Mode
module Report = Report

let wrap_up = Reporter.wrap_up

let log lvl msgf =
  if Mode.should_log lvl then
    let report = ReportBuilder.info "" (Debug msgf) () in
    Reporter.log report

let normal msgf = log Normal msgf

let verbose msgf = log Verbose msgf

let tmi msgf = log TMI msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)

let normal_phase = ReportBuilder.start_phase Normal

let verbose_phase = ReportBuilder.start_phase Verbose

let tmi_phase = ReportBuilder.start_phase TMI

let end_phase = ReportBuilder.end_phase

(*$
  let () =
    print_newline ();
    Printf.printf
      "let phaseBefore level phase () = ReportBuilder.start_phase level phase";
    print_newline ();
    Printf.printf "let phaseAfter phase () = ReportBuilder.end_phase phase";
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
        "let withPhase%d level phase f = dec%d ~before:(phaseBefore level \
         phase) ~after:(phaseAfter phase) f"
        i i;
      print_newline ()
    in
    for i = 1 to 5 do
      dec i
    done
*)
let phaseBefore level phase () = ReportBuilder.start_phase level phase

let phaseAfter phase () = ReportBuilder.end_phase phase

let dec1 ~before ~after f =
  let wrapper a1 =
    before ();
    let res = f a1 in
    after ();
    res
  in
  wrapper

let withPhase1 level phase f =
  dec1 ~before:(phaseBefore level phase) ~after:(phaseAfter phase) f

let dec2 ~before ~after f =
  let wrapper a1 a2 =
    before ();
    let res = f a1 a2 in
    after ();
    res
  in
  wrapper

let withPhase2 level phase f =
  dec2 ~before:(phaseBefore level phase) ~after:(phaseAfter phase) f

let dec3 ~before ~after f =
  let wrapper a1 a2 a3 =
    before ();
    let res = f a1 a2 a3 in
    after ();
    res
  in
  wrapper

let withPhase3 level phase f =
  dec3 ~before:(phaseBefore level phase) ~after:(phaseAfter phase) f

let dec4 ~before ~after f =
  let wrapper a1 a2 a3 a4 =
    before ();
    let res = f a1 a2 a3 a4 in
    after ();
    res
  in
  wrapper

let withPhase4 level phase f =
  dec4 ~before:(phaseBefore level phase) ~after:(phaseAfter phase) f

let dec5 ~before ~after f =
  let wrapper a1 a2 a3 a4 a5 =
    before ();
    let res = f a1 a2 a3 a4 a5 in
    after ();
    res
  in
  wrapper

let withPhase5 level phase f =
  dec5 ~before:(phaseBefore level phase) ~after:(phaseAfter phase) f

(*$*)
