module L = Logging
module Gillian_result = Utils.Gillian_result

let file_name = "./gillian-debugger.log"
let rpc_ref : Debug_rpc.t option ref = ref None

module Public = struct
  module JsonMap = struct
    type t = (string * Yojson.Safe.t) list

    let to_yojson jsonMap : Yojson.Safe.t = `Assoc jsonMap

    let of_yojson (yojson : Yojson.Safe.t) =
      match yojson with
      | `Assoc jsonMap -> Ok jsonMap
      | _ -> Error "Invalid yojson for jsonMap"
  end

  let get_fmt_with_json msgf : string * JsonMap.t =
    let result = ref ("", []) in
    (fun ?(json : JsonMap.t = []) fmt ->
      Format.kasprintf (fun s -> result := (s, json)) fmt)
    |> msgf;
    !result

  exception FailureJson of string * JsonMap.t

  let to_file line =
    let out = open_out_gen [ Open_append; Open_creat ] 0o666 file_name in
    Printf.fprintf out "%s\n" line;
    close_out out

  let enabled () = Option.is_some !rpc_ref
  let should_log_verbose v = !Utils.Config.debug_log_verbose || not v
  let reset () = if Sys.file_exists file_name then Sys.remove file_name else ()

  module Log_event = struct
    let type_ = "log"

    module Payload = struct
      type t = { msg : string; json : JsonMap.t }
      [@@deriving yojson { strict = false }]
    end
  end

  let to_rpc msg json =
    match !rpc_ref with
    | Some rpc -> Debug_rpc.send_event rpc (module Log_event) { msg; json }
    | None -> Lwt.return_unit

  let json_msg_to_file msg json =
    let full_msg =
      match json with
      | _ :: _ ->
          let err_info =
            match List.assoc_opt "backtrace" json with
            | Some (`String bt) ->
                let json_s =
                  json |> JsonMap.to_yojson |> Yojson.Safe.pretty_to_string
                in
                Fmt.str "\n%s\n%s" json_s bt
            | _ -> ""
          in
          msg ^ " (+)" ^ err_info
      | [] -> msg
    in
    to_file full_msg

  let log_async ?(v = false) f =
    if enabled () then
      let msg, json = get_fmt_with_json f in
      let () = json_msg_to_file msg json in
      if should_log_verbose v then to_rpc msg json else Lwt.return_unit
    else Lwt.return_unit

  let log ?v f = log_async ?v f |> ignore

  let show_report ?v id msg =
    log ?v (fun m ->
        let report_json : Yojson.Safe.t =
          match L.Log_queryer.get_report id with
          | Some (content, type_) ->
              `Assoc
                [
                  ("content", Yojson.Safe.from_string content);
                  ("type", `String type_);
                ]
          | None -> `Null
        in
        m ~json:[ ("report", report_json) ] "%s" msg)

  let failwith json_f msg =
    if enabled () then
      let json = json_f () in
      raise @@ FailureJson (msg, json)
    else failwith msg
end

include Public

let setup rpc =
  reset ();
  rpc_ref := Some rpc

let dump_dbg : (unit -> Yojson.Safe.t) option ref = ref None
let set_debug_state_dumper f = dump_dbg := Some f
let reraise exc = Printexc.(raise_with_backtrace exc (get_raw_backtrace ()))

let try' ~name f x =
  let err_json backtrace =
    let name_json =
      match name with
      | Some name -> [ ("dap_cmd", `String name) ]
      | None -> []
    in
    let dbg_json =
      match !dump_dbg with
      | Some dump_dbg -> [ ("debug_state", dump_dbg ()) ]
      | None -> []
    in
    name_json @ dbg_json @ [ ("backtrace", `String backtrace) ]
  in
  let%lwt () =
    match name with
    | Some name -> log_async ~v:true (fun m -> m "%s request received" name)
    | None -> Lwt.return_unit
  in
  try%lwt f x with
  | FailureJson (e, json) ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      let json = err_json @ json in
      log_async (fun m -> m ~json "[Error] %s" e);%lwt
      reraise (Failure e)
  | (Failure e | Invalid_argument e) as err ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      log_async (fun m -> m ~json:err_json "[Error] %s" e);%lwt
      reraise err
  | Not_found ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      log_async (fun m -> m ~json:err_json "[Error] Not found");%lwt
      reraise Not_found
  | Effect.Unhandled _ as e ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      let s = Printexc.to_string e in
      log_async (fun m -> m ~json:err_json "[Error] Unhandled effect\n%s" s);%lwt
      reraise e
  | Gillian_result.Exc.Gillian_error g as e ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      log_async (fun m ->
          m ~json:err_json "[Error] %a" Gillian_result.Error.pp g);%lwt
      reraise e
  | e ->
      let backtrace = Printexc.get_backtrace () in
      let err_json = err_json backtrace in
      let s = Printexc.to_string e in
      log_async (fun m -> m ~json:err_json "[Error] Unhandled exception\n%s" s);%lwt
      reraise e

let set_rpc_command_handler rpc ?name ?(catchall = true) module_ f =
  let f x = if catchall then try' ~name f x else f x in
  Debug_rpc.set_command_handler rpc module_ f
