(** MCP client and packed tool registry with abstract transport engines (Pure Eio / Unix / SSE). *)

open Types
open Tool
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type mcp_tool_def = {
  name : string;
  description : string;
  schema : yojson_safe; [@key "inputSchema"]
} [@@deriving yojson]

type mcp_client = {
  name : string;
  write_line : string -> unit;
  read_line : unit -> string option;
  close : unit -> unit;
  mutable next_id : int;
  mutex : Mutex.t;
}

type registry = {
  mutable clients : mcp_client list;
}

let global_registry = { clients = [] }

let rec read_response_matching client expected_id =
  match client.read_line () with
  | None -> Error "Connection closed"
  | Some line ->
    if String.trim line = "" then
      read_response_matching client expected_id
    else
      match Yojson.Safe.from_string line with
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception _ ->
        Trace.log "debug" "[MCP stdout] %s" line;
        read_response_matching client expected_id
      | json ->
        let open Yojson.Safe.Util in
        match json |> member "id" with
        | `Int id when id = expected_id -> Ok json
        | _ ->
          (match json |> member "method" |> to_string_option with
           | Some "notifications/message"
           | Some "notifications/log" ->
             let params = json |> member "params" in
             let text = params |> member "text" |> to_string_option |> Option.value ~default:"" in
             let level = params |> member "level" |> to_string_option |> Option.value ~default:"info" in
             Trace.log level "[MCP %s] %s" client.name text
           | _ -> ());
          read_response_matching client expected_id

let make_request id method_name params =
  let assoc = [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_name);
  ] in
  let assoc =
    match params with
    | Some p -> ("params", p) :: assoc
    | None -> assoc
  in
  `Assoc assoc

let make_notification method_name params =
  let assoc = [
    ("jsonrpc", `String "2.0");
    ("method", `String method_name);
  ] in
  let assoc =
    match params with
    | Some p -> ("params", p) :: assoc
    | None -> assoc
  in
  `Assoc assoc

let send_request client method_name params =
  Mutex.lock client.mutex;
  let id = client.next_id in
  client.next_id <- client.next_id + 1;
  let req = make_request id method_name params in
  let req_str = Yojson.Safe.to_string req ^ "\n" in
  try
    client.write_line req_str;
    let res = read_response_matching client id in
    Mutex.unlock client.mutex;
    res
  with
  | Eio.Cancel.Cancelled _ as exn ->
    Mutex.unlock client.mutex;
    raise exn
  | exn ->
    Mutex.unlock client.mutex;
    Error (Printexc.to_string exn)

let send_notification client method_name params =
  Mutex.lock client.mutex;
  let req = make_notification method_name params in
  let req_str = Yojson.Safe.to_string req ^ "\n" in
  try
    client.write_line req_str;
    Mutex.unlock client.mutex
  with
  | Eio.Cancel.Cancelled _ as exn ->
    Mutex.unlock client.mutex;
    raise exn
  | _ ->
    Mutex.unlock client.mutex

let spawn_server_eio ~sw mgr name command args =
  try
    let cmd = command :: args in
    let (stdin_r, stdin_w) = Eio.Process.pipe ~sw mgr in
    let (stdout_r, stdout_w) = Eio.Process.pipe ~sw mgr in
    let proc = Eio.Process.spawn ~sw mgr ~stdin:stdin_r ~stdout:stdout_w cmd in
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:65536 stdout_r in
    let write_line str = Eio.Flow.copy_string str stdin_w in
    let read_line () =
      try Some (Eio.Buf_read.line stdout_buf)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> None
    in
    let close () = try Eio.Process.await proc |> ignore with _ -> () in
    Ok { name; write_line; read_line; close; next_id = 1; mutex = Mutex.create () }
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (Printf.sprintf "Failed to spawn Eio MCP server %s: %s" name (Printexc.to_string exn))

(** Read lines from a raw descriptor, with an optional per-read deadline.

    [input_line] on a buffered channel cannot time out, so a server that
    accepts the connection and then says nothing blocked forever — which
    is what made [caravan mcp add] hang for as long as npx took to fetch
    a package the first time, with no way out but Ctrl-C.  Reading the
    descriptor directly means [Unix.select] can bound the wait; the
    buffering that [input_line] was providing is done here instead.

    [timeout = 0.] blocks, which is what long-lived tool calls want. *)
let make_line_reader ?(timeout = 0.) fd =
  let pending = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let eof = ref false in
  fun () ->
    let rec take () =
      let s = Buffer.contents pending in
      match String.index_opt s '\n' with
      | Some i ->
        Buffer.clear pending;
        Buffer.add_string pending (String.sub s (i + 1) (String.length s - i - 1));
        (* Tolerate CRLF from a server written for Windows. *)
        let line = String.sub s 0 i in
        let len = String.length line in
        Some (if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1) else line)
      | None ->
        if !eof then (if s = "" then None
                      else (Buffer.clear pending; Some s))
        else fill ()
    and fill () =
      let ready =
        timeout <= 0. ||
        (match Unix.select [fd] [] [] timeout with
         | ([], _, _) -> false
         | _ -> true
         | exception Unix.Unix_error (Unix.EINTR, _, _) -> true)
      in
      if not ready then None
      else
        match Unix.read fd chunk 0 (Bytes.length chunk) with
        | 0 -> eof := true; take ()
        | n -> Buffer.add_subbytes pending chunk 0 n; take ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> fill ()
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception _ -> eof := true; take ()
    in
    take ()

(* [Unix.open_process_full cmd env] takes a shell command line and an
   *environment* — this passed [| command; arg1; … |] as the environment,
   so every stdio MCP server was started as `sh -c "<command>"` with no
   arguments and a garbage environment.  `npx -y @modelcontextprotocol/…`
   ran as bare `npx`, which waits for input, which is why adding a server
   hung.  [create_process] takes argv properly and inherits the real
   environment. *)
let spawn_server_unix ?timeout name command args =
  try
    (* Close-on-exec throughout: [create_process] dup2s its three
       descriptors onto the child's 0/1/2, and dup2 clears the flag on the
       copy.  Without it the child also inherits our *write* end of its own
       stdin, never sees EOF when we close ours, and never exits. *)
    let (stdin_r,  stdin_w)  = Unix.pipe ~cloexec:true () in
    let (stdout_r, stdout_w) = Unix.pipe ~cloexec:true () in
    let (stderr_r, stderr_w) = Unix.pipe ~cloexec:true () in
    let argv = Array.of_list (command :: args) in
    let pid =
      Fun.protect
        ~finally:(fun () ->
          List.iter (fun fd -> try Unix.close fd with _ -> ())
            [stdin_r; stdout_w; stderr_w])
        (fun () -> Unix.create_process command argv stdin_r stdout_w stderr_w)
    in
    (* Drain stderr into the trace so a server's diagnostics are not lost
       and cannot fill its pipe and wedge it. *)
    let err_chan = Unix.in_channel_of_descr stderr_r in
    ignore (Thread.create (fun () ->
      try
        while true do
          Trace.log "warn" "[MCP stderr %s] %s" name (input_line err_chan)
        done
      with _ -> (try close_in_noerr err_chan with _ -> ())) ());
    let write_line str =
      let b = Bytes.of_string str in
      let rec go off =
        if off < Bytes.length b then
          go (off + Unix.write stdin_w b off (Bytes.length b - off))
      in
      go 0
    in
    let read_line = make_line_reader ?timeout stdout_r in
    let close () =
      (* Closing stdin is how a stdio MCP server is asked to stop. Give it
         a moment, then insist, so one that ignores EOF cannot wedge us. *)
      List.iter (fun fd -> try Unix.close fd with _ -> ()) [stdin_w; stdout_r];
      let reaped =
        try fst (Unix.waitpid [Unix.WNOHANG] pid) <> 0 with _ -> true
      in
      if not reaped then begin
        let deadline = Unix.gettimeofday () +. 1.0 in
        let rec wait () =
          match Unix.waitpid [Unix.WNOHANG] pid with
          | 0, _ when Unix.gettimeofday () < deadline ->
            Thread.delay 0.02; wait ()
          | 0, _ ->
            (try Unix.kill pid Sys.sigterm with _ -> ());
            (try ignore (Unix.waitpid [] pid) with _ -> ())
          | _ -> ()
          | exception _ -> ()
        in
        wait ()
      end
    in
    Ok { name; write_line; read_line; close; next_id = 1; mutex = Mutex.create () }
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    Error (Printf.sprintf "'%s' not found in PATH" command)
  | exn ->
    Error (Printf.sprintf "Failed to spawn MCP server %s: %s" name (Printexc.to_string exn))

let connect ?mgr ?sw ?timeout name command args =
  let client_res =
    match mgr, sw with
    | Some mgr, Some sw -> spawn_server_eio ~sw mgr name command args
    | _ -> spawn_server_unix ?timeout name command args
  in
  match client_res with
  | Error err -> Error err
  | Ok client ->
    let init_params = `Assoc [
      ("protocolVersion", `String "2024-11-05");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [
        ("name", `String "Caravan");
        ("version", `String Version.v);
      ]);
    ] in
    match send_request client "initialize" (Some init_params) with
    | Error err -> Error (Printf.sprintf "Initialization failed for %s: %s" name err)
    | Ok _res ->
      send_notification client "notifications/initialized" None;
      Ok client

let list_tools client =
  match send_request client "tools/list" (Some (`Assoc [])) with
  | Error err ->
    Trace.log "error" "MCP: failed to list tools for %s: %s" client.name err;
    []
  | Ok json ->
    let open Yojson.Safe.Util in
    try
      let tools_list = json |> member "result" |> member "tools" |> to_list in
      List.filter_map (fun t_json ->
        try Some (mcp_tool_def_of_yojson t_json)
        with _ -> None
      ) tools_list
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
      Trace.log "error" "MCP: error parsing tools for %s: %s" client.name (Printexc.to_string exn);
      []

(** How long a probe waits for the server to say anything.  Generous
    enough for a cold `npx` that has to fetch a package, short enough
    that a wrong command reports rather than hangs. *)
let default_probe_timeout = 45.

(** Start [command], initialise, and list its tools.  Unlike a mounted
    server this gives up: an unresponsive one fails with a message
    instead of blocking the terminal. *)
let probe_server ?mgr ?sw ?(timeout = default_probe_timeout) name command args =
  let started = Unix.gettimeofday () in
  match connect ?mgr ?sw ~timeout name command args with
  | Error err ->
    (* Distinguish "said nothing" from "failed to start": the first wants
       --no-probe, the second wants a corrected command. *)
    if Unix.gettimeofday () -. started >= timeout then
      Error (Printf.sprintf
               "'%s' did not respond within %gs — add it with --no-probe if it \
                is just slow to start" name timeout)
    else Error err
  | Ok client ->
    let tools = list_tools client in
    Ok (client, tools)


let parse_call_response json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Assoc err ->
    let msg =
      match List.assoc_opt "message" err with
      | Some (`String s) -> s
      | _ -> "Unknown error"
    in Error msg
  | _ ->
    match json |> member "result" with
    | `Null -> Error "Empty result from server"
    | result ->
      match result |> member "isError" |> to_bool_option with
      | Some true ->
        let content_list = result |> member "content" |> to_list in
        let text_contents = List.filter_map (fun item ->
          item |> member "text" |> to_string_option
        ) content_list in
        Error (String.concat "\n" text_contents)
      | _ ->
        let content_list = result |> member "content" |> to_list in
        let text_contents = List.filter_map (fun item ->
          item |> member "text" |> to_string_option
        ) content_list in
        Ok (String.concat "\n" text_contents)

let call_tool client original_name args =
  let params = `Assoc [
    ("name", `String original_name);
    ("arguments", args);
  ] in
  match send_request client "tools/call" (Some params) with
  | Error err -> Printf.sprintf "Error calling tool %s: %s" original_name err
  | Ok json ->
    match parse_call_response json with
    | Ok txt -> txt
    | Error err -> Printf.sprintf "Error: %s" err

let make_packed_tool (client : mcp_client) (tool_def : mcp_tool_def) =
  let caravan_name = client.name ^ "_" ^ tool_def.name in
  let module T = struct
    let name = caravan_name
    let aliases = [tool_def.name]
    let description = tool_def.description
    type input = Yojson.Safe.t
    type output = string

    let json_schema () = tool_def.schema
    let parse_args json = Ok json
    let format_output s = s

    let is_mutating = true
    let describe_action _args = Printf.sprintf "Execute MCP tool '%s'" caravan_name

    type _ Effect.t += Exec : input -> output Effect.t
    let execute args = call_tool client tool_def.name args
  end in
  Tool.Tool (module T)

let close_all () =
  List.iter (fun client -> try client.close () with _ -> ()) global_registry.clients;
  global_registry.clients <- []

let () =
  at_exit close_all

let init_mcp_servers ?mgr ?sw configs =
  close_all ();
  let clients = List.filter_map (fun (cfg : Config.mcp_server_config) ->
    Trace.log "info" "MCP: connecting to '%s' (%s %s %s)"
      cfg.name cfg.transport cfg.command (String.concat " " cfg.args);
    match connect ?mgr ?sw cfg.name cfg.command cfg.args with
    | Ok client ->
      Trace.log "info" "MCP: connected to '%s'" cfg.name;
      Some client
    | Error err ->
      Trace.log "error" "MCP: failed to connect to '%s': %s" cfg.name err;
      None
  ) configs in
  global_registry.clients <- clients;
  let all_tools = List.concat_map (fun client ->
    let tools = list_tools client in
    Trace.log "info" "MCP: discovered %d tools from '%s'" (List.length tools) client.name;
    List.map (fun t ->
      let packed = make_packed_tool client t in
      Trace.log "info" "MCP: registered tool %s" (Tool.name_of_packed packed);
      packed
    ) tools
  ) clients in
  all_tools
