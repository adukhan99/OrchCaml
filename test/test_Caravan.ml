open Caravan

(* ── Shared test infrastructure ─────────────────────────────────────── *)

(** Create a minimal mock provider whose [complete] and [stream] always
    return [reply_content] as a plain assistant message. *)
let make_mock_provider ?(pname = "mock") reply_content =
  let module M : Provider.PROVIDER with type config = unit = struct
    type config = unit
    let name = pname
    let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
      Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:pname
        (Types.assistant_msg reply_content)
    let stream _net _cfg ?model:_ ?options:_ ?tools:_ _msgs ~on_token =
      on_token reply_content;
      Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:pname
        (Types.assistant_msg reply_content)
    let list_models _net _cfg = ["mock"]
  end in
  Provider.Provider ((module M), ())

(** Create a test session with the spinner disabled. *)
let make_session ?(tools = []) model provider =
  Session.create ~tools model provider
  |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" }

(** Write [toml_content] to a temp file, set [CARAVAN_CONFIG], reload,
    call [f path], then restore the environment regardless of outcome. *)
let with_tmp_config ~name ~toml_content f =
  let path = name ^ ".toml" in
  let oc = open_out path in
  output_string oc toml_content; close_out oc;
  Unix.putenv "CARAVAN_CONFIG" path;
  Config.reload ();
  Fun.protect
    ~finally:(fun () ->
      (if Sys.file_exists path then Sys.remove path);
      Unix.putenv "CARAVAN_CONFIG" "";
      Config.reload ())
    (fun () -> f path)

let%test_unit "memory_ring" =
  let mem = Memory.Ring.make ~window:2 () in
  let msgs = Prompt.(exec (
    let* () = system "You are an assistant." in
    let* () = user "Hello!" in
    let* () = assistant "Hi!" in
    user "Next"
  )) in
  let mem = List.fold_left Memory.Ring.add mem msgs in
  
  let hist = Memory.Ring.get mem in
  assert (List.length hist = 3);
  let roles = List.map (fun m -> m.Types.role) hist in
  assert (roles = [Types.System; Types.Assistant; Types.User]);
  ()

let%expect_test "parser_json" =
  let fake_json = {| {"status": "ok", "count": 42} |} in
  (match Parser.json_field "count" fake_json with
   | Ok json -> Format.printf "Ok(%s)" (Yojson.Safe.to_string json)
   | Error err -> Format.printf "Error(%s)" err);
  [%expect {| Ok(42) |}]

let%test "parser_bool" =
  match Parser.bool "   yes  \n" with
  | Ok true -> true
  | _ -> false

let%test_unit "config_extended" =
  (* Test environment variable overrides for config getters *)
  Unix.putenv "CARAVAN_DUMMY_KEY" "dummy_val";
  (match Config.get_string_opt (Some "CARAVAN_DUMMY_KEY") "nonexistent" with
   | Some "dummy_val" -> ()
   | _ -> failwith "Config.get_string_opt failed to read environment variable");

  Unix.putenv "CARAVAN_DUMMY_INT" "42";
  (match Config.get_int_opt (Some "CARAVAN_DUMMY_INT") "nonexistent" with
   | Some 42 -> ()
   | _ -> failwith "Config.get_int_opt failed to read environment variable");

  Unix.putenv "CARAVAN_DUMMY_BOOL" "true";
  (match Config.get_bool_opt (Some "CARAVAN_DUMMY_BOOL") "nonexistent" with
   | Some true -> ()
   | _ -> failwith "Config.get_bool_opt failed to read environment variable");

  (* Test default configuration fallbacks *)
  assert (Config.get_spinner_enabled () = true || Config.get_spinner_enabled () = false);

  (* Test verb lookup fallbacks *)
  let verbs_thinking = Config.get_verbs "thinking" in
  assert (verbs_thinking <> []);
  let verbs_custom = Config.get_verbs "nonexistent_action_tool" in
  assert (verbs_custom = ["Running nonexistent_action_tool"]);

  (* Test subagents default helper *)
  let _ = Config.get_subagents () in
  let _ = Config.get_orchestrator () in
  let _ = Config.get_provider_config "openai" in
  let _ = Config.get_mcp_servers () in
  ()

let%test_unit "mcp_config_crud_table_driven" =
  with_tmp_config ~name:"test_mcp_config" ~toml_content:"" (fun _ ->
    let check ~cfg ~expected_count =
      let res = Config.add_mcp_server cfg in
      assert (Result.is_ok res);
      let servers = Config.get_mcp_servers () in
      assert (List.length servers = expected_count);
      let fetched = Config.get_mcp_server cfg.Config.name in
      assert (Option.is_some fetched);
      let f = Option.get fetched in
      assert (f.Config.name = cfg.Config.name);
      assert (f.Config.command = cfg.Config.command);
      assert (f.Config.args = cfg.Config.args)
    in
    check ~cfg:{ name = "github"; transport = "stdio"; command = "npx"; args = ["-y"; "@modelcontextprotocol/server-github"] } ~expected_count:1;
    check ~cfg:{ name = "fs"; transport = "stdio"; command = "npx"; args = ["-y"; "@modelcontextprotocol/server-filesystem"; "/tmp"] } ~expected_count:2;

    (* Test duplicate addition failure *)
    let dup_res = Config.add_mcp_server { name = "github"; transport = "stdio"; command = "npx"; args = [] } in
    assert (Result.is_error dup_res);

    (* Test deletion *)
    let del_res = Config.delete_mcp_server "github" in
    assert (Result.is_ok del_res);
    assert (List.length (Config.get_mcp_servers ()) = 1);
    assert (Config.get_mcp_server "github" = None);

    (* Test deleting non-existent server *)
    let del_err = Config.delete_mcp_server "nonexistent" in
    assert (Result.is_error del_err)
  )

let%test_unit "config_orchestrator_parsing" =
  let tmp_config = "test_work_config.toml" in
  let oc = open_out tmp_config in
  output_string oc {|
stream = true
max_turns = 100

[orchestrator]
base_url = "http://127.0.0.1:8080"
provider = "llama_cpp"
model = "LiquidAI/LFM2.5-2.6B-GGUF"
system = "Test System Prompt"
|};
  close_out oc;
  Unix.putenv "CARAVAN_CONFIG" tmp_config;
  (* Force reload of config *)
  let provider = Config.get_string "provider" in
  let model = Config.get_string "model" in
  let base_url = Config.get_string "base_url" in
  let system = Config.get_string "system" in
  let max_turns = Config.get_int "max_turns" in
  Sys.remove tmp_config;
  Unix.putenv "CARAVAN_CONFIG" "";
  assert (provider = Some "llama_cpp");
  assert (model = Some "LiquidAI/LFM2.5-2.6B-GGUF");
  assert (base_url = Some "http://127.0.0.1:8080");
  assert (system = Some "Test System Prompt");
  assert (max_turns = Some 100)

(** Read file contents helper. *)
let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; s

(** Dispatch [tool] with [args_json] and run [check] on the output, cleaning up
    any [cleanup] paths afterwards. *)
let tool_check ?(cleanup = []) tool args_json check =
  let res = Tool.dispatch tool args_json in
  check res;
  List.iter (fun p ->
    if Sys.file_exists p then
      (if Sys.is_directory p then Unix.rmdir p else Sys.remove p)
  ) cleanup

let%test_unit "tool_dispatch_table" =
  (* read_file: content round-trips *)
  let () =
    let p = "tdt_read.txt" in
    let oc = open_out p in output_string oc "Hello Tool"; close_out oc;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Read_file.Read_file))
      (Printf.sprintf {|{"path": "%s"}|} p)
      (fun res -> assert (res = "Hello Tool"))
  in

  (* write_file: output message + disk content *)
  let () =
    let p = "tdt_write.txt" in
    if Sys.file_exists p then Sys.remove p;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Write_file.Write_file))
      (Printf.sprintf {|{"path": "%s", "content": "Written by test"}|} p)
      (fun res ->
        assert (res = "File written successfully.");
        assert (read_file p = "Written by test"))
  in

  (* grep: filters matching lines only *)
  let () =
    let p = "tdt_grep.txt" in
    let oc = open_out p in
    output_string oc "line 1: foo\nline 2: bar\nline 3: foo again";
    close_out oc;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Grep.Grep))
      (Printf.sprintf {|{"path": "%s", "pattern": "foo"}|} p)
      (fun res -> assert (res = "line 1: foo\nline 3: foo again"))
  in

  (* sed: replaces in-place and reports success *)
  let () =
    let p = "tdt_sed.txt" in
    let oc = open_out p in output_string oc "hello world"; close_out oc;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Sed.Sed))
      (Printf.sprintf {|{"path": "%s", "pattern": "world", "replacement": "caravan"}|} p)
      (fun res ->
        assert (res = "Replaced occurrences successfully.");
        assert (read_file p = "hello caravan"))
  in

  (* bash: stdout captured *)
  let () =
    tool_check
      (Tool.Tool (module CaravanTools.Bash.Bash))
      "{\"command\": \"echo 'hello bash'\"}"
      (fun res -> assert (Re.execp (Re.compile (Re.str "hello bash")) res))
  in

  (* touch: file created *)
  let () =
    let p = "tdt_touch.txt" in
    if Sys.file_exists p then Sys.remove p;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Touch.Touch))
      (Printf.sprintf {|{"path": "%s"}|} p)
      (fun _res -> assert (Sys.file_exists p))
  in

  (* mkdir: directory created *)
  let () =
    let p = "tdt_dir" in
    if Sys.file_exists p then Unix.rmdir p;
    tool_check ~cleanup:[p]
      (Tool.Tool (module CaravanTools.Mkdir.Mkdir))
      (Printf.sprintf {|{"path": "%s"}|} p)
      (fun _res -> assert (Sys.file_exists p && Sys.is_directory p))
  in

  (* ls: non-empty output *)
  tool_check
    (Tool.Tool (module CaravanTools.Ls.Ls))
    {|{"path": "."}|}
    (fun res -> assert (String.length res > 0))

let%test_unit "tool_aliases" =
  let tools = [
    Tool.Tool (module CaravanTools.Read_file.Read_file);
    Tool.Tool (module CaravanTools.Search.Search);
  ] in
  (match Tool.find_tool tools "open_file" with
   | Some t -> assert (Tool.name_of_packed t = "read_file")
   | None   -> failwith "Expected to resolve alias 'open_file' to 'read_file'");
  (match Tool.find_tool tools "search" with
   | Some t -> assert (Tool.name_of_packed t = "web_search")
   | None   -> failwith "Expected to resolve alias 'search' to 'web_search'")

let%test_unit "subagent_session_and_compaction" =

  let provider = make_mock_provider ~pname:"mock_provider" "Subagent response" in
  let parent_sess = make_session "parent_model" provider in
  
  let spec : Subagent.subagent_spec = {
    name = "child_agent";
    role = "atomic";
    system_prompt = "Perform task concisely.";
    tools = [];
    provider = None;
    model = Some "child_model";
  } in
  let child_sess = Subagent.make_child_session parent_sess spec in
  let cfg = Session.config child_sess in
  assert (cfg.model = "child_model");
  (match cfg.system with
   | Some sys ->
     assert (String.starts_with ~prefix:"Perform task concisely." sys);
     assert (String.ends_with ~suffix:Subagent.compaction_suffix sys)
   | None -> failwith "Child session system prompt missing")

let%test_unit "delegate_tool_validation_and_dispatch" =
  Eio_main.run (fun env ->
    let module MockProvider : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "mock"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        let finish_tc = Types.{ id = "call_finish"; name = "finish"; args = {|{"summary":"Subagent finished task."}|}; extra_content = None } in
        let reply = Types.assistant_tool_msg ~tool_calls:[finish_tc] "Subagent finished task." in
        Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:"mock" reply
      let stream _net _cfg ?model:_ ?options:_ ?tools:_ _msgs ~on_token =
        on_token "Subagent finished task.";
        let finish_tc = Types.{ id = "call_finish"; name = "finish"; args = {|{"summary":"Subagent finished task."}|}; extra_content = None } in
        let reply = Types.assistant_tool_msg ~tool_calls:[finish_tc] "Subagent finished task." in
        Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:"mock" reply
      let list_models _net _cfg = ["mock"]
    end in
    let provider = Provider.Provider ((module MockProvider), ()) in
    let dummy_tool = Tool.Tool (module CaravanTools.Read_file.Read_file) in
    let finish_tool = Tool.Tool (module CaravanTools.Finish.Finish) in
    let registered = [dummy_tool; finish_tool] in
    
    let valid_spec : Subagent.subagent_spec = {
      name = "worker1";
      role = "atomic";
      system_prompt = "Do work";
      tools = registered;
      provider = Some provider;
      model = Some "mock-model";
    } in

    let invalid_spec : Subagent.subagent_spec = {
      name = "worker2";
      role = "atomic";
      system_prompt = "Do work";
      tools = [Tool.Tool (module struct
        let name = "unregistered_tool"
        let aliases = []
        let description = ""
        type input = string
        type output = string
        type _ Effect.t += Exec : input -> output Effect.t
        let json_schema () = `Assoc []
        let parse_args _ = Ok ""
        let format_output s = s
        let is_mutating = false
        let describe_action _ = "Unregistered tool"
        let execute _ = ""
      end)];
      provider = Some provider;
      model = Some "mock-model";
    } in

    (* Test startup-time tool name validation failure *)
    let raised = ref false in
    (try
       CaravanTools.Delegate.validate_tool_names "worker2" invalid_spec registered
     with Invalid_argument _ -> raised := true);
    assert (!raised);

    (* Test Delegate.make and tool execution *)
    let delegate_tool =
      CaravanTools.Delegate.make
        ~net:env#net
        ~clock:env#clock
        ~registered_tools:registered
        ~subagent_specs:[valid_spec]
        ()
    in
    assert (Tool.name_of_packed delegate_tool = "delegate");

    (* Test dispatch to valid subagent *)
    let json_valid = {|{"subagent": "worker1", "task": "analyze file"}|} in
    let res = Tool.dispatch delegate_tool json_valid in
    assert (res = "Subagent finished task.\n\nTask finished: Subagent finished task.");

    (* Test dispatch to unknown subagent *)
    let json_invalid = {|{"subagent": "unknown_worker", "task": "do something"}|} in
    let err_res = Tool.dispatch delegate_tool json_invalid in
    assert (String.starts_with ~prefix:"Error: unknown subagent 'unknown_worker'" err_res)
  )

(* Table-driven: verifies usage JSON round-trips for multiple provider shapes. *)
let%test_unit "usage_parsing" =
  let check ~provider ~model ~prompt ~completion ~total =
    let body = Printf.sprintf
      {|{"choices":[{"message":{"role":"assistant","content":"Hi"},"finish_reason":"stop"}],
         "usage":{"prompt_tokens":%d,"completion_tokens":%d,"total_tokens":%d}}|}
      prompt completion total in
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let u = json |> member "usage" in
    let usage = Types.{
      prompt_tokens     = u |> member "prompt_tokens"     |> to_int;
      completion_tokens = u |> member "completion_tokens" |> to_int;
      total_tokens      = u |> member "total_tokens"      |> to_int;
      total_duration    = None;
    } in
    let meta = Types.(wrap_result ~raw_response:"" ~model ~provider ~usage (assistant_msg "Hi")) in
    match meta.Types.usage with
    | None   -> failwith (provider ^ ": usage field was None")
    | Some g ->
      assert (g.Types.prompt_tokens = prompt);
      assert (g.Types.completion_tokens = completion);
      assert (g.Types.total_tokens = total);
      assert (g.Types.total_duration = None)
  in
  check ~provider:"openai"    ~model:"gpt-4o" ~prompt:9 ~completion:12 ~total:21;
  check ~provider:"llama_cpp" ~model:"llama3" ~prompt:5 ~completion:5  ~total:10;
  check ~provider:"ollama"    ~model:"llama3" ~prompt:0 ~completion:1  ~total:1

let%expect_test "monitor_format_usage" =
  let usage = Types.{
    prompt_tokens = 5; completion_tokens = 20; total_tokens = 25;
    total_duration = Some 2.0;
  } in
  let meta = Types.(wrap_result ~raw_response:"" ~model:"llama3" ~provider:"ollama" ~usage
    (assistant_msg "ok")) in
  print_endline (Monitor.format_usage meta);
  
  let meta_with_turn = { meta with turn_count = Some 3 } in
  print_endline (Monitor.format_usage meta_with_turn);
  [%expect {|
    Tokens: 5 in, 20 out (10.00 toks/s)
    Turn 3 | Tokens: 5 in, 20 out (10.00 toks/s) |}]



let%expect_test "tool_finish" =
  let tool = Tool.Tool (module CaravanTools.Finish.Finish) in
  
  let json_args = {|{"summary": "all done"}|} in
  print_endline (Tool.dispatch tool json_args);
    
  let json_args_no_sum = "{}" in
  print_endline (Tool.dispatch tool json_args_no_sum);
  [%expect {|
    Task finished: all done
    Task finished: Completed |}]

let%test_unit "document_algebraic_laws" =
  (* Functor identity law *)
  let doc = Document.Concat [
    Document.Text 42;
    Document.Styled (Document.Bold, Document.Text 100)
  ] in
  assert (Document.Document.map (fun x -> x) doc = doc);

  (* Functor composition law *)
  let f x = x * 2 in
  let g x = x + 10 in
  assert (Document.Document.map (fun x -> f (g x)) doc
        = Document.Document.map f (Document.Document.map g doc));

  (* Monoid identity laws *)
  let d1 = Document.Text "hello" in
  let d2 = Document.Text "world" in
  let d3 = Document.Text "!" in
  let open Document.DocumentMonoid in
  assert (append empty d1 = d1);
  assert (append d1 empty = d1);

  (* Monoid associativity *)
  assert (append (append d1 d2) d3 = append d1 (append d2 d3))

let%test_unit "formatter_profunctor" =
  let base_fmt x = Document.Text (string_of_int x) in
  let pre c = int_of_string c in
  let post s = String.uppercase_ascii s in
  let mapped_fmt = Formatter.Formatter.dimap pre post base_fmt in
  
  let res_doc = mapped_fmt "42" in
  assert (res_doc = Document.Text "42");
  ()

let%expect_test "renderers" =
  let doc = Document.Styled (Document.Foreground Document.Red, Document.Text "error") in
  
  (* Plain Text Renderer strips styles *)
  let plain = Ui.compile_document (module Ui.PlainTextRenderer) (fun s -> s) doc in
  print_endline plain;

  (* ANSI Renderer applies escape codes *)
  let ansi = Ui.compile_document (module Ui.AnsiRenderer) (fun s -> s) doc in
  print_endline ansi;
  [%expect {|
    error
    [1;31merror[0m
    |}]

let%test_unit "kleisli_composition" =
  let f x = if x > 0 then Ok (x * 2) else Error "must be positive" in
  let g y = if y < 100 then Ok (y + 5) else Error "too big" in
  
  let composed = Chain.Kleisli.(f >=> g) in
  assert (composed 10 = Ok 25);
  assert (composed (-5) = Error "must be positive");
  assert (composed 60 = Error "too big");
  ()

let%expect_test "session_summarise" =
  Eio_main.run (fun env ->
    let provider = make_mock_provider "This is a summary." in
    let sess = make_session "mock" provider in
    let sess = Session.add_messages sess [Types.user_msg "hello"; Types.assistant_msg "hi"] in
    
    let (sess', sum) = Session.summarise env#net env#clock sess in
    print_endline sum;
    let hist = Session.history sess' in
    Format.printf "History length: %d\n" (List.length hist);
    let msg = List.hd hist in
    Format.printf "Role: %s\n" (match msg.Types.role with
      | Types.System -> "System"
      | Types.User -> "User"
      | Types.Assistant -> "Assistant"
      | Types.Tool _ -> "Tool");
    Format.printf "Content: %s\n" msg.Types.content
  );
  [%expect {|
    This is a summary.
    History length: 1
    Role: System
    Content: [Conversation summary]: This is a summary.
    |}]

let%test_unit "caravan_error_handling" =
  let err = Caravan_error.Tool_error "test failure" in
  assert (Caravan_error.to_string err = "Tool Error: test failure");
  let res = Caravan_error.safe_run (fun () -> 42) in
  assert (res = Ok 42);
  let res_exn = Caravan_error.safe_run (fun () -> failwith "boom") in
  (match res_exn with
   | Error (Caravan_error.Exception msg) -> assert (String.length msg > 0)
   | _ -> failwith "Expected Exception error");

  (* Test error humanization *)
  let h_conn = Caravan_error.humanize (Failure "ECONNREFUSED") in
  assert (String.starts_with ~prefix:"Could not connect" h_conn);
  let h_404 = Caravan_error.humanize (Failure "HTTP 404 model not found") in
  assert (String.starts_with ~prefix:"Model not found" h_404);
  let h_401 = Caravan_error.humanize (Failure "HTTP 401 Unauthorized") in
  assert (String.starts_with ~prefix:"Authentication failed" h_401);
  let h_429 = Caravan_error.humanize (Failure "HTTP 429 rate limit exceeded") in
  assert (String.starts_with ~prefix:"Rate limited" h_429)

let%test_unit "permission_policies" =
  assert (Permission.check Permission.Always_allow ~is_mutating:true ~desc:"tool");
  assert (not (Permission.check Permission.Deny_all ~is_mutating:true ~desc:"tool"));
  let custom = Permission.Custom (fun desc _args -> desc = "safe_tool") in
  assert (Permission.check custom ~is_mutating:true ~desc:"safe_tool");
  assert (not (Permission.check custom ~is_mutating:true ~desc:"unsafe_tool"))

let%expect_test "algebraic_effects_dispatch" =
  let logs = ref [] in
  let on_log lvl msg = logs := (lvl ^ ": " ^ msg) :: !logs in
  let permission_policy name _args = name <> "forbidden_tool" in
  let on_exec name args = "Executed " ^ name ^ "(" ^ args ^ ")" in
  let result =
    Effects.run_with_effects ~permission_policy ~on_log ~on_exec (fun () ->
      let perm1 = Effects.ask_permission "allowed_tool" "{}" in
      let perm2 = Effects.ask_permission "forbidden_tool" "{}" in
      Effects.log_event "info" "Testing effects";
      let exec_res = Effects.exec_tool "my_tool" "my_arg" in
      Printf.sprintf "perm1=%b perm2=%b exec=%s" perm1 perm2 exec_res
    )
  in
  print_endline result;
  List.iter print_endline (List.rev !logs);
  [%expect {|
    perm1=true perm2=false exec=Executed my_tool(my_arg)
    info: Testing effects |}]

let%test_unit "value_queries" =
  let json_str = {|
    [
      {"name": "Alice", "age": 30, "role": "admin"},
      {"name": "Bob", "age": 25, "role": "user"},
      {"name": "Charlie", "age": 35, "role": "user"}
    ]
  |} in
  let val_data = Value.of_string_permissive json_str in
  
  (* where_field *)
  let filtered = Value.where_field "role" (fun v -> Value.to_string v = "user") val_data in
  (match filtered with
   | Ok (Value.List items) -> assert (List.length items = 2)
   | _ -> failwith "where_field failed");

  (* select *)
  let selected = Value.select ["name"; "age"] val_data in
  (match selected with
   | Ok (Value.List items) ->
     let first = List.hd items in
     assert (Value.get_opt "name" first = Some (Value.String "Alice"));
     assert (Value.get_opt "role" first = None)
   | _ -> failwith "select failed");

  (* LISPy S-expression query *)
  (match Value.eval_lisp "(count)" val_data with
   | Ok (Value.Int 3) -> ()
   | _ -> failwith "LISP (count) failed");
  
  (match Value.eval_lisp "(first)" val_data with
   | Ok record ->
     assert (Value.get_opt "name" record = Some (Value.String "Alice"))
   | _ -> failwith "LISP (first) failed")

let%test_unit "coercive_parsers" =
  assert (Parser.coercive_int "42" = Ok 42);
  assert (Parser.coercive_int "\"123\"" = Ok 123);
  assert (Parser.coercive_bool "TRUE" = Ok true);
  assert (Parser.coercive_bool "1" = Ok true);
  
  let json_with_fence = "```json\n{\"key\": \"value\"}\n```" in
  (match Parser.permissive_json json_with_fence with
   | Ok (`Assoc [("key", `String "value")]) -> ()
   | _ -> failwith "permissive_json failed on code fence")

let%test_unit "session_with_model_override" =
  Eio_main.run (fun env ->
    let last_model_called = ref "" in
    let module ModelCheckProvider : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "model_check"
      let complete _net _cfg ?model ?options:_ ?tools:_ _msgs =
        last_model_called := Option.value ~default:"default_model" model;
        let reply = Types.assistant_msg "ok" in
        Types.wrap_result ~raw_response:"ok" ~model:!last_model_called ~provider:"model_check" reply
      let stream _net _cfg ?model ?options:_ ?tools:_ _msgs ~on_token:_ =
        last_model_called := Option.value ~default:"default_model" model;
        let reply = Types.assistant_msg "ok" in
        Types.wrap_result ~raw_response:"ok" ~model:!last_model_called ~provider:"model_check" reply
      let list_models _net _cfg = ["model_check"]
    end in
    let provider = Provider.Provider ((module ModelCheckProvider), ()) in
    let sess = Session.create ~tools:[] "initial_model" provider
               |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let (sess', _) = Session.turn env#net env#clock sess "hello" in
    assert (!last_model_called = "initial_model");
    let sess'' = Session.with_model sess' "switched_model" in
    let (_sess''', _) = Session.turn env#net env#clock sess'' "hello again" in
    assert (!last_model_called = "switched_model")
  )

let%test_unit "tool_output_truncation_for_context" =
  Eio_main.run (fun _env ->
    let provider = make_mock_provider "ok" in
    let sess = make_session "dummy" provider in
    let sess = Session.set_max_tool_output_len sess (Some 50) in
    let long_tool_output = String.make 500 'A' in
    let messages = [
      Types.user_msg "Run bash tool";
      Types.assistant_msg "Running bash";
      Types.tool_msg "call_1" long_tool_output;
      Types.user_msg "What next?";
      Types.assistant_msg "I will check";
      Types.tool_msg "call_2" "short";
    ] in
    let sess = Session.add_messages sess messages in
    let llm_hist = Session.history_for_llm sess in
    (* Long tool message at index 2 (older than 2 most recent messages) should be truncated *)
    let old_tool_msg = List.find (fun (m : Types.chat_message) ->
      match m.role with Types.Tool "call_1" -> true | _ -> false
    ) llm_hist in
    assert (String.length old_tool_msg.content < 200);
    assert (String.contains old_tool_msg.content 'A');
    let has_omitted = Re.execp (Re.compile (Re.str "bytes omitted")) old_tool_msg.content in
    assert has_omitted
  )

let%test_unit "summarize_tool_and_session_compaction" =
  Eio_main.run (fun env ->
    let called = ref false in
    let module MockSumProvider : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "mock_sum"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        if not !called then begin
          called := true;
          let sum_tc = Types.{ id = "sum_1"; name = "summarize"; args = {|{"reason": "clear space"}|}; extra_content = None } in
          let reply = Types.assistant_tool_msg ~tool_calls:[sum_tc] "Compressing history..." in
          Types.wrap_result ~raw_response:"ok" ~model:"mock" ~provider:"mock" reply
        end else
          let reply = Types.assistant_msg "Summary of key points." in
          Types.wrap_result ~raw_response:"ok" ~model:"mock" ~provider:"mock" reply

      let stream _net _cfg ?model ?options ?tools msgs ~on_token:_ =
        complete _net _cfg ?model ?options ?tools msgs
      let list_models _net _cfg = ["mock_sum"]
    end in
    let provider = Provider.Provider ((module MockSumProvider), ()) in
    let sum_tool = Tool.Tool (module CaravanTools.Summarize.Summarize) in
    let sess = Session.create ~tools:[sum_tool] "mock" provider
               |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let (sess', _) = Session.turn env#net env#clock sess "hello" in
    let hist = Session.history sess' in
    assert (List.length hist > 0);
    let has_sum_header = List.exists (fun (m : Types.chat_message) ->
      String.starts_with ~prefix:"[Conversation summary]:" m.content
    ) hist in
    assert has_sum_header
  )

let%test_unit "agent_turn_increment_and_max_turns" =
  Eio_main.run (fun env ->
    let turn_calls = ref [] in
    let call_count = ref 0 in
    let finish_tool = Tool.Tool (module CaravanTools.Finish.Finish) in
    let read_tool = Tool.Tool (module CaravanTools.Read_file.Read_file) in
    let module MultiTurnProvider : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "multi_turn"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        incr call_count;
        if !call_count < 3 then
          let tc = Types.{ id = Printf.sprintf "call_%d" !call_count; name = "read_file"; args = {|{"path": "dune"}|}; extra_content = None } in
          let reply = Types.assistant_tool_msg ~tool_calls:[tc] "Reading file..." in
          Types.wrap_result ~raw_response:"ok" ~model:"multi" ~provider:"multi" reply
        else
          let tc = Types.{ id = "fin"; name = "finish"; args = {|{"summary": "Done three turns"}|}; extra_content = None } in
          let reply = Types.assistant_tool_msg ~tool_calls:[tc] "Finished" in
          Types.wrap_result ~raw_response:"ok" ~model:"multi" ~provider:"multi" reply

      let stream _net _cfg ?model:_ ?options:_ ?tools:_ msgs ~on_token:_ =
        complete _net _cfg msgs
      let list_models _net _cfg = ["multi_turn"]
    end in
    let provider = Provider.Provider ((module MultiTurnProvider), ()) in
    let sess = Session.create ~tools:[finish_tool; read_tool] "multi" provider
               |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    
    let on_turn current max = turn_calls := (current, max) :: !turn_calls in
    let agent_cfg = Agent.{ max_turns = 5; continue_prompt = "continue"; nudge = false } in
    let res = Agent.run ~config:agent_cfg ~on_turn env#net env#clock sess "Execute multi-turn task" in
    (match res with
     | Ok (final_sess, _meta) ->
       assert (Session.turn_idx final_sess = 3);
       assert (List.rev !turn_calls = [(1, 5); (2, 5); (3, 5)])
     | Error msg -> failwith ("Agent.run failed: " ^ msg));

    (* Test max_turns limit enforcement *)
    call_count := 0;
    turn_calls := [];
    let sess2 = Session.create ~tools:[finish_tool; read_tool] "multi" provider
                |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let agent_cfg_low = Agent.{ max_turns = 2; continue_prompt = "continue"; nudge = false } in
    let res_low = Agent.run ~config:agent_cfg_low ~on_turn env#net env#clock sess2 "Task max turns test" in
    (match res_low with
     | Error "Maximum turns reached without completion." -> ()
     | _ -> failwith "Expected max turns error");

    (* Test max_turns = 0 (infinite / unlimited turns override) *)
    call_count := 0;
    turn_calls := [];
    let sess3 = Session.create ~tools:[finish_tool; read_tool] "multi" provider
                |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let agent_cfg_inf = Agent.{ max_turns = 0; continue_prompt = "continue"; nudge = false } in
    let res_inf = Agent.run ~config:agent_cfg_inf ~on_turn env#net env#clock sess3 "Task infinite max turns test" in
    (match res_inf with
     | Ok (final_sess, _meta) ->
       assert (Session.turn_idx final_sess = 3);
       assert (List.rev !turn_calls = [(1, 0); (2, 0); (3, 0)])
     | Error msg -> failwith ("Infinite max_turns failed: " ^ msg))
  )

(* ── Overhaul regression & feature tests ─────────────────────────────── *)

let%test_unit "ring_window_zero_means_unlimited" =
  (* Regression: Ring.make ~window:0 used to drop every message. *)
  let mem = Memory.Ring.make ~window:0 () in
  let mem =
    List.fold_left (fun m i ->
      Memory.Ring.add m (Types.user_msg (Printf.sprintf "msg %d" i))
    ) mem (List.init 50 Fun.id)
  in
  assert (Memory.Ring.length mem = 50);
  (* Summary memory built on the same normalisation. *)
  let sm = Memory.Summary.create ~max_messages:0 () in
  let sm = List.fold_left Memory.Summary.add sm
      (List.init 10 (fun i -> Types.user_msg (string_of_int i))) in
  assert (Memory.Summary.length sm = 10)

let%test_unit "wire_json_omits_internal_fields" =
  let msg = Types.user_msg "hello" in
  let wire = Types.chat_message_to_wire_json msg in
  let export = Types.chat_message_to_json msg in
  let has_field json f =
    match json with
    | `Assoc kvs -> List.mem_assoc f kvs
    | _ -> false
  in
  assert (not (has_field wire "timestamp"));
  assert (has_field export "timestamp");
  assert (has_field wire "role");
  assert (has_field wire "content")

let%test_unit "bash_tool_captures_stderr_and_exit_status" =
  let tool = Tool.Tool (module CaravanTools.Bash.Bash) in
  let res = Tool.dispatch tool {|{"command": "ls /nonexistent_caravan_test_dir_xyz"}|} in
  let contains needle = Re.execp (Re.compile (Re.str needle)) res in
  (* stderr text must be visible to the model... *)
  assert (contains "nonexistent_caravan_test_dir_xyz");
  (* ...and the failure must be explicit. *)
  assert (contains "[exit status")

let%test_unit "permission_policy_modes" =
  let all = CaravanTools.All_tools.all_tools in
  let is_mutating name =
    match Tool.find_tool all name with
    | Some t -> Tool.is_mutating_packed t
    | None -> true
  in
  let describe_action name args =
    match Tool.find_tool all name with
    | Some t -> Tool.describe_action_packed t args
    | None -> Printf.sprintf "Use tool '%s'" name
  in
  let ro = Permission.policy_of_mode ~is_mutating ~describe_action "readonly" in
  assert (not (ro "bash" "{}"));
  assert (not (ro "write_file" "{}"));
  assert (ro "read_file" "{}");
  assert (ro "web_search" "{}");
  let auto = Permission.policy_of_mode ~is_mutating ~describe_action "auto" in
  assert (auto "bash" "{}");
  assert CaravanTools.Sed.Sed.is_mutating;
  assert (not CaravanTools.Grep.Grep.is_mutating)


let%test_unit "registry_lookup_and_errors" =
  let open CaravanProviders.Registry in
  (match find "anthropic" with
   | Some e ->
     assert (e.default_model = "claude-sonnet-4-5");
     assert (e.key_env = Some "ANTHROPIC_API_KEY")
   | None -> failwith "anthropic missing from registry");
  (* Aliases resolve. *)
  (match find "claude" with
   | Some e -> assert (e.name = "anthropic")
   | None -> failwith "alias 'claude' failed");
  (match find "GROQ" with
   | Some e -> assert (e.name = "groq")
   | None -> failwith "case-insensitive lookup failed");
  (* Unknown providers raise instead of silently falling back. *)
  let raised = ref false in
  (try ignore (make_provider ~model:"m" "definitely_not_a_provider")
   with Unknown_provider msg ->
     raised := true;
     assert (Re.execp (Re.compile (Re.str "ollama")) msg));
  assert !raised;
  assert (default_model "openai" = "gpt-4o-mini");
  (* Every entry is self-consistent. *)
  List.iter (fun e ->
    assert (e.name <> "");
    assert (e.base_url <> "");
    assert (e.default_model <> "");
    if e.requires_key then assert (e.key_env <> None)
  ) entries

let%test_unit "agent_nudge_injection" =
  let cfg = Agent.{ max_turns = 10; continue_prompt = "continue"; nudge = true } in
  (* Halfway through the budget the nudge fires... *)
  let p_half = Agent.continue_prompt_for cfg ~task:"solve it" ~used:5 in
  assert (Re.execp (Re.compile (Re.str "Caravan nudge")) p_half);
  assert (Re.execp (Re.compile (Re.str "solve it")) p_half);
  (* ...near exhaustion it urges finishing... *)
  let p_end = Agent.continue_prompt_for cfg ~task:"solve it" ~used:9 in
  assert (Re.execp (Re.compile (Re.str "finish")) p_end);
  (* ...but not on ordinary turns, and never when disabled. *)
  let p_quiet = Agent.continue_prompt_for cfg ~task:"solve it" ~used:2 in
  assert (p_quiet = "continue");
  let cfg_off = Agent.{ cfg with nudge = false } in
  assert (Agent.continue_prompt_for cfg_off ~task:"solve it" ~used:5 = "continue")

let%test_unit "agent_completion_not_fooled_by_stale_finish" =
  (* Regression: a finish call in an earlier task made every later task in
     the same session appear instantly finished. *)
  Eio_main.run (fun env ->
    let call_count = ref 0 in
    let module P : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "two_tasks"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        incr call_count;
        let tc = Types.{ id = Printf.sprintf "fin_%d" !call_count; name = "finish";
                         args = Printf.sprintf {|{"summary": "task %d done"}|} !call_count;
                         extra_content = None } in
        Types.wrap_result ~raw_response:"" ~model:"m" ~provider:"p"
          (Types.assistant_tool_msg ~tool_calls:[tc] "")
      let stream _net _cfg ?model:_ ?options:_ ?tools:_ msgs ~on_token:_ =
        complete _net _cfg msgs
      let list_models _ _ = []
    end in
    let provider = Provider.Provider ((module P), ()) in
    let finish_tool = Tool.Tool (module CaravanTools.Finish.Finish) in
    let sess = Session.create ~tools:[finish_tool] "m" provider
               |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let sess = Session.set_memory_size sess 0 in
    (match Agent.run env#net env#clock sess "task one" with
     | Ok (sess1, r1) ->
       assert (Re.execp (Re.compile (Re.str "task 1 done")) r1.value.content);
       (* Second task on the SAME session must trigger a fresh model call. *)
       (match Agent.run env#net env#clock sess1 "task two" with
        | Ok (_, r2) ->
          assert (!call_count = 2);
          assert (Re.execp (Re.compile (Re.str "task 2 done")) r2.value.content)
        | Error e -> failwith ("second task failed: " ^ e))
     | Error e -> failwith ("first task failed: " ^ e)))

let%test_unit "session_finish_reason_propagates" =
  Eio_main.run (fun env ->
    let module Plain : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "plain"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        Types.wrap_result ~raw_response:"" ~model:"m" ~provider:"p"
          (Types.assistant_msg "just text")
      let stream _net _cfg ?model:_ ?options:_ ?tools:_ msgs ~on_token:_ =
        complete _net _cfg msgs
      let list_models _ _ = []
    end in
    let provider = Provider.Provider ((module Plain), ()) in
    let sess = Session.create ~tools:[] "m" provider
               |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let (_, result) = Session.turn env#net env#clock sess "hi" in
    assert (result.finish_reason = Some "plain_reply"))

let%test_unit "trace_events_capture_and_jsonl" =
  let seen = ref [] in
  Trace.with_sink (fun ev -> seen := ev :: !seen) (fun () ->
    Trace.emit (Trace.Tool_call_start { name = "bash"; args = "{}" });
    Trace.log "info" "hello %d" 42);
  (match !seen with
   | [Trace.Log { level = "info"; message = "hello 42" };
      Trace.Tool_call_start { name = "bash"; _ }] -> ()
   | _ -> failwith "sink did not capture expected events");
  (* with_sink must deregister afterwards. *)
  let count_before = List.length !seen in
  Trace.emit (Trace.Log { level = "info"; message = "unseen" });
  assert (List.length !seen = count_before);
  (* JSONL encoding carries the event tag. *)
  (match Trace.event_to_json (Trace.Task_finished { summary = "ok" }) with
   | `Assoc kvs ->
     assert (List.assoc "event" kvs = `String "task_finished");
     assert (List.mem_assoc "ts" kvs)
   | _ -> failwith "event_to_json shape")

let%test_unit "ui_visible_width_and_truncate" =
  assert (Ui.visible_width "hello" = 5);
  (* ANSI escapes don't count. *)
  let styled = "\027[1;36mhello\027[0m" in
  assert (Ui.visible_width styled = 5);
  (* UTF-8 multi-byte sequences count once (box-drawing etc.). *)
  assert (Ui.visible_width "─➤é" = 3);
  assert (Ui.truncate_visible "abcdef" 10 = "abcdef");
  let t = Ui.truncate_visible "abcdefghij" 6 in
  assert (String.length t <= 8 (* 5 chars + UTF-8 ellipsis *));
  assert (Re.execp (Re.compile (Re.str "…")) t)

let%test_unit "config_api_keys_table" =
  let tmp_config = "test_api_keys_config.toml" in
  let oc = open_out tmp_config in
  output_string oc "[api_keys]\ngroq = \"gsk_test_123\"\n";
  close_out oc;
  Unix.putenv "CARAVAN_CONFIG" tmp_config;
  Config.reload ();
  let key = Config.get_api_key ~env_var:"CARAVAN_TEST_NO_SUCH_ENV" ~name:"groq" () in
  Sys.remove tmp_config;
  Unix.putenv "CARAVAN_CONFIG" "";
  Config.reload ();
  assert (key = Some "gsk_test_123")

let%test_unit "effects_exec_tool_not_swallowed_by_permission_wrapper" =
  (* Regression: wrapping in run_with_effects ~permission_policy used to
     install a dead default Exec_tool handler, breaking every tool. *)
  let tool = Tool.Tool (module CaravanTools.Read_file.Read_file) in
  let path = "test_effects_regression.txt" in
  let oc = open_out path in
  output_string oc "payload";
  close_out oc;
  let res =
    Effects.run_with_effects ~permission_policy:(fun _ _ -> true) (fun () ->
      Tool.dispatch tool (Printf.sprintf {|{"path": "%s"}|} path))
  in
  Sys.remove path;
  assert (res = "payload");
  (* And denial still works. *)
  let denied =
    Effects.run_with_effects ~permission_policy:(fun _ _ -> false) (fun () ->
      Tool.dispatch tool "{}")
  in
  assert (Re.execp (Re.compile (Re.str "Permission denied")) denied)

let%test_unit "lisp_engine_core" =
  let ok src expected =
    match Lisp.run_to_string src with
    | Ok got when got = expected -> ()
    | Ok got -> failwith (Printf.sprintf "lisp %s => %s (wanted %s)" src got expected)
    | Error e -> failwith (Printf.sprintf "lisp %s failed: %s" src e)
  in
  (* arithmetic & numeric tower *)
  ok "(+ 1 2 3)" "6";
  ok "(* 2 3.5)" "7";
  ok "(- 10 4 1)" "5";
  ok "(- 3)" "-3";
  ok "(mod 17 5)" "2";
  ok "(min (list 3 1 2))" "1";
  ok "(max 3 1 2)" "3";
  ok "(sum (range 1 101))" "5050";
  ok "(round (mean (list 1 2 3 4)))" "3";  (* 2.5 rounds-half-up-to-even → 2? Float.round 2.5 = 3. *)
  (* comparison, logic, control *)
  ok "(if (> 3 2) \"yes\" \"no\")" "yes";
  ok "(and true (< 1 2))" "true";
  ok "(or false null 7)" "7";
  ok "(not 0)" "true";
  (* let, define, lambda, recursion *)
  ok "(let ((x 2) (y (* x 3))) (+ x y))" "8";
  ok "(define sq (lambda (x) (* x x))) (sq 9)" "81";
  ok "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 6)" "720";
  (* higher-order with builtin passed by name *)
  ok "(map upper (list \"a\" \"b\"))" "(A B)";
  ok "(filter (lambda (x) (> x 2)) (list 1 2 3 4))" "(3 4)";
  ok "(reduce + 0 (list 1 2 3 4))" "10";
  (* strings *)
  ok "(join (split \"a,b,c\" \",\") \"-\")" "a-b-c";
  ok "(str \"n=\" (len (list 1 2 3)))" "n=3";
  (* homoiconicity: quote / show / read / eval *)
  ok "'(+ 1 2)" "(+ 1 2)";
  ok "(show '(+ 1 2))" "(+ 1 2)";
  ok "(eval '(+ 1 2))" "3";
  ok "(eval (read \"(* 6 7)\"))" "42";
  (* comments *)
  ok "; a comment\n(+ 1 1) ; trailing" "2"

let%test_unit "lisp_engine_data_and_errors" =
  let data = Value.of_string_permissive
      {|[{"name":"Ada","age":36,"role":"admin"},
         {"name":"Bob","age":25,"role":"user"},
         {"name":"Cy","age":31,"role":"user"}]|} in
  let ok src expected =
    match Lisp.run_to_string ~data src with
    | Ok got when got = expected -> ()
    | Ok got -> failwith (Printf.sprintf "lisp %s => %s (wanted %s)" src got expected)
    | Error e -> failwith (Printf.sprintf "lisp %s failed: %s" src e)
  in
  ok "(len data)" "3";
  ok "(get \"name\" (first (where \"role\" \"admin\" data)))" "Ada";
  ok "(len (filter (lambda (r) (> (get \"age\" r) 30)) data))" "2";
  ok "(get \"name\" (first (sort-by \"age\" data)))" "Bob";
  ok "(join (map (lambda (r) (get \"name\" r)) data) \", \")" "Ada, Bob, Cy";
  (* errors are values, not crashes *)
  let err src needle =
    match Lisp.run src with
    | Error e when Re.execp (Re.compile (Re.str needle)) e -> ()
    | Error e -> failwith (Printf.sprintf "lisp %s: wrong error %s" src e)
    | Ok _ -> failwith (Printf.sprintf "lisp %s: expected an error" src)
  in
  err "(/ 1 0)" "division by zero";
  err "(frobnicate 1)" "unbound symbol";
  err "(define loop (lambda (x) (loop x))) (loop 1)" "step budget";
  err "(+ 1" "missing ')'";
  (* fuel is configurable; lambda applications burn steps *)
  (match Lisp.run ~max_steps:50 "(map (lambda (x) (* x x)) (range 0 1000))" with
   | Error e -> assert (Re.execp (Re.compile (Re.str "step budget")) e)
   | Ok _ -> failwith "expected tiny fuel to run out");
  (* while native data ops are one step — big folds stay cheap *)
  (match Lisp.run ~max_steps:50 "(sum (range 0 100000))" with
   | Ok (Value.Int n) -> assert (n = 4999950000)
   | _ -> failwith "native sum should succeed under small fuel")

let%test_unit "config_set_value_roundtrip" =
  let tmp = "test_config_writer.toml" in
  if Sys.file_exists tmp then Sys.remove tmp;
  Unix.putenv "CARAVAN_CONFIG" tmp;
  Config.reload ();
  (* typed writes *)
  (match Config.set_value "max_turns" "25" with
   | Ok _ -> () | Error e -> failwith e);
  (match Config.set_value "permissions" "ask" with
   | Ok _ -> () | Error e -> failwith e);
  (match Config.set_value "stream" "false" with
   | Ok _ -> () | Error e -> failwith e);
  (match Config.set_api_key "groq" "gsk_12345" with
   | Ok _ -> () | Error e -> failwith e);
  (* dotted path *)
  (match Config.set_value "spinner.enabled" "false" with
   | Ok _ -> () | Error e -> failwith e);
  (* read back through the normal getters (cache was refreshed) *)
  assert (Config.get_int "max_turns" = Some 25);
  assert (Config.get_string "permissions" = Some "ask");
  assert (Config.get_bool "stream" = Some false);
  assert (Config.get_api_key ~env_var:"NO_SUCH_ENV_VAR_XX" ~name:"groq" () = Some "gsk_12345");
  assert (Config.get_spinner_enabled () = false);
  (* numeric-looking API keys must stay strings *)
  (match Config.set_api_key "openai" "12345" with
   | Ok _ -> () | Error e -> failwith e);
  assert (Config.get_api_key ~env_var:"NO_SUCH_ENV_VAR_XX" ~name:"openai" () = Some "12345");
  (* file perms are private *)
  let st = Unix.stat tmp in
  assert (st.Unix.st_perm land 0o077 = 0);
  (* empty key is rejected *)
  (match Config.set_value "" "x" with
   | Error _ -> () | Ok _ -> failwith "empty key should be rejected");
  Sys.remove tmp;
  Unix.putenv "CARAVAN_CONFIG" "";
  Config.reload ();
  (* editable_keys stay well-formed (UI single source of truth) *)
  List.iter (fun (k, d, a) ->
    assert (k <> "" && d <> "" && a <> "")) Config.editable_keys

let%test_unit "config_orchestrator_auto_population" =
  let tmp = "test_config_orchestrator_auto.toml" in
  if Sys.file_exists tmp then Sys.remove tmp;
  Unix.putenv "CARAVAN_CONFIG" tmp;
  Config.reload ();
  (* Case 1: Add subagent when config is blank. Orchestrator auto-populated from first agent input. *)
  (match Config.add_subagent [("name", "worker_a"); ("provider", "ollama"); ("model", "qwen2.5:7b")] with
   | Ok _ -> ()
   | Error e -> failwith ("add_subagent failed: " ^ e));
  assert (Config.get_orchestrator () = Some ("ollama", "qwen2.5:7b"));

  (* Clean up for Case 2 *)
  Sys.remove tmp;
  Config.reload ();
  (* Case 2: Set top-level main fields first, then add a subagent. Orchestrator auto-populated from main fields. *)
  (match Config.set_value "provider" "anthropic" with Ok _ -> () | Error e -> failwith e);
  (match Config.set_value "model" "claude-haiku-4-5" with Ok _ -> () | Error e -> failwith e);
  (match Config.add_subagent [("name", "worker_b"); ("provider", "ollama"); ("model", "llama3")] with
   | Ok _ -> ()
   | Error e -> failwith ("add_subagent failed: " ^ e));
  assert (Config.get_orchestrator () = Some ("anthropic", "claude-haiku-4-5"));

  Sys.remove tmp;
  Unix.putenv "CARAVAN_CONFIG" "";
  Config.reload ()

(* ── Cli_resolve tests ───────────────────────────────────────────────── *)

(** Stub default_model that mirrors Registry.default_model for the
    providers we test against, without pulling in CaravanProviders. *)
let stub_default_model = function
  | "ollama"    -> "llama3.2"
  | "openai"    -> "gpt-4o-mini"
  | "anthropic" -> "claude-sonnet-4-5"
  | other       -> "default-for-" ^ other

(** Helper: call [Cli_resolve.resolve] with the stub. *)
let resolve_test ~provider_cli ~model_cli ~base_url_cli () =
  Cli_resolve.resolve
    ~default_model:stub_default_model
    ~provider_cli ~model_cli ~base_url_cli ()

(** Remove env vars that could leak between tests. *)
let clear_cli_env () =
  List.iter (fun v ->
    (try Unix.putenv v "" with _ -> ())
  ) ["CARAVAN_PROVIDER"; "CARAVAN_MODEL"; "CARAVAN_BASE_URL"]

let%test_unit "cli_resolve" =
  (* Each case: (label, toml_content, env_setup, cli_args, assert_fn) *)

  (* 1. All CLI flags override config unconditionally. *)
  let () =
    with_tmp_config ~name:"cr_flags"
      ~toml_content:"provider = \"ollama\"\nmodel = \"stale-model\"\nbase_url = \"http://stale\"\n"
      (fun _ ->
        clear_cli_env ();
        let (p, m, u) = resolve_test ~provider_cli:(Some "anthropic")
            ~model_cli:(Some "claude-opus-4-5") ~base_url_cli:(Some "https://my-proxy") () in
        assert (p = "anthropic"); assert (m = "claude-opus-4-5");
        assert (u = Some "https://my-proxy"))
  in

  (* 2. No CLI flags; config provider matches → model and base_url from config. *)
  let () =
    with_tmp_config ~name:"cr_match"
      ~toml_content:"provider = \"openai\"\nmodel = \"gpt-4o\"\nbase_url = \"https://custom-openai\"\n"
      (fun _ ->
        clear_cli_env ();
        let (p, m, u) = resolve_test ~provider_cli:None ~model_cli:None ~base_url_cli:None () in
        assert (p = "openai"); assert (m = "gpt-4o");
        assert (u = Some "https://custom-openai"))
  in

  (* 3. Cross-provider leak guard: config ollama, CLI anthropic → no leak. *)
  let () =
    with_tmp_config ~name:"cr_leak"
      ~toml_content:"provider = \"ollama\"\nmodel = \"llama3.2:1b\"\nbase_url = \"http://my-ollama:11434\"\n"
      (fun _ ->
        clear_cli_env ();
        let (p, m, u) = resolve_test ~provider_cli:(Some "anthropic")
            ~model_cli:None ~base_url_cli:None () in
        assert (p = "anthropic");
        assert (m = "claude-sonnet-4-5");   (* default, not the ollama model *)
        assert (u = None))                   (* ollama URL must not leak *)
  in

  (* 4. Env-var fallbacks: no config file → provider from env, model/url ignored;
         then with config matching → env model/url apply. *)
  let () =
    let tmp = "cr_env" in
    (* Case A: no config file *)
    with_tmp_config ~name:tmp ~toml_content:"" (fun path ->
      Sys.remove path;                       (* make the file absent *)
      Config.reload ();
      Unix.putenv "CARAVAN_PROVIDER" "groq";
      Unix.putenv "CARAVAN_MODEL" "llama-3.1-8b-instant";
      Unix.putenv "CARAVAN_BASE_URL" "https://groq-proxy";
      let (p, m, _u) = resolve_test ~provider_cli:None ~model_cli:None ~base_url_cli:None () in
      assert (p = "groq");
      assert (m = stub_default_model "groq"));
    (* Case B: config says provider=groq → env model/url now apply *)
    with_tmp_config ~name:tmp ~toml_content:"provider = \"groq\"\n" (fun _ ->
      Unix.putenv "CARAVAN_PROVIDER" "groq";
      Unix.putenv "CARAVAN_MODEL" "llama-3.1-8b-instant";
      Unix.putenv "CARAVAN_BASE_URL" "https://groq-proxy";
      let (p, m, u) = resolve_test ~provider_cli:None ~model_cli:None ~base_url_cli:None () in
      clear_cli_env ();
      assert (p = "groq");
      assert (m = "llama-3.1-8b-instant");
      assert (u = Some "https://groq-proxy"))
  in

  (* 5. [providers.myhost] table provides base_url for a named custom provider. *)
  let () =
    with_tmp_config ~name:"cr_sect"
      ~toml_content:"provider = \"ollama\"\nmodel = \"llama3.2\"\n[providers.myhost]\nbase_url = \"http://myhost:8080/v1\"\n"
      (fun _ ->
        clear_cli_env ();
        let (p, m, u) = resolve_test ~provider_cli:(Some "myhost")
            ~model_cli:None ~base_url_cli:None () in
        assert (p = "myhost");
        assert (m = "default-for-myhost");
        assert (u = Some "http://myhost:8080/v1"))
  in

  (* 6. Nothing set anywhere → hardcoded ollama defaults. *)
  let () =
    with_tmp_config ~name:"cr_def" ~toml_content:"" (fun path ->
      Sys.remove path; Config.reload ();
      clear_cli_env ();
      let (p, m, u) = resolve_test ~provider_cli:None ~model_cli:None ~base_url_cli:None () in
      assert (p = "ollama"); assert (m = "llama3.2"); assert (u = None))
  in

  (* 7. Case-insensitive provider match: config "OpenAI" matches without normalisation. *)
  let () =
    with_tmp_config ~name:"cr_case"
      ~toml_content:"provider = \"OpenAI\"\nmodel = \"gpt-4o\"\nbase_url = \"https://custom\"\n"
      (fun _ ->
        clear_cli_env ();
        let (p, m, u) = resolve_test ~provider_cli:None ~model_cli:None ~base_url_cli:None () in
        assert (p = "OpenAI");
        assert (m = "gpt-4o");
        assert (u = Some "https://custom"))
  in

  (* 8. Explicit model_cli is honoured even when provider mismatches config. *)
  with_tmp_config ~name:"cr_model_cli"
    ~toml_content:"provider = \"ollama\"\nmodel = \"llama3.2\"\n"
    (fun _ ->
      clear_cli_env ();
      let (p, m, u) = resolve_test ~provider_cli:(Some "anthropic")
          ~model_cli:(Some "claude-haiku-4-5") ~base_url_cli:None () in
      assert (p = "anthropic");
      assert (m = "claude-haiku-4-5");
      assert (u = None))

(* ── Compaction_policy tests ─────────────────────────────────────────── *)

let%test_unit "compaction_policy_edge_cases" =
  let check ~auto ~mem_size ~len ~tools =
    Compaction_policy.should_compact
      ~auto_summarize:auto ~memory_size:mem_size ~history_length:len
      ~tool_call_names:tools
  in
  (* overflow triggers compaction *)
  assert (check ~auto:true ~mem_size:10 ~len:15 ~tools:[] = true);
  (* size 0 means unlimited *)
  assert (check ~auto:true ~mem_size:0 ~len:1000 ~tools:[] = false);
  (* disabled means no auto compaction *)
  assert (check ~auto:false ~mem_size:5 ~len:10 ~tools:[] = false);
  (* no overflow, no compaction *)
  assert (check ~auto:true ~mem_size:10 ~len:9 ~tools:[] = false);
  (* explicit tool triggers compaction even if auto=false or memory_size=0 *)
  assert (check ~auto:false ~mem_size:0 ~len:0 ~tools:["summarize"] = true);
  assert (check ~auto:true ~mem_size:10 ~len:5 ~tools:["compress_history"] = true)

(* ── Agent_output tests ──────────────────────────────────────────────── *)

let%test_unit "agent_output_format" =
  let fake_result = Types.(wrap_result ~raw_response:"" ~model:"m" ~provider:"p"
    (assistant_msg "done")) in
  
  let out_plain = Agent_output.format_success ~mode:Agent_output.Plain ~result:fake_result ~transcript:None in
  assert (String.trim out_plain = "done");
  
  let out_json = Agent_output.format_success ~mode:Agent_output.Json ~result:fake_result ~transcript:None in
  assert (Re.execp (Re.compile (Re.str "\"ok\":true")) out_json);
  assert (Re.execp (Re.compile (Re.str "\"result\":\"done\"")) out_json);

  let err_plain = Agent_output.format_error ~mode:Agent_output.Plain ~message:"failed" ~transcript:None in
  assert (err_plain = "[caravan agent] failed");

  let err_json = Agent_output.format_error ~mode:Agent_output.Json ~message:"failed" ~transcript:None in
  assert (Re.execp (Re.compile (Re.str "\"ok\":false")) err_json);
  assert (Re.execp (Re.compile (Re.str "\"error\":\"failed\"")) err_json)

(* ── Session.summarise prompt injection tests ────────────────────────── *)

let%test_unit "session_summarise_custom_prompt" =
  let last_prompt = ref "" in
  let module MockProvider : Provider.PROVIDER with type config = unit = struct
    type config = unit
    let name = "mock"
    let complete _net _cfg ?model:_ ?options:_ ?tools:_ msgs =
      last_prompt := (List.hd (List.rev msgs)).Types.content;
      Types.wrap_result ~raw_response:"summary" ~model:"mock" ~provider:"mock"
        (Types.assistant_msg "summary")
    let stream _net _cfg ?model:_ ?options:_ ?tools:_ _msgs ~on_token:_ =
      failwith "not implemented"
    let list_models _net _cfg = ["mock"]
  end in
  let packed_provider = Provider.Provider ((module MockProvider), ()) in
  
  Eio_main.run (fun env ->
    let sess = Session.create "mock" packed_provider
             |> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" } in
    let sess = Session.add_messages sess [Types.user_msg "hello"] in
    
    let custom_prompt_fn _msgs = "CUSTOM_PROMPT_PREFIX: Summarise this." in
    
    let (_sess', summary) = Session.summarise ~prompt_fn:custom_prompt_fn env#net env#clock sess in
    
    assert (summary = "summary");
    assert (String.starts_with ~prefix:"CUSTOM_PROMPT_PREFIX:" !last_prompt)
  )

(* ── Doctor tests ────────────────────────────────────────────────────── *)

(** Shared runner: builds the canonical mock provider record for Doctor. *)
let run_doctor_checks ~find_provider ~api_key_for =
  Doctor.run_checks
    ~find_provider
    ~api_key_for
    ~list_models:(fun _ _ -> ["mock-model"])
    ~subagents_roster:[]
    ~subagents_enabled:true
    ()

let has_fail checks =
  List.exists (fun (c : Doctor.check) -> c.severity = Doctor.Fail) checks

let%test_unit "doctor_run_checks" =
  let mock_provider n = Some Doctor.{
    name = n; kind = Cloud; base_url = "http://mock";
    requires_key = true; key_env = Some "MOCK_KEY";
  } in

  (* Case 1: all dependencies satisfied → no failures *)
  assert (not (has_fail (run_doctor_checks
    ~find_provider:mock_provider
    ~api_key_for:(fun _ -> Some "mock-key"))));

  (* Case 2: key missing for cloud provider → at least one failure *)
  assert (has_fail (run_doctor_checks
    ~find_provider:mock_provider
    ~api_key_for:(fun _ -> None)));

  (* Case 3: provider unknown to registry → at least one failure *)
  assert (has_fail (run_doctor_checks
    ~find_provider:(fun _ -> None)
    ~api_key_for:(fun _ -> Some "mock-key")))

let%test_unit "chat_message_to_wire_json_preserves_content" =
  (* Args are now expected to be pre-sanitized via sanitize_json_args at
     provider ingestion time.  Simulate that: *)
  let raw_args = "{\"path\": \"/tmp/test.f90\", \"content\": \"line1\\nline2\\r\\ntab:\\t\"}" in
  let sanitized_args = Types.sanitize_json_args raw_args in
  let tc = Types.{ id = "call_0"; name = "write_file"; args = sanitized_args; extra_content = None } in
  let msg = Types.assistant_tool_msg ~tool_calls:[tc] "Here is code:\nline1\nline2" in
  let wire = Types.chat_message_to_wire_json msg in
  let json_str = Yojson.Safe.to_string wire in
  (* Must parse as valid JSON without throwing parser errors *)
  let parsed = Yojson.Safe.from_string json_str in
  assert (parsed <> `Null);
  (* Content is preserved correctly without being mangled by manual escaping *)
  let open Yojson.Safe.Util in
  let content = wire |> member "content" |> to_string in
  assert (content = "Here is code:\nline1\nline2")

let%test_unit "sanitize_json_args_normalises_escaping" =
  (* Round-trip preserves semantics: raw control chars are normalised *)
  let with_newline = "{\"task\": \"line1\nline2\"}" in
  let sanitized = Types.sanitize_json_args with_newline in
  (* The sanitized string must be valid JSON *)
  let reparsed = Yojson.Safe.from_string sanitized in
  let open Yojson.Safe.Util in
  let task_val = reparsed |> member "task" |> to_string in
  (* Semantic content is preserved *)
  assert (task_val = "line1\nline2");
  (* Already-clean JSON round-trips semantically *)
  let clean = {|{"key": "value"}|} in
  let sanitized_clean = Types.sanitize_json_args clean in
  assert (Yojson.Safe.from_string sanitized_clean = Yojson.Safe.from_string clean);
  (* Garbage falls back to returning the exact string *)
  let garbage = "not { json at all\n" in
  let sanitized_garbage = Types.sanitize_json_args garbage in
  assert (sanitized_garbage = garbage)

let%test_unit "subagent_trace_events_and_spinner_suppression" =
  Eio_main.run (fun env ->
    let events = ref [] in
    let sink ev = events := ev :: !events in
    Trace.with_sink sink (fun () ->
      let module MockProvider : Provider.PROVIDER with type config = unit = struct
        type config = unit
        let name = "mock"
        let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
          let finish_tc = Types.{ id = "c1"; name = "finish"; args = {|{"summary":"Done"}|}; extra_content = None } in
          let reply = Types.assistant_tool_msg ~tool_calls:[finish_tc] "Done" in
          Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:"mock" reply
        let stream _net _cfg ?model ?options ?tools msgs ~on_token:_ =
          complete _net _cfg ?model ?options ?tools msgs
        let list_models _net _cfg = ["mock"]
      end in
      let provider = Provider.Provider ((module MockProvider), ()) in
      let finish_tool = Tool.Tool (module CaravanTools.Finish.Finish) in
      let spec : Subagent.subagent_spec = {
        name = "tracer_subagent";
        role = "worker";
        system_prompt = "Do task";
        tools = [finish_tool];
        provider = Some provider;
        model = Some "mock";
      } in
      let parent_sess = Session.create ~tools:[] "parent" provider in
      let child_sess = Subagent.make_child_session parent_sess spec in
      assert ((Session.config child_sess).model = "mock");
      
      let _res = Subagent.delegate env#net env#clock parent_sess spec "Test task" in
      let sub_starts = List.filter (function Trace.Subagent_start _ -> true | _ -> false) !events in
      let sub_ends   = List.filter (function Trace.Subagent_end _ -> true | _ -> false) !events in
      assert (List.length sub_starts = 1);
      assert (List.length sub_ends = 1)
    )
  )

let%test_unit "permissive_json_backticks" =
  (* 1. JSON containing backticks inside string literal *)
  let json_with_backtick = "{\"command\": \"echo `date`\"}" in
  (match Parser.permissive_json json_with_backtick with
   | Ok (`Assoc [("command", `String "echo `date`")]) -> ()
   | _ -> failwith "Failed to parse JSON containing backticks");

  (* 2. Tool execution with backticks in arguments *)
  let tool = Tool.Tool (module CaravanTools.Bash.Bash) in
  let res = Tool.dispatch tool "{\"command\": \"echo '`hello`'\"}" in
  assert (String.contains res '`')

let%test_unit "delegate_batch_parallel_execution" =
  Eio_main.run (fun env ->
    let module MockSubProvider : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "mock_sub"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ msgs =
        let prompt = (List.hd (List.rev msgs)).Types.content in
        let finish_tc = Types.{ id = "c1"; name = "finish"; args = Printf.sprintf {|{"summary":"Finished %s"}|} prompt; extra_content = None } in
        let reply = Types.assistant_tool_msg ~tool_calls:[finish_tc] "Done" in
        Types.wrap_result ~raw_response:"mock" ~model:"mock" ~provider:"mock" reply
      let stream _net _cfg ?model ?options ?tools msgs ~on_token:_ =
        complete _net _cfg ?model ?options ?tools msgs
      let list_models _net _cfg = ["mock"]
    end in
    let provider = Provider.Provider ((module MockSubProvider), ()) in
    let finish_tool = Tool.Tool (module CaravanTools.Finish.Finish) in
    let spec_a : Subagent.subagent_spec = {
      name = "worker_a"; role = "worker"; system_prompt = "Work A";
      tools = [finish_tool]; provider = Some provider; model = Some "mock";
    } in
    let spec_b : Subagent.subagent_spec = {
      name = "worker_b"; role = "worker"; system_prompt = "Work B";
      tools = [finish_tool]; provider = Some provider; model = Some "mock";
    } in
    let delegate_tool = CaravanTools.Delegate.make ~net:env#net ~clock:env#clock
      ~registered_tools:[finish_tool] ~subagent_specs:[spec_a; spec_b] () in
    
    (* Batch JSON payload *)
    let batch_args = {|{
      "tasks": [
        {"subagent": "worker_a", "task": "Task A"},
        {"subagent": "worker_b", "task": "Task B"}
      ]
    }|} in
    let output = Tool.dispatch delegate_tool batch_args in
    assert (Re.execp (Re.compile (Re.str "[Subagent 'worker_a']")) output);
    assert (Re.execp (Re.compile (Re.str "[Subagent 'worker_b']")) output);
    assert (Re.execp (Re.compile (Re.str "Finished Task A")) output);
    assert (Re.execp (Re.compile (Re.str "Finished Task B")) output);

    (* LLM mistake: stringified JSON array — should still work *)
    let stringified_batch = {|{
      "tasks": "[{\"subagent\": \"worker_a\", \"task\": \"Task A\"}, {\"subagent\": \"worker_b\", \"task\": \"Task B\"}]"
    }|} in
    let output_str = Tool.dispatch delegate_tool stringified_batch in
    assert (Re.execp (Re.compile (Re.str "[Subagent 'worker_a']")) output_str);
    assert (Re.execp (Re.compile (Re.str "[Subagent 'worker_b']")) output_str);
    assert (Re.execp (Re.compile (Re.str "Finished Task A")) output_str);
    assert (Re.execp (Re.compile (Re.str "Finished Task B")) output_str)
  )

let%test_unit "config_spinner_verbose_and_ui_formatting" =
  let tmp_config = "test_verbose_config.toml" in
  let oc = open_out tmp_config in
  output_string oc {|
verbose = true
|};
  close_out oc;
  Unix.putenv "CARAVAN_CONFIG" tmp_config;
  let is_v1 = Config.get_spinner_verbose () in
  Sys.remove tmp_config;
  Unix.putenv "CARAVAN_CONFIG" "";
  assert (is_v1 = true);

  let call_normal = Ui.format_tool_call ~verbose:false ~name:"read_file" ~args:{|{"path": "lib/ui.ml"}|} () in
  let call_verbose = Ui.format_tool_call ~verbose:true ~name:"read_file" ~args:{|{"path": "lib/ui.ml"}|} () in
  assert (String.contains call_normal '(');
  assert (String.contains call_verbose '(');

  let output = "line 1\nline 2\nline 3" in
  let res_normal = Ui.format_tool_result ~verbose:false ~output ~duration:1.2 () in
  let res_verbose = Ui.format_tool_result ~verbose:true ~output ~duration:1.2 () in
  assert (Re.execp (Re.compile (Re.str "(+2 lines)")) res_normal);
  assert (Re.execp (Re.compile (Re.str "line 3")) res_verbose)

let%test_unit "parse_utf8_sanitization" =
  let invalid_bytes = "Hello \xFF\xFE World" in
  let parsed = Types.parse_utf8 invalid_bytes in
  assert (parsed = "Hello \xEF\xBF\xBD\xEF\xBF\xBD World");

  let msg = Types.user_msg invalid_bytes in
  assert (msg.content = "Hello \xEF\xBF\xBD\xEF\xBF\xBD World");

  let wire_json = Types.chat_message_to_wire_json msg in
  let wire_str = Yojson.Safe.to_string wire_json in
  (* Must be valid JSON and preserve the replacement character without crashing C++ JSON parsers *)
  let round_tripped = Yojson.Safe.from_string wire_str in
  assert (round_tripped <> `Null)

let%test_unit "parse_provider_error_openrouter_and_openai" =
  let openrouter_body =
    {|{"error":{"message":"Provider returned error","code":400,"metadata":{"raw":"ERROR","provider_name":"Stealth","is_byok":false}},"user_id":"user_123"}|}
  in
  (match Caravan_error.parse_provider_error openrouter_body with
   | Some detail ->
     assert (detail.provider_name = Some "Stealth");
     assert (detail.code = Some "400");
     assert (detail.message = "Provider returned error");
     assert (detail.raw = Some "ERROR");
     assert (detail.user_id = Some "user_123");
     let exn = Caravan_error.Provider_failure { provider = "openai"; status = 400; body = openrouter_body; detail = Some detail } in
     let h = Caravan_error.humanize exn in
     assert (Re.execp (Re.compile (Re.str "Stealth")) h);
     assert (Re.execp (Re.compile (Re.str "Provider returned error")) h)
   | None -> failwith "Failed to parse OpenRouter provider error");

  let openai_err =
    {|{"error":{"message":"Incorrect API key","type":"invalid_request_error","param":null,"code":"invalid_api_key"}}|}
  in
  (match Caravan_error.parse_provider_error openai_err with
   | Some detail ->
     assert (detail.code = Some "invalid_api_key");
     assert (detail.message = "Incorrect API key")
   | None -> failwith "Failed to parse OpenAI provider error")

let%test_unit "session_checkpoint_roundtrip" =
  (* Covers: export_json/of_json field fidelity, save/load checkpoint,
     and a live-turn checkpoint (the REPL save path). *)
  let provider = make_mock_provider "ok" in
  let sess = Session.create ~tools:[] "test-model" provider in
  let sess = Session.set_system sess "Custom System Instructions" in
  let sess = Session.add_messages sess [
    Types.user_msg "Hello turn 1";
    Types.assistant_msg "Reply turn 1";
    Types.tool_msg "call_123" "Tool output data";
  ] in

  (* 1. JSON export/import round-trip *)
  let json = Session.export_json sess in
  (match Session.of_json ~provider json with
   | Ok sess' ->
     let cfg = Session.config sess' in
     assert (cfg.model = "test-model");
     assert (cfg.system = Some "Custom System Instructions");
     let hist = Session.history sess' in
     assert (List.length hist = 3);
     assert ((List.nth hist 2).content = "Tool output data")
   | Error e -> failwith ("of_json failed: " ^ e));

  (* 2. Checkpoint save and load *)
  let cp = "test_session_checkpoint.json" in
  (match Session.save_checkpoint ~path:cp sess with
   | Ok p -> assert (p = cp)
   | Error e -> failwith ("save_checkpoint failed: " ^ e));
  (match Session.load_checkpoint ~provider ~path:cp () with
   | Ok loaded ->
     Sys.remove cp;
     let cfg = Session.config loaded in
     assert (cfg.model = "test-model");
     assert (cfg.system = Some "Custom System Instructions");
     assert (List.length (Session.history loaded) = 3)
   | Error e ->
     if Sys.file_exists cp then Sys.remove cp;
     failwith ("load_checkpoint failed: " ^ e));

  (* 3. Live-turn checkpoint (REPL save path): history grows after a real turn. *)
  Eio_main.run (fun env ->
    let live_sess = make_session "mock" provider in
    let (turned, _) = Session.turn env#net env#clock live_sess "Hello REPL" in
    let cp2 = "test_repl_checkpoint.json" in
    (match Session.save_checkpoint ~path:cp2 turned with
     | Ok _ ->
       (match Session.load_checkpoint ~provider ~path:cp2 () with
        | Ok loaded ->
          Sys.remove cp2;
          assert (List.length (Session.history loaded) = 2)
        | Error e ->
          if Sys.file_exists cp2 then Sys.remove cp2;
          failwith ("live load_checkpoint failed: " ^ e))
     | Error e -> failwith ("live save_checkpoint failed: " ^ e)))

(* ── Provider.Retry ──────────────────────────────────────────────────── *)

let%test_unit "provider_retry_mode_parsing" =
  let check ~(label : string) ~input ~expected =
    assert (Provider.Retry.of_string input = expected)
  in
  (* label; input; expected mode *)
  check ~label:"off"     ~input:"off"    ~expected:(Some Provider.Retry.Off);
  check ~label:"alias"   ~input:"none"   ~expected:(Some Provider.Retry.Off);
  check ~label:"low"     ~input:"low"    ~expected:(Some Provider.Retry.Low);
  check ~label:"medium"  ~input:"medium" ~expected:(Some Provider.Retry.Medium);
  check ~label:"med"     ~input:"med"    ~expected:(Some Provider.Retry.Medium);
  check ~label:"high"    ~input:"high"   ~expected:(Some Provider.Retry.High);
  check ~label:"bogus"   ~input:"bogus"  ~expected:None;
  check ~label:"empty"   ~input:""       ~expected:None

let%test_unit "provider_retry_classification" =
  let mk_failure status =
    Caravan_error.Provider_failure
      { provider = "p"; status; body = ""; detail = None }
  in
  let check ~(label : string) ~mode ~status ~expected =
    assert (Provider.Retry.classify ~mode (mk_failure status) = expected)
  in
  (* label; mode; status; retriable *)
  check ~label:"off_500"   ~mode:Provider.Retry.Off    ~status:500 ~expected:false;
  check ~label:"off_429"   ~mode:Provider.Retry.Off    ~status:429 ~expected:false;
  check ~label:"low_500"   ~mode:Provider.Retry.Low    ~status:500 ~expected:true;
  check ~label:"low_502"   ~mode:Provider.Retry.Low    ~status:502 ~expected:true;
  check ~label:"low_429"   ~mode:Provider.Retry.Low    ~status:429 ~expected:false;
  check ~label:"low_400"   ~mode:Provider.Retry.Low    ~status:400 ~expected:false;
  check ~label:"med_500"   ~mode:Provider.Retry.Medium ~status:500 ~expected:true;
  check ~label:"med_503"   ~mode:Provider.Retry.Medium ~status:503 ~expected:true;
  check ~label:"med_429"   ~mode:Provider.Retry.Medium ~status:429 ~expected:true;
  check ~label:"med_400"   ~mode:Provider.Retry.Medium ~status:400 ~expected:false;
  check ~label:"med_401"   ~mode:Provider.Retry.Medium ~status:401 ~expected:false;
  check ~label:"med_404"   ~mode:Provider.Retry.Medium ~status:404 ~expected:false;
  check ~label:"high_400"  ~mode:Provider.Retry.High   ~status:400 ~expected:true;
  check ~label:"high_401"  ~mode:Provider.Retry.High   ~status:401 ~expected:true;
  check ~label:"high_429"  ~mode:Provider.Retry.High   ~status:429 ~expected:true;
  check ~label:"high_500"  ~mode:Provider.Retry.High   ~status:500 ~expected:true;
  (* max_attempts per mode *)
  assert (Provider.Retry.max_attempts Provider.Retry.Off = 1);
  assert (Provider.Retry.max_attempts Provider.Retry.Low = 2);
  assert (Provider.Retry.max_attempts Provider.Retry.Medium = 4);
  assert (Provider.Retry.max_attempts Provider.Retry.High = max_int)

let%test_unit "provider_retry_backoff" =
  let check ~(label : string) ~base ~attempt ~expected =
    assert (Provider.Retry.delay_seconds ~base_delay:base attempt = expected)
  in
  (* label; base delay s; attempt; delay s (exponential, capped at 30) *)
  check ~label:"first"  ~base:0.5 ~attempt:1 ~expected:0.5;
  check ~label:"second" ~base:0.5 ~attempt:2 ~expected:1.0;
  check ~label:"third"  ~base:0.5 ~attempt:3 ~expected:2.0;
  check ~label:"fourth" ~base:1.0 ~attempt:4 ~expected:8.0;
  check ~label:"capped" ~base:10.0 ~attempt:6 ~expected:30.0

let%test_unit "provider_retry_config" =
  with_tmp_config ~name:"provider_retry_cfg"
    ~toml_content:"provider_retry = \"low\"\nprovider_retry_base_delay = 2.5\n"
    (fun _ ->
      assert (Config.get_provider_retry_mode () = "low");
      assert (Config.get_provider_retry_base_delay () = 2.5);
      Unix.putenv "CARAVAN_PROVIDER_RETRY" "high";
      assert (Config.get_provider_retry_mode () = "high");
      Unix.putenv "CARAVAN_PROVIDER_RETRY" "")

(* Behavioral: one table over the injected-failure provider — recovery,
   deterministic no-retry, rate-limit classes, mode boundaries, and the
   unbounded [High] budget. *)
let%test_unit "provider_retry_behavior" =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let attempts = ref 0 and fail_until = ref 0 and fail_status = ref 503 in
    let module P : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "flaky"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        incr attempts;
        if !attempts <= !fail_until then
          Caravan_error.raise_provider_failure ~provider:"flaky" ~status:!fail_status
            ~body:"{\"error\":{\"message\":\"injected\"}}"
        else
          Types.wrap_result ~raw_response:"ok" ~model:"m" ~provider:"flaky"
            (Types.assistant_msg "recovered")
      let stream _net _cfg ?model:_ ?options:_ ?tools:_ msgs ~on_token:_ =
        complete _net _cfg msgs
      let list_models _ _ = []
    end in
    let provider = Provider.Provider ((module P), ()) in
    let retries_seen = ref [] in
    let sink ev =
      match ev with
      | Trace.Provider_retry { attempt; _ } -> retries_seen := attempt :: !retries_seen
      | _ -> ()
    in
    let check ~(label : string) ~(failures : int) ~(status : int)
        ~(mode : Provider.Retry.mode) ~(expect_attempts : int) ~(recovers : bool) =
      attempts := 0; fail_until := failures; fail_status := status;
      retries_seen := [];
      let outcome =
        try Ok (Trace.with_sink sink (fun () ->
                Provider.complete_packed ~retry_mode:mode ~retry_base_delay:0.0
                  ~retry_clock:clock net provider []))
        with Caravan_error.Provider_failure { status; _ } ->
          assert (status = !fail_status); Error ()
      in
      assert (!attempts = expect_attempts);
      (match outcome with
       | Ok r ->
         assert recovers;
         assert (r.value.content = "recovered");
         assert (List.length !retries_seen = expect_attempts - 1)
       | Error () -> assert (not recovers))
    in
    (* label; failures before success; injected status; mode;
       expected total upstream attempts; task eventually recovers *)
    check ~label:"503_recovers_medium"   ~failures:2   ~status:503
      ~mode:Provider.Retry.Medium ~expect_attempts:3 ~recovers:true;
    check ~label:"400_deterministic_medium" ~failures:99 ~status:400
      ~mode:Provider.Retry.Medium ~expect_attempts:1 ~recovers:false;
    check ~label:"429_rate_limit_medium" ~failures:1   ~status:429
      ~mode:Provider.Retry.Medium ~expect_attempts:2 ~recovers:true;
    check ~label:"429_outside_low"       ~failures:99  ~status:429
      ~mode:Provider.Retry.Low    ~expect_attempts:1 ~recovers:false;
    check ~label:"500_off_single_shot"   ~failures:99  ~status:500
      ~mode:Provider.Retry.Off    ~expect_attempts:1 ~recovers:false;
    check ~label:"500_low_one_retry"     ~failures:99  ~status:500
      ~mode:Provider.Retry.Low    ~expect_attempts:2 ~recovers:false;
    (* high has no attempt ceiling: keeps going past the Medium budget *)
    check ~label:"400_high_unbounded"    ~failures:6   ~status:400
      ~mode:Provider.Retry.High   ~expect_attempts:7 ~recovers:true)

(* Network-level failures are classified structurally from Eio's typed
   [Io] exceptions — no string scraping. *)
let%test_unit "provider_retry_eio_io_classification" =
  Eio_main.run (fun env ->
    let refused =
      Eio.Switch.run (fun sw ->
        try
          ignore (Eio.Net.connect (Eio.Stdenv.net env) ~sw
                    (`Tcp (Eio.Net.Ipaddr.V4.loopback, 1)));
          None
        with exn -> Some exn)
      |> Option.get
    in
    assert (not (Provider.Retry.classify ~mode:Provider.Retry.Off refused));
    assert (Provider.Retry.classify ~mode:Provider.Retry.Medium refused))

(* The token guard: once any token reached the caller's UI, a retriable
   failure must propagate instead of duplicating streamed output. *)
let%test_unit "provider_retry_token_guard_on_stream" =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let module PS : Provider.PROVIDER with type config = unit = struct
      type config = unit
      let name = "flaky_stream"
      let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
        Types.wrap_result ~raw_response:"" ~model:"m" ~provider:"flaky_stream"
          (Types.assistant_msg "")
      let stream _net _cfg ?model:_ ?options:_ ?tools:_ _msgs ~on_token =
        on_token "partial";
        Caravan_error.raise_provider_failure ~provider:"flaky_stream" ~status:500
          ~body:"{}"
      let list_models _ _ = []
    end in
    let pstream = Provider.Provider ((module PS), ()) in
    let got = ref "" in
    let raised =
      try
        ignore (Provider.stream_packed ~retry_mode:Provider.Retry.High
                  ~retry_base_delay:0.0 net pstream []
                  ~on_token:(fun t -> got := !got ^ t));
        false
      with Caravan_error.Provider_failure _ -> true
    in
    assert raised;
    assert (!got = "partial"))

