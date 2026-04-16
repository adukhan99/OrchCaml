open OrchCaml

let test_memory_buffer () =
  let mem = Memory.Buffer.create ~window:2 () in
  let s1 = Types.system_msg "You are an assistant." in
  let m1 = Types.user_msg "Hello!" in
  let m2 = Types.assistant_msg "Hi!" in
  let m3 = Types.user_msg "Next" in
  Memory.Buffer.add mem s1;
  Memory.Buffer.add mem m1;
  Memory.Buffer.add mem m2;
  Memory.Buffer.add mem m3;
  
  let hist = Memory.Buffer.get mem in
  (* Note: we configured window=2, so m1 is expelled but s1 is safely isolated and prepended! *)
  assert (List.length hist = 3);
  let roles = List.map (fun m -> m.Types.role) hist in
  assert (roles = [Types.System; Types.Assistant; Types.User]);
  ()

let test_parser_json () =
  let fake_json = {| {"status": "ok", "count": 42} |} in
  match Parser.json_field "count" fake_json with
  | Ok (`Int 42) -> ()
  | _ -> failwith "JSON parser count field failure"

let test_parser_bool () =
  match Parser.bool "   yes  \n" with
  | Ok true -> ()
  | _ -> failwith "Bool parser failure"

let run_tests () =
  Printf.printf "Running tests...\n";
  test_memory_buffer ();
  test_parser_json ();
  test_parser_bool ();
  Printf.printf "All tests passed.\n"

let () = run_tests ()