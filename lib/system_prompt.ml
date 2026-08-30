(** Default system prompt and environment preamble.

    Claude-Code-class harnesses feel competent out of the box in large
    part because of the half of the equation that is not the model: a
    tuned system prompt plus injected environment context.  Small and
    free-tier models depend on that scaffolding far more than frontier
    ones — without it they narrate instead of acting, rediscover the
    working directory with metered [ls]/[pwd] round trips, and never
    learn that [finish] is a protocol rather than one tool among many.

    The prompt is layered, not monolithic:

    - {!base} — the harness contract (act via tools, verify, finish);
    - {!tool_format_layer} — tool-call format guidance, included only
      for models whose native tool calling is flaky or absent (per
      {!Capability.lookup});
    - {!environment_preamble} — cwd, OS, date, git state, a shallow
      listing.  Assembled {b once per session} so the request prefix
      stays byte-stable for prompt caching (audit H3): never rebuild it
      per turn.

    User configuration composes with these via {!compose}: the [system]
    setting appends a final layer, and [system_replace = true] discards
    the shipped layers entirely for users who want full control. *)

let base () =
  "You are Caravan, an autonomous agent that completes tasks using the \
   tools provided to you.\n\
   \n\
   Core rules:\n\
   - Act through tool calls. Do not describe what you would do — do it. \
   Never claim an action succeeded without having called the tool and \
   seen its output.\n\
   - Work in small verified steps: issue a tool call, read its result, \
   then decide the next step. Prefer checking an assumption over \
   guessing.\n\
   - If a tool call errors, read the error, adjust the arguments or \
   approach, and try again. Do not repeat the identical failing call. \
   If you are blocked after a few honest attempts, finish and say \
   exactly what blocked you.\n\
   - The environment context below tells you the working directory, \
   date, and git state. Do not spend tool calls rediscovering it.\n\
   - Be concise. Skip preamble like \"Sure, I'll...\" — go straight to \
   the work.\n\
   \n\
   Completion protocol:\n\
   - When the task is done (or definitively blocked), call the `finish` \
   tool with a short summary of what you did and what the outcome was.\n\
   - A plain text reply does not end the task. Only `finish` does."

(** Tool-call format guidance for models with flaky or absent native
    tool calling — the text protocol the fallback parser (C2)
    understands. Returns [None] for models with reliable native calls,
    which do not need the reminder (it wastes their context). *)
let tool_format_layer (cap : Capability.t) =
  match cap.tool_calling with
  | Capability.Native -> None
  | Capability.Flaky | Capability.Text_only ->
    Some
      "Tool-call format:\n\
       - Use your native function/tool-calling mechanism if you have one.\n\
       - Otherwise reply with EXACTLY one JSON object and nothing else:\n\
      \  {\"tool\": \"<tool name>\", \"arguments\": { ... }}\n\
       - One tool call per reply. No prose before or after the JSON.\n\
       - Do not put a tool call in a reply that also answers the user; \
       a reply is either a tool call or a final answer via `finish`."

(* Run a command, returning its trimmed stdout on exit 0 and [None]
   otherwise.  stderr is discarded — this is best-effort context
   gathering, not diagnostics. *)
let command_output cmd =
  try
    let ic = Unix.open_process_in (Printf.sprintf "( %s ) 2>/dev/null" cmd) in
    let out = In_channel.input_all ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 ->
      let t = String.trim out in
      if t = "" then None else Some t
    | _ -> None
  with _ -> None

let listing_limit = 25

(** Shallow non-hidden listing of [dir], directories suffixed with "/",
    capped at {!listing_limit} entries. *)
let shallow_listing dir =
  try
    let entries =
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun e -> String.length e > 0 && e.[0] <> '.')
      |> List.sort compare
    in
    let shown = List.filteri (fun i _ -> i < listing_limit) entries in
    let render e =
      if (try Sys.is_directory (Filename.concat dir e) with _ -> false)
      then e ^ "/" else e
    in
    let suffix =
      let omitted = List.length entries - List.length shown in
      if omitted > 0 then [Printf.sprintf "... (%d more entries)" omitted] else []
    in
    match List.map render shown @ suffix with
    | [] -> None
    | items -> Some (String.concat "  " items)
  with _ -> None

(** Environment context: cwd, OS, date, git branch/cleanliness, shallow
    listing.  Deterministic for a given environment — call it once at
    session construction and never again, so the serialised system
    message stays byte-identical across turns (prompt-cache prefix). *)
let environment_preamble () =
  let cwd = try Sys.getcwd () with _ -> "(unknown)" in
  let os =
    match command_output "uname -sr" with
    | Some u -> u
    | None -> Sys.os_type
  in
  let date =
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  in
  let git_line =
    match command_output "git rev-parse --abbrev-ref HEAD" with
    | None -> "Git: not a repository"
    | Some branch ->
      let dirty =
        match command_output "git status --porcelain" with
        | Some s when s <> "" -> "dirty"
        | Some _ | None -> "clean"
      in
      Printf.sprintf "Git: branch %s (%s)" branch dirty
  in
  let listing =
    match shallow_listing cwd with
    | Some l -> "Directory contents: " ^ l
    | None -> "Directory contents: (unreadable)"
  in
  String.concat "\n" [
    "Environment:";
    "Working directory: " ^ cwd;
    "OS: " ^ os;
    "Date: " ^ date;
    git_line;
    listing;
  ]

(** Assemble the full system prompt.

    [replace = true] hands full control to [user_system]: the shipped
    layers are dropped and the result is exactly the user text (or
    [None] when there is none — the pre-refactor behaviour).

    Otherwise: base, then the capability-conditioned tool-format layer,
    then the environment preamble, then [user_system] appended as the
    final layer.  Returns [None] only under [replace] with no user
    text. *)
let compose ?(capability = Capability.conservative) ?user_system
    ?(replace = false) () =
  if replace then user_system
  else
    let layers =
      [base ()]
      @ (match tool_format_layer capability with Some l -> [l] | None -> [])
      @ [environment_preamble ()]
      @ (match user_system with
         | Some s when String.trim s <> "" -> [s]
         | _ -> [])
    in
    Some (String.concat "\n\n" layers)
