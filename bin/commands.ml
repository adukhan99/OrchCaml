(** The REPL's command table.

    [/help], the live completion palette, and Tab completion all read this
    one list, so they cannot drift apart — before it existed, [help_groups]
    and [palette] were maintained by hand and [/help] had stopped
    mentioning [/doctor], [/init], [/web], [/stop] and the sampling knobs.

    A command's [complete] is called with the argument tokens typed so far
    (the last of which is the one being completed, possibly empty) and
    returns the candidates for it.  It runs at Tab time, so it sees live
    config and a live registry. *)

open Caravan

module Registry = CaravanProviders.Registry

type t = {
  name     : string;                     (** e.g. ["/config"] *)
  aliases  : string list;                (** accepted but not listed *)
  args     : string;                     (** display only, e.g. ["<task>"] *)
  doc      : string;
  group    : string;
  example  : string option;              (** shown by [/help] *)
  complete : string list -> string list; (** args so far -> candidates *)
}

let none : string list -> string list = fun _ -> []

let cmd ?(aliases = []) ?(args = "") ?example ?(complete = none) ~group name doc =
  { name; aliases; args; doc; group; example; complete }

(* ── Completers ───────────────────────────────────────────────────────── *)

(** Values a setting accepts, when they are a closed set worth offering. *)
let setting_values key =
  match Config.find_setting key with
  | Some { Config.kind = Config.Enum vs; _ } -> vs
  | Some { Config.kind = Config.Bool; _ } -> ["true"; "false"]
  | _ -> []

let setting_keys () = List.map (fun (s : Config.setting) -> s.Config.key) Config.settings

let complete_config = function
  | [_] -> ["keys"; "get"; "set"; "unset"; "edit"; "show"]
  | [("set" | "unset" | "get"); _] -> setting_keys ()
  | ["set"; key; _] -> setting_values key
  | _ -> []

let provider_names () = List.map (fun (e : Registry.entry) -> e.Registry.name) Registry.entries

let key_provider_names () =
  List.filter_map (fun (e : Registry.entry) ->
    if e.Registry.requires_key then Some e.Registry.name else None) Registry.entries

let mcp_names () =
  List.map (fun (s : Config.mcp_server_config) -> s.Config.name) (Config.get_mcp_servers ())

let complete_mcp = function
  | [_] -> ["list"; "get"; "add"; "remove"]
  | [("get" | "remove" | "rm"); _] -> mcp_names ()
  | _ -> []

let last_is_first = function [_] -> true | _ -> false

(* ── The table ────────────────────────────────────────────────────────── *)

let group_order =
  ["Chat"; "Model and Provider"; "Safety and Tuning"; "Session";
   "Diagnostics"; "Exit"]

let all : t list = [
  (* Chat *)
  cmd "/agent" "Let the AI work autonomously on a task" ~group:"Chat"
    ~args:"<task>" ~example:"/agent summarize the files in this directory";
  cmd "/nudge" "Queue a steering note for the next model call" ~group:"Chat"
    ~args:"<text>";
  cmd "/lisp" "Evaluate a Slip expression (the model's calculator)" ~group:"Chat"
    ~args:"<program>" ~example:"/lisp (mean (list 4 8 15 16 23 42))";
  cmd "/system" "Set instructions for the AI's personality" ~group:"Chat"
    ~args:"[text]";
  cmd "/clear" "Start a fresh conversation" ~group:"Chat";

  (* Model and Provider *)
  cmd "/model" "Switch AI model" ~group:"Model and Provider"
    ~args:"<name>" ~example:"/model claude-sonnet-5";
  cmd "/models" "Browse available models" ~group:"Model and Provider";
  cmd "/provider" "Switch AI provider" ~group:"Model and Provider"
    ~args:"<name> [url]" ~example:"/provider anthropic"
    ~complete:(fun a -> if last_is_first a then provider_names () else []);
  cmd "/providers" "List supported providers and key status"
    ~group:"Model and Provider";
  cmd "/subagents" "Configured subagent workers; add or remove one"
    ~group:"Model and Provider" ~args:"[add|remove <name>]"
    ~complete:(fun a ->
      match a with
      | [_] -> ["add"; "remove"]
      | [("remove" | "rm"); _] ->
        List.map (fun (c : Config.subagent_config) -> c.Config.name)
          (Config.get_subagents ())
      | _ -> []);
  cmd "/key" "Store an API key (input hidden, file 0600)"
    ~group:"Model and Provider" ~args:"<provider>"
    ~complete:(fun a -> if last_is_first a then key_provider_names () else []);

  (* Safety and Tuning *)
  cmd "/permissions" "Tool policy: auto | ask | readonly" ~group:"Safety and Tuning"
    ~args:"[mode]"
    ~complete:(fun a -> if last_is_first a then ["auto"; "ask"; "readonly"] else []);
  cmd "/temp" "Creativity level (higher = more creative)" ~group:"Safety and Tuning"
    ~args:"<0.0-2.0>";
  cmd "/top_p" "Nucleus sampling" ~group:"Safety and Tuning" ~args:"<0.0-1.0>";
  cmd "/top_k" "Top-k sampling" ~group:"Safety and Tuning" ~args:"<n>";
  cmd "/max_tokens" "Response token cap" ~group:"Safety and Tuning" ~args:"<n>";
  cmd "/seed" "Sampling seed" ~group:"Safety and Tuning" ~args:"<n>";
  cmd "/stop" "Stop sequences (no argument clears them)"
    ~group:"Safety and Tuning" ~args:"[seq …]";
  cmd "/memory" "How many messages to remember (0 = unlimited)"
    ~group:"Safety and Tuning" ~args:"<n>";

  (* Session *)
  cmd "/summarise" "Compress the conversation to save context" ~group:"Session"
    ~aliases:["/summarize"];
  cmd "/history" "Show the conversation so far" ~group:"Session";
  cmd "/export" "Save the conversation to a file" ~group:"Session" ~args:"[file]";
  cmd "/resume" "Restore the conversation from a checkpoint" ~group:"Session"
    ~args:"[file]";
  cmd "/tools" "List available tools for the agent (✎ = mutating)" ~group:"Session";
  cmd "/mcp" "Manage MCP server connections and tools" ~group:"Session"
    ~args:"[list|add|get|remove]" ~complete:complete_mcp
    ~example:"/mcp add github -- npx -y @modelcontextprotocol/server-github";
  cmd "/plugins" "List composed plugins; enable or disable one by id"
    ~group:"Session" ~args:"[enable|disable <id>]"
    ~complete:(fun a -> if last_is_first a then ["enable"; "disable"] else []);
  cmd "/config" "Browse and edit settings (Enter changes one)"
    ~group:"Session" ~args:"[keys|set|unset|get|edit]"
    ~complete:complete_config
    ~example:"/config set permissions ask   (/config keys lists them all)";

  (* Diagnostics *)
  cmd "/doctor" "Run system and configuration diagnostics" ~group:"Diagnostics";
  cmd "/init" "Re-run the setup wizard" ~group:"Diagnostics";
  cmd "/web" "How to launch the web cockpit" ~group:"Diagnostics";
  cmd "/help" "All commands, grouped" ~group:"Diagnostics" ~aliases:["/?"];

  (* Exit *)
  cmd "/quit" "Exit Caravan" ~group:"Exit" ~aliases:["/exit"; "/q"];
]

let find name =
  List.find_opt (fun c -> c.name = name || List.mem name c.aliases) all

(** The closest command to a mistyped one, for "did you mean". *)
let suggest name =
  let scored =
    List.concat_map (fun c -> (c.name :: c.aliases)) all
    |> List.map (fun n -> (Config.edit_distance name n, n))
  in
  match List.sort compare scored with
  | (d, n) :: _ when d > 0 && d <= 3 && d < String.length n -> Some n
  | _ -> None

(** [all] grouped for [/help], in the declared group order. *)
let grouped () =
  List.filter_map (fun g ->
    match List.filter (fun c -> c.group = g) all with
    | [] -> None
    | cs -> Some (g, cs)) group_order
