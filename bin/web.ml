(** [caravan web] — a minimal, self-contained web front-end.

    Serves a single embedded HTML page on localhost (no assets on disk, no
    JS toolchain) and a JSON API:

      POST /api/chat   {"message": "..."}          → chat turn
      POST /api/agent  {"task": "..."}             → autonomous agent run
      GET  /api/state                              → provider/model/usage

    Tool activity is captured per-request via a temporary [Trace] sink and
    returned alongside the reply so the page can show an audit trail.
    The server binds 127.0.0.1 only — it is a personal cockpit, not a
    deployment target. *)

open Caravan

let html_page = {html|<!doctype html>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Caravan</title>
<style>
  :root {
    --bg: #14100c; --panel: #1e1813; --ink: #e8ddcf; --dim: #8d8172;
    --amber: #e6a23c; --rose: #e0685e; --teal: #4fb8a8; --line: #33291f;
  }
  * { box-sizing: border-box; }
  body { margin: 0; background: var(--bg); color: var(--ink);
         font: 15px/1.5 ui-monospace, "JetBrains Mono", Menlo, monospace; }
  header { padding: 14px 20px; border-bottom: 1px solid var(--line);
           display: flex; align-items: baseline; gap: 14px; }
  header h1 { margin: 0; font-size: 17px; letter-spacing: 4px;
              background: linear-gradient(90deg, var(--amber), var(--rose));
              -webkit-background-clip: text; background-clip: text; color: transparent; }
  header .meta { color: var(--dim); font-size: 12px; }
  #log { padding: 20px; max-width: 900px; margin: 0 auto;
         padding-bottom: 140px; }
  .msg { margin: 14px 0; white-space: pre-wrap; word-wrap: break-word; }
  .msg.user   { color: var(--amber); }
  .msg.user::before { content: "you ❯ "; color: var(--dim); }
  .msg.bot    { color: var(--ink); }
  .msg.bot::before { content: "caravan ❯ "; color: var(--teal); }
  .msg.err    { color: var(--rose); }
  .tools { border-left: 2px solid var(--line); margin: 6px 0 6px 8px;
           padding-left: 12px; color: var(--dim); font-size: 12.5px; }
  .tools .t::before { content: "⏺ "; color: var(--rose); }
  .usage { color: var(--dim); font-size: 11.5px; margin-top: 4px; }
  form { position: fixed; bottom: 0; left: 0; right: 0;
         background: linear-gradient(transparent, var(--bg) 30%);
         padding: 24px 20px 20px; }
  .bar { max-width: 900px; margin: 0 auto; display: flex; gap: 10px;
         background: var(--panel); border: 1px solid var(--line);
         border-radius: 10px; padding: 10px 12px; }
  .bar input[type=text] { flex: 1; background: none; border: none; outline: none;
         color: var(--ink); font: inherit; }
  .bar button { background: var(--amber); color: #14100c; border: none;
         border-radius: 6px; padding: 6px 14px; font: inherit; cursor: pointer; }
  .bar button:disabled { opacity: .4; cursor: wait; }
  .bar label { color: var(--dim); font-size: 12px; display: flex;
         align-items: center; gap: 5px; user-select: none; }
  .spin { display: inline-block; animation: r 1s linear infinite; }
  @keyframes r { to { transform: rotate(360deg); } }
  #gear { margin-left: auto; background: none; border: 1px solid var(--line);
          color: var(--dim); border-radius: 6px; padding: 3px 10px;
          font: inherit; cursor: pointer; }
  #gear:hover { color: var(--amber); border-color: var(--amber); }
  #settings { position: fixed; inset: 0; background: rgba(10,8,6,.75);
              display: none; align-items: flex-start; justify-content: center;
              overflow-y: auto; padding: 40px 16px; z-index: 10; }
  #settings.open { display: flex; }
  .card { background: var(--panel); border: 1px solid var(--line);
          border-radius: 12px; max-width: 640px; width: 100%;
          padding: 20px 22px; }
  .card h2 { margin: 0 0 4px; font-size: 15px; color: var(--amber); }
  .card .path { color: var(--dim); font-size: 11.5px; margin-bottom: 14px; }
  .srow { display: flex; gap: 10px; align-items: center; margin: 7px 0; }
  .srow label { width: 150px; color: var(--ink); font-size: 13px; }
  .srow input, .srow select { flex: 1; background: var(--bg);
          border: 1px solid var(--line); color: var(--ink); font: inherit;
          font-size: 13px; border-radius: 6px; padding: 5px 8px; }
  .srow .hint { width: 170px; color: var(--dim); font-size: 11px; }
  .srow button { background: var(--teal); color: #14100c; border: none;
          border-radius: 6px; padding: 5px 10px; font: inherit;
          font-size: 12.5px; cursor: pointer; }
  .sect { border-top: 1px solid var(--line); margin-top: 14px;
          padding-top: 12px; }
  .sect h3 { margin: 0 0 8px; font-size: 13px; color: var(--teal);
             letter-spacing: 1px; text-transform: uppercase; }
  .sa-entry { background: var(--bg); border: 1px solid var(--line);
              border-radius: 8px; padding: 8px 12px; margin: 6px 0;
              display: flex; justify-content: space-between; align-items: center;
              font-size: 12.5px; }
  .sa-entry .sa-info { color: var(--ink); }
  .sa-entry .sa-meta { color: var(--dim); font-size: 11px; }
  .sa-del { background: var(--rose); color: #14100c; border: none;
            border-radius: 5px; padding: 3px 8px; font: inherit;
            font-size: 11px; cursor: pointer; }
  .sa-del:hover { opacity: .85; }
  .sa-none { color: var(--dim); font-size: 12px; font-style: italic; }
  #smsg { font-size: 12px; min-height: 16px; margin-top: 8px; }
  #smsg.ok { color: var(--teal); } #smsg.err { color: var(--rose); }
</style>
<header>
  <h1>☾ CARAVAN</h1>
  <span class="meta" id="meta">…</span>
  <button id="gear" title="settings">⚙ settings</button>
</header>
<div id="settings">
  <div class="card">
    <h2>Settings</h2>
    <div class="path" id="cfgpath"></div>
    <div id="srows"></div>
    <div class="sect">
      <div class="srow">
        <label>API key</label>
        <select id="kprov"></select>
        <input type="password" id="kval" placeholder="paste key — stored 0600, never displayed">
        <button id="ksave">save</button>
      </div>
    </div>
    <div class="sect">
      <h3>☷ Subagents</h3>
      <div id="saroster"></div>
      <div id="sanew"></div>
    </div>
    <div id="smsg"></div>
    <div class="sect" style="text-align:right">
      <button id="sclose" style="background:var(--line);color:var(--ink)">close</button>
    </div>
  </div>
</div>
<div id="log"></div>
<form id="f">
  <div class="bar">
    <input type="text" id="q" placeholder="message — or describe a task and tick agent"
           autocomplete="off" autofocus>
    <label><input type="checkbox" id="agent"> agent</label>
    <button id="go">send</button>
  </div>
</form>
<script>
const log = document.getElementById('log');
const q = document.getElementById('q');
const go = document.getElementById('go');
const agent = document.getElementById('agent');

function add(cls, text) {
  const d = document.createElement('div');
  d.className = 'msg ' + cls;
  d.textContent = text;
  log.appendChild(d);
  window.scrollTo(0, document.body.scrollHeight);
  return d;
}

async function refreshMeta() {
  try {
    const r = await fetch('/api/state');
    const s = await r.json();
    document.getElementById('meta').textContent =
      s.provider + '/' + s.model + ' · ▲' + s.tokens_in + ' ▼' + s.tokens_out + ' tok';
  } catch (e) {}
}

document.getElementById('f').addEventListener('submit', async (ev) => {
  ev.preventDefault();
  const text = q.value.trim();
  if (!text) return;
  q.value = '';
  add('user', text);
  const wait = add('bot', '');
  wait.innerHTML = '<span class="spin">◐</span>';
  go.disabled = true;
  try {
    const url = agent.checked ? '/api/agent' : '/api/chat';
    const key = agent.checked ? 'task' : 'message';
    const r = await fetch(url, {
      method: 'POST',
      headers: {'content-type': 'application/json'},
      body: JSON.stringify({[key]: text})
    });
    const j = await r.json();
    wait.remove();
    if (j.tools && j.tools.length) {
      const t = document.createElement('div');
      t.className = 'tools';
      j.tools.forEach(x => {
        const e = document.createElement('div');
        e.className = 't';
        e.textContent = x;
        t.appendChild(e);
      });
      log.appendChild(t);
    }
    if (j.error) add('err', j.error);
    else {
      add('bot', j.reply);
      if (j.usage) {
        const u = document.createElement('div');
        u.className = 'usage';
        u.textContent = j.usage;
        log.appendChild(u);
      }
    }
  } catch (e) {
    wait.remove();
    add('err', 'request failed: ' + e);
  }
  go.disabled = false;
  q.focus();
  refreshMeta();
});

// ── Settings modal ─────────────────────────────────────────────────────
const modal = document.getElementById('settings');
const smsg = document.getElementById('smsg');
function note(ok, text) { smsg.className = ok ? 'ok' : 'err'; smsg.textContent = text; }

async function openSettings() {
  const r = await fetch('/api/config');
  const cfg = await r.json();
  document.getElementById('cfgpath').textContent = cfg.path;
  const rows = document.getElementById('srows');
  rows.innerHTML = '';
  cfg.settings.forEach(s => {
    const row = document.createElement('div');
    row.className = 'srow';
    const lbl = document.createElement('label');
    lbl.textContent = s.key; lbl.title = s.description;
    const inp = document.createElement('input');
    inp.value = s.value === null ? '' : s.value;
    inp.placeholder = '(unset)';
    const hint = document.createElement('span');
    hint.className = 'hint'; hint.textContent = s.accepts;
    const btn = document.createElement('button');
    btn.textContent = 'save';
    btn.onclick = async () => {
      const rr = await fetch('/api/config', {
        method: 'POST',
        headers: {'content-type': 'application/json'},
        body: JSON.stringify({key: s.key, value: inp.value})
      });
      const j = await rr.json();
      note(!j.error, j.error || ('saved ' + s.key + (j.note ? ' — ' + j.note : '')));
      refreshMeta();
    };
    row.append(lbl, inp, hint, btn);
    rows.appendChild(row);
  });
  const sel = document.getElementById('kprov');
  sel.innerHTML = '';
  cfg.providers.forEach(p => {
    const o = document.createElement('option');
    o.value = p.name;
    o.textContent = p.name + (p.key_set ? ' ✓' : ' — no key');
    sel.appendChild(o);
  });
  // ── Subagent roster ──
  const roster = document.getElementById('saroster');
  roster.innerHTML = '';
  if (cfg.subagents && cfg.subagents.length) {
    cfg.subagents.forEach(sa => {
      const e = document.createElement('div'); e.className = 'sa-entry';
      const info = document.createElement('div');
      info.innerHTML = '<span class="sa-info">' + sa.name + '</span> '
        + '<span class="sa-meta">' + sa.provider + '/' + sa.model
        + (sa.tools.length ? ' · ' + sa.tools.join(', ') : '') + '</span>';
      const del = document.createElement('button'); del.className = 'sa-del';
      del.textContent = '✕'; del.title = 'delete ' + sa.name;
      del.onclick = async () => {
        const rr = await fetch('/api/subagents', {
          method: 'DELETE',
          headers: {'content-type': 'application/json'},
          body: JSON.stringify({name: sa.name})
        });
        const j = await rr.json();
        note(!j.error, j.error || 'deleted ' + sa.name);
        if (!j.error) openSettings();
      };
      e.append(info, del); roster.appendChild(e);
    });
  } else {
    const none = document.createElement('div'); none.className = 'sa-none';
    none.textContent = 'No subagents configured.';
    roster.appendChild(none);
  }
  // ── Add subagent form ──
  const sanew = document.getElementById('sanew');
  sanew.innerHTML = '';
  if (cfg.subagent_fields) {
    const inputs = {};
    cfg.subagent_fields.forEach(f => {
      const row = document.createElement('div'); row.className = 'srow';
      const lbl = document.createElement('label');
      lbl.textContent = f.label + (f.required ? ' *' : '');
      const inp = document.createElement('input');
      inp.placeholder = f.placeholder; inp.dataset.key = f.key;
      inputs[f.key] = inp;
      row.append(lbl, inp); sanew.appendChild(row);
    });
    const brow = document.createElement('div'); brow.className = 'srow';
    brow.style.justifyContent = 'flex-end';
    const abtn = document.createElement('button');
    abtn.textContent = '+ add subagent'; abtn.style.background = 'var(--amber)';
    abtn.onclick = async () => {
      const payload = {};
      for (const [k, inp] of Object.entries(inputs)) {
        if (inp.value.trim()) payload[k] = inp.value.trim();
      }
      const rr = await fetch('/api/subagents', {
        method: 'POST',
        headers: {'content-type': 'application/json'},
        body: JSON.stringify(payload)
      });
      const j = await rr.json();
      note(!j.error, j.error || ('added' + (j.note ? ' — ' + j.note : '')));
      if (!j.error) openSettings();
    };
    brow.appendChild(abtn); sanew.appendChild(brow);
  }
  smsg.textContent = '';
  modal.classList.add('open');
}
document.getElementById('gear').onclick = openSettings;
document.getElementById('sclose').onclick = () => modal.classList.remove('open');
modal.addEventListener('click', e => { if (e.target === modal) modal.classList.remove('open'); });
document.getElementById('ksave').onclick = async () => {
  const kv = document.getElementById('kval');
  const rr = await fetch('/api/key', {
    method: 'POST',
    headers: {'content-type': 'application/json'},
    body: JSON.stringify({provider: document.getElementById('kprov').value, key: kv.value})
  });
  const j = await rr.json();
  note(!j.error, j.error || 'API key stored');
  if (!j.error) { kv.value = ''; openSettings(); }
};

refreshMeta();
</script>
|html}

type state = {
  mutable session    : Session.t;
  mutable tokens_in  : int;
  mutable tokens_out : int;
  provider_name      : string;
  model              : string;
}

let read_body body =
  Eio.Buf_read.(of_flow body ~max_size:10_000_000 |> take_all)

let json_response ?(status = `OK) json =
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list [("content-type", "application/json")])
    ~status ~body:(Yojson.Safe.to_string json) ()

let html_response () =
  Cohttp_eio.Server.respond_string
    ~headers:(Http.Header.of_list [("content-type", "text/html; charset=utf-8")])
    ~status:`OK ~body:html_page ()

(** Web permission policy: no interactive prompt is possible, so "ask"
    degrades to deny rather than silently allowing. *)
let web_permission_policy tools () =
  let is_mutating name =
    match Tool.find_tool tools name with
    | Some t -> Tool.is_mutating_packed t
    | None -> true
  in
  let describe_action name args =
    match Tool.find_tool tools name with
    | Some t -> Tool.describe_action_packed t args
    | None -> Printf.sprintf "Use tool '%s'" name
  in
  let mode = match Config.get_permission_mode () with
    | "ask" -> "readonly"  (* no prompt surface in web UI *)
    | m -> m
  in
  Permission.policy_of_mode ~is_mutating ~describe_action mode


let record_usage st (result : _ Types.result_with_meta) =
  match result.usage with
  | Some u ->
    st.tokens_in <- st.tokens_in + u.prompt_tokens;
    st.tokens_out <- st.tokens_out + u.completion_tokens
  | None -> ()

(** Run [f] capturing tool-trace lines for the response payload. *)
let with_captured_tools f =
  let captured = ref [] in
  let sink ev =
    match ev with
    | Trace.Tool_call_start { name; args } ->
      let preview = if String.length args > 80 then String.sub args 0 80 ^ "…" else args in
      captured := Printf.sprintf "%s %s" name preview :: !captured
    | Trace.Subagent_start { name; task } ->
      let preview = if String.length task > 80 then String.sub task 0 80 ^ "…" else task in
      captured := Printf.sprintf "subagent %s: %s" name preview :: !captured
    | _ -> ()
  in
  let result = Trace.with_sink sink f in
  (result, List.rev !captured)

let handle_message st net clock ~agent_mode text =
  let (outcome, tools) =
    with_captured_tools (fun () ->
      Effects.run_with_effects
        ~permission_policy:(web_permission_policy (Session.tools st.session) ()) @@ fun () ->
      if agent_mode then
        match Agent.run net clock st.session text with
        | Ok (sess', result) ->
          st.session <- sess';
          record_usage st result;
          Ok result
        | Error e -> Error e
      else begin
        let (sess', result) = Session.turn net clock st.session text in
        st.session <- sess';
        record_usage st result;
        Ok result
      end)
  in
  match outcome with
  | Ok result ->
    let usage_line = Monitor.format_usage result in
    `Assoc [
      ("reply", `String result.Types.value.Types.content);
      ("tools", `List (List.map (fun t -> `String t) tools));
      ("usage", `String usage_line);
    ]
  | Error e ->
    `Assoc [
      ("error", `String e);
      ("tools", `List (List.map (fun t -> `String t) tools));
    ]

(** Current values of the editable keys, plus per-provider API-key
    presence. Key VALUES are never serialized — only set/unset. *)
let config_snapshot () =
  let value_of key =
    match Config.get_string key with
    | Some v -> `String v
    | None ->
      match Config.get_int key with
      | Some i -> `String (string_of_int i)
      | None ->
        match Config.get_bool key with
        | Some b -> `String (string_of_bool b)
        | None -> `Null
  in
  let settings =
    List.map (fun (s : Config.setting) ->
      `Assoc [
        ("key", `String s.Config.key);
        ("description", `String s.Config.doc);
        ("accepts", `String (Config.accepts_of_kind s.Config.kind));
        ("default", `String s.Config.default);
        ("effect", `String (Config.scope_note s.Config.scope));
        ("choices", (match s.Config.kind with
           | Config.Enum vs -> `List (List.map (fun v -> `String v) vs)
           | Config.Bool -> `List [`String "true"; `String "false"]
           | _ -> `Null));
        ("env_shadow", (match Config.env_shadow s.Config.key with
           | Some (var, _) -> `String var
           | None -> `Null));
        ("value", value_of s.Config.key);
      ]) Config.settings
  in
  let providers =
    List.filter_map (fun (e : CaravanProviders.Registry.entry) ->
      if not e.requires_key then None
      else Some (`Assoc [
        ("name", `String e.name);
        ("key_env", `String (Option.value ~default:"" e.key_env));
        ("key_set", `Bool (CaravanProviders.Registry.api_key_for e <> None));
      ])) CaravanProviders.Registry.entries
  in
  let subagent_fields =
    List.map (fun (key, label, placeholder, required) ->
      `Assoc [
        ("key", `String key);
        ("label", `String label);
        ("placeholder", `String placeholder);
        ("required", `Bool required);
      ]) Config.editable_subagent_fields
  in
  let subagents =
    List.map Config.subagent_to_json (Config.get_subagents ())
  in
  `Assoc [
    ("path", `String (Config.config_path ()));
    ("settings", `List settings);
    ("providers", `List providers);
    ("subagent_fields", `List subagent_fields);
    ("subagents", `List subagents);
  ]

let callback st net clock _conn request body =
  let path = Http.Request.resource request in
  let meth = Http.Request.meth request in
  match meth, path with
  | `GET, "/" -> html_response ()
  | `GET, "/api/state" ->
    json_response (`Assoc [
      ("provider",   `String st.provider_name);
      ("model",      `String st.model);
      ("tokens_in",  `Int st.tokens_in);
      ("tokens_out", `Int st.tokens_out);
      ("permissions", `String (Config.get_permission_mode ()));
    ])
  | `GET, "/api/config" -> json_response (config_snapshot ())
  | `POST, "/api/config" ->
    let raw = read_body body in
    (try
       let json = Yojson.Safe.from_string raw in
       let open Yojson.Safe.Util in
       let key = json |> member "key" |> to_string in
       let value = json |> member "value" |> to_string in
       (* Only schema settings are editable over HTTP: API keys have their
          own endpoint, and nothing else is reachable from the browser. *)
       if Config.find_setting key = None then
         json_response ~status:`Forbidden
           (`Assoc [("error", `String (key ^ " is not editable here"))])
       else
         (match Config.set_checked key value with
          | Ok _ ->
            Trace.log "info" "web: config %s updated" key;
            json_response (`Assoc [("ok", `Bool true); ("note", `String
              "Saved. provider/model/base_url apply when the server restarts.")])
          | Error e ->
            (* A rejected value is the caller's mistake, not the server's. *)
            json_response ~status:`Bad_request
              (`Assoc [("error", `String e)]))
     with _ ->
       json_response ~status:`Bad_request
         (`Assoc [("error", `String "expected {\"key\":…, \"value\":…}")]))
  | `POST, "/api/key" ->
    let raw = read_body body in
    (try
       let json = Yojson.Safe.from_string raw in
       let open Yojson.Safe.Util in
       let provider = json |> member "provider" |> to_string in
       let key = json |> member "key" |> to_string in
       (match CaravanProviders.Registry.find provider with
        | None ->
          json_response ~status:`Bad_request
            (`Assoc [("error", `String ("unknown provider: " ^ provider))])
        | Some e when not e.requires_key ->
          json_response ~status:`Bad_request
            (`Assoc [("error", `String (e.name ^ " is local — no key needed"))])
        | Some e ->
          if String.trim key = "" then
            json_response ~status:`Bad_request
              (`Assoc [("error", `String "empty key")])
          else
            (match Config.set_api_key e.name key with
             | Ok _ ->
               Trace.log "info" "web: api key for %s stored" e.name;
               json_response (`Assoc [("ok", `Bool true)])
             | Error err ->
               json_response ~status:`Internal_server_error
                 (`Assoc [("error", `String err)])))
     with _ ->
       json_response ~status:`Bad_request
         (`Assoc [("error", `String "expected {\"provider\":…, \"key\":…}")]))
  | `GET, "/api/subagents" ->
    let entries = List.map Config.subagent_to_json (Config.get_subagents ()) in
    json_response (`Assoc [("subagents", `List entries)])
  | `POST, "/api/subagents" ->
    let raw = read_body body in
    (try
       let json = Yojson.Safe.from_string raw in
       let open Yojson.Safe.Util in
       let fields =
         List.filter_map (fun (key, _, _, _) ->
           match json |> member key with
           | `String v -> Some (key, v)
           | `Null -> None
           | _ -> None
         ) Config.editable_subagent_fields
       in
       (match Config.add_subagent fields with
        | Ok _ ->
          Trace.log "info" "web: subagent added";
          json_response (`Assoc [("ok", `Bool true); ("note", `String
            "Saved. Restart the server for the delegate tool to pick up new workers.")])
        | Error e ->
          json_response ~status:`Bad_request
            (`Assoc [("error", `String e)]))
     with _ ->
       json_response ~status:`Bad_request
         (`Assoc [("error", `String "expected JSON with subagent fields")]))
  | `DELETE, "/api/subagents" ->
    let raw = read_body body in
    (try
       let json = Yojson.Safe.from_string raw in
       let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
       (match Config.delete_subagent name with
        | Ok _ ->
          Trace.log "info" "web: subagent '%s' deleted" name;
          json_response (`Assoc [("ok", `Bool true)])
        | Error e ->
          json_response ~status:`Bad_request
            (`Assoc [("error", `String e)]))
     with _ ->
       json_response ~status:`Bad_request
         (`Assoc [("error", `String "expected {\"name\":\"…\"}")]))
  | `POST, ("/api/chat" | "/api/agent") ->
    let agent_mode = path = "/api/agent" in
    let raw = read_body body in
    (try
       let json = Yojson.Safe.from_string raw in
       let key = if agent_mode then "task" else "message" in
       (match Yojson.Safe.Util.member key json with
        | `String text when String.trim text <> "" ->
          json_response (handle_message st net clock ~agent_mode text)
        | _ ->
          json_response ~status:`Bad_request
            (`Assoc [("error", `String (Printf.sprintf "missing '%s' field" key))]))
     with exn ->
       json_response ~status:`Internal_server_error
         (`Assoc [("error", `String (Caravan_error.humanize exn))]))
  | _ ->
    Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"not found" ()

let serve ~port ~provider_name ~model ~make_session =
  Eio_main.run @@ fun env ->
  Effects.with_net env#net @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let session = make_session env in
  let st = { session; tokens_in = 0; tokens_out = 0; provider_name; model } in
  let socket =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server =
    Cohttp_eio.Server.make ()
      ~callback:(fun conn req body -> callback st env#net env#clock conn req body)
  in
  Ui.println_ansi (Ui.green (Printf.sprintf "  ☾ Caravan web UI listening on http://127.0.0.1:%d" port));
  Ui.println_ansi (Ui.dim "    (localhost only — Ctrl-C to stop)");
  Cohttp_eio.Server.run socket server
    ~on_error:(fun exn -> Trace.log "error" "web: %s" (Printexc.to_string exn))
