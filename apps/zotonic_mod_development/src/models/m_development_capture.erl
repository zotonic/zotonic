%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Administrator API for browser capture sessions and console diagnostics.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_development_capture).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "frontend_developer", "model",
        "development_and_debugging", "logging_and_monitoring"
    ]
}).
-moduledoc("
Capture template rendering and browser console output from an open page without
losing its current state. Use this model with `model#development_tools` to inspect
how a page is built and verify conditional or lazy rendering.

For local and remote workflows, see the
[LLM site development guide](../../../../doc/guides/llm-site-development.md).

## Access and setup

Enable `module#mod_development` on the site. All data and mutation endpoints
require an administrator with `use mod_development` permission and a site
environment of `development`, `test`, or `acceptance`. Production is denied,
including trusted CLI calls. The `console_allowed` capability probe returns a
boolean rather than an access error.

Read-only administrators can list captures and read their status, traces, and
logs. Attaching, starting, stopping, and submitting logs require write access.
Attaching to an existing capture and submitting logs also require the browser
session that created it. Session IDs are never returned.

The browser API is `window.z_development_capture`. Its script is included on
authorized pages when the floating template trace button is enabled. Use the
normal development head assets; the button and JavaScript use the same API.

## Capture an open page

Run this in the intended page's browser console or browser execution tool:

```javascript
const capture = await window.z_development_capture.attach();
// Interact with the page: scroll, open a panel, or trigger a lazy component.
await window.z_development_capture.flush();
const trace = await window.z_development_capture.trace();
const logs = await window.z_development_capture.console();
await window.z_development_capture.stop();
```

`trace` contains `{capture, nodes, edges}`. `logs` contains
`{entries, truncated}`; each entry has `seq`, `text`, and `received_at`.
Read results through the execution tool's return value rather than logging them
back into the capture. Reads do not submit or clear browser logs.

## JavaScript reference

All methods return promises. Permission and transport failures reject; model
errors expose `error.code`. An optional ID defaults to the page's attachment.

| Method | Behavior |
| --- | --- |
| `list()` | GET all available capture summaries; returns `[]` when none exist. |
| `status(captureId?)` | GET a capture summary. |
| `trace(captureId?)` | GET the accumulated template graph. |
| `console(captureId?)` | GET stored console entries without flushing pending logs. |
| `attach()` | Reuse the page's attachment or create one; start it if stopped. |
| `attach(captureId)` | Validate write access and session ownership, then reconnect and start if stopped. |
| `start(captureId)` | Start an existing capture; an ID is required. |
| `stop()` | Flush this page's pending logs, then stop its capture. |
| `stop(captureId)` | Stop an existing capture without flushing this page's logs. |
| `flush()` | Submit this page's pending logs without starting or stopping its capture. |
| `detach()` | Clear the local attachment and pending logs, restore `console.log`, and leave the server capture running. |

Status and evidence reads return `null` when the capture is missing or expired,
or when no page attachment exists. Explicit-ID start/stop also return `null`
for a missing capture. Explicit-ID reads and start/stop do not change the local
attachment or sessionStorage, including on errors.

`attach(captureId)` rejects missing, expired, or foreign-session IDs; it never
creates a replacement. Validation and start must succeed before the ID is saved.
Switching IDs discards the previous browser buffer; call `flush()` first to retain
it. Reconnecting the same ID preserves that buffer.

`flush()` returns status, or `null` when unattached or expired. It does not submit
to a stopped capture. `detach()` returns `null`, makes no network request, and
works offline and with read-only access. Queued local commands and late replies
cannot reattach a detached page; already-sent server requests may still complete.
Do not automatically retry mutations after a timeout.

## Ownership and capture summaries

List and status results contain these fields:

| Field | Meaning |
| --- | --- |
| `id` | Capture ID used by every interface. |
| `path` | Page path recorded at attachment; tracing is session-scoped, not restricted to this path. |
| `state` | `active` or `stopped`. |
| `expires` | Absolute expiry time, in Unix seconds. |
| `truncated` | A graph or server console limit has been reached. |
| `trace_scope` | `session`. Other tabs in the session may contribute renders. |
| `is_owner` | Whether this request has the browser session that created the capture. False without a session. |
| `write_allowed` | Whether the caller may mutate captures; independent of ownership. |
| `console_allowed` | Whether the caller may access console diagnostics. |

To find a capture this browser can reconnect to:

```javascript
const captures = await window.z_development_capture.list();
const owned = captures.find(c => c.is_owner && c.write_allowed);
if (owned) {
    await window.z_development_capture.attach(owned.id);
}
```

Read-only owners cannot attach or submit logs. Writable non-owners can start or
stop captures by ID but cannot attach or submit browser logs. Server checks are
authoritative; the flags help clients select the right operation.

The browser requires `is_owner === true` before wrapping `console.log` or
submitting logs. Local attach/flush/stop and background polling refresh status
first. Lost or missing ownership restores normal console output and discards
pending logs. Local mutations reject with `eacces`; background polling uses GET
only. Use `detach()` to clear a stale local attachment.

## HTTP and MQTT endpoints

HTTP uses `/api/model/development_capture/<method>/<path>`. Browser MQTT calls
use `bridge/origin/model/development_capture/<method>/<path>`; a direct server
MQTT client uses `model/development_capture/<method>/<path>`.

| Method | Path | Payload | Result |
| --- | --- | --- | --- |
| GET | `console_allowed` | None | Capability boolean. |
| GET | `list` | None | Array of capture summaries. |
| GET | `status/ID` | None | Capture summary. |
| GET | `trace/ID` | None | `{capture, nodes, edges}`. |
| GET | `console/ID` | None | `{entries, truncated}`. |
| POST | `attach` | `{path: \"/example\"}` | New, stopped capture bound to the caller's browser session. |
| POST | `start/ID` | None | Active capture summary; existing evidence is retained. |
| POST | `stop/ID` | None | Stopped capture summary. |
| POST | `poll/ID` | `{entries: [\"message\"]}` | Submit logs to an active capture and return status; requires the owning session. |

HTTP JSON and MQTT response payloads use `{status: \"ok\", result: ...}` or
`{status: \"error\", error: ...}`. Raw model endpoints return `enoent` for missing
or expired captures; JavaScript methods translate it to `null` where described
above. `eacces` denotes denied access; invalid arguments or a foreign-session
poll can return `badarg`.

An OAuth2 administrator token can inspect existing captures and, with write
access, start/stop them. A token without the owning browser session cannot
attach to that capture or submit its console logs. The browser normally handles
attachment and polling; CLI/API control alone does not collect browser output.

## CLI and SSH

Run from the server's Zotonic checkout as its normal server user:

```sh
bin/zotonic capture mysite list
bin/zotonic capture mysite status CAPTURE_ID
bin/zotonic capture mysite start CAPTURE_ID
bin/zotonic capture mysite trace CAPTURE_ID
bin/zotonic capture mysite console CAPTURE_ID
bin/zotonic capture mysite stop CAPTURE_ID
```

For remote access, run the same commands through SSH:

```sh
ssh site-host 'cd /srv/zotonic && bin/zotonic capture mysite list'
```

CLI commands use trusted Erlang node RPC and still enforce the environment
restriction. They return JSON `{ok: true, result: ...}` or
`{ok: false, error: ...}`, with a nonzero exit status for failures.

## Lifetime and limits

- A capture expires ten minutes after creation. Polling and restarting do not
  extend it. Stopping retains evidence until expiry.
- At most 16 captures exist per site. Attach a fresh capture for a fresh graph;
  starting an existing one is idempotent and retains evidence.
- The graph is capped at approximately 1,000 nodes and 5,000 edges. Console
  history retains the latest 500 entries, up to 4,096 bytes each and 50 per upload.
- Browser console collection covers subsequent `console.log` calls only. It is
  best effort: overflow, navigation, disconnection, or remote stopping can lose logs.
- The browser polls every second. Attachment survives reload via sessionStorage;
  renders before attachment cannot be reconstructed. Attach before reloading to
  trace the initial render. Duplicated tabs may inherit the stored attachment.
- This API provides template traces and console text, not screenshots, DOM
  snapshots, historical console output, or browser action execution.

Existing human template tracing remains independent of capture sessions.
").
-behaviour(zotonic_model).
-export([m_get/3, m_post/3, cli/3, console_allowed/1]).

-spec console_allowed(Context) -> boolean() when Context :: z:context().
-spec cli(SiteName, Operation, Id) -> {ok, term()} | {error, term()}
    when SiteName :: string(), Operation :: string(), Id :: string().

m_get([<<"console_allowed">> | Rest], _, Context) ->
    {ok, {console_allowed(Context), Rest}};
m_get(Path, _, Context) ->
    case authorized(Context) of
        true -> get(Path, Context);
        false -> {error, eacces}
    end.

get([<<"list">> | Rest], Context) -> result(call(list, #{}, Context), Rest);
get([<<"status">>, Id | Rest], Context) -> result(call(status, #{id => Id}, Context), Rest);
get([<<"trace">>, Id | Rest], Context) -> result(call(trace, #{id => Id}, Context), Rest);
get([<<"console">>, Id | Rest], Context) ->
    case console_allowed(Context) of
        true -> result(call(console, #{id => Id}, Context), Rest);
        false -> {error, eacces}
    end;
get(_, _) -> {error, unknown_path}.

m_post(Path, Msg, Context) ->
    case authorized(Context) andalso not z_acl:is_read_only(Context) of
        true -> post(Path, Msg, Context);
        false -> {error, eacces}
    end.

post([<<"attach">>], #{payload := #{<<"path">> := Path}}, Context)
    when is_binary(Path), byte_size(Path) =< 2048 ->
    case {z_context:session_id(Context), z_context:is_site_url(Path, Context)} of
        {{ok, Sid}, true} -> call(attach, #{sid => Sid, path => Path}, Context);
        _ -> {error, badarg}
    end;
post([<<"start">>, Id], _, Context) -> call(start, #{id => Id}, Context);
post([<<"stop">>, Id], _, Context) -> call(stop, #{id => Id}, Context);
post([<<"poll">>, Id], #{payload := #{<<"entries">> := Entries}}, Context)
    when is_list(Entries), length(Entries) =< 50 ->
    Valid = lists:all(fun(E) -> is_binary(E) andalso byte_size(E) =< 4096 end, Entries),
    Allowed = Entries =:= [] orelse console_allowed(Context),
    case Valid andalso Allowed of
        true ->
            case z_context:session_id(Context) of
                {ok, Sid} -> call(poll, #{id => Id, sid => Sid, entries => Entries}, Context);
                _ -> {error, eacces}
            end;
        false -> {error, eacces}
    end;
post(_, _, _) -> {error, badarg}.

console_allowed(Context) ->
    authorized(Context).

authorized(Context) ->
    m_development_tools:allowed(Context).

call(Op, Payload, Context) ->
    %% Ownership is relative to the authenticated request, never a client payload.
    Sid = case z_context:session_id(Context) of
        {ok, SessionId} -> SessionId;
        _ -> undefined
    end,
    case mod_development:capture(Op, Payload#{request_sid => Sid}, Context) of
        {ok, #{id := _} = Value} ->
            {ok, capabilities(Value, Context)};
        {ok, Values} when is_list(Values) ->
            {ok, [capabilities(Value, Context) || Value <- Values]};
        Result -> Result
    end.

capabilities(Value, Context) ->
    Value#{console_allowed => console_allowed(Context),
        write_allowed => authorized(Context) andalso not z_acl:is_read_only(Context)}.
result({ok, Value}, Rest) -> {ok, {Value, Rest}};
result({error, _} = Error, _) -> Error.

%% Only reachable by trusted node RPC; never expose this function as a model path.
cli(SiteName, Operation, Id) ->
    Sites = z_sites_manager:get_sites(),
    case [S || S <- maps:keys(Sites), atom_to_binary(S, utf8) =:= unicode:characters_to_binary(SiteName)] of
        [Site] ->
            Context = z_acl:sudo(z_context:new(Site)),
            Path = case Operation of
                "list" -> [<<"list">>];
                _ -> [unicode:characters_to_binary(Operation), unicode:characters_to_binary(Id)]
            end,
            case Path of
                [<<"start">> | _] -> m_post(Path, #{}, Context);
                [<<"stop">> | _] -> m_post(Path, #{}, Context);
                _ -> case m_get(Path, #{}, Context) of
                    {ok, {Value, []}} -> {ok, Value};
                    Error -> Error
                end
            end;
        [] -> {error, unknown_site}
    end.
