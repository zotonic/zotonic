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
Admin-only capture API. GET list, status/:id, trace/:id, console/:id;
POST attach {path}, start/:id, stop/:id, poll/:id {entries: [text]}.
All operations require development, test or acceptance environment.
Poll is bound to the attaching browser session. All results expire after ten minutes.
SSH uses cli/3 over the trusted Erlang distribution connection.

Click the floating debug button and choose Attach capture. The panel displays
an id. From the server checkout run:

```sh
bin/zotonic capture mysite list
bin/zotonic capture mysite start CAPTURE_ID
bin/zotonic capture mysite trace CAPTURE_ID
bin/zotonic capture mysite console CAPTURE_ID
bin/zotonic capture mysite stop CAPTURE_ID
```

HTTP clients use the existing authenticated model API, for example
GET /api/model/development_capture/get/trace/CAPTURE_ID and
POST /api/model/development_capture/post/stop/CAPTURE_ID.
MQTT uses model/development_capture/get|post/... with the same paths.
Start is idempotent and retains evidence; attach a new capture for a fresh graph.
An attachment starts stopped; the debug button attaches and then starts it.
The browser polls every second; console collection starts when it observes active
state and only covers subsequent console.log calls. Logs are best effort, text
snapshots, capped at 500 entries and 4096 bytes per entry (50 per upload).
No cookies or browser session ids are returned. The HTTP API requires normal
administrator authentication; the SSH command uses trusted node RPC authority.
All capture access is denied in production, including over SSH.

Captures survive page reload via sessionStorage. The initial page render is
only traced if capture was already active before loading it. Template traces
are session-scoped, so other tabs may contribute. Console entries come from the
attached page; duplicated tabs may inherit sessionStorage and should attach a
fresh capture. Existing human template tracing remains independent. A capture
expires ten minutes after attachment, including its graph and console entries.
The graph is capped at approximately 1000 nodes / 5000 edges. The truncated
flag reports graph or server log limits. Browser buffers are best effort and
can drop entries on overflow, disconnect, navigation or stopping over SSH.
Captures do not provide screenshots, historic browser console entries, DOM
snapshots or browser action execution in this first version.
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
    case authorized(Context) of
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
    case mod_development:capture(Op, Payload, Context) of
        {ok, #{id := _} = Value} -> {ok, Value#{console_allowed => console_allowed(Context)}};
        Result -> Result
    end.
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
