%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Maintain bounded capture state owned by mod_development. Callers must authorize operations.
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

-module(z_development_capture).
-export([command/3, render/3]).

-define(TTL, 600).
-define(MAX_LOGS, 500).

-spec command(Operation, Payload, Captures) -> {{ok, term()} | {error, term()}, map()}
    when Operation :: atom(), Payload :: map(), Captures :: map().
-spec render(SessionId, Render, Captures) -> map()
    when SessionId :: binary() | undefined, Render :: fun((map()) -> map()), Captures :: map().

%% Expiry is absolute: polling cannot keep sensitive diagnostics indefinitely.
command(Op, Payload, Captures0) ->
    Captures = prune(Captures0),
    command_1(Op, Payload, Captures).

command_1(attach, #{sid := Sid, path := Path}, Captures)
    when is_binary(Sid), is_binary(Path), byte_size(Path) =< 2048, map_size(Captures) < 16 ->
    Id = z_ids:id(24),
    Capture = #{id => Id, sid => Sid, path => Path, state => stopped,
        expires => erlang:system_time(second) + ?TTL,
        graph => #{nodes => #{}, edges => #{}}, logs => [], sequence => 0,
        truncated => false},
    {{ok, public(Capture)}, Captures#{Id => Capture}};
command_1(list, _, Captures) ->
    {{ok, [public(C) || C <- maps:values(Captures)]}, Captures};
command_1(Op, #{id := Id} = Payload, Captures) ->
    case maps:find(Id, Captures) of
        {ok, C} ->
            {Result, C1} = operation(Op, Payload, C),
            {Result, Captures#{Id => C1}};
        error -> {{error, enoent}, Captures}
    end;
command_1(_, _, Captures) ->
    {{error, badarg}, Captures}.

operation(start, _, C) ->
    %% Start is idempotent and retains evidence. Attach again for a fresh capture.
    C1 = C#{state => active},
    {{ok, public(C1)}, C1};
operation(stop, _, C) ->
    C1 = C#{state => stopped},
    {{ok, public(C1)}, C1};
operation(status, _, C) -> {{ok, public(C)}, C};
operation(trace, _, #{graph := #{nodes := Ns, edges := Es}} = C) ->
    {{ok, #{capture => public(C), nodes => maps:values(Ns), edges => maps:values(Es)}}, C};
operation(console, _, C) ->
    {{ok, #{entries => lists:reverse(maps:get(logs, C)), truncated => maps:get(truncated, C)}}, C};
operation(poll, #{sid := Sid, entries := Entries}, #{sid := Sid} = C) ->
    C1 = case maps:get(state, C) of
        active -> lists:foldl(fun add_log/2, C, Entries);
        stopped -> C
    end,
    {{ok, public(C1)}, C1};
operation(_, _, C) -> {{error, badarg}, C}.

add_log(Text, #{logs := Logs, sequence := Seq} = C) ->
    C#{logs => lists:sublist([#{seq => Seq + 1, text => Text,
        received_at => erlang:system_time(millisecond)} | Logs], ?MAX_LOGS),
        sequence => Seq + 1,
        truncated => maps:get(truncated, C) orelse length(Logs) >= ?MAX_LOGS}.

render(Sid, Render, Captures) ->
    maps:map(fun
        (_, #{sid := S, state := active, graph := G} = C) when S =:= Sid, is_binary(Sid) ->
            case map_size(maps:get(nodes, G)) < 1000 andalso map_size(maps:get(edges, G)) < 5000 of
                true -> C#{graph => Render(G)};
                false -> C#{truncated => true}
            end;
        (_, C) -> C
    end, prune(Captures)).

public(C) ->
    (maps:with([id, path, state, expires, truncated], C))#{trace_scope => session}.

prune(Captures) ->
    Now = erlang:system_time(second),
    maps:filter(fun(_, #{expires := Expires}) -> Expires > Now end, Captures).
