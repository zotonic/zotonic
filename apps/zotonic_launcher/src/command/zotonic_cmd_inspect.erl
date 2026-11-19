%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Inspect non-production site dispatch rules, templates and modules.
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

-module(zotonic_cmd_inspect).
-export([info/0, run/1]).

info() -> "Inspect a non-production site: inspect <site> dispatch|dispatch-trace|template|templates|modules [arguments].".

run([Site, Operation | Args]) ->
    case zotonic_command:net_start() of
        ok ->
            case zotonic_command:rpc(m_development_tools, cli, [Site, [Operation | Args]]) of
                {ok, Value} -> output(#{ok => true, result => Value});
                {error, Reason} -> output(#{ok => false, error => reason(Reason)}), halt(1);
                Other -> output(#{ok => false, error => reason(Other)}), halt(1)
            end;
        Error -> zotonic_command:format_error(Error), halt(1)
    end;
run(_) ->
    io:format(standard_error, "Usage: zotonic inspect <site> dispatch|dispatch-trace PATH|template NAME [RESOURCE]|templates [PREFIX]|modules~n", []),
    halt(1).

reason(R) when is_atom(R); is_binary(R) -> R;
reason(R) -> iolist_to_binary(io_lib:format("~p", [R])).
output(Value) -> io:format("~s~n", [z_json:encode(Value)]).
