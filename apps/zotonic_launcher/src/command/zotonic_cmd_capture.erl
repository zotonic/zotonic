%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Control development capture sessions from the command line.
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

-module(zotonic_cmd_capture).
-export([info/0, run/1]).

info() -> "Control an attached development capture: capture <site> list|start|stop|status|trace|console [id].".

run([Site, "list"]) -> execute(Site, "list", "");
run([Site, Operation, Id]) when Operation =:= "start"; Operation =:= "stop";
    Operation =:= "status"; Operation =:= "trace"; Operation =:= "console" ->
    execute(Site, Operation, Id);
run(_) ->
    io:format(standard_error, "Usage: zotonic capture <site> list|start|stop|status|trace|console [id]~n", []),
    halt(1).

execute(Site, Operation, Id) ->
    case zotonic_command:net_start() of
        ok ->
            Result = zotonic_command:rpc(m_development_capture, cli, [Site, Operation, Id]),
            case Result of
                {ok, Value} -> output(#{ok => true, result => Value});
                {error, Reason} -> output(#{ok => false, error => z_convert:to_binary(Reason)}), halt(1);
                Other -> output(#{ok => false, error => iolist_to_binary(io_lib:format("~p", [Other]))}), halt(1)
            end;
        Error -> zotonic_command:format_error(Error)
    end.

output(Value) -> io:format("~s~n", [z_json:encode(Value)]).
