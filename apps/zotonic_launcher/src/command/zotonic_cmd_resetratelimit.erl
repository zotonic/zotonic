%%%-------------------------------------------------------------------
%%% @author Marco
%%% @doc
%%% vim: noai:ts=4:sw=4:sts=4:expandtab
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(zotonic_cmd_resetratelimit).
-author("Marco").

%% API
-export([info/0, run/1]).

info() ->
    "Reset the rate limiter for a site".

is_module_active(Modules) ->
    case lists:member(mod_ratelimit, Modules) of
        true -> ok;
        false -> {error, nomodule}
    end.

check_modules(Context) ->
	Modules = zotonic_command:rpc(z_module_manager, active, [ Context ]),
	case Modules of
        [] -> {error, nosite};
        [_|_] -> is_module_active(Modules)
    end.

format_result(ok) ->
        io:format("ok~n");
format_result({error, nosite}) ->
        io:format("Error: No such site~n");
format_result({error, nomodule}) ->
        io:format("Error: Site is not running mod_ratelimit~n").

run([ Arg ]) ->
    SiteName = list_to_atom(Arg),
    Context = z:c(SiteName),
    case zotonic_command:net_start() of
        ok ->
            Result = case check_modules(Context) of
                ok ->
                    zotonic_command:rpc(m_ratelimit, reset, [ Context ]);
                {error, _} = Error -> Error
            end,
            format_result(Result);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: resetratelimit <site>~n"),
    halt().
