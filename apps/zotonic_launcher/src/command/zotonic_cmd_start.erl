%%%-------------------------------------------------------------------
%%% @author fabu
%%% @doc
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
%%% Created : 18. Dec 2017 3:53 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_start).
-author("Blaise").

%% API
-export([run/1]).

run(_) ->
    case heart(os:getenv("HEART")) of
        {ok, HeartEnv} ->
            case zotonic_command:base_cmd() of
                {ok, BaseCmd} ->
                    io:format("~s ~s -heart -detached -s zotonic", [ BaseCmd, HeartEnv ]);
                {error, Error} ->
                    io:format(standard_error, "~s", [ Error ]),
                    halt(1)
            end;
        {error, _} ->
            io:format(standard_error, "Too many restarts, stopping.~n", []),
            halt(1)
    end.

heart(Heart) when Heart =:= false; Heart =:= "" ->
    heart_env(0, z_datetime:timestamp());
heart(_Heart) ->
    % Was restarted by heart, check if we are in a restart loop
    Start = z_convert:to_integer( os:getenv("ZOTONIC_HEART_START", "0") ),
    Restarts = z_convert:to_integer( os:getenv("ZOTONIC_HEART_RESTARTS", "0") ),
    Now = z_datetime:timestamp(),
    case Now - Start of
        TimeDiff when TimeDiff =< 60 ->
            case Restarts of
                N when N =< 5 ->
                    heart_env(Restarts+1, Start);
                _ ->
                    {error, too_many_restarts}
            end;
        _ ->
            heart_env(0, Now)
    end.

heart_env(Count, Timestamp) ->
    RestartCmd = lists:flatten([
        os:getenv("ENV", "/usr/bin/env"),
        " HEART=true",
        " ZOTONIC_HEART_RESTARTS=", integer_to_list(Count),
        " ZOTONIC_HEART_START=", integer_to_list(Timestamp),
        " ", os:getenv("ZOTONIC_BIN", "bin"), "/zotonic start"
    ]),
    {ok, lists:flatten("-env HEART_COMMAND ", z_utils:os_filename(RestartCmd))}.
