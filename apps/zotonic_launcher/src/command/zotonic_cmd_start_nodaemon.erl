%%%-------------------------------------------------------------------
%%% @author Blaise
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
%%% Created : 18. Dec 2017 4:04 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_start_nodaemon).
-author("Blaise").

%% API
-export([run/1]).

run(_) ->
    case zotonic_launcher_app:is_root() of
        true ->
            zotonic_command:format_error({error, not_running_as_root}),
            halt(1);
        false ->
            case zotonic_command:base_cmd() of
                {ok, BaseCmd} ->
                    io:format("~s", [ BaseCmd ++ " -s zotonic -noshell " ]);
                {error, Error} ->
                    io:format(standard_error, "~s", [ Error ]),
                    halt(1)
            end
    end.
