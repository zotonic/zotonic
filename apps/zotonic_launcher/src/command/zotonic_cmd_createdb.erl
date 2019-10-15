%%%-------------------------------------------------------------------
%%% @author Blaise
%%%Licensed under the Apache License, Version 2.0 (the "License");
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
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 7:44 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_createdb).
-author("Blaise").

%% API
-export([run/1]).

run([ Site ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            Res = zotonic_command:rpc(z_db, prepare_database, [SiteName]),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: createdb <site_name>~n"),
    halt(1).
