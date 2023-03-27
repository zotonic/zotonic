%%%-------------------------------------------------------------------
%%% @author blaise
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
%%% Created : 18. Dec 2017 12:19 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_sitetest).
-author("Blaise").

%% API
-export([info/0, run/1]).

info() ->
    "Run the tests for a site.".

run([ Site ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            io:format("Running tests for site ~p .. ", [ SiteName ]),
            Res = zotonic_command:rpc(z_sitetest, run, [ SiteName ]),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: sitetest <site_name>~n"),
    halt().