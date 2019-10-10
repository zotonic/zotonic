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
%%% @end
%%% Created : 18. Dec 2017 11:06 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_flush).
-author("Blaise").

%% API
-export([run/1]).

run([ Site ]) ->
    SiteName = list_to_atom(Site),
    flush([ SiteName ]);
run([]) ->
    flush([]);
run(_) ->
    io:format("USAGE: flush [site_name]~n").


flush(Args) ->
    case zotonic_command:net_start() of
        ok ->
            Res = zotonic_command:rpc(z, flush, Args),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
