%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Show dispatch information for a site

%% Copyright 2021 Marc Worrell
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

-module(zotonic_cmd_dispatch).
-author("Marc Worrell").

%% API
-export([info/0, run/1]).

info() ->
    "Show dispatch information for a site or url.".

run([ "http:" ++ _ = URL ]) ->
    dispatch_url(URL);
run([ "https:" ++ _ = URL ]) ->
    dispatch_url(URL);
run([ Site ]) ->
    dispatch_site_list(Site);
run([ Site, Path ]) ->
    dispatch_site_path(Site, Path);
run(_) ->
    io:format("USAGE: dispatch <site_name> [path]~n"),
    io:format("       dispatch <URL>~n"),
    halt().

dispatch_url(URL) ->
    case zotonic_command:net_start() of
        ok ->
            Res = zotonic_command:rpc(zotonic_filehandler_compile, ld, []),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

dispatch_site_list(Site) ->
    case zotonic_command:net_start() of
        ok ->
            Res = zotonic_command:rpc(zotonic_filehandler_compile, ld, []),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

dispatch_site_path(Site, Path) ->
    case zotonic_command:net_start() of
        ok ->
            Res = zotonic_command:rpc(zotonic_filehandler_compile, ld, []),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
