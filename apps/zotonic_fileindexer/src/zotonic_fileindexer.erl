%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Indexes directories of applications.

%% Copyright 2018 Marc Worrell
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

-module(zotonic_fileindexer).

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1,

    scan/2,
    scan/3,

    flush/1,
    flush/2
]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").
-include_lib("zotonic_fileindexer/include/zotonic_fileindexer.hrl").

-type fileindex() :: #fileindex{}.

-export_type([fileindex/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ensure_started(zotonic_fileindexer).

start(_StartType, _StartArgs) ->
    zotonic_fileindexer_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%% @doc Scan an application/dir for files matching a file pattern
-spec scan(atom(), file:filename_all()) -> {ok, list( zotonic_fileindexer:fileindex() )} | {error, term()}.
scan(App, SubDir) when is_atom(App) ->
    scan(App, SubDir, undefined).

-spec scan(atom(), file:filename_all(), string()|binary()) -> {ok, list( zotonic_fileindexer:fileindex() )} | {error, term()}.
scan(App, SubDir, Pattern) when is_atom(App) ->
    zotonic_fileindexer_cache:find(App, SubDir, Pattern).

%% @doc Clear the cache for the given application. Useful to force a rescan.
-spec flush(atom()) -> ok.
flush(App) ->
    flush(App, <<>>).

%% @doc Clear the cache for the given application and subdir prefix. Useful to force a rescan.
-spec flush(atom(), file:filename_all()) -> ok.
flush(App, SubDir) ->
    zotonic_fileindexer_cache:flush(App, SubDir).

%%====================================================================
%% Internal functions
%%====================================================================

-spec ensure_started(atom()) -> ok | {error, term()}.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            case ensure_started(Dep) of
                ok -> ensure_started(App);
                {error, _} = Error -> Error
            end;
        {error, {already_started, App}} ->
            ok;
        {error, {Tag, Msg}} when is_list(Tag), is_list(Msg) ->
            {error, lists:flatten(io_lib:format("~s: ~s", [Tag, Msg]))};
        {error, {bad_return, {{M, F, Args}, Return}}} ->
            A = string:join([io_lib:format("~p", [A])|| A <- Args], ", "),
            {error, lists:flatten(
                        io_lib:format("~s failed to start due to a bad return value from call ~s:~s(~s):~n~p",
                                      [App, M, F, A, Return]))};
        {error, Reason} ->
            {error, Reason}
    end.
