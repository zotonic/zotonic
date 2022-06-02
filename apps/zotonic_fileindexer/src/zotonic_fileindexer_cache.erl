%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Find (and cache) files in directories.

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

-module(zotonic_fileindexer_cache).

-behaviour(gen_server).

-export([find/3,
         flush/0,
         flush/2,
         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").

-record(findex, {key :: tuple(), app :: atom(), subdir :: binary(), files :: [zotonic_fileindexer:fileindex()]}).
-record(state, {}).

-define(TIMEOUT, infinity).

%% @doc Find all files underneath a app/dir. Optional with a certain extension.
-spec find(atom(), file:filename_all(), binary() | list() | undefined) ->
              {ok, [zotonic_fileindexer:fileindex()]} | {error, term()}.
find(App, SubDir, OptPattern) when is_atom(App) ->
    {ok, AppDir} = app_dir(App),
    case filelib:is_dir(AppDir) of
        true ->
            find_1(App, AppDir, SubDir, pattern(OptPattern));
        false ->
            {error, notfound}
    end.

find_1(App, AppDir, SubDir, Pattern) ->
    AppDir1 = unicode:characters_to_binary(AppDir),
    SubDir1 = unicode:characters_to_binary(SubDir),
    case ets:lookup(?MODULE, key(App, SubDir1, Pattern)) of
        [] ->
            gen_server:call(?MODULE, {index, App, AppDir1, SubDir1, Pattern}, ?TIMEOUT);
        [#findex{ files = Files }] ->
            {ok, Files}
    end.

-spec flush() -> ok.
flush() ->
    gen_server:call(?MODULE, flush, infinity).

-spec flush(atom(), file:filename_all()) -> ok.
flush(App, SubDir) when is_atom(App) ->
    gen_server:call(?MODULE, {flush, App, unicode:characters_to_binary(SubDir)}, infinity).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    ets:new(?MODULE, [set, {keypos, #findex.key}, protected, named_table]),
    {ok, #state{  }}.

handle_call({index, App, AppDir, SubDir, Pattern}, _From, State) ->
    Files =
        zotonic_fileindexer_scan:scan(
            filename:join(AppDir, SubDir), Pattern),
    Key = key(App, SubDir, Pattern),
    FIndex =
        #findex{
            key = Key,
            app = App,
            subdir = unicode:characters_to_binary(SubDir),
            files = Files
        },
    ets:insert(?MODULE, FIndex),
    {reply, {ok, Files}, State};
handle_call({flush, App, SubDir}, _From, State) ->
    do_flush(App, SubDir),
    {reply, ok, State};
handle_call(flush, _From, State) ->
    ets:delete_all_objects(?MODULE),
    {reply, ok, State};
handle_call(_Cmd, _From, State) ->
    {reply, {error, uknown_command}, State}.

handle_cast(_Cmd, State) ->
    {noreply, State}.

handle_info(_Cmd, State) ->
    {noreply, State}.

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Local functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_dir(App) ->
    case code:lib_dir(App) of
        {error, bad_name} ->
            % Not started - assume it is located in our _build directory
            {ok, filename:join([build_dir(), App])};
        Path when is_list(Path) ->
            {ok, Path}
    end.

build_dir() ->
    filename:dirname(
        filename:dirname(
            code:lib_dir(zotonic_fileindexer))).

key(App, SubDir, OptPattern) ->
    P = pattern(OptPattern),
    {App, unicode:characters_to_binary(SubDir), P}.

pattern(undefined) ->
    <<".">>;
pattern("") ->
    <<".">>;
pattern(<<>>) ->
    <<".">>;
pattern(P) ->
    unicode:characters_to_binary(P).

do_flush(App, SubDir) ->
    Ks = ets:foldl(fun(FIndex, Acc) ->
                      case match(FIndex, App, SubDir) of
                          true ->
                              [FIndex#findex.key | Acc];
                          false ->
                              Acc
                      end
                   end,
                   [],
                   ?MODULE),
    lists:foreach(fun(K) ->
                     ets:delete(?MODULE, K)
                  end,
                  Ks).

match(#findex{
          app = App,
          subdir = FSub
      },
      App,
      SubDir) ->
    is_prefix(SubDir, FSub);
match(_, _App, _SubDir) ->
    false.

is_prefix(<<>>, _) ->
    true;
is_prefix(<<C/utf8, A/binary>>, <<C/utf8, B/binary>>) ->
    is_prefix(A, B);
is_prefix(_, _) ->
    false.
