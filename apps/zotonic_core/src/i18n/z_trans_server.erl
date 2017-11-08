%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Simple server to manage the translations, owns the ets table containing all translations.
%% When new translations are read then the previous table is kept and the one before the previous is deleted.

%% Copyright 2010-2017 Marc Worrell
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

-module(z_trans_server).
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_tests/0,start_link/1]).

%% interface functions
-export([
    load_translations/1,
    load_translations/2,
    table/1,
    set_context_table/1,
    observe_module_ready/2
]).

-include_lib("zotonic.hrl").

-record(state, {table, site}).

%%====================================================================
%% API
%%====================================================================

-spec start_tests() -> {ok, pid()} | {error, term()}.
start_tests() ->
    io:format("Starting trans server.~n"),
    gen_server:start_link({local, 'z_trans_server$test'}, ?MODULE, test, []).

%% @doc Starts the server
-spec start_link(Site :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, {Site, Name}, []).


%% @doc Parse all .po files and reload the found translations in the trans server
-spec load_translations(z:context()) -> ok.
load_translations(Context) ->
    Ts = z_trans:parse_translations(Context),
    load_translations(Ts, Context).

%% @doc Take a proplist with dicts and reload the translations table.
%% After reloading the the template server is flushed.
-spec load_translations(map(), z:context()) -> ok.
load_translations(Trans, Context) ->
    Name = z_utils:name_for_site(?MODULE, z_context:site(Context)),
    gen_server:cast(Name, {load_translations, Trans}).

%% @doc Return the name of the ets table holding all translations
-spec table(atom()|z:context()) -> atom().
table(Site) when is_atom(Site) ->
    z_utils:name_for_site(?MODULE, Site);
table(#context{} = Context) ->
    Context#context.translation_table.

%% @doc Set the table id in the context to the newest table id
-spec set_context_table(z:context()) -> z:context().
set_context_table(#context{} = Context) ->
    Context#context{translation_table=table(z_context:site(Context))}.

%% @doc Reload the translations when modules are changed.
-spec observe_module_ready(module_ready, z:context()) -> ok.
observe_module_ready(module_ready, Context) ->
    load_translations(Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init({ Site :: atom(), Name :: atom() }) -> {ok, #state{}}.
init({Site, Name}) ->
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    process_flag(trap_exit, true),
    z_notifier:observe(module_ready, {?MODULE, observe_module_ready}, Site),
    Table = ets:new(Name, [named_table, set, protected, {read_concurrency, true}]),
    {ok, #state{table=Table, site=Site}}.

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Rebuild the translations table. Call the template flush routines afterwards.
%% Trans is a map with all translations per translatable string.
handle_cast({load_translations, Trans}, State) ->
    F = fun(Key, Value, Acc) ->
            Value1 = case proplists:get_value(en, Value) of
                undefined -> [{en,Key}|Value];
                _ -> Value
            end,
            [{Key,Value1}|Acc]
        end,
    List = maps:fold(F, [], Trans),
    sync_to_table(List, State#state.table),
    z_template:reset(State#state.site),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(module_ready, State#state.site),
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
	case State of
		{state, Table, _OldTable} ->
			{ok, #state{table=Table}};
		_ ->
		    {ok, State}
	end.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Sync a list of translations to the ets table containing all translations
sync_to_table(List, Table) ->
	LT = lists:sort(ets:tab2list(Table)),
	List1 = lists:sort(List),
	sync(List1, LT, Table).


sync([], [], _Table) ->
	ok;
sync(L, [], Table) ->
	ets:insert(Table, L);
sync([], L, Table) ->
	lists:map(fun({Key,_}) -> ets:delete(Table, Key) end, L);
sync([H|NewList], [H|OldList], Table) ->
	sync(NewList, OldList, Table);
sync([{K,V}|NewList], [{K,_}|OldList], Table) ->
	ets:insert(Table, [{K,V}]),
	sync(NewList, OldList, Table);
sync([{K1,V1}|NewList], [{K2,_}|_] = OldList, Table) when K1 < K2 ->
	ets:insert(Table, [{K1,V1}]),
	sync(NewList, OldList, Table);
sync([{K1,_}|_] = NewList, [{K2,_}|OldList], Table) when K1 > K2 ->
	ets:delete(Table, K2),
	sync(NewList, OldList, Table).

