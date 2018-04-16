%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2013 Marc Worrell
%% @doc Manage, compile and find mediaclass definitions per context/site.

%% Copyright 2012-2013 Marc Worrell
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

-module(z_mediaclass).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% API exports
-export([
    new_ets/0,
    get/2,

    expand_mediaclass_checksum/1,
    expand_mediaclass_checksum/2,
    expand_mediaclass/2,

    reset/1,
    module_reindexed/2
]).

-record(state, {
    context :: z:context(),
    last = []
}).

%% Index record for the mediaclass ets table.
-record(mediaclass_index_key, {
    site :: atom(),
    mediaclass
}).
-record(mediaclass_index, {
    key,
    props = [],
    checksum,
    tag
}).

-define(MEDIACLASS_FILENAME, <<"mediaclass.config">>).
-define(RESCAN_PERIOD, 10000).

%% Name of the global mediaclass index table
-define(MEDIACLASS_INDEX, 'zotonic$mediaclass_index').

-include("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================

-spec new_ets() -> ets:tid() | atom().
new_ets() ->
    ets:new(?MEDIACLASS_INDEX, [set, public, named_table, {keypos, #mediaclass_index.key}]).


%% @spec start_link(Props) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Fetch the mediaclass definition for the current context.
-spec get(MediaClass :: list() | binary(), #context{}) -> {ok, PreviewProps :: list(), Checksum :: binary()} | {error, term()}.
get(MediaClass, Context) ->
    case ets:lookup(?MEDIACLASS_INDEX,
                    #mediaclass_index_key{
                        site=z_context:site(Context),
                        mediaclass=z_convert:to_binary(MediaClass)})
    of
        [] -> {ok, [], <<>>};
        [MC|_] -> {ok, MC#mediaclass_index.props, MC#mediaclass_index.checksum};
        {error, _} = Error -> Error
    end.

%% @doc Expand the mediaclass, use the checksum when available
-spec expand_mediaclass_checksum(list() | binary()) -> {ok, list()} | {error, term()}.
expand_mediaclass_checksum(Checksum) when is_binary(Checksum) ->
    expand_mediaclass_checksum(Checksum, []);
expand_mediaclass_checksum(Props) ->
    case proplists:get_value(mediaclass, Props) of
        undefined ->
            {ok, Props};
        {MC, Checksum} when Checksum =/= undefined ->
            expand_mediaclass_checksum_1(MC, Checksum, Props);
        Checksum ->
            expand_mediaclass_checksum_1(undefined, z_convert:to_binary(Checksum), Props)
    end.

-spec expand_mediaclass_checksum(Checksum :: binary(), Props :: list()) -> list().
expand_mediaclass_checksum(Checksum, Props) ->
    expand_mediaclass_checksum_1(undefined, Checksum, Props).

    expand_mediaclass_checksum_1(Class, Checksum, Props) ->
        % Expand for preview generation, we got the checksum from the URL.
        case ets:lookup(?MEDIACLASS_INDEX, Checksum) of
            [#mediaclass_index{props=Ps}|_] ->
                {ok, expand_mediaclass_2(Props, Ps)};
            [] ->
                lager:warning("mediaclass expand for unknown mediaclass checksum ~p:~p", [Class, Checksum]),
                {error, checksum};
            {error, _} = Error ->
                Error
        end.


%% @doc Expand the optional mediaclass for tag generation
-spec expand_mediaclass(list(), #context{}) -> {ok, list()} | {error, term()}.
expand_mediaclass(Props, Context) ->
    case proplists:get_value(mediaclass, Props) of
        MC when is_list(MC); is_binary(MC) ->
            % Expand for tag generation
            expand_mediaclass_1(MC, Props, Context);
        {MC, _Checksum} when is_list(MC); is_binary(MC) ->
            expand_mediaclass_1(MC, Props, Context);
        undefined ->
            {ok, Props}
    end.

expand_mediaclass_1(MC, Props, Context) ->
    case get(MC, Context) of
        {ok, Ps, _Checksum} ->
            {ok, expand_mediaclass_2(Props, Ps)};
        Error ->
            Error
    end.

expand_mediaclass_2(Props, ClassProps) ->
    lists:foldl(fun(KV, Acc) ->
                    K = key(KV),
                    case proplists:is_defined(K, Acc) of
                        false -> [ KV | Acc ];
                        true -> Acc
                    end
                end,
                proplists:delete(mediaclass, Props),
                ClassProps).

    key({K,_}) -> K;
    key(K) -> K.

%% @doc Call this to force a re-index and parse of all moduleclass definitions.
-spec reset(#context{}) -> ok.
reset(Context) ->
    gen_server:cast(z_utils:name_for_site(?MODULE, Context), module_reindexed).


%% @doc Observer, triggered when there are new module files indexed
-spec module_reindexed(module_reindexed, #context{}) -> ok.
module_reindexed(module_reindexed, Context) ->
    reset(Context).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(SiteProps) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(SiteProps) ->
    process_flag(trap_exit, true),
    {site, Site} = proplists:lookup(site, SiteProps),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    Context = z_context:new(Site),
    z_notifier:observe(module_reindexed, {?MODULE, module_reindexed}, Context),
    {ok, #state{context=Context}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State, ?RESCAN_PERIOD}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(module_reindexed, State) ->
    {noreply, reindex(State), ?RESCAN_PERIOD};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State, ?RESCAN_PERIOD}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Check every RESCAN_PERIOD msecs if there are changes.
handle_info(timeout, State) ->
    {noreply, reindex(State), ?RESCAN_PERIOD};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State, ?RESCAN_PERIOD}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(module_reindexed, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Find all mediaclass files for all device types.
reindex(#state{context=Context, last=Last} = State) ->
    case collect_files(Context) of
        {error, timeout} ->
            lager:warning("timeout on module indexer"),
            State;
        {ok, Last} ->
            State;
        {ok, {Fs,_MaxMod} = New} ->
            % Something changed, parse and update all classes in the files.
            Site = z_context:site(State#state.context),
            ok = reindex_files(Fs, Site),
            lager:debug("Re-indexed mediaclass definitions"),
            State#state{last=New}
    end.


collect_files(Context) ->
    case z_module_indexer:find_all(template, ?MEDIACLASS_FILENAME, Context) of
        [] ->
            {ok, {[], undefined}};
        Ms when is_list(Ms) ->
            Paths = [ {Module, Path} || #module_index{filepath=Path, module=Module} <- Ms ],
            {ok, {Paths, lists:max([ filelib:last_modified(Path) || {_, Path} <- Paths ])}};
        {error, _} = Error ->
            Error
    end.


reindex_files(Files, Site) ->
    Tag = make_ref(),
    MCs = lists:flatten([ expand_file(MP) || MP <- Files ]),
    ByModulePrio = prio_sort(MCs),
    % Insert least prio first, later overwrite with higher priority modules
    [ insert_mcs(MCs1, Tag, Site) || MCs1 <- ByModulePrio ],
    cleanup_ets(Tag, Site),
    ok.

% Sort all defs, lowest prio first, higher prios later
prio_sort(MProps) ->
    WithPrio = [ {z_module_manager:prio(M), M, X} || {M, X} <- MProps ],
    Sorted = lists:sort(fun prio_comp/2, WithPrio),
    [ X || {_Prio, _M, X} <- Sorted ].

prio_comp({P1, M1, _}, {P2, M2, _}) ->
    {P1, M1} > {P2, M2}.


insert_mcs(MCs, Tag, Site) ->
    Defs = [
        begin
            Props1 = lists:sort(Props),
            {z_convert:to_binary(MediaClass),
             Props1,
             z_convert:to_binary(
                z_string:to_lower(iolist_to_binary(z_utils:hex_encode(crypto:hash(sha, term_to_binary(Props1)))))
             )}
        end
        || {MediaClass, Props} <- lists:flatten(MCs)
    ],
    insert_defs(Defs, Tag, Site).

insert_defs(Defs, Tag, Site) ->
    [ insert_def(Def, Tag, Site) || Def <- Defs ].

% Insert the mediaclass definition by lookup key and checksum
insert_def({MC, Ps, Checksum}, Tag, Site) ->
    K = #mediaclass_index_key{site=Site, mediaclass=MC},
    ets:insert(?MEDIACLASS_INDEX,
                #mediaclass_index{
                    key=K,
                    props=Ps,
                    checksum=Checksum,
                    tag=Tag
                }),
    ets:insert(?MEDIACLASS_INDEX,
                #mediaclass_index{
                    key=Checksum,
                    props=Ps,
                    checksum=Checksum,
                    tag=Tag
                }).


expand_file({Module, Path}) ->
    {Module, lists:flatten(consult_file(Path))}.

consult_file(Path) ->
    case file:consult(Path) of
        {error, Reason} ->
            % log an error and continue
            lager:error("Error consulting media class file ~p: ~p (skipped)", [Path, Reason]),
            [];
        {ok, MediaClasses} ->
            MediaClasses
    end.


%% @doc Remove all ets entries for this host with an old tag
cleanup_ets(Tag, Site) ->
    ets:safe_fixtable(?MEDIACLASS_INDEX, true),
    try
        cleanup_ets_1(ets:first(?MEDIACLASS_INDEX), Tag, Site, [])
    after
        ets:safe_fixtable(?MEDIACLASS_INDEX, false)
    end.

cleanup_ets_1('$end_of_table', _Tag, _Site, Acc) ->
    [ ets:delete(?MEDIACLASS_INDEX, K) || K <- Acc ];
cleanup_ets_1(#mediaclass_index_key{site=Site} = K, Tag, Site, Acc) ->
    case ets:lookup(?MEDIACLASS_INDEX, K) of
        [#mediaclass_index{tag=Tag}] ->
            cleanup_ets_1(ets:next(?MEDIACLASS_INDEX, K), Tag, Site, Acc);
        _ ->
            cleanup_ets_1(ets:next(?MEDIACLASS_INDEX, K), Tag, Site, [K|Acc])
    end;
cleanup_ets_1(K, Tag, Site, Acc) ->
    cleanup_ets_1(ets:next(?MEDIACLASS_INDEX, K), Tag, Site, Acc).


