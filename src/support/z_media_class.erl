%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Manage, compile and find mediaclass definitions per context/site.

%% Copyright 2012 Marc Worrell
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

-module(z_media_class).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% API exports
-export([
    get/2,
    get/3,
    
    expand_mediaclass_checksum/1,
    expand_mediaclass_checksum/2,
    expand_mediaclass/2,
    
    reset/1,
    module_reindexed/2
]).

-record(state, {context, last=[]}).
-define(MEDIACLASS_FILENAME, "mediaclass.config").
-define(RESCAN_PERIOD, 10000).

-include("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Props) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Fetch the mediaclass definition for the current context.
-spec get(MediaClass :: list() | binary(), #context{}) -> {ok, PreviewProps :: list(), Checksum :: binary()} | {error, term()}.
get(MediaClass, Context) ->
    get(z_user_agent:get_class(Context), MediaClass, Context).


%% @doc Fetch the mediaclass definition, returns a property list and a checksum.
%%      When there is no mediaclass definition for the current context then 
-spec get(ua_classifier:device_type(), MediaClass :: list() | binary(), #context{}) -> {ok, Props :: list(), Checksum :: binary()} | {error, term()}.
get(UAClass, MediaClass, Context) ->
    case ets:lookup(?MEDIACLASS_INDEX, 
                    #mediaclass_index_key{
                        site=z_context:site(Context), 
                        mediaclass=z_convert:to_binary(MediaClass), 
                        ua_class=UAClass})
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
                {ok, expand_mediaclass_1(Props, Ps)};
            [] ->
                lager:warning("mediaclass expand for unknown mediaclass checksum ~p:~p", [Class, Checksum]),
                {ok, Props};
            {error, _} = Error ->
                Error
        end.


%% @doc Expand the mediaclass for tag generation
-spec expand_mediaclass(list(), #context{}) -> {ok, list()} | {error, term()}.
expand_mediaclass(Props, Context) ->
    case proplists:get_value(mediaclass, Props) of
        MC when is_list(MC); is_binary(MC) ->
            % Expand for tag generation
            case get(MC, Context) of
                {ok, Ps, _Checksum} ->
                    {ok, expand_mediaclass_1(Props, Ps)};
                Error ->
                    Error
            end
    end.
    
expand_mediaclass_1(Props, ClassProps) ->
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
    gen_server:cast(z_utils:name_for_host(?MODULE, Context), module_reindexed).


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
    Context = z_context:new(proplists:get_value(host, SiteProps)),
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
    z_notifier:detach(module_reindexed, {?MODULE, module_reindexed}, State#state.context),
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
    Files = [
        case z_module_indexer:find_ua_class_all(template, UAClass, ?MEDIACLASS_FILENAME, Context) of
            [] -> 
                {UAClass, undefined, undefined};
            Ms ->
                Paths = [ Path || #module_index{filepath=Path} <- Ms ],
                {UAClass, Paths, lists:max([ filelib:last_modified(Path) || Path <- Paths ])}
        end
        || UAClass <- [ generic | z_user_agent:classes() ]
    ],
    case Files of
        Last ->
            State;
        _ ->
            % Something changed, parse and update all classes in the files.
            reindex_files(Files, State#state{last=Files})
    end.

reindex_files(Files, State) ->
    Tag = now(),
    Site = z_context:site(State#state.context),
    [ reindex_file(F, Tag, Site) || F <- Files ],
    cleanup_ets(Tag, Site),
    State.

reindex_file({_UAClass, undefined, undefined}, _Tag, _Site) ->
    % No mediaclass file for the ua class
    ok;
reindex_file({_UAClass, _Path, 0}, _Tag, _Site) ->
    % Could not stat the mediaclass file, file might have disappeared.
    ok;
reindex_file({UAClass, Paths, _Modified}, Tag, Site) ->
    Forms = [ 
        case file:consult(Path) of
            {error, Reason} ->
                % log an error and continue
                lager:error("Error consulting media class file ~p: ~p", [Path, Reason]),
                [];
            {ok, Fs} -> 
                Fs
        end
        || Path <- Paths
    ],
    % Process the mediaclass definitions
    Defs = [
        begin
            Props1 = lists:sort(Props),
            {z_convert:to_binary(MediaClass),
             Props1,
             z_convert:to_binary(
                z_string:to_lower(iolist_to_binary(z_utils:hex_encode(crypto:sha(term_to_binary(Props1)))))
             )}
        end
        || {MediaClass, Props} <- lists:flatten(Forms)
    ],
    % Insert all into the mediaclass ets table
    % Also insert by checksum for the lookup of a mediaclass definition by url.
    [
        begin
            K = #mediaclass_index_key{site=Site, ua_class=UAClass, mediaclass=MC},
            case ets:lookup(?MEDIACLASS_INDEX, K) of
                [#mediaclass_index{tag=Tag}|_] ->
                    ok;
                _Other ->
                    ets:insert(?MEDIACLASS_INDEX,
                                #mediaclass_index{
                                    key=K,
                                    props=Ps,
                                    checksum=Checksum,
                                    tag=Tag
                                })
            end,
            case ets:lookup(?MEDIACLASS_INDEX, Checksum) of
                [#mediaclass_index{tag=Tag}|_] ->
                    ok;
                _ ->
                    ets:insert(?MEDIACLASS_INDEX,
                                #mediaclass_index{
                                    key=Checksum,
                                    props=Ps,
                                    checksum=Checksum,
                                    tag=Tag
                                })
            end
        end
        || {MC, Ps, Checksum} <- Defs
    ],
    ok.


%% @doc Remove all ets entries for this host with an old tag
cleanup_ets(Tag, Site) ->
    cleanup_ets_1(ets:first(?MEDIACLASS_INDEX), Tag, Site, []).
    
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


