%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2009-06-09
%% @doc Defines Solr queries for extended content searches in Zotonic.

-module(mod_search_solr).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("Solr Search Queries").
-mod_description("Use Solr for extended content searches in Zotonic.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         search/4
]).

-include("zotonic.hrl").

-record(state, {context, solr}).



%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    
    {ok, Solr} = esolr:start_link(),
    esolr:set_auto_commit({time, 3000}, Solr),

    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(search_query, self(), Context),

    %% Watch for changes to resources
    z_notifier:observe(rsc_pivot_done, self(), Context),
    z_notifier:observe(rsc_delete, self(), Context),

    {ok, #state{context=z_context:new(Context),solr=Solr}}.


%% @doc Trap unknown calls
handle_call({{search_query, Search, Limit}, Context}, _From, State=#state{solr=Solr}) ->
    Reply = ?MODULE:search(Search, Limit, Context, Solr),
    {reply, Reply, State};
handle_call(Message, _From, State) ->
    ?DEBUG(111),
    {stop, {unknown_call, Message}, State}.


%% @doc Trap unknown casts
handle_cast({{rsc_pivot_done, Id, _IsA}, _Ctx}, State=#state{context=Context,solr=Solr}) ->
    ?DEBUG("putting"),
    ok = solr_store:put(Id, Context, Solr),
    {noreply, State};
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    Context = State#state.context,
    z_notifier:detach(search_query, self(), Context),
    z_notifier:detach(rsc_pivot_done, self(), Context),
    z_notifier:detach(rsc_delete, self(), Context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% Retrieve the previous id (on publication date) 
search({solr, Query}, _OffsetLimit, _Context, Solr) ->
    ?DEBUG("SOLR!"),
    {ok, RespAttrs, Docs, _AdditionalInfo} = esolr:search("search", Query, Solr),
    ?DEBUG(RespAttrs),
    ?DEBUG(Docs),
    undefined;

search(_, _, _, _) ->
    undefined.
