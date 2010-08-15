%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2009-06-09
%% @doc Defines Solr queries for extended content searches in Zotonic.

-module(mod_search_solr).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("Solr Search Queries").
-mod_description("Use Solr for extended content searches in Zotonic.").
-mod_prio(900).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    pid_observe_search_query/3,
    pid_observe_rsc_pivot_done/3,
    pid_observe_rsc_delete/3
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


pid_observe_search_query(Pid, {search_query, {solr, _Query}, _Limit} = Search, Context) ->
    gen_server:call(Pid, {Search, Context});
pid_observe_search_query(Pid, {search_query, {match, [{id,_Id}]}, _Limit} = Search, Context) ->
    gen_server:call(Pid, {Search, Context});
pid_observe_search_query(_Pid, _Query, _Context) ->
    undefined.

pid_observe_rsc_pivot_done(Pid, Msg, _Context) ->
    gen_server:cast(Pid, Msg).

pid_observe_rsc_delete(Pid, Msg, _Context) ->
    gen_server:cast(Pid, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    DefaultConnection = "http://127.0.0.1:8983/solr/" ++ z_convert:to_list(z_context:site(Context)) ++ "/",

    SolrUrl = m_config:get_value(?MODULE, solr, DefaultConnection, Context),
    SearchUrl = z_convert:to_list(SolrUrl) ++ "select",
    UpdateUrl = z_convert:to_list(SolrUrl) ++ "update",
    {ok, Solr} = esolr:start_link([{select_url, SearchUrl}, {update_url, UpdateUrl}]),

    AutoCommit = z_convert:to_integer(m_config:get_value(?MODULE, autocommit_time, 3000, Context)),
    esolr:set_auto_commit({time, AutoCommit}, Solr),
    %% Test the connection.. this will crash when there is no valid connection
    solr_search:match(1, {0, 1}, Solr, Context),
    %% Ready
    {ok, #state{context=z_context:new(Context),solr=Solr}}.


%% @doc A generic Solr query
handle_call({{search_query, {solr, Query}, Limit}, Context}, _From, State=#state{solr=Solr}) ->
    Reply = solr_search:search(Query, Limit, Solr, Context),
    {reply, Reply, State};

%% A "match" query (for sidebars and such)
handle_call({{search_query, {match, [{id,Id}]}, Limit}, Context}, _From, State=#state{solr=Solr}) ->
    Reply = solr_search:search(Id, Limit, Solr, Context),
    {reply, Reply, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Pivot-hook for putting document in solr.
handle_cast({rsc_pivot_done, Id, _IsA}, State=#state{context=Context,solr=Solr}) ->
    ok = solr_store:put(Id, Context, Solr),
    {noreply, State};

handle_cast({rsc_delete, Id}, State=#state{context=Context,solr=Solr}) ->
    ok = solr_store:delete(Id, Context, Solr),
    {noreply, State};

%% @doc Trap unknown casts
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
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================
