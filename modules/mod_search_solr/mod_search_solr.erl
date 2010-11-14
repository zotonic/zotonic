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

-record(state, {context, solr, default_search=true}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


pid_observe_search_query(Pid, {search_query, _Query, _Limit} = Search, Context) ->
    gen_server:call(Pid, {Search, Context}, infinity).

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

    DefaultSearch = z_convert:to_bool(m_config:get_value(?MODULE, default_search, false, Context)),

    %% Test the connection.. this will crash when there is no valid connection
    solr_search:match(1, {0, 1}, Solr, Context),
    %% Ready
    {ok, #state{context=z_context:new(Context),solr=Solr,default_search=DefaultSearch}}.


%% @doc A generic Solr query
handle_call({{search_query, Query, Limit}, Context}, _From, State) ->
    {reply, search(Query, Limit, Context, State), State};

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


search({solr, Query}, Limit, Context, #state{solr=Solr}) ->
    solr_search:search(Query, Limit, Solr, Context);
search({match, [{id,Id}]}, Limit, Context, #state{solr=Solr}) ->
    solr_search:match(Id, Limit, Solr, Context);
search({Type, Args}, Limit, Context, #state{default_search=true,solr=Solr}) ->
    case map_search(Type, Args) of
        undefined ->
            undefined;
        Query ->
            solr_search:search(Query, Limit, Solr, Context)
    end;
search(_, _Limit, _Context, _State) ->
    undefined.


%%
%% Map the simpler (e.g., the ones using 'query') search functions
%% from mod_search.erl onto Solr:
%%

map_search(featured, [])                             -> [{is_featured, true}];
map_search(featured, [{cat, Cat}])                   -> [{is_featured, true}, {cat, Cat}];
%map_search(all, [])                                 -> [];
%map_search(all, [{cat, Cat}])                       -> [{cat, Cat}];
map_search(published, [])                            -> [{sort, "-publication_start"}];
map_search(published, [{cat, Cat}])                  -> [{sort, "-publication_start"}, {cat, Cat}];
%map_search(latest, [])                              -> [{sort, "-modified"}];
%map_search(latest, [{cat, Cat}])                    -> [{sort, "-modified"}, {cat, Cat}];
%map_search(upcoming, [{cat, Cat}])                  -> [{sort, "date_start"}, {upcoming, true}, {cat, Cat}];
map_search(autocomplete, [{text, Text}])             -> [{text, z_convert:to_list(Text)++"*"}];
map_search(autocomplete, [{cat, Cat}, {text, Text}]) -> [{cat, Cat}, {text, z_convert:to_list(Text)++"*"}, {return_format, ranked}];
map_search(fulltext, [{text, Text}])                 -> [{text, Text}];
map_search(fulltext, [{cat, Cat}, {text, Text}])     -> [{cat, Cat}, {text, Text}, {return_format, ranked}];
map_search('query',[{hasobject,_},{sort,_}])         -> undefined;
map_search('query',[{hassubject,_},{sort,_}])        -> undefined;
map_search('query',[{hasobject,_}])                  -> undefined;
map_search('query',[{hassubject,_}])                 -> undefined;
map_search('query', Query)                           -> Query;
map_search(_, _)                                     -> undefined.
