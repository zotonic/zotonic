%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Server for rendering and caching scomps.  Scomps can be caching and
%%      non caching, depending on the result of the render/2 function.
%% @todo Handle code change events for all scomps     

%% Copyright 2009 Marc Worrell
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

-module(z_scomp).

-behaviour(gen_server).
-author("Marc Worrell <marc@worrell.nl>").

%% API
-export([start_link/1, render/4, render_all/4, scomp_ready/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("zotonic.hrl").


%% @doc The state of the scomp server.  'Cache' holds the cached entries, 'waiting' holds the list
%%      of entries being rendered for the cache.  'Now' is the current time in seconds, updated with 
%%      a timer to prevent too many time lookups.
-record(state, {context, waiting, scomps}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @spec render(ScompName, Args, Vars, Context) -> {ok, Context} | {ok, io_list} | {error, Reason}
%% @doc Render the names scomp, Args are the scomp arguments and Vars are the variables given to the template
render(ScompName, Args, Vars, Context) ->
    case z_module_indexer:find(scomp, ScompName, Context) of
        {ok, ModuleName} ->
        	ScompContext = z_context:prune_for_scomp(z_acl:args_to_visible_for(Args), Context), 
            render_scomp_module(ModuleName, Args, Vars, ScompContext, Context);
        {error, enoent} ->
            %% No such scomp, as we can switch on/off functionality we do a quiet skip
            ?LOG("No scomp enabled for \"~p\"", [ScompName]),
            {ok, <<>>}
    end.

render_all(ScompName, Args, Vars, Context) ->
    case z_module_indexer:find_all(scomp, ScompName, Context) of
        [] -> [];
        ModuleNames when is_list(ModuleNames) ->
        	ScompContext = z_context:prune_for_scomp(a_acl:args_to_visible_for(Args), Context),
        	RenderFun = fun(ModuleName) ->
        	    case render_scomp_module(ModuleName, Args, Vars, ScompContext, Context) of
        	        {ok, Result} -> z_context:prune_for_template(Result);
        	        {error, Reason} -> throw({error, Reason})
        	    end
        	end,
            [ RenderFun(ModuleName) || ModuleName <- ModuleNames ]
    end.


%% @spec scomp_ready(ScompPid, Key, Result, MaxAge, Varies) -> none()
%% @doc Called when a cacheable scomp has been rendered, places the scomp in the cache and
%%      posts the result to all waiting processes
scomp_ready(ScompPid, Key, Result, MaxAge, Varies) ->
    gen_server:cast(ScompPid, {scomp_ready, Key, Result, MaxAge, Varies}).



render_scomp_module(ModuleName, Args, Vars, ScompContext, Context) ->
    case gen_server:call(Context#context.scomp_server, {render, ModuleName, Args, ScompContext}) of
        {renderer, M, F, ScompState} ->
            ScompContextWM = ScompContext#context{wm_reqdata=Context#context.wm_reqdata},
            erlang:apply(M, F, [Args, Vars, ScompContextWM, ScompState]);
        Result -> 
            Result
	end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(SiteProps) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(SiteProps) ->
	process_flag(trap_exit, true),
    Host = proplists:get_value(host, SiteProps),
    Context = z_context:new(Host),
    timer:start(),
    {ok, #state{context=Context, waiting=dict:new(), scomps=dict:new()}}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @doc Render a scomp. Args are the scomp parameters, Context is the request context
%%      at the moment of the call to the template engine.  The scomp is a module that
%%      must be present in the ebin directory. 
handle_call({render, ModuleName, Args, Context}, From, State) ->
    ScompVaries = ModuleName:varies(Args, Context),
    {Cached, State1} =  case ScompVaries of
                            undefined -> 
                                {undefined, State};
                            {Essential, Max, Vary} ->
                                case Max of
                                    0 -> {undefined, State};
                                    _ -> {cache_lookup(ModuleName, Essential, Vary, Context), State}
                                end
                        end,
    case Cached of 
        undefined ->
            case scomp_state(ModuleName, State1) of
                {error, Error} ->
                    ?ERROR("Error initializing scomp ~p: ~p", [ModuleName, Error]),
                    {reply, {ok, <<>>}, State1};

                {ok, ScompState, State2} ->
                    %% No cached entry, we need to redo the scomp.
                    %% When the scomp is cacheable we record it to prevent parallel rendering
                    %% When the scomp is not cacheable we just return the renderer to be evaluated in the process of the caller
                    case ScompVaries of
                        undefined ->
                            {reply, {renderer, ModuleName, render, ScompState}, State2};
                        {EssentialParams, MaxAge, Varies} ->
                            StateSpawned = spawn_cacheable_renderer(self(), ModuleName, ScompState, EssentialParams, MaxAge, Varies, Context, From, State2),
                            {noreply, StateSpawned}
                    end
            end;
        CachedResult ->
            %% Cached content, return the html and context
            %% The context must be filtered from the returned html before serving
            {reply, CachedResult, State1}
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% @desc Store the rendered scomp in the cache, send the result to all waiting processes
handle_cast({scomp_ready, Key, Result, MaxAge, Varies}, State) ->
    case dict:find(Key, State#state.waiting) of
        {ok, List} -> lists:foreach(fun(Pid) -> catch gen_server:reply(Pid, Result) end, List);
        _ -> ok
    end,

    %% If maxage is 0 then there is slam-dunk protection but no additional caching.
    State1 = State#state{ waiting=dict:erase(Key, State#state.waiting) },
    case z_convert:to_integer(MaxAge) of
        undefined -> 
            {noreply, State1};
        0 -> 
            {noreply, State1};
        Max ->
            z_depcache:set(Key, Result, Max, Varies, State#state.context),
            {noreply, State1}
    end;
    
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({From, Tag, get_modules}, State) ->
    From ! {element(2,Tag), scomp_list(State)},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
	dict:map(fun(ScompModule, ScompState) -> 
				ScompModule:terminate(ScompState, State#state.context) 
			end, State#state.scomps),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

cache_lookup(_ScompName, undefined, _Varies, _Context) ->
    undefined;
cache_lookup(ScompName, EssentialParams, _Varies, Context) ->
    Key = key(ScompName, EssentialParams, Context),
    case z_depcache:get(Key, Context) of
        {ok, Result} -> Result;
        _ -> undefined
    end.


%% @doc Render a scomp where we can cache the result.  The scomp is rendered and reported
%%      back to this scomp server, which then replies all callee processes that are waiting and
%%      caches the result.
spawn_cacheable_renderer(ScompPid, ScompName, ScompState, EssentialParams, MaxAge, Varies, Context, From, State) ->
    Key = key(ScompName, EssentialParams, Context),
    Waiting = try
                dict:append(Key, From, State#state.waiting)
             catch
                _M:_E -> dict:store(Key, [From], State#state.waiting)
             end,
    Renderer = fun() ->
                     Result = ScompName:render(EssentialParams, [], Context, ScompState),
                     ?MODULE:scomp_ready(ScompPid, Key, Result, MaxAge, Varies)
                end,
    spawn(Renderer),
    State#state{waiting=Waiting}.
 
 
%% @doc Make sure that the scomp module is registered with the list of modules for which we want to receive code change events
scomp_state(ScompName, State) ->
    case dict:find(ScompName, State#state.scomps) of
        {ok, ScompState} ->  
            {ok, ScompState, State};
        _ ->
            case ScompName:init([]) of
                {ok, ScompState} ->
                    Dict2 = dict:store(ScompName, ScompState, State#state.scomps),
                    {ok, ScompState, State#state{scomps=Dict2}};
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Return the list of loaded scomps
scomp_list(State) ->
    dict:fetch_keys(State#state.scomps).


%% @doc Create an unique key for the scomp and the visibility level it is rendered for
%% @spec key(atom(), proplist(), context()) -> term()
key(ScompName, EssentialParams, Context) ->
    {ScompName, EssentialParams, z_acl:cache_key(Context), Context#context.language}.
