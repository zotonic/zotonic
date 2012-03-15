%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% @doc Manage dispatch lists (aka definitions for url patterns). Constructs named urls from dispatch lists.

%% Copyright 2009-2011 Marc Worrell
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

-module(z_dispatcher).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% z_dispatch exports
-export([
	url_for/2,
	url_for/3,
	url_for/4,
	hostname/1,
	hostname_port/1,
	dispatchinfo/1,
	update/1,
	reload/1,
	reload/2,
	drop_port/1
]).

-include_lib("zotonic.hrl").

-record(state, {dispatchlist=undefined, lookup=undefined, context, 
                host, hostname, hostname_port, streamhost, smtphost, hostalias, 
				redirect=true}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the dispatch server
start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @spec url_for(atom(), Context) -> iolist()
%% @doc Construct an uri from a named dispatch, assuming no parameters. Use html escape.
url_for(Name, #context{dispatcher=Dispatcher} = Context) ->
    rewrite(gen_server:call(Dispatcher, {'url_for', Name, [], html}), Name, [], Context).


%% @spec url_for(atom(), Args, Context) -> iolist()
%%    Args = proplist()
%% @doc Construct an uri from a named dispatch and the parameters. Use html escape.
url_for(Name, Args, #context{dispatcher=Dispatcher} = Context) ->
    Args1 = append_extra_args(Args, Context),
    rewrite(gen_server:call(Dispatcher, {'url_for', Name, Args1, html}), Name, Args1, Context).


%% @spec url_for(atom(), Args, atom(), Context) -> iolist()
%%        Args = proplist()
%% @doc Construct an uri from a named dispatch and the parameters
url_for(Name, Args, Escape, #context{dispatcher=Dispatcher} = Context) ->
    Args1 = append_extra_args(Args, Context),
    rewrite(gen_server:call(Dispatcher, {'url_for', Name, Args1, Escape}), Name, Args1, Context).


%% @spec hostname(Context) -> iolist()
%% @doc Fetch the preferred hostname for this site
hostname(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'hostname').

%% @spec hostname_port(Context) -> iolist()
%% @doc Fetch the preferred hostname, including port, for this site
hostname_port(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'hostname_port').


%% @spec dispatchinfo(Context) -> {host, hostname, streamhost, smtphost, hostaliases, redirect, dispatchlist}
%% @doc Fetch the dispatchlist for the site.
dispatchinfo(#context{dispatcher=Dispatcher}) -> 
    gen_server:call(Dispatcher, 'dispatchinfo');
dispatchinfo(Server) when is_pid(Server) orelse is_atom(Server) -> 
    gen_server:call(Server, 'dispatchinfo').


%% @doc Update the dispatch list but don't reload it yet. Used when flushing all sites, see z:flush/0
update(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'reload').


%% @doc Reload all dispatch lists.  Finds new dispatch lists and adds them to the dispatcher
reload(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'reload'),
    z_sites_dispatcher:update_dispatchinfo().

reload(module_ready, Context) ->
    reload(Context).


%% @doc rewrite generated uris
rewrite(undefined, _Dispatch, _Args, _Context) ->
    undefined;
rewrite(Url, Dispatch, Args, Context) ->
    z_notifier:foldl(#url_rewrite{dispatch=Dispatch, args=Args}, iolist_to_binary(Url), Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, loads the dispatch list into the webmachine dispatcher
init(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    {hostname, Hostname} = proplists:lookup(hostname, SiteProps),
    Streamhost = proplists:get_value(streamhost, SiteProps),
    Smtphost = proplists:get_value(smtphost, SiteProps),
    HostAlias = proplists:get_all_values(hostalias, SiteProps),
    Context = z_context:new(Host),
    process_flag(trap_exit, true),
    State  = #state{
                dispatchlist=[], 
                lookup=dict:new(),
                context=Context, 
                host=Host, 
                streamhost=drop_port(Streamhost),
				smtphost=drop_port(Smtphost),
                hostname=drop_port(Hostname),
                hostname_port=Hostname,
                hostalias=[ drop_port(Alias) || Alias <- HostAlias ],
                redirect=z_convert:to_bool(proplists:get_value(redirect, SiteProps, true))
    },
    z_notifier:observe(module_ready, {?MODULE, reload}, Context),
    {ok, State}.

% @doc Drop the portnumber from the hostname
%
drop_port(undefined) ->
    undefined;
drop_port(none) ->
    "localhost";
drop_port(Hostname) ->
    hd(string:tokens(Hostname, ":")).


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Create the url for the dispatch rule with name and arguments Args.
handle_call({'url_for', Name, Args, Escape}, _From, State) ->
    Uri = make_url_for(Name, Args, Escape, State#state.lookup),
    {reply, Uri, State};

%% @doc Return the preferred hostname for the site
handle_call('hostname', _From, State) ->
    {reply, State#state.hostname, State};

%% @doc Return the preferred hostname, and port, for the site
handle_call('hostname_port', _From, State) ->
    {reply, State#state.hostname_port, State};

%% @doc Return the dispatchinfo for the site  {host, hostname, streamhost, smtphost, hostaliases, redirect, dispatchlist}
handle_call('dispatchinfo', _From, State) ->
    {reply,
     {State#state.host, State#state.hostname, State#state.streamhost, State#state.smtphost,
      State#state.hostalias, State#state.redirect, State#state.dispatchlist}, 
     State};

%% @doc Reload the dispatch list, signal the sites supervisor that the dispatch list has been changed.
%% The site supervisor will collect all dispatch lists and send them at once to webmachine.
handle_call('reload', _From, State) ->
    State1 = reload_dispatch_list(State),
    {reply, ok, State1}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(module_ready, {?MODULE, reload}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Reload the dispatch list and send it to the webmachine dispatcher.
reload_dispatch_list(#state{context=Context} = State) ->
    DispatchList = try 
                       collect_dispatch_lists(Context)
                   catch
                       _:{error, Msg} ->
                           z_session_manager:broadcast(#broadcast{type="error", message="Dispatch error! " ++ Msg, title="Dispatcher", stay=false}, Context),
                           State#state.dispatchlist
                   end,
    LookupDict = dispatch_for_uri_lookup(DispatchList),
    State#state{dispatchlist=DispatchList, lookup=LookupDict}.


%% @doc Collect all dispatch lists.  Checks priv/dispatch for all dispatch list definitions.
collect_dispatch_lists(Context) ->
    Files      = filelib:wildcard(filename:join([z_utils:lib_dir(priv), "sites", Context#context.host, "dispatch", "*"])),
    Modules    = z_module_manager:active(Context),
    ModuleDirs = z_module_manager:scan(Context),
    ModDisp    = [ {M, filelib:wildcard(filename:join([proplists:get_value(M, ModuleDirs), "dispatch", "*"]))} || M <- Modules ],
    ModDispOnPrio = lists:concat([ ModFiles || {_Mod, ModFiles} <- z_module_manager:prio_sort(ModDisp) ]),
    Dispatch   = lists:map(fun get_file_dispatch/1, ModDispOnPrio++Files),
    lists:flatten(Dispatch).


%% @doc Read a dispatch file, the file should contain a valid Erlang dispatch datastructure.
%% @spec get_file_dispatch(filename()) -> DispatchList
get_file_dispatch(File) ->
    try
        case filelib:is_regular(File) of
            true ->
                Basename = filename:basename(File),
                case Basename of
                    "." ++ _ -> 
                        [];
                    _Other  ->
                        {ok, Disp} = file:consult(File),
                        Disp
                end;
            false -> 
                []
        end
    catch 
        M:E ->
            ?ERROR("File dispatch error: ~p  ~p", [File, {M,E}]),
            throw({error, "Parse error in " ++ z_convert:to_list(File)})
    end.


%% @doc Transform the dispatchlist into a datastructure for building uris from name/vars
%% Datastructure needed is:   name -> [vars, pattern]
dispatch_for_uri_lookup(DispatchList) ->
    dispatch_for_uri_lookup1(DispatchList, dict:new()).
    
dispatch_for_uri_lookup1([], Dict) ->
    Dict;
dispatch_for_uri_lookup1([{Name, Pattern, _Resource, _Args}|T], Dict) ->
    Vars  = lists:filter(fun({A,_RegExp}) -> is_atom(A);
                            (A) -> is_atom(A)
                         end,
                         Pattern),
    Dict1 = case dict:is_key(Name, Dict) of
                true  -> dict:append(Name, {length(Vars), Vars, Pattern}, Dict);
                false -> dict:store(Name, [{length(Vars), Vars, Pattern}], Dict)
            end,
    dispatch_for_uri_lookup1(T, Dict1).



%% @doc Make an uri for the named dispatch with the given parameters
make_url_for(Name, Args, Escape, UriLookup) ->
    Name1 = z_convert:to_atom(Name),
    Args1 = lists:filter(fun
            ({_, <<>>}) -> false;
            ({_, []}) -> false;
            ({_, undefined}) -> false;
            (_) -> true
        end, Args),
    case dict:find(Name1, UriLookup) of
        {ok, Patterns} -> make_url_for1(Args1, Patterns, Escape, undefined);
        error -> undefined
    end.


%% @doc Try to match all patterns with the arguments
make_url_for1(_Args, [], _Escape, undefined) ->
    undefined;
make_url_for1(Args, [], Escape, {QueryStringArgs, Pattern}) -> 
    ReplArgs =  fun 
                    ('*') -> proplists:get_value(star, Args);
                    (V) when is_atom(V) -> mochiweb_util:quote_plus(proplists:get_value(V, Args));
                    ({V, _Pattern}) when is_atom(V) -> mochiweb_util:quote_plus(proplists:get_value(V, Args));
                    (S) -> S
                end,
    UriParts = lists:map(ReplArgs, Pattern), 
    Uri      = [$/ | z_utils:combine($/, UriParts)],
    case QueryStringArgs of
        [] -> Uri;
        _  ->
            Sep = case Escape of
                    xml  -> "&amp;";
                    html -> "&amp;";
                    _    -> $&
                  end,
            [Uri, $?, urlencode(QueryStringArgs, Sep)]
    end;
make_url_for1(Args, [Pattern|T], Escape, Best) ->
    Best1 = select_best_pattern(Args, Pattern, Best),
    make_url_for1(Args, T, Escape, Best1).
    

select_best_pattern(Args, {PCount, PArgs, Pattern}, Best) ->
    if 
        length(Args) >= PCount ->
            %% Check if all PArgs are part of Args
            {PathArgs, QueryStringArgs} = lists:partition(
                                            fun
                                                ({star,_}) -> lists:member('*', PArgs);
                                                ({A,_}) -> lists:member(A, PArgs)
                                            end, Args),
            case length(PathArgs) of
                PCount ->
                    % Could fill all path args, this match satisfies
                    select_best_pattern1({QueryStringArgs,Pattern}, Best);
                _ ->
                    Best
            end;
        true ->
            Best
    end.

select_best_pattern1(A, undefined) -> 
    A;
select_best_pattern1({AQS, _APat}=A, {BQS, _BPat}=B) ->
    if 
        length(BQS) > length(AQS) -> A;
        true -> B
    end.


%% @spec urlencode([{Key, Value}], Join) -> string()
%% @doc URL encode the property list.
urlencode(Props, Join) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[mochiweb_util:quote_plus(K), $=, mochiweb_util:quote_plus(V)] | Acc]
                           end, [], Props),
    lists:flatten(revjoin(RevPairs, Join, [])).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).


%% @doc Append extra arguments to the url, depending if 'qargs' or 'varargs' is set.
append_extra_args(Args, Context) ->
    append_qargs(append_varargs(Args, Context), Context).


%% @doc Append all query arguments iff they are not mentioned in the arglist and if qargs parameter is set
append_qargs(Args, Context) ->
    case proplists:get_value(qargs, Args) of
        undefined ->
            Args;
        false -> 
            proplists:delete(qargs, Args);
        true ->
            Args1 = proplists:delete(qargs, Args),
            Qs = z_context:get_q_all(Context),
            lists:foldr(fun 
                            ({[$q|_]=Key,_Value}=A, Acc) ->
                                case proplists:is_defined(Key, Args) of
                                    true -> Acc;
                                    false -> [A|Acc]
                                end;
                            (_, Acc) -> 
                                Acc
                        end,
                        Args1,
                        Qs)
    end.

%% @doc Append all varargs argument, names are given in a list.
append_varargs(Args, Context) ->
    case proplists:get_value(varargs, Args) of
        undefined ->
            Args;
        Varargs ->
            append_varargs(Varargs, proplists:delete(varargs, Args), Context)
    end.

append_varargs([], Args, _Context) ->
    Args;
append_varargs([{Name, Value}|Varargs], Args, Context) ->
    append_varargs(Varargs, append_vararg(Name, Value, Args), Context);
append_varargs([[Name, Value]|Varargs], Args, Context) ->
    append_varargs(Varargs, append_vararg(Name, Value, Args), Context);
append_varargs([Name|Varargs], Args, Context) ->
    Key = z_convert:to_atom(Name),
    append_varargs(Varargs, append_vararg(Key, z_context:get(Key, Context), Args), Context).

append_vararg(Name, Value, Args) ->
    Key = z_convert:to_atom(Name),
    case proplists:is_defined(Key, Args) of
        true -> Args;
        false -> [{Key, Value}|Args]
    end.
