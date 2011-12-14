%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc OEmbed client

%% Copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(oembed_client).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([discover/1]).

-include_lib("../include/oembed.hrl").


-record(state, {providers=[]}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link([#oembed_provider{}]) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Providers) when is_list(Providers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Providers, []).

discover(Url) ->
    gen_server:call(?MODULE, {discover, Url}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Providers) ->
    {ok, #state{providers=Providers}}.

%% @doc Discover
handle_call({discover, Url}, _From, State) ->
    {reply, do_discover(Url, State), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



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
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% Endpoint for embed.ly oembed service
-define(EMBEDLY_ENDPOINT, "http://api.embed.ly/1/oembed?format=json&maxwidth=500&url=").


do_discover(Url, State) ->
    discover_per_provider(Url, State#state.providers).


discover_per_provider(Url, [Provider=#oembed_provider{}|Rest]) ->
    %% io:format("~p ~p~n", [Provider#oembed_provider.title, Provider#oembed_provider.url_re]),
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} ->
            case Provider#oembed_provider.callback of
                F when is_function(F) ->
                    F(Url);
                undefined ->
                    RequestUrl = Provider#oembed_provider.endpoint_url
                        ++ "?format=json&url=" ++ z_utils:url_encode(Url),
                    oembed_request(RequestUrl)
            end;
        nomatch ->
            discover_per_provider(Url, Rest)
    end;

discover_per_provider(Url, []) ->
    io:format("Fallback embed.ly discovery for url: ~p~n", [Url]),
    %% Use embed.ly service...
    oembed_request(?EMBEDLY_ENDPOINT ++ z_utils:url_encode(Url)).


oembed_request(RequestUrl) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(get, {RequestUrl, []}, [], []),
    z_convert:convert_json(mochijson2:decode(Body)).
