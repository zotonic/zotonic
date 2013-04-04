%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2013 Arjan Scherpenisse
%% @doc OEmbed client

%% Copyright 2011-2013 Arjan Scherpenisse <arjan@scherpenisse.net>
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
-export([discover/2, providers/2]).

-include_lib("../include/oembed.hrl").

-record(state, {context, providers=[]}).

-define(HTTP_GET_TIMEOUT, 20000).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link([#oembed_provider{}]) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Context) ->
    gen_server:start_link({local, srv_name(Context)}, ?MODULE, Context, []).

discover(Url, Context) ->
    gen_server:call(srv_name(Context), {discover, Url}).

providers(Url, Context) ->
    gen_server:call(srv_name(Context), {providers, Url}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Context) ->
    {ok, #state{context=Context}}.

%% @doc Discover
handle_call({discover, Url}, _From, State) ->
    {reply, do_discover(Url, State), State};

handle_call({providers, Url}, _From, State) ->
    {reply, find_providers(Url, State), State};

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
-define(EMBEDLY_ENDPOINT, "http://api.embed.ly/1/oembed?format=json&url=").


do_discover(Url, State) ->
    UrlExtra = oembed_url_extra(State#state.context),
    discover_per_provider(Url, UrlExtra, oembed_providers:list()).


discover_per_provider(Url, UrlExtra, [Provider=#oembed_provider{}|Rest]) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} ->
            case Provider#oembed_provider.callback of
                F when is_function(F) ->
                    F(Url);
                undefined ->
                    RequestUrl = Provider#oembed_provider.endpoint_url
                        ++ "?format=json&url=" ++ z_utils:url_encode(Url) ++ UrlExtra,
                    oembed_request(RequestUrl)
            end;
        nomatch ->
            discover_per_provider(Url, UrlExtra, Rest)
    end;

discover_per_provider(Url, UrlExtra, []) ->
    lager:warning("Fallback embed.ly discovery for url: ~p~n", [Url]),
    oembed_request(?EMBEDLY_ENDPOINT ++ z_utils:url_encode(Url) ++ UrlExtra).


find_providers(Url, _State) ->
    find_providers(Url, oembed_providers:list(), []).

find_providers(_Url, [], Acc) ->
    lists:reverse(Acc);
find_providers(Url, [Provider=#oembed_provider{}|Rest], Acc) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} -> find_providers(Url, Rest, [Provider|Acc]);
        nomatch -> find_providers(Url, Rest, Acc)
    end.

oembed_request(RequestUrl) ->
    HttpOptions = [
        {autoredirect, true},
        {timeout, ?HTTP_GET_TIMEOUT},
        {relaxed, true}
    ],
    {ok, {{_, Code, _}, Headers, Body}} = httpc:request(get, {RequestUrl, []}, HttpOptions, []),
    case Code of
        200 -> 
            {ok, z_convert:convert_json(mochijson2:decode(Body))};
        _Other ->
            lager:warning("OEmbed HTTP Request returned ~p for '~p' (~p ~p)", [Code, RequestUrl, Headers, Body]),
            {error, {http, Code, Body}}  %% empty proplist
    end.

%% @doc Construct extra URL arguments to the OEmbed client request from the oembed module config.
oembed_url_extra(Context) ->
    W = m_config:get_value(oembed, maxwidth, 640, Context),
    X1 =  "&maxwidth=" ++ z_utils:url_encode(z_convert:to_list(W)),
    X2 = case m_config:get_value(oembed, maxheight, Context) of
             undefined -> X1;
             H -> X1 ++ "&maxheight=" ++ z_utils:url_encode(z_convert:to_list(H))
         end,
    X2.

%% @doc Name of the oembed client gen_server for this site
srv_name(Context) ->
    z_utils:name_for_host(?MODULE, z_context:site(Context)).
