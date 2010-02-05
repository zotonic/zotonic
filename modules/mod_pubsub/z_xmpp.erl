%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2010-01-31
%% @doc XMPP utilities.

%% Copyright 2009 Arjan Scherpenisse
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

-module(z_xmpp).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("zotonic.hrl").

-export([parse_xmpp_uri/1,
         xmpp_uri_from_html/1,
         resource_uri_from_html/1,
         discover_xmpp_uri/2,
         discover_resource_uri/2
        ]).


%% @doc Parse an XMPP uri into its JID and args. See XEP 0147.
%% @spec parse_xmpp_uri(string()) -> {Jid, Action, Args} | error
parse_xmpp_uri(Uri) ->
    case re:run(Uri, "^xmpp:([^;\\?]+)(\\?([^;]*))?(;(.*))?", [{capture,[1,3,5],list}]) of
        {match, [Jid, Action, Args]} ->
            ArgList = string:tokens(Args, ";"),
            {exmpp_jid:parse(Jid), Action, [mkarg(A)||A<-ArgList]};
        nomatch ->
            error
    end.

mkarg(S) ->
    case re:run(S, "^([^=]*)(=(.*))?", [{capture, [1,3],list}]) of
        {match, [Key, Value]} ->
            case Value of 
                [] -> {oauth_uri:decode(Key), true};
                _ -> {oauth_uri:decode(Key), oauth_uri:decode(Value)}
            end
    end.

                    

%% @doc Given a HTML text, get the Link element with rel=xmpp.feed and return the parsed XMPP uri.
%% @spec xmpp_uri_from_html(Html) -> {Jid, Action, Args}
xmpp_uri_from_html(Html) ->
    Links = z_html:scrape_link_elements(Html),
    case lists:filter(fun(Attrs) -> proplists:get_value("rel", Attrs) =:= "xmpp.feed" end, Links) of
        [] -> undefined;
        [Attrs|_] -> proplists:get_value("href", Attrs)
    end.


%% @doc Given a HTML text, get the Link element with rel=xmpp.feed and return the parsed XMPP uri.
%% @spec xmpp_uri_from_html(Html) -> {Jid, Action, Args}
resource_uri_from_html(Html) ->
    Links = z_html:scrape_link_elements(Html),
    case lists:filter(fun(Attrs) -> proplists:get_value("rel", Attrs) =:= "self" end, Links) of
        [] -> undefined;
        [Attrs|_] -> proplists:get_value("href", Attrs)
    end.



%% @doc Given a HTTP URL, discover the XMPP URI that is responsible for the content updates for this page.
discover_xmpp_uri(Uri, Context) ->
    case cached_http_request(Uri, Context) of
        {ok, Response} ->
            case Response of
                {{ _, 200, _}, _Headers, Body} ->
                    {ok, xmpp_uri_from_html(Body)};
                {{ _, Code, _}, _Headers, _Body} ->
                    {error, {http, Code}}
            end;
        {error, E} ->
            {error,E}
    end.


%% @doc Given a HTTP URL, discover the rel=self uri.
discover_resource_uri(Uri, Context) ->
    case cached_http_request(Uri, Context) of
        {ok, Response} ->
            case Response of
                {{ _, 200, _}, _Headers, Body} ->
                    {ok, resource_uri_from_html(Body)};
                {{ _, Code, _}, _Headers, _Body} ->
                    {error, {http, Code}}
            end;
        {error, E} ->
            {error,E}
    end.


cached_http_request(Uri, Context) ->
    Key = {z_xmpp_httprequest, Uri},
    case z_depcache:get(Key, Context) of
        {ok, Response} ->
            {ok, Response};
        undefined ->
            case http:request(get, {Uri, []}, [], []) of
                {ok, Response} ->
                    z_depcache:set(Key, Response, 60, Context),
                    {ok, Response};
                {error, E} ->
                    {error, E}
            end
    end.
