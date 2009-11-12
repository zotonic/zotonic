%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Virtual host support for webmachine

-module(webmachine_host).
-author('Marc Worrell <marc@worrell.nl>').
-export([get_host_dispatch_list/1]).

-include_lib("wm_host_dispatch_list.hrl").


%% @doc Fetch the host and dispatch list for the request
%% @spec get_host_dispatch_list(webmachine_request()) -> {ok, Host::atom(), DispatchList::list()} | {redirect, Hostname::string()} | no_host_match
get_host_dispatch_list(Req) ->
    {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
    case DispatchList of
        [#wm_host_dispatch_list{}|_] ->
            {Host, Port} = get_host(Req),
            case get_dispatch_host(Host, DispatchList) of
                {ok, DL} ->
                    {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list};

                undefined ->
                    FoundHost = case get_dispatch_alias(Host, DispatchList) of
                                    {ok, _} = Found -> Found;
                                    undefined -> get_dispatch_default(DispatchList)
                                end,
                    case FoundHost of
                        {ok, DL} ->
                            case DL#wm_host_dispatch_list.redirect andalso is_hostname(DL#wm_host_dispatch_list.hostname) andalso Req:method() =:= 'GET' of
                                true ->
                                    % Redirect, keep the port number
                                    Hostname = DL#wm_host_dispatch_list.hostname,
                                    Hostname1 = case Port of
                                                    "80" -> Hostname;
                                                    _ -> Hostname ++ [$:|Port]
                                                end,
                                    {redirect, Hostname1};
                                false ->
                                    {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list}
                            end;
                        undefined ->
                            no_host_match
                    end
            end;
        _ ->
            {ok, default, DispatchList} 
    end.


get_host(Req) ->
    Host =  case Req:get_header_value("X-Forwarded-Host") of
                undefined -> 
                    case Req:get_header_value("X-Host") of
                        undefined -> Req:get_header_value("Host");
                        XHost -> XHost
                    end;
                XFwdHost -> XFwdHost
            end,
    case Host of
        undefined -> 
            {"", "80"};
        _ -> 
            % Split the optional port number from the host name
            [H|Rest] = string:tokens(string:to_lower(Host), ":"),
            case Rest of
                [] -> {H, "80"};
                [Port|_] -> {H, Port}
            end
    end.


%% @doc Search the host where the main hostname matches the requested host
get_dispatch_host(_Host, []) ->
    undefined;
get_dispatch_host(Host, [#wm_host_dispatch_list{hostname=Host} = DL|_]) ->
    {ok, DL};
get_dispatch_host(Host, [_|Rest]) ->
    get_dispatch_host(Host, Rest).


%% @doc Search the host where the req hostname is an alias of main host
get_dispatch_alias(_Host, []) ->
    undefined;
get_dispatch_alias(Host, [#wm_host_dispatch_list{hostalias=Alias} = DL|Rest]) ->
    case lists:member(Host, Alias) of
        true  -> {ok, DL};
        false -> get_dispatch_alias(Host, Rest)
    end.


%% @doc Search the host with the name 'default' for fallback of unknown hostnames.
get_dispatch_default([]) ->
    undefined;
get_dispatch_default([#wm_host_dispatch_list{host=default} = DL|_]) ->
    {ok, DL};
get_dispatch_default([_|Rest]) ->
    get_dispatch_default(Rest).


%% @doc Check if the hostname is a hostname suitable to redirect to
is_hostname(undefined) -> false;
is_hostname("") -> false;
is_hostname("localhost") -> false;
is_hostname("127.0.0.1") -> false;
is_hostname(_) -> true.

