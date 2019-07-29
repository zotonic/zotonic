%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-07-22
%%
%% @doc Model for the accessing the request fields. Exposes Webmachine's wrq.erl

%% Copyright 2010 Marc Worrell
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

-module(m_req).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Key | Rest ], _Msg, Context) when is_atom(Key) ->
    {ok, {get(Key, Context), Rest}};
m_get([], _Msg, Context) ->
    {ok, {values(Context), []}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Non atomic key get: ~p", [Vs]),
    {error, unknown_path}.


%% @doc Fetch the field from the cowmachine_req interface.
-spec get(atom(), z:context() | cowboy_req:req() | undefined) -> any().
get(undefined, _) -> undefined;
get(_, undefined) -> undefined;
get(site, #context{} = Context) -> z_context:site(Context);
get(timezone, #context{} = Context) -> z_context:tz(Context);
get(language, #context{} = Context) -> z_context:language(Context);
get(is_crawler, #context{} = Context) -> z_user_agent:is_crawler(Context);
get(peer_ip, #context{} = Context) ->
    case z_context:get(peer_ip, Context) of
        undefined -> get_req(peer_ip, Context);
        PeerIP -> PeerIP
    end;
get(What, #context{} = Context) ->
    case z_context:is_request(Context) of
        true ->  get_req(What, Context);
        false -> undefined
    end.

-spec get_req( atom(), z:context() ) -> any().
get_req(method, Context) -> cowmachine_req:method(Context);
get_req(version, Context) -> cowmachine_req:version(Context);
get_req(peer, Context) -> cowmachine_req:peer(Context);
get_req(peer_ip, Context) -> cowmachine_req:peer_ip(Context);
get_req(is_ssl, Context) -> cowmachine_req:is_ssl(Context);
get_req(scheme, Context) -> cowmachine_req:scheme(Context);
get_req(host, Context) -> cowmachine_req:host(Context);
get_req(port, Context) ->
    case cowmachine_req:is_proxy(Context) of
        true -> cowmachine_req:port(Context);
        false ->
            case cowmachine_req:is_ssl(Context) of
                true -> z_config:get(ssl_port);
                false -> z_config:get(port)
            end
    end;
get_req(raw_path, Context) -> cowmachine_req:raw_path(Context);
get_req(path, Context) -> cowmachine_req:path(Context);
get_req(qs, Context) -> cowmachine_req:req_qs(Context);
get_req(headers, Context) -> cowmachine_req:get_req_headers(Context);
get_req(user_agent, Context) -> cowmachine_req:get_req_header(<<"user-agent">>, Context);
get_req(referer, Context) -> cowmachine_req:get_req_header(<<"referer">>, Context);
get_req(referrer, Context) -> get_req(referer, Context);
get_req(is_crawler, Context) -> z_user_agent:is_crawler(Context);
get_req(_Key, _Context) -> undefined.


-spec values( z:context() ) -> list({atom(), any()}).
values(Context) ->
    [ {K, get(K, Context)} || K <- [
            method, version, peer, is_ssl, host, port, raw_path, path, qs, referrer, user_agent, is_crawler,
            headers, timezone, language
        ]
    ].


