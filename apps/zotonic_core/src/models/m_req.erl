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
    m_get/2,

    get/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ Key | Rest ], Context) ->
    {get(Key, Context), Rest};
m_get([], Context) ->
    {values(Context), []}.


%% @doc Fetch the field from the cowmachine_req interface.
-spec get(atom(), #context{} | cowboy_req:req() | undefined) -> any().
get(undefined, _) -> undefined;
get(_, undefined) -> undefined;
get(site, #context{} = Context) -> z_context:site(Context);
get(timezone, #context{} = Context) -> z_context:tz(Context);
get(language, #context{} = Context) -> z_context:language(Context);
get(is_crawler, #context{} = Context) -> z_user_agent:is_crawler(Context);
get(What, #context{} = Context) -> get_req(What, z_context:get_reqdata(Context));
get(What, Req) -> get_req(What, Req).

get_req(_, undefined) -> undefined;
get_req(method, RD) -> cowmachine_req:method(RD);
get_req(version, RD) -> cowmachine_req:version(RD);
get_req(peer, RD) -> cowmachine_req:peer(RD);
get_req(peer_ip, RD) -> cowmachine_req:peer_ip(RD);
get_req(is_ssl, RD) -> cowmachine_req:is_ssl(RD);
get_req(scheme, RD) -> cowmachine_req:scheme(RD);
get_req(host, RD) -> cowmachine_req:host(RD);
get_req(port, RD) ->
    case cowmachine_req:is_proxy(RD) of
        true -> cowmachine_req:port(RD);
        false ->
            case cowmachine_req:is_ssl(RD) of
                true -> z_config:get(ssl_port);
                false -> z_config:get(port)
            end
    end;
get_req(raw_path, RD) -> cowmachine_req:raw_path(RD);
get_req(path, RD) -> cowmachine_req:path(RD);
get_req(qs, RD) -> cowmachine_req:req_qs(RD);
get_req(headers, RD) -> cowmachine_req:get_req_headers(RD);
get_req(user_agent, RD) -> cowmachine_req:get_req_header(<<"user-agent">>, RD);
get_req(referer, RD) -> cowmachine_req:get_req_header(<<"referer">>, RD);
get_req(referrer, RD) -> get_req(referer, RD);
get_req(is_crawler, RD) -> z_user_agent:is_crawler(RD);
get_req(_Key, _RD) -> undefined.


-spec values(#context{}) -> list({atom(), any()}).
values(Context) ->
    [ {K, get(K, Context)} || K <- [
            method, version, peer, is_ssl, host, port, raw_path, path, qs, referrer, user_agent, is_crawler,
            headers, timezone, language
        ]
    ].


