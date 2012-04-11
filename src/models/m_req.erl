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

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    get/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    values(z_context:get_reqdata(Context)).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    values(z_context:get_reqdata(Context)).


%% @doc Fetch the field from the wrq interface.
get(ua_class, #context{} = Context) ->
    z_user_agent:get_class(Context);
get(ua_props, #context{} = Context) ->
    z_user_agent:get_props(Context);

get(What, #context{} = Context) -> get(What, z_context:get_reqdata(Context));
get(_What, undefined) -> undefined;
get(method, RD) -> wrq:method(RD);
get(version, RD) -> wrq:version(RD);
get(peer, RD) -> wrq:peer(RD);
get(is_ssl, RD) ->
    case wrq:port(RD) of
        {ssl, _} -> true;
        _ -> false
    end; 
get(host, RD) -> wrq:get_req_header_lc("host", RD);
get(raw_path, RD) -> wrq:raw_path(RD);
get(path, RD) -> wrq:path(RD);
get(qs, RD) -> wrq:req_qs(RD);
get(headers, RD) -> wrq:req_headers(RD);
get(user_agent, RD) -> proplists:get_value("user-agent", wrq:req_headers(RD));
get(_Key, _RD) -> undefined.


values(RD) ->
    [ {K, get(K,RD)} || K <- [
            method, version, peer, is_ssl, host, raw_path, path, qs, headers
        ]
    ].

