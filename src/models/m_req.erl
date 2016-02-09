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
-include_lib("webzmachine/include/webmachine_logger.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    values(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    values(Context).


%% @doc Fetch the field from the wrq interface.
-spec get(atom(), #context{} | #wm_reqdata{} | undefined) -> any(). 
get(undefined, _) -> undefined;
get(_, undefined) -> undefined;
get(ua_class, #context{} = Context) -> z_user_agent:get_class(Context);
get(ua_props, #context{} = Context) -> z_user_agent:get_props(Context);
get(timezone, #context{} = Context) -> z_context:tz(Context);
get(language, #context{} = Context) -> z_context:language(Context);
get(What, #context{} = Context) -> get_req(What, z_context:get_reqdata(Context));
get(What, #wm_reqdata{} = RD) -> get_req(What, RD).

get_req(_What, undefined) -> undefined;
get_req(method, RD) -> wrq:method(RD);
get_req(version, RD) -> wrq:version(RD);
get_req(peer, RD) -> wrq:peer(RD);
get_req(is_ssl, RD) -> wrq:is_ssl(RD);
get_req(host, RD) -> wrq:get_req_header_lc("host", RD);
get_req(raw_path, RD) -> wrq:raw_path(RD);
get_req(path, RD) -> wrq:path(RD);
get_req(qs, RD) -> wrq:req_qs(RD);
get_req(headers, RD) -> wrq:req_headers(RD);
get_req(user_agent, RD) -> wrq:get_req_header_lc("user-agent", RD);
get_req(referer, RD) -> wrq:get_req_header_lc("referer", RD);
get_req(referrer, RD) -> wrq:get_req_header_lc("referer", RD);
get_req(req_id, #wm_reqdata{log_data=#wm_log_data{req_id=ReqId}}) -> ReqId;
get_req(_Key, _RD) -> undefined.


-spec values(#context{}) -> list({atom(), any()}).
values(Context) ->
    [ {K, get(K, Context)} || K <- [
            method, version, peer, is_ssl, host, raw_path, path, qs, referrer, user_agent, req_id, headers,
            ua_class, ua_props, timezone, language
        ]
    ].

