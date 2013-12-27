%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% Date: 2013-12-27
%% @doc Retrieve cookies from the current session.

%% Copyright 2013 Maas-Maarten Zeeman
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

-module(controller_cookies).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1,
    resource_exists/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2,
    response/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(_Args) -> 
    {ok, []}.

resource_exists(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_all(Context),
    {true, ReqData, Context1}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET', 'HEAD'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", response}], ReqData, Context}.

process_post(ReqData, Context) ->
    response(ReqData, Context).

response(ReqData, Context) ->
    ContextRd = z_context:set_reqdata(ReqData, Context),

    Cookies = z_session:get_cookies(ContextRd),
    ContextCookie = set_cookies(Cookies, ContextRd),
    z_session:clear_cookies(Context),

    ReqData1 = wrq:set_resp_body([], z_context:get_reqdata(ContextCookie)),
    {{halt, 204}, ReqData1, ContextCookie}.

set_cookies([], Context) ->
    Context;
set_cookies([{Key, Value, Options}|Rest], Context) ->
    set_cookies(Rest, z_context:set_cookie(Key, Value, Options, Context)).

