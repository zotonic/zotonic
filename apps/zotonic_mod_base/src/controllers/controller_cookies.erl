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
    resource_exists/1,
    allowed_methods/1,
    content_types_provided/1,
    process_post/1,
    response/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    Context1 = z_context:ensure_qs(Context),
    z_context:lager_md(Context1),
    {true, Context1}.

allowed_methods(Context) ->
    {[<<"POST">>, <<"GET">>, <<"HEAD">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, response}], Context}.

process_post(Context) ->
    response(Context).

response(Context) ->
    Cookies = z_session:get_cookies(Context),
    ContextCookie = set_cookies(Cookies, Context),
    z_session:clear_cookies(Context),
    ContextBody = cowmachine_req:set_resp_body(<<>>, ContextCookie),
    {{halt, 204}, ContextBody}.

set_cookies([], Context) ->
    Context;
set_cookies([{Key, Value, Options}|Rest], Context) ->
    set_cookies(Rest, z_context:set_cookie(Key, Value, Options, Context)).

