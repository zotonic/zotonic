%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2026 Marc Worrell
%% @doc Set the language, redirect back to the page q.p
%% @end

%% Copyright 2012-2026 Marc Worrell
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

-module(controller_language_set).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    resource_exists/2,
    previously_existed/2,
    moved_temporarily/2,
    moved_permanently/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    Context1 = z_context:ensure_qs(Context),
    Context2 = z_context:ensure_session(Context1),
    Context3 = z_context:set_noindex_header(Context2),
    Context4 = z_context:set_nocache_headers(Context3),
    z_context:lager_md(Context4),
    ?WM_REPLY(true, Context4).

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

moved_permanently(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Context1 = mod_translation:set_user_language(z_context:get_q("code", Context), Context),
    Page = z_context:get_q("p", Context1),
    Location = case Page of
        "/" ++ _ -> Page;
        _ -> "/"
    end,
    Location1 = z_sanitize:uri(Location),
    AbsUrl = z_context:abs_url(
                    add_language(mod_translation:url_strip_language(Location1), Context1),
                    Context1),
    ?WM_REPLY({true, AbsUrl}, Context1).

add_language(<<>>, Context) ->
    add_language(<<"/">>, Context);
add_language("", Context) ->
    add_language(<<"/">>, Context);
add_language(Url, Context) ->
    Lang = z_convert:to_list(z_context:language(Context)),
    iolist_to_binary([$/, Lang, Url]).

