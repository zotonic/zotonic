%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Set the language, redirect back to the page q.p

%% Copyright 2012 Marc Worrell
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

-include_lib("webmachine_controller.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, z_context:ensure_qs(Context)),
    Context2 = z_context:ensure_session(Context1),
    ?WM_REPLY(true, Context2).

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Context1 = mod_translation:set_user_language(z_context:get_q("code", Context), Context),
    Page = z_context:get_q("p", Context1),
    Location = case z_utils:is_empty(Page) of
                   true -> "/";
                   false -> Page
               end,
    AbsUrl = z_context:abs_url(add_language(mod_translation:url_strip_language(Location), Context1), Context1),
    ?WM_REPLY({true, AbsUrl}, Context1).

moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.


add_language(Url, Context) ->
    Lang = z_convert:to_list(z_context:language(Context)),
    iolist_to_binary([$/, Lang, Url]).

