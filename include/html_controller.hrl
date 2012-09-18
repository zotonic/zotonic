%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Empty HTML controller, defining the basic callbacks needed for a html page.  All is needed is the 'html' function.
%% Make a new resource (resource_plop.erl) by:
%%
%% -module(resource_plop).
%% -author("Your Name <me@example.com>").
%% -include_lib("html_controller.hrl").
%% 
%% html(Context) ->
%%    Html = z_template:render("plop.tpl", Context),
%%    z_context:output(Html, Context).

%% Copyright 2009 Marc Worrell
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

-export([init/1, to_html/2, service_available/2, charsets_provided/2]).

-include_lib("webmachine_controller.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> 
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

to_html(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    {Result, ResultContext} = html(Context2),
    ?WM_REPLY(Result, ResultContext).
