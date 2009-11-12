%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

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

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([provide_content/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[{"text/html", provide_content}], ReqData, Context};
        Mime -> 
            {[{Mime, provide_content}], ReqData, Context}
    end.

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = case z_context:get(anonymous, Context) of
        true -> z_context:ensure_qs(Context1);
        _ -> z_context:ensure_all(Context1)
    end,
    Template = z_context:get(template, Context2),
    Rendered = z_template:render(Template, z_context:get_all(Context), Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    ?WM_REPLY(Output, OutputContext).
