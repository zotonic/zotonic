%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

%% Copyright 2009-2022 Marc Worrell
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

-module(controller_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    content_types_provided/1,
    is_authorized/1,
    process/4
]).

service_available(Context) ->
    Context1 = case z_convert:to_bool(z_context:get(nocache, Context)) of
        true -> z_context:set_nocache_headers(Context);
        false -> Context
    end,
    {true, Context1}.

content_types_provided(Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[ {<<"text">>, <<"html">>, []} ], Context};
        Mime when is_list(Mime) ->
            {[ z_convert:to_binary(Mime) ], Context};
        Mime ->
            {[ Mime ], Context}
    end.

%% @doc Check if the current user is allowed to view the resource.
is_authorized(Context) ->
    z_context:logger_md(Context),
    case z_context:get(anonymous, Context) of
        true ->
            {true, Context};
        _ ->
            z_controller_helper:is_authorized(Context)
    end.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = z_context:get_all(Context),
    {Vars1, OptRscId} = maybe_options_id(Vars, Context),
    Context0 = z_context:set_noindex_header(m_rsc:p_no_acl(OptRscId, seo_noindex, Context), Context),
    Context1 = z_context:set_resource_headers(OptRscId, Context0),
    Context2 = set_optional_cache_header(Context1),
    Template = z_context:get(template, Context2),
    Rendered = z_template:render(Template, Vars1, Context2),
    z_context:output(Rendered, Context2).

-spec maybe_options_id(list(), z:context()) -> {list(), m_rsc:resource_id()|undefined}.
maybe_options_id(Vars, Context) ->
    case lists:keyfind(id, 1, Vars) of
        {id, PageId} ->
            {lists:keydelete(id, 1, Vars), m_rsc:rid(PageId, Context)};
        false ->
            {Vars, undefined}
    end.

set_optional_cache_header(Context) ->
    case z_context:get(max_age, Context) of
        undefined ->
            Context;
        MaxAge when is_integer(MaxAge) ->
            z_context:set_resp_header(
                <<"cache-control">>,
                <<"public, max-age=", (z_convert:to_binary(MaxAge))/binary>>,
                Context)
    end.
