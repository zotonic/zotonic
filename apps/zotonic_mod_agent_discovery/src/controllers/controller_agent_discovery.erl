%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(controller_agent_discovery).
-moduledoc(#{zotonic_keywords => ["reference", "backend_developer", "controller", "http"]}).
-moduledoc("
Serve public agent discovery documents with GET and HEAD.
The dispatch option `document` selects `llms`, `catalog`, `openapi` or `help`.
Rendering uses anonymous permissions, even for authenticated visitors.
Responses are not shared-cacheable; site-wide noindex settings are respected.
").
-export([service_available/1, allowed_methods/1, content_types_provided/1, process/4]).

-spec service_available(Context) -> {boolean(), Context} when Context :: z:context().
service_available(Context) ->
    Public = z_acl:anondo(Context),
    {true, z_context:set_noindex_header(z_context:set_nocache_headers(Public))}.

-spec allowed_methods(Context) -> {[binary()], Context} when Context :: z:context().
allowed_methods(Context) -> {[<<"GET">>, <<"HEAD">>], Context}.

-spec content_types_provided(Context) -> {[binary()], Context} when Context :: z:context().
content_types_provided(Context) ->
    Type = case z_context:get(document, Context) of
        llms -> <<"text/plain">>;
        catalog -> <<"application/linkset+json">>;
        openapi -> <<"application/json">>;
        help -> <<"text/html">>
    end,
    {[Type], Context}.

-spec process(Method, Accepted, Provided, Context) -> {iodata(), Context} when
    Method :: binary(), Accepted :: term(), Provided :: term(), Context :: z:context().
process(_Method, _Accepted, _Provided, Context0) ->
    Context = z_context:set_resource_headers(undefined, z_acl:anondo(Context0)),
    Info = m_agent_discovery:info(Context),
    case z_context:get(document, Context) of
        llms -> {agent_discovery_document:llms(Info), Context};
        catalog ->
            C = z_context:set_resp_header(<<"content-type">>,
                <<"application/linkset+json; profile=\"https://www.rfc-editor.org/info/rfc9727\"">>, Context),
            {z_json:encode(agent_discovery_document:catalog(Info)), C};
        openapi -> {z_json:encode(agent_discovery_document:openapi(Info)), Context};
        help ->
            z_context:output(z_template:render("agent_api_help.tpl", [{discovery, Info}], Context), Context)
    end.
