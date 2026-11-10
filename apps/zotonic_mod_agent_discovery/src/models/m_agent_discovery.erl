%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(m_agent_discovery).
-behaviour(zotonic_model).
-moduledoc(#{zotonic_keywords => ["reference", "backend_developer", "model", "http", "structured_data"]}).
-moduledoc("
Public discovery metadata, always evaluated with anonymous permissions.

* `get /info`: site title, public description, document URLs and documented reads.
* `get /links`: site-level discovery links.
* `get /links/+id`: also include JSON export for an anonymously visible, indexable resource.

Links are maps with `href`, `rel` and `type`. Escape values in HTML templates.
Resource exclusions affect advertisements, not existing API access permissions.
").
-export([m_get/3, info/1, links/2]).

-spec m_get(Path, Message, Context) -> Result when
    Path :: list(), Message :: zotonic_model:opt_msg(), Context :: z:context(),
    Result :: zotonic_model:return().
m_get([<<"info">> | Rest], _Message, Context) ->
    {ok, {info(Context), Rest}};
m_get([<<"links">>, Id | Rest], _Message, Context) ->
    {ok, {links(Id, Context), Rest}};
m_get([<<"links">>], _Message, Context) ->
    {ok, {links(undefined, Context), []}};
m_get(_, _Message, _Context) ->
    {error, unknown_path}.

-spec info(Context) -> Info when Context :: z:context(), Info :: map().
info(Context0) ->
    Context = public_context(Context0),
    Search = url(api, [{star, <<"model/search/get/paged">>}], Context),
    Export = url(api_rsc_export, [{id, <<"AGENT_RESOURCE_ID">>}], Context),
    #{
        title => z_convert:to_binary(m_site:get(title, Context)),
        description => z_convert:to_binary(m_config:get_value(mod_agent_discovery, description, <<>>, Context)),
        home => z_context:abs_url(<<"/">>, Context),
        llms => url(agent_llms, [], Context),
        catalog => url(agent_api_catalog, [], Context),
        openapi => url(agent_openapi, [], Context),
        help => url(agent_api_help, [], Context),
        sitemap => case z_module_manager:active(mod_seo_sitemap, Context) of
            true -> url(sitemap_xml, [], Context);
            false -> undefined
        end,
        operations => agent_discovery_document:operations(Search, Export)
    }.

-spec links(Id, Context) -> Links when
    Id :: term(), Context :: z:context(), Links :: [map()].
links(Id, Context0) ->
    Context = public_context(Context0),
    Info = info(Context),
    SiteLinks = [
        link(maps:get(catalog, Info), <<"api-catalog">>, <<"application/linkset+json">>),
        link(maps:get(openapi, Info), <<"service-desc">>, <<"application/json">>),
        link(maps:get(help, Info), <<"service-doc">>, <<"text/html">>),
        link(maps:get(llms, Info), <<"describedby">>, <<"text/plain">>)
    ],
    case public_resource(Id, Context) of
        undefined -> SiteLinks;
        RscId -> SiteLinks ++ [link(url(api_rsc_export, [{id, RscId}], Context),
            <<"alternate">>, <<"application/json">>)]
    end.

public_resource(Id, Context) when is_integer(Id); is_binary(Id); is_atom(Id), Id =/= undefined ->
    case m_rsc:rid(Id, Context) of
        undefined -> undefined;
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                false -> undefined;
                true ->
                    Cat = m_rsc:p(RscId, category_id, Context),
                    NoIndex = m_config:get_boolean(seo, noindex, Context)
                        orelse z_convert:to_bool(m_rsc:p(RscId, seo_noindex, Context))
                        orelse z_convert:to_bool(m_rsc:p(Cat, is_seo_noindex_cat, Context)),
                    case NoIndex of true -> undefined; false -> RscId end
            end
    end;
public_resource(_, _Context) -> undefined.

public_context(Context) ->
    z_context:set_language('x-default', z_acl:anondo(Context)).

url(Dispatch, Args, Context) ->
    z_context:abs_url(z_dispatcher:url_for(Dispatch, Args, Context), Context).

link(Href, Rel, Type) -> #{href => Href, rel => Rel, type => Type}.
