%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(mod_agent_discovery).
-mod_title("Agent discovery").
-mod_description("Help visitors discover public search and resource APIs.").
-mod_depends([base, mod_search]).
%% Collect links after the normal resource-header observers, including WebSub.
-mod_prio(1000).
-moduledoc(#{zotonic_keywords => ["reference", "operator", "module", "http", "structured_data"]}).
-moduledoc("
Advertise bounded anonymous reads and existing resource export through `/llms.txt`,
`/.well-known/api-catalog`, `/openapi.json` and `/api-help`.

Enable this module on public publishing sites. Templates using the standard
`{% all include \"_html_head.tpl\" %}` hook receive discovery links automatically.
Discovery stays in HTTP headers, the HTML head and dedicated documents; no links
are added to human-facing page bodies or footers.
Resource response headers use the same links and preserve existing Link values.

Discovery metadata and resource visibility checks always use an anonymous
context. Existing model APIs retain their own ACL and authentication behavior.
This module does not change permissions, robots rules or API indexing headers.
Additional JSON-LD and Turtle support awaits the SPARQL implementation merge.

The `description` configuration is optional public text describing the site.
It is not a license grant. Site templates can override `agent_api_help.tpl`.

Discovery model: `m.agent_discovery.info` and `m.agent_discovery.links[id]`.
Content model: `m.agent_content` provides bounded search, resource, related and
schema reads. Markdown uses category-aware `_agent_content.tpl` composition and
`_agent_block_<type>.tpl` renderers. Survey blocks are always excluded.
Content routes support GET/HEAD and require ETag revalidation after anonymous
permission/publication checks. See the agent discovery guide for request limits.
").
-mod_config([
    #{key => description, type => string, default => <<>>,
      description => "Public description of the information available on this site."}
]).

-export([observe_resource_headers/3, observe_content_types_dispatch/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec observe_resource_headers(Event, Headers, Context) -> Headers when
    Event :: #resource_headers{},
    Headers :: [{binary(), binary()}],
    Context :: z:context().
observe_resource_headers(#resource_headers{id = Id}, Headers, Context) ->
    agent_discovery_document:link_header(m_agent_discovery:links(Id, Context), Headers).

%% Advertise an explicit representation; existing human page routes stay intact.
-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), z:context()) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Types, _Context) ->
    Types ++ [{{<<"text">>, <<"markdown">>, []}, agent_markdown}].
