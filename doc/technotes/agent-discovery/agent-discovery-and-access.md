# Agent discovery: current design and future work

Zotonic publishing sites want their public information to be found, understood,
cited and shared. `mod_agent_discovery` gives ordinary HTTP clients a documented
way to search, read and navigate that information without credentials, JavaScript
execution or a special user-agent string. Public access remains governed by the
existing resource and property permissions.

The initial implementation and phase 4 pilot on `zotonicwww2` are complete as of
11 November 2026. Additional JSON-LD and Turtle representations remain deferred
until the SPARQL merge. This note describes the implemented design and subsequent
work; it is not an outstanding implementation checklist.

## Documentation map

- [Setup and API guide](../../guides/agent-discovery.md): activation, parameters,
  limits, caching and Markdown customization.
- [Pilot findings](pilot-findings.md): observed discovery and retrieval behavior,
  defects corrected, acceptance coverage and limits of the evidence.
- [Bounded edge pages](bounded-edge-pages.md): core model API, indexing choices,
  ordering and query-plan measurements for high-degree relationships.
- [Text-reader experiment](text-reader-discovery.md): why hidden HTML links were
  rejected, the extraction matrix and reusable synthetic fixtures.

## Current discovery surface

The module is optional and depends on `mod_base` and `mod_search`, not `mod_seo`.
Discovery documents share capability definitions rather than independently
inventing descriptions of the API. Published URLs are generated through dispatch.

| Surface | Purpose |
| --- | --- |
| HTTP `Link` headers and HTML head links | Advertise API descriptions, catalog and available resource representations from ordinary pages |
| `/llms.txt` | Concise site guidance and discovery links |
| `/.well-known/api-catalog` | Linkset catalog of public endpoints and descriptions |
| `/openapi.json` | OpenAPI 3.1.0 contract for concise reads and compatibility APIs |
| `/api-help` | Readable explanation and executable examples |
| Markdown “API access” section | Link to API help, OpenAPI and the current resource's JSON representation |

Human-facing page bodies and footers contain no added discovery links, visible
or hidden. The Markdown section is part of the dedicated representation, not
the JSON resource's editorial `markdown` field. Custom HTML controllers must call
`z_context:set_resource_headers/2` to participate in resource-header discovery;
standard head hooks supply the HTML links. Existing Link values are preserved.

This deliberately accepts that a reader exposing only HTML body text may miss
discovery. Hidden body links did not survive readers consistently. Markdown
helps clients that already receive it; it does not guarantee discovery of that
representation from an ordinary page URL.

## Public reads and representations

The facade exposes search, resource reads, one-hop relationships and public
category/predicate descriptions under `/api/agent/`. Dedicated Markdown is
available at `/agent/resource/{id}/markdown` and through content negotiation on
`/id/{id}`. Existing search and full resource export remain compatible.

The model always uses a fresh anonymous context, even for signed-in callers.
It returns explicit projections rather than arbitrary database properties.
Listings respect publication and site/resource/category noindex rules; direct
reads can return otherwise public noindex resources. Missing, private and
unpublished resources are indistinguishable through a 404 response.

Language defaults to the configured site language. Responses distinguish the
requested language, actual content language and fallback, retain supplied
licensing/attribution, and provide ordinary human page URLs for citations.
An entity URI and a citation URL serve different purposes and are both retained.

Search supports text, category, inclusive UTC publication dates, and explicit
ID/publication/modification ordering. Explicit sort and the ID tie-breaker take
precedence over full-text relevance. Relationships support both directions and
an optional predicate; endpoint-ID ordering permits efficient indexed reads.

Pages contain at most 100 candidates, with a default of 20 and a 10,000-candidate
exploration bound. Reads have a five-second worker budget and a 1 MiB response
bound. Relationship selection fetches only one page plus lookahead in SQL.
Permission/noindex filtering can leave an empty page with a continuation: clients
must follow `next_url` until `next` is false. No totals are exposed.
`limit_reached` distinguishes truncation from exhaustion. Offset pagination is
not a snapshot and can shift under concurrent edge edits.

Successful content responses require cache revalidation. Conditional requests
rebuild the representation after anonymous permission and publication checks,
then calculate the ETag. No server-side representation cache is introduced.
Discovery documents and errors use no-store. Generic API indexing and CORS
policies are not changed by enabling the module.

## Markdown composition and implementation boundaries

Editorial content is composed through `_agent_content.tpl` and
`_agent_block_<type>.tpl`, with site/category overrides, then converted using
Zotonic's existing Markdown converter. Text, headings and one level of page
references are supported. All `survey_...` blocks are skipped before template
selection. Unsupported or nested content is explicitly marked incomplete;
custom fields belong in composition templates, not an unrestricted export.

A rendering error must not become successful empty content.
`z_template:render_to_iolist_result/3` preserves template errors for the agent
composer, which returns an uncached 503. The existing tolerant renderer retains
its behavior for ordinary HTML. Legitimately empty rendered content is valid.

The main implementation boundaries are:

- [Module observers](../../../apps/zotonic_mod_agent_discovery/src/mod_agent_discovery.erl):
  headers and representation registration.
- [Public content model](../../../apps/zotonic_mod_agent_discovery/src/models/m_agent_content.erl):
  argument validation, anonymous reads, bounded execution and projections.
- [Discovery documents](../../../apps/zotonic_mod_agent_discovery/src/support/agent_discovery_document.erl):
  shared descriptions for clients and API help.
- [Markdown composer](../../../apps/zotonic_mod_agent_discovery/src/support/agent_content_markdown.erl):
  localized editorial composition and omission reporting.
- [Core edge model](../../../apps/zotonic_core/src/models/m_edge.erl):
  bounded `get_page/6`, `o_page` and `s_page` reads. These generic entry points use
  caller ACLs; the facade adds its anonymous publication/noindex policy.

## Future work

### Semantic representations after the SPARQL merge

Review the merged RDF model and libraries before adding JSON-LD/Turtle routes,
mappings or serializers. Reuse that implementation, provide explicit URLs and
register media types through `content_types_dispatch`. Raw JSON-LD should use
its own media type, not an encoded string inside the generic API envelope.
Existing SEO JSON-LD remains in place meanwhile.

Acceptance should demonstrate that both serializations describe the same public
graph, preserve language and attribution, enforce permissions, negotiate correct
media types and are advertised only when available. This dependency does not
block merging the existing discovery, JSON, relationships and Markdown support.

### Discovery across real clients

Compare clients starting at ordinary URLs with those receiving Markdown or API
documentation explicitly. Test hosted URL readers separately from local HTML
extraction and raw-HTML reader services. Controlled variants can isolate HTML
baseline, `llms.txt`, header/head links and combined discovery, without adding
body links to live pages.

Keep independently established expected answers, client capabilities, request
traces and task budgets. Measure correct answers and citations as well as API
use: an agent completing a task through ordinary links is not a failure. Expand
providers, languages and partial-translation fixtures before drawing adoption
claims. Token/cost measurements need explicit instrumentation.

### Larger sites and new access patterns

Profile full-text queries and concurrent workloads on production-sized data;
bounded returned rows do not imply constant query cost. OFFSET still visits
skipped entries. Consider cursor/snapshot traversal when clients need stable or
larger graph walks, and bulk exports/change feeds when repeated paging becomes
a demonstrated workload. Any representation cache must account for dependent
resources, edges, timed publication and permission changes before serving 304s.

Read-only MCP adapters and richer semantic search are optional responses to
client demand. Build them over shared operations; do not advertise unavailable
endpoints. None of these extensions is an outstanding initial-pilot criterion.
