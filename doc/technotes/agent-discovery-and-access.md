# Public information discovery and access for agents

**Status: implementation in progress; discovery module started.** Reviewed 10 November 2026
against repository revision `e7c53472b`.

### Implementation progress

The first implementation of `mod_agent_discovery` now provides discovery routes,
shared descriptions of existing search/export reads, HTML and HTTP links, and a
visible API-help fragment. See the [setup guide](../guides/agent-discovery.md).
It does not yet implement the concise read facade or new content representations.
Discovery documents use no-store responses pending the cache work below.
Live pilot fixtures and agent trials are still required; local contract tests
do not establish deployed-site behavior or independent agent discovery.

## Purpose

Zotonic sites publish information that we want people and software to find,
understand, cite, and share. Make exploration of public content straightforward:
an agent arriving at an ordinary page should discover how to read that resource,
search the site, and follow meaningful relationships without registration or
reverse-engineering HTML.

The primary audience is agents arriving through search engines and hyperlinks,
as clarified in the [shared ChatGPT discussion](https://chatgpt.com/share/6af32e29-9bbc-83eb-88af-8b036a776c62).
Explicitly connected assistants are a secondary audience. Public access should
work with ordinary HTTP GETs and ordinary links, without an MCP client, an API
key, JavaScript execution, or a special user-agent string.

Openness applies to deliberately public information. Existing resource and
property permissions remain authoritative. Enabling discovery must not publish
drafts or private fields. Conversely, do not introduce a second per-resource
approval process for content that the publisher already makes public. Operational
limits should make sustained exploration affordable, with pagination and clear
retry guidance, rather than discourage automated readers.

## Evaluation of the discussion

The revised proposal is a good starting point: advertise capabilities on the
page where the agent arrives, provide a human-readable explanation, and measure
whether agents actually use them. Its initial emphasis on MCP is less relevant
to this audience. Keep MCP as a later adapter over shared operations.

| Proposal | Assessment and decision |
| --- | --- |
| HTML links and HTTP `Link` headers | First priority. Include ordinary visible links because extracted-text browsing tools may discard headers and HTML head metadata. |
| OpenAPI for search and retrieval | Adopt a small, tested public contract. Model callback documentation alone does not describe HTTP serialization, errors, or site-specific capabilities. |
| `llms.txt` | Cheap complementary guidance. Generate it from the same capabilities as the API documentation; do not depend on clients looking for it. |
| `/.well-known/api-catalog` | Adopt the defined Linkset format, including endpoint links. An arbitrary JSON capability map or OpenAPI document at that URL is insufficient. |
| JSON-LD and semantic navigation | Keep existing RDF and SEO output. Defer additional JSON-LD support until the SPARQL implementation merges, then build on its new libraries for JSON-LD and Turtle. Public graph navigation can proceed independently. |
| Markdown representations | Useful early, particularly for clients limited to fetching links. Reuse the existing converter and resource model; avoid converting the entire rendered page with its navigation. |
| Shared information-access layer | Agree, but keep it thin. Do not replace the search engine, resource export format, ACLs, or resource graph. |
| Agent experiments | Essential. Separate independent discovery from API usability when the agent is explicitly given the documentation. |
| Vector search, answer generation, MCP | Defer until experiments demonstrate a need. None is required to expose existing information well. |

Standards checks support the discovery proposal, with two qualifications:

- [RFC 9727](https://www.rfc-editor.org/rfc/rfc9727.html) is a Standards Track
  specification, not evidence of universal client adoption. It defines the
  `api-catalog` relation and well-known location, requires endpoint hyperlinks
  and `application/linkset+json`, and recommends its profile parameter.
- The current [llms.txt v2 proposal](https://llmstxt.org/) recommends Markdown
  alternates and `describedby` links to the applicable `llms.txt`. This remains
  a proposal, not a guarantee of crawler or assistant behavior.

Use [RFC 8631](https://www.rfc-editor.org/rfc/rfc8631.html)'s `service-desc` and
`service-doc` relations for machine and human API descriptions. Pin an
[OpenAPI specification version](https://spec.openapis.org/oas/v3.2.1.html)
supported by the chosen validator and clients; do not silently track `latest`.

## What the code already provides

The following are source observations, not results from a deployed-site audit.
Module activation, templates, ACL rules, proxies, and site overrides can change
what a visitor actually receives.

| Area | Existing implementation | Consequence for this plan |
| --- | --- | --- |
| HTTP model access | [Base dispatch](../../apps/zotonic_mod_base/priv/dispatch/dispatch) routes `/api/*` to [controller_api](../../apps/zotonic_mod_base/src/controllers/controller_api.erl). GET model paths use `model/<model>/get/...`; HEAD is supported for model reads. The controller also supports other methods and MQTT behavior. | Reuse model access, but document selected synchronous reads rather than advertise the entire transport as an agent API. |
| Search | [m_search](../../apps/zotonic_core/src/models/m_search.erl) implements `/paged`, named searches, counts, paging and options. [z_search](../../apps/zotonic_core/src/support/z_search.erl) expands selected properties and integrates ACL checks. | The discussion's paged search example is grounded in the code. Add a bounded, documented subset rather than invent new search semantics. Review each advertised search and its counts. |
| Resource export | [m_rsc_export](../../apps/zotonic_core/src/models/m_rsc_export.erl) returns identifiers, category ancestry, resource properties, media URLs and outgoing edges. It uses `m_rsc:get/2`, filters invisible predicates and objects, omits `hasusergroup`, and offers `rsc_export_done` customization. | Keep this portable format compatible. It is useful for complete retrieval but is not a concise search-result format. Site observers must be included in verification. |
| Property permissions | [m_rsc](../../apps/zotonic_core/src/models/m_rsc.erl)'s `get/2` and `filter_props_acl/3` apply resource/property visibility. | Read through the existing models; resource visibility alone is not permission to serialize every stored field. |
| Identity and negotiation | [controller_id](../../apps/zotonic_mod_base/src/controllers/controller_id.erl) negotiates `/id/<id>` and redirects with 303. [mod_base](../../apps/zotonic_mod_base/src/mod_base.erl)'s `content_types_dispatch` observer registers JSON export and other API encodings. | JSON negotiation already exists at the identity URL. Arbitrary human page URLs do not thereby support Markdown or JSON-LD negotiation. |
| Response headers | [z_context:set_resource_headers/2](../../apps/zotonic_core/src/support/z_context.erl) sets `x-resource-uri` and folds `resource_headers`. Page, identity and resource-export responses use this mechanism. [mod_websub](../../apps/zotonic_mod_websub/src/mod_websub.erl) already contributes links there. | Add discovery through the existing observer, merging existing `Link` values rather than overwriting WebSub links. |
| Structured metadata | [m_rdf](../../apps/zotonic_core/src/models/m_rdf.erl) provides summaries and category-to-Schema.org mappings. [seo_jsonld_webpage](../../apps/zotonic_mod_seo/src/support/seo_jsonld_webpage.erl) builds page/site/publisher graphs. [m_seo](../../apps/zotonic_mod_seo/src/models/m_seo.erl) generates JSON-LD in an anonymous context; [_html_head_seo.tpl](../../apps/zotonic_mod_seo/priv/templates/_html_head_seo.tpl) embeds it. | Preserve current output and revisit extensions after the SPARQL merge. The model returns encoded JSON text; that API response is not itself a raw `application/ld+json` representation. |
| Markdown | [z_markdown](../../apps/zotonic_core/src/markdown/z_markdown.erl) already converts HTML to Markdown through `markupz`. | Add resource composition and an HTTP representation, not a new converter. |
| Crawling | Base [robots.txt](../../apps/zotonic_mod_base/priv/lib/misc/robots.txt) allows normal content and excludes admin/language-switch paths. [m_seo_sitemap](../../apps/zotonic_mod_seo_sitemap/src/models/m_seo_sitemap.erl) tracks public indexable resource URLs and languages. | Retain and link existing sitemaps. Verify actual site robots/proxy behavior; do not introduce AI-specific blocks. |
| API indexing | `controller_api:service_available/1` forces `X-Robots-Tag: noindex,nofollow`. CORS is separately site-configurable through `service_api_cors`. | Retrieval remains possible, but indexing/following policy needs an explicit decision for the new public representations. CORS matters to browser clients, not ordinary server-side fetches. |

No generic `llms.txt`, API catalog, OpenAPI publication, or public Markdown
representation was found in the inspected bundled modules. This is not a claim
about external modules or individual sites.

### Correctly describe the existing wire format

For example, these existing paths are useful starting points:

```text
GET /api/model/search/get/paged?cat=article&page=1&pagelen=20&options.properties=title,summary,page_url_abs
GET /api/model/rsc_export/get/123
GET /id/123                     Accept: application/json
```

The last request redirects to the representation; follow the redirect. Examples
must use real public IDs/categories from the pilot site when executed.

The model API normally wraps values as `{"status":"ok","result":...}`. A
search result has its own result list and paging metadata. Translated properties
can be translation objects rather than strings. In a full export, `page_url` is
a map keyed by language, including `x-default`, not a single citation string.
Resource URIs identify entities; canonical human page URLs serve as citations.
Keep both and document the difference. Capture actual HTTP fixtures before
publishing schemas, including errors and estimated search totals.

## Proposed implementation

Introduce `zotonic_mod_agent_discovery` (`mod_agent_discovery`) as an optional
module for public publishing sites. Activation enables its complete discovery
surface with useful defaults for already-public content. Private sites can leave
it disabled. Do not change ACL rules or require editors to tag every public page
as suitable for AI. Offer site-level customization for descriptions, examples,
public content categories, licenses and exceptional exclusions.

Keep responsibilities small:

- `mod_agent_discovery`: module configuration and observers for resource headers
  and representation registration.
- `m_agent_discovery`: capability metadata, validated read operations and public
  projections using `m_search`, `m_rsc`, `m_rsc_export`, categories and predicates.
- Support modules: OpenAPI/Linkset serialization and resource-to-Markdown
  composition. Controllers handle HTTP representations and conditional requests.
- Templates: `_html_head.tpl` contribution, an API help page, and a reusable
  visible link fragment for site footers or resource pages.

Maintain one explicit capability definition used by documentation, OpenAPI,
catalog and `llms.txt`. Include only implemented operations available on the
site. Do not infer a public contract by enumerating all loaded models. Generate
URLs with dispatch rules; preserve host, language and site overrides.

### Discovery surface

| Proposed location/surface | Content |
| --- | --- |
| `/llms.txt` | Site purpose, public-use invitation, key collections, API help, search examples, sitemap and representation links. Concise and editorially overridable. |
| `/.well-known/api-catalog` | Linkset catalog of enabled public APIs, with endpoint links and links to their descriptions. |
| `/openapi.json` | Selected GET operations, parameters, response envelopes, language behavior, paging and errors. |
| `/api-help` | Server-rendered explanation with clickable examples and ordinary links to representations and search. The path is provisional and dispatch-generated. |
| Resource HTML head and headers | Catalog, API documentation and supported resource alternates; `describedby` for `llms.txt`. |
| Resource body/footer | A visible “Data and API” link, plus a readable-version link where appropriate. Supply a fragment and document its integration in custom site templates. |

Only advertise representations that exist and are public. A page without a
resource ID can still advertise site-level search and documentation. Use one
link builder for HTML and headers, with correct escaping for each context.
Do not put an unavailable `/mcp` endpoint in generated guidance.

### Public read contract

Start by documenting the existing search and full-export routes. Add a small
model facade where concise output, validated arguments or bounded traversal is
needed; its route names are to be fixed in the first implementation PR.

| Operation | Contract to implement |
| --- | --- |
| Search | Text, category, language, selected date filters, supported sort and pagination. Return concise items with ID, URI, citation URL, title, summary, actual language and modification time; expose a next-page link. |
| Read resource | Concise or full content, source/citation links, available languages, media and relationship links. Preserve the existing full-export endpoint and format. |
| Related resources | One hop, incoming or outgoing, with an optional predicate and bounded paging. Return visible endpoints and predicate descriptions; preserve meaningful edge order. |
| Describe schema | Public categories and predicates with names, labels, descriptions, semantic URIs where present, and supported query capabilities. Describe site-specific fields explicitly; do not dump database columns or internal model paths. |

Use normal search and graph functions beneath the facade. Advertise useful
capabilities such as date filtering only when implemented; do not assume every
site has an `artist` predicate or an `upcoming` filter. An empty query should
support browsing public content, not require the visitor to know a search term.

For the new facade, propose a default page length of 20 and maximum of 100,
one-hop traversal, a capped query length and execution budget. Validate these
against the pilot dataset. Reject unsupported arguments with useful errors;
expose continuation links and clearly label estimated totals. Bound processing
as well as response size: truncating an already-built full export is insufficient
for a resource with a very large edge set.

### Readable and semantic representations

Add explicit dispatch URLs for Markdown, then register them through
`content_types_dispatch` for `/id/<id>`. Explicit linked URLs work for
clients that cannot set `Accept`. Avoid a blanket `.md` rewrite across arbitrary
site page paths; aliases and custom controllers need deliberate handling.

Compose Markdown from the selected-language title, summary, body, supported
blocks, relevant media descriptions and source/relationship links. Convert HTML
fields using `z_markdown:to_markdown/2`. Provide category/site overrides for
content whose meaning is in blocks or custom fields; do not silently label a
body-only extract as complete. Preserve code, tables, links and image alt text.
Unsupported interactive content should retain a link to its human page.

Additional JSON-LD support depends on the planned SPARQL implementation merge.
Hold off on new JSON-LD endpoints and mapping extensions until that merge is
complete. Then inspect the merged implementation and use its new libraries for
JSON-LD and Turtle support, sharing its RDF representation and serialization
where appropriate. Keep existing SEO JSON-LD output in place in the meantime.
The post-merge work should provide explicit representation URLs and register
their content types through `content_types_dispatch`; raw JSON-LD should use
its own media type rather than the API's encoded-string envelope.

Record requested and actual language/fallback in the concise contract. Never
invent a translation or strip attribution and supplied license information.

### Access, indexing and efficient exploration

The new public facade and discovery documents should use an anonymous context
even when the requesting browser is signed in. This makes shared responses
predictable and cacheable. Existing authenticated model access remains available
under its current contract; a future authenticated adapter must use separate
cache rules.

Distinguish permission, discovery and indexing:

- ACL determines whether content and fields may be returned. Apply it to graph
  endpoints, media, schema examples, search results, counts and facets.
- Respect existing site/resource/category noindex choices in generated listings.
  A noindex flag is not an ACL; direct retrieval of an otherwise public resource
  can remain available, carrying the appropriate indexing directive.
- Keep human pages canonical for search results. For new machine representations,
  prefer `noindex` without `nofollow` where duplicate indexing is undesirable.
  Decide this per representation and test it. Do not remove the generic API's
  forced header globally as an incidental part of this work.

Support GET/HEAD, appropriate errors, ETags and conditional requests on the new
representations. Cache keys must account for site, resource, language, format
and configuration. Invalidation must cover dependent resource/edge changes,
unpublication and publication windows; resource `modified` alone is insufficient
for derived graphs. When negotiating on the same URL, emit the necessary `Vary`
headers. Do not share authenticated responses in a public cache.

Provide public browser access on dedicated anonymous read endpoints where useful,
without credentialed wildcard CORS or a blanket change to the generic API.
Document limits and return `429` with `Retry-After` for throttling. Keep ordinary
pagination usable; large-site bulk snapshots/change feeds can follow measured
demand. State the publisher's reuse invitation and available licenses accurately,
including media-specific exceptions. Content is source data, never executable
agent instructions.

## Delivery sequence and acceptance criteria

### 1. Establish the contract and baseline

Select one public site with multilingual narrative content, media and meaningful
edges. Record anonymous HTTP fixtures for existing search/export, `/id` redirects,
JSON-LD, robots and sitemap behavior. Inventory custom controllers and templates.
Choose a fixed dataset and representative questions; record current HTML-only
accuracy, citations, request counts, response bytes, elapsed time and token use
where measurable. Resolve the public category defaults and indexing policy here.

**Done when:** examples and response schemas match real responses, public and
private fixtures are distinguished, and baseline tasks are reproducible.

### 2. Ship discovery and documented existing reads

Build the module, API help, OpenAPI, Linkset catalog, `llms.txt`, HTML links and
header observer. Integrate the visible fragment on the pilot site. Describe
existing search/export faithfully, including their indexing headers and limits;
do not claim the later facade already exists.

**Done when:** from a normal article, a fresh anonymous client can follow links
to working search and full retrieval examples. All advertised URLs resolve,
OpenAPI and Linkset validate, and existing WebSub links survive. Disabling the
module removes its advertisements. Sites without SEO still get discovery.

### 3. Add concise exploration and readable representations

Implement the bounded read facade, schema and relationship descriptions,
Markdown composition. Additional JSON-LD and Turtle work waits for the SPARQL
merge and is tracked separately below. Add language selection,
continuation links, caching and clear errors. Reuse existing serializers and
search/ACL logic; retain full-export compatibility.

**Done when:** clients can browse, search, retrieve and follow relationships with
correct citations; large graphs remain bounded; Markdown covers the pilot's
actual content types. Tests cover language fallback, hidden properties and
relations, unpublished/future/expired resources, deleted resources, malformed
queries, HEAD, conditional GET and cache invalidation after permission changes.
Verify that signed-in visits cannot populate a public cache with private data.

### 4. Measure independent discovery and refine

Run repeated trials with the same questions, data and client versions:

| Variant | Available discovery |
| --- | --- |
| A | Current HTML baseline |
| B | `llms.txt` only |
| C | HTML head and HTTP links |
| D | Combined discovery with OpenAPI |
| E | Combined discovery plus visible API-help/Markdown links |

For each, distinguish agents given only a starting article URL from agents given
the API documentation. Test both extracted-text browsers and HTTP-capable agents.
Record whether the client can see headers/head links, discovered the API, chose
it unaided, completed the task, followed the correct predicates, and cited the
right language/source. Compare cost and correctness as well as adoption; lack of
API use in a restricted browsing tool is not automatically an API defect.

Use tasks such as finding three related recent articles, listing upcoming events,
following a collection object's creator, and reading a page in another language.
Keep expected answers and trial traces. Ship broader defaults only after a pilot
shows useful exploration and no permission regressions. If discovery fails,
improve visible links and readable content before adding more protocols.

### 5. Add semantic representations after the SPARQL merge

Once the SPARQL implementation has merged, review its RDF model and new libraries
before designing additional JSON-LD and Turtle representations. Reuse that work
for serialization, mappings and content negotiation, and add the delivered
formats to discovery metadata. This dependency does not block discovery,
ordinary JSON APIs, graph navigation or Markdown delivery.

**Done when:** the merged libraries back the new representations, equivalent
JSON-LD and Turtle output describes the same public graph, and tests cover
permissions, language handling, media types and negotiation.

### 6. Optional extensions

Add read-only MCP tools over the same model operations when connected-client
demand exists. Evaluate bulk export/change feeds for large collections and richer
semantic search only against demonstrated workload needs. Neither should block
the initial public discovery release.

## Decisions for the first implementation PR

1. Confirm the module name, dedicated representation/facade paths and supported
   OpenAPI version; generate every published URL through dispatch.
2. Choose sensible public category defaults and the behavior of explicit site
   exclusions without adding a per-page AI opt-in requirement.
3. Set the pilot's language fallback, indexing and cache policy, including
   dependent content and timed publication changes. --> use configured default site language
4. Identify custom block renderers and semantic vocabulary descriptions needed
   to make the pilot's real content complete and navigable.
5. Select test clients and baseline questions before interpreting discovery
   results. Document capability restrictions alongside every trial.

This technote deliberately stays in `doc/technotes` as a development plan.
Implementation should add user-facing setup documentation under `doc/guides`
and model/module reference documentation with the delivered code.
