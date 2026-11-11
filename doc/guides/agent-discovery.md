---
keywords:
  - how_to_guide
  - backend_developer
  - http
module: mod_agent_discovery
---

# Help agents discover a site's public APIs

Enable `mod_agent_discovery` on a public publishing site, using the Modules
page in the admin. It depends on `mod_base` and `mod_search`; it does not require
`mod_seo`. Activation adds these routes:

| URL | Purpose |
| --- | --- |
| `/api-help` | Readable API explanation with a clickable public search example |
| `/openapi.json` | OpenAPI 3.1.0 description of concise reads and compatibility search/export |
| `/.well-known/api-catalog` | Linkset catalog pointing to the public API and its descriptions |
| `/llms.txt` | Concise site guidance and links for agents |

Set `mod_agent_discovery.description` to an optional public description of the
site's information. The site title comes from the existing site configuration.
No API key is needed to read public content. The existing APIs continue to apply
the caller's permissions; this module does not grant access or change query limits.

## Link from pages

Sites using the normal head hook automatically get discovery links:

```django
{% all include "_html_head.tpl" %}
```

Keep discovery out of human-facing page bodies and footers. Do not add visible
API-help/Markdown navigation or hidden body links. Agents can discover the APIs
through HTTP headers, the HTML head and dedicated discovery documents. Clients
that only expose body text may not discover them; that is an accepted constraint.

Override `agent_api_help.tpl` to customize the explanation for the site.
Its `discovery` variable contains public metadata and documented operations.
The sitemap is linked when `mod_seo_sitemap` is active.

Standard resource responses receive HTTP discovery links as well. Custom
controllers must call `z_context:set_resource_headers/2` to receive them.
Existing Link values, including WebSub advertisements, are preserved when the
discovery observer runs. Later custom observers must also merge Link values.

JSON export and Markdown alternates are advertised only when the resource is visible to an
anonymous visitor and is not excluded by site, resource or category noindex
settings. Visibility is checked even when the visitor is an administrator.
These exclusions affect discovery, not the permissions of existing API routes.

## Verify the site

Visit an ordinary public resource while signed out. Check its HTML links and
HTTP `Link` header, follow the API-help link, run the search example, and use an
ID from the result to retrieve a full export. Check the export's language-specific
`page_url` map when citing it. Compare a private and a noindex resource to confirm
that no JSON alternate is advertised for either.

Check site-specific routes and proxy rules if a discovery URL is unavailable.
The module generates URLs through dispatch and uses anonymous metadata. Discovery
documents currently use no-cache headers; existing APIs keep their cache and
`X-Robots-Tag: noindex,nofollow` behavior. No robots.txt or CORS settings are changed.
Do not infer reuse licenses from public accessibility; retain the site's stated
content and media attribution and licensing terms.

## Read concise public content

The following routes always use anonymous permissions, including for signed-in
administrators. JSON uses the `status`/`result` envelope.

| URL | Purpose |
| --- | --- |
| `/api/agent/search` | Browse or full-text search localized summaries |
| `/api/agent/resource/{id}` | Read localized Markdown with citation and media links |
| `/api/agent/related/{id}` | Follow one hop of outgoing or incoming relationships |
| `/api/agent/schema` | Discover category or predicate resources |
| `/agent/resource/{id}/markdown` | Read a resource as `text/markdown` |

Request `Accept: text/markdown` on `/id/{id}` to negotiate the Markdown URL.
Markdown alternates are advertised in HTML head and HTTP Link metadata on public
indexable pages.

All reads accept `language`, defaulting to the configured site language. Resources
report `requested_language`, actual `language`, `available_languages` and
`is_fallback`. Missing requested translations fall back to the site language,
then the resource's first language. The human `page_url` is the citation URL;
`uri` is the entity identifier.

Search accepts `text` (up to 512 bytes) and `cat` (category name or ID).
`sort` accepts `id` (resource ID ascending, the default), `newest` (publication
start descending), `oldest` (publication start ascending) or `modified`
(modification time descending). All orders use resource ID ascending as a stable
tie-breaker. `publication_after` and `publication_before`
accept valid `YYYY-MM-DD` dates and filter publication start inclusively in UTC.
The before date includes the whole day. Reversed ranges are rejected.
Summaries include the resource's UTC `modified` timestamp.

Relationships accept `direction=in` or `direction=out` (default) and an optional
`predicate` ID or name. Unknown or inaccessible predicates return 404.
Schema accepts `kind=category` (default) or `kind=predicate`. Categories include
their visible parent. Predicates include up to 100 candidate subject/object
category constraints, filtered by public visibility and noindex. These describe
public vocabulary, not validation or access rules; hidden constraints are never
exported. `constraints_truncated` marks the bound. Names, localized descriptions
and resource URIs identify the site's vocabulary without exposing database fields.
Paged reads accept `page` and `pagelen`: default 20, maximum 100 per page,
with `page * pagelen <= 10000`. Unknown, duplicate and malformed query parameters
are rejected. Queries have a five-second execution budget.

Follow `next_url` until `next` is false. These are **candidate pages**: public
visibility and site/resource/category noindex filtering can leave a page empty
while later pages still contain results. No totals are exposed. `limit_reached`
is true if exploration stops at the 10,000-candidate limit; refine the search.
Direct reads allow public noindex content. Missing, private and unpublished
resources all return 404. The facade never returns arbitrary resource properties.

Relationship pages fetch only `pagelen + 1` candidate edges from PostgreSQL,
through `m_edge:get_page/6`, before visibility/noindex filtering. An optional
predicate is applied in SQL. Ordering is by predicate ID and endpoint resource ID,
not editorial edge sequence. This matches the existing outgoing/incoming unique
edge indexes and avoids sorting all incoming edges of a popular keyword.
Offsets still cost work proportional to the skipped candidates; the existing
10,000-candidate traversal limit remains. Concurrent edge edits can shift pages.

Other callers can use `/api/model/edge/get/o/{id}/{predicate}`, `s`, `o_props`
or `s_props` with optional `offset` and `limit` query parameters. Without either,
these methods retain their full-list behavior. With either, offset defaults to
0 and limit to 20 (maximum 100, offset plus limit at most 10,000). They return the
existing edge tuples or property lists, in editorial `seq, id` order, with the
same caller ACL semantics and no continuation metadata. The predicate is required.
SQL selects the requested subset, but sequence ordering may require a sort.
The agent facade uses the separate Erlang `get_page/6` helper for indexed
endpoint ordering, filtered candidate pages and continuation metadata.

Markdown includes body HTML, text and header blocks, and one level of page-reference content. All `survey_...` blocks are intentionally skipped without marking the content
incomplete. Unsupported blocks and oversized
content are explicitly marked; check `content_complete` and follow the source
page for omitted material. Inline images use the existing Markdown conversion.
The Markdown response ends with a concise “API access” section linking to API
help, the OpenAPI specification and this resource's JSON endpoint in the content's
actual language. These links are not added to human-facing HTML or to the JSON
resource's editorial `markdown` field. They assist clients that already fetch
the Markdown representation; header/head discovery remains unchanged.
If an editorial template cannot render, the representation returns HTTP 503 with
`Cache-Control: no-store`, rather than successful empty content. This can occur
while the site's templates are becoming available after startup; retry later.
Legitimate empty content still renders successfully.
JSON includes a resource's own downloadable medium when available, exposing only
MIME type, dimensions, size and download URL. Preserve supplied licensing and
attribution. Page references include a source link, language and supplied licensing/attribution;
private or unpublished references are omitted. Nested non-survey blocks are not
expanded recursively and mark the representation incomplete. This prevents cycles
and large graphs from turning a resource read into an unbounded export.

Content responses use ETags and `Cache-Control: public, max-age=0, must-revalidate`.
Each conditional request rebuilds the representation after anonymous permission
and publication checks. No server-side representation cache is introduced.
Errors use no-store: 400 for invalid requests, 404 for unavailable resources,
413 for responses over 1 MiB, 503 for backend failures and 504 for timeouts.
Both GET and HEAD are supported; no credentialed CORS policy is added.

The initial pilot is complete. Implementation decisions, pilot findings and
future work are recorded in the
[design notes](../technotes/agent-discovery/agent-discovery-and-access.md).
Additional JSON-LD and Turtle work waits for the SPARQL merge. No MCP endpoint is
advertised.

## Customize Markdown composition

The model renders editorial HTML under anonymous permissions, then converts it
with Zotonic's faithful Markdown converter. Override `_agent_content.tpl` in the
site for site-wide composition, or `_agent_content.article.tpl` (and other category
suffixes) for category-specific composition. Variables are `id`, `body_html` and
`blocks_html`: the latter contains already-rendered block HTML, with all
`survey_...` blocks removed before template selection. Do not re-read `id.blocks`
or introduce cached privileged fragments. Custom fields can be read through
`m.rsc[id]`, which retains the anonymous property checks.

To support a custom editorial block, provide `_agent_block_<type>.tpl`; category
variants such as `_agent_block_example.article.tpl` work too. Block type names
must contain lowercase ASCII letters, digits or underscores, starting with a
letter. Variables are `id` (the owning resource), `blk` (the ACL-filtered block,
with localized `body` and `header`) and `reference` (for page blocks). Missing
renderers produce an explicit omission marker and `content_complete=false`.
Survey renderers are never selected, even if one exists in the site.

Custom renderers are responsible for representing their block's meaning faithfully.
Escape non-HTML custom values and attributes, preserve licensing, and link to the
source for interactive content. The rendering budget and size limits also apply
to overrides. Existing templates for normal HTML pages are not used implicitly.
