# Agent discovery on zotonic.com

Anonymous production smoke test, 17 November 2026. The module is enabled on
[zotonic.com](https://zotonic.com/). This checks the deployed HTTP behavior, not
independent agent adoption or a load benchmark. No site data or configuration was
changed. Requests used normal certificate verification and no credentials.

## Working behavior

The test made 42 direct HTTP requests, including the homepage, small API reads,
HEAD and conditional requests. Redirects can add network transactions.

| Area | Observed result |
| --- | --- |
| Ordinary-page discovery | Homepage and [catinclude](https://zotonic.com/docs/1325/catinclude) advertise catalog, API descriptions, export and Markdown through HTTP Link headers. WebSub links remain present. The article's HTML head advertises Markdown; no API-help/Markdown links were injected into its body. |
| Discovery documents | `llms.txt`, OpenAPI, API help and the well-known catalog return 200. OpenAPI lists the six implemented JSON operations. The catalog is JSON Linkset with the RFC 9727 profile media type. JSON was parsed; this was not a full standards-validator run. |
| Content | The catinclude resource (1325) returns complete editorial content: 2,334 Markdown characters, with `content_complete=true`. Dedicated Markdown includes API access links; the JSON editorial field does not. |
| Negotiation and caching | `/id/1325` with `Accept: text/markdown` resolves to the same Markdown. HEAD has no body. Repeating JSON and Markdown GETs with their ETags returns 304. |
| Search | `catinclude` search finds the resource. For `template`, two pages of three equal one page of six in all four sort modes. ID and modification ordering were checked directly; publication ordering was not independently checked against database values. |
| Relationships | Outgoing `refers` returns include and all_catinclude. Incoming references span multiple pages without overlap in the sample, preserving direction, predicate and page size in continuation URLs. |
| Keyword traversal | Following catinclude's `subject` link to Reference (2555), then incoming `subject` relationships, returns two bounded pages of three candidates. This is a prefix check, not an exhaustive or measured high-degree traversal. |
| Schema | Public category and predicate pages return data with parent/category-constraint descriptions and continuation links. |
| Errors | Excessive page size, unknown or repeated query parameters and traversal beyond the candidate bound return uncached 400s. A nonexistent resource returns an uncached 404. |
| Crawler entry points | robots.txt allows ordinary public paths while excluding admin/language-switch paths. sitemap.xml returns a sitemap index. |

## Configuration and client limits

Only English is enabled for frontend use, confirmed through the public
`/api/model/translation/get/enabled_language_codes` read. A German resource
request returns 400. The initial test expected fallback, but that expectation
was inappropriate here: the facade validates the requested language against
site-enabled languages before selecting resource fallback. Multilingual fallback
therefore remains untested on this production configuration.

The hosted web reader fetched the homepage, but reported inaccessible URLs for
API help, llms.txt, OpenAPI and Markdown, and a cache miss for the ordinary
catinclude page. Direct HTTP requests to those URLs succeeded. This is a client
compatibility observation, not evidence of an API outage or a diagnosed cause.
It also does not show whether a fresh agent would independently choose the APIs.

Discovery documents and content representations currently return
`X-Robots-Tag: noindex,nofollow`. robots.txt does not disallow those URLs. Do not
attribute the hosted reader's failures to that header without a controlled test.
The site's llms.txt description is empty (`>`); setting
`mod_agent_discovery.description` would provide useful site-specific guidance.

## Follow-up

- Supply a short public site description if desired.
- Investigate hosted URL-reader behavior separately from direct HTTP, including
  the ordinary-page cache miss. Keep discovery out of HTML bodies and footers.
- Repeat language/fallback checks if additional frontend languages are enabled.
- Retain the earlier local pilot's ACL, startup and boundary tests: this read-only
  production run did not create private fixtures, change permissions, restart the
  site, or verify survey-block omission against a production survey fixture.

Additional JSON-LD/Turtle work remains outside this test's scope. The request
captures and small test scripts were saved locally under
`/tmp/zotonic-live-discovery/`; they are diagnostic artifacts, not a durable
snapshot of the site's mutable content.
