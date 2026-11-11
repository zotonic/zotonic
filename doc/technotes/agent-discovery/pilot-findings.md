# Agent discovery: pilot findings and regression coverage

The initial pilot ran on local `zotonicwww2` on 10–11 November 2026 and closed on
11 November. It demonstrated useful discovery and bounded public retrieval on
one site. The [current design](agent-discovery-and-access.md) records the resulting
implementation and future work; the [guide](../../guides/agent-discovery.md)
describes its public contract.

This note consolidates the staged trial reports. Per-request dumps, transient
resource IDs, startup polling logs and superseded plans have been removed from
the documentation. The reusable text-reader experiment and edge query plans are
retained separately. The observations below are historical pilot results, not a
promise that the site's mutable dataset will produce the same answers today.

## What the agent trials established

Fresh isolated Codex agents used logged anonymous, same-site GET helpers. They
could not inspect the repository, helper source or other runs. The tooling did
not expose the exact model/build. Most runs allowed eight logical fetches;
multi-page tasks allowed twelve. Expected answers were established independently
of participant answers using public reads and ordinary resource/edge checks.

HTTP mode exposed headers and response bodies. Text mode removed head metadata,
scripts and styles but retained body text and link destinations. Markdown and
JSON remained readable. This helper was a controlled access mode, not a test of
a particular chat product's browsing integration.

| Task and starting point | Observation | Interpretation |
| --- | --- | --- |
| Ordinary-page documentation research | Three runs found and cited suitable pages through existing article links without using APIs | API adoption depends on the task; ordinary navigation may already suffice |
| Ordinary-page recency query | After the sorting fix, an HTTP agent discovered `llms.txt`, OpenAPI and search, then verified the correct top three on human pages; the text-only repeat could not verify recency | Headers/head metadata can enable unaided discovery; body-only clients can miss it |
| Ordinary-page references and languages | Both modes correctly distinguished cross-reference direction, Dutch content and English fallback for a German URL; one HTTP agent used the existing JSON export | Correct answers do not imply adoption of the concise facade |
| Markdown recency and language | Two recency agents used discovery document links and one search call each; a third used the resource API to verify Dutch content and German fallback | The API access section is useful once Markdown is supplied |
| Markdown relationships | Three agents used the Related resources link and API documentation; two verified both directions and one enumerated all fifteen public referring pages | Combined Markdown links support direction-sensitive navigation; the API section was not the only discovery path |
| High-degree keyword from Markdown | Two agents retrieved the correct first sixty resources over three 25-candidate pages from a keyword with 861 candidates | Bounded multi-page navigation worked for the requested prefix, not an exhaustive traversal |
| Empty filtered page, API URL supplied | An agent followed continuation and returned the next three listed resources | Empty items must not be interpreted as exhaustion; this was a continuation test, not discovery |

Markdown API-access trials had no paired section-free control, and their final
reporting prompt mentioned structured interfaces. The later relationship trials
removed that cue. High-degree tasks explicitly constrained page size and requested
subset. These small trials do not establish a causal adoption improvement,
universal compatibility or statistical success rate. Local response bytes and
fetch latency were observed, but token cost and end-to-end reasoning time were
not measured.

## Decisions and defects that matter for future changes

### Explicit sorting must precede full-text relevance

The first recency trial found the API but could not establish the globally newest
matches: ordinary `sort` allowed relevance ranking to take precedence. The facade
now uses `asort`, including the resource-ID tie-breaker. Verification covered all
four sort modes over 100 matches and checked that two pages of three matched one
page of six. Tests exercise the actual SQL-term combiner with a relevance term.

Displayed modification timestamps have second precision; database timestamps can
include fractional seconds. Equal displayed timestamps are not necessarily exact
database ties. Expected ordering must use database precision.

### Hidden HTML is not a portable discovery mechanism

The [text-reader experiment](text-reader-discovery.md) tested 29 synthetic cases
across seven reader configurations. Article selection discarded end-of-body and
footer links even when visible. Some class-hidden links survived within article
content, but no treatment survived every pipeline. This does not establish any
search-engine acceptance of hidden links. Keep header/head discovery and the
concise Markdown section; do not add visible or hidden HTML-body discovery.

### High-degree relationships need indexed candidate selection

Limiting a result after loading all edges is insufficient. Even SQL LIMIT can
still sort a complete adjacency list if the ordering does not match an index.
The [bounded edge design](bounded-edge-pages.md) uses predicate/endpoint ordering
and one lookahead row. A 100,000-edge synthetic comparison removed the full-set
sort; deep OFFSET remained measurably more expensive. Existing sequence-oriented
full-list accessors were not changed.

### Template errors must not look like complete content

Cold startup exposed a successful Markdown response containing metadata and
links without the expected article body. The tolerant template renderer had
converted an error into empty output. The agent path now uses an error-preserving
renderer and returns HTTP 503 with no-store and no ETag when rendering fails.
Legitimate empty templates still succeed, and normal HTML keeps its established
rendering behavior.

A pre-fix restart produced twenty incomplete HTTP 200 responses. Two fixed
restarts produced none: each reached twenty consecutive complete responses with
the same body hash as the complete pre-fix representation. All 255 observed
rendering-failure responses across those fixed runs were uncached 503s without
ETags. Connection failures and early dispatch 400s still occurred before the
site was ready; this is not a guarantee of uniform 503 behavior during startup.

## Acceptance coverage at closure

The repository compiled and 33 relevant tests passed: sixteen content, nine
discovery, five core edge-page and three template-result tests. These targeted
checks do not replace the repository's full CI matrix for merging later changes.

| Area | Coverage |
| --- | --- |
| Discovery and HTTP contract | Working advertised routes, search and schema, both relationship directions, Markdown negotiation, HEAD, conditional GET and malformed requests |
| Content and language | Requested/actual language and fallback, category overrides, headings/code/tables/image alt text, one-level page references, omission markers and skipping every `survey_...` block |
| Public access | Hidden properties/endpoints, private resources, unpublished/future/expired/deleted resources, and anonymous behavior for privileged callers |
| Cache revalidation | Content updates change ETags; moving a resource to a private group defeats stale conditional reads, including `If-None-Match: *` |
| Pagination | Consecutive empty windows, a final empty page, continuation query preservation, exact exhaustion at 10,000 candidates, truncation with lookahead and rejection beyond the bound |
| Permission change between pages | An existing next-page URL omits a newly private endpoint and gets a new ETag; traversal continues to later public endpoints |
| Rendering and budgets | Template failure versus legitimate empty output, size bounds, worker timeout and termination |

Earlier session tests compared anonymous and signed-in administrator HTTP reads:
public bodies were identical and newly private resources returned 404 to both,
while the administrator could still read them through the ordinary model API.
The closure repeat additionally checked administrator-context model isolation;
it was not a new authenticated HTTP session trial.

The four final closure checks—cold start, pagination boundaries, permission
changes between pages and documentation consistency—passed. Fourteen live HTTP
boundary/permission checks supplemented regression tests. All 216 resources
created for these final checks and their edges were removed. No existing resource
permissions, ACL rules or human-facing templates were changed.

## Limits and useful next experiments

The pilot is complete within its initial scope. Offset traversal can shift under
concurrent edits and is not a snapshot. Rendering startup tests cover the selected
representation, not every custom template. Custom site block renderers still need
site-specific verification. Text-only clients starting at ordinary HTML may miss
the APIs entirely.

Useful follow-ups are actual hosted URL readers, additional providers, partial
translations and more languages, controlled discovery variants, and load tests
on larger sites. Preserve the distinction between endpoint discovery, API
usability and answer correctness in each experiment. Additional JSON-LD/Turtle
work remains deferred until the SPARQL merge.
