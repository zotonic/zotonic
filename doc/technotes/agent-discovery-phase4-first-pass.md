# Agent discovery: phase 4 first pass on zotonicwww2

Date: 10 November 2026. This is a small, directional pilot of the
[agent discovery plan](agent-discovery-and-access.md), not a statistical benchmark.

## Result

**An HTTP-capable agent independently discovered and used the APIs when its task
needed structured metadata. The recency task then exposed an API ordering defect.**
Three simpler research runs succeeded entirely through ordinary documentation
links. Two extracted-text runs did not discover the API; the starting page does
not expose the API-help/Markdown fragment in its visible body.

The useful conclusion is task-dependent adoption, not a general discovery rate.
Do not present the current `text` plus `sort=modified` behavior as verified correct.

## Setup and controls

All five agents started from the same ordinary resource URL:

`https://zotonicwww2.test:8443/en/docs/13301/catinclude`

Each was a fresh agent instance with no conversation history, using the current
Codex model without an override. The exact model/build was not exposed by the
trial tooling. Agents were instructed to use only a logged, read-only HTTP helper,
remain on this site, and make at most eight requests. They could not inspect the
repository, other agents, the helper source, or other runs' files. No prompt
mentioned APIs, OpenAPI, `llms.txt`, or a preferred navigation strategy. Prompts
specified the content task, starting URL, access capabilities and budget.

- **HTTP mode:** response headers and bodies were visible; agents could parse
  their own downloaded responses.
- **Extracted-text mode:** HTML head, scripts, styles and SVG were removed;
  visible body text, anchor destinations and image alt text remained. Headers
  and raw HTML files were unavailable. Non-HTML documents remained readable.
- Both modes limited displayed responses to 80,000 characters. This affected the
  large sitemap in the extracted-text recency run. The extraction is a helper
  simulation, not a browser rendering or an actual third-party browsing product.
- The local site's self-signed TLS certificate was accepted by the helper.
  Initial sandbox network failures in the first three runs were retried with
  network access; they did not reach the site or consume the request budget.
- Site data, templates, discovery configuration and application code were not
  changed during the experiment. Only these technote artifacts were added.
  Parent/evaluator validation requests are separate from participant counts.

The page advertised catalog, OpenAPI, API-help, `llms.txt`, JSON export and Markdown
links in both the HTML head and HTTP Link header. The body contained useful
related-documentation links, but **no visible API-help or Markdown link**. Thus
this tests the current combined discovery setup without the visible-link variant;
it does not compare all planned A–E variants.

## Task 1: ordinary documentation research

Prompt:

> Starting at the supplied URL, find three OTHER documentation pages on this site
> about composing or reusing templates. Explain each mechanism in one sentence
> and cite its ordinary human page URL. Work in English.

| Run | Access | Requests | Body bytes received | API use | Answer |
| --- | --- | ---: | ---: | --- | --- |
| HTTP-A | Headers and bodies | 4 | 153,218 | None | Correct: compose, fragment, include |
| HTTP-B | Headers and bodies | 4 | 153,218 | None | Correct: compose, fragment, include |
| Text-A | Extracted text | 4 | 143,738 | None | Correct: compose, fragment, useblock |

All three followed the starting page's “Related documentation” links and read
three destination pages. Answers correctly distinguished composing with block
overrides, defining reusable fragments, including another template, and rendering
a fragment with a body/overrides. All citations were ordinary English page URLs
that the agent had fetched successfully. There were no invented pages.

The HTTP agents were exposed to discovery metadata but did not fetch it. Exposure
is not proof that they consciously noticed it. This task was easy to complete with
existing links, so non-use is not a usability failure and does not establish that
API discovery is ineffective.

## Task 2: recency and exact metadata

Prompt:

> Starting at the supplied URL, find the THREE most recently modified public pages
> on this site matching a search for 'template'. Give each title, exact last
> modification timestamp, and ordinary human page URL, newest first. If you cannot
> verify the ordering or timestamps, say so rather than guessing.

This task deliberately tests a potential benefit of structured access. It is not
an unbiased sample of all documentation questions.

| Run | Access | Requests | Body bytes received | API use | Answer |
| --- | --- | ---: | ---: | --- | --- |
| HTTP-C | Headers and bodies | 8 | 342,934 | Yes: 2 discovery documents, 5 search requests | Partial; correctly declined to assert global order |
| Text-B | Extracted text | 6 | 555,798 | None | Unable to verify; did not fabricate results |

### HTTP-C: discovery succeeded, ordering failed

Observed navigation:

1. Ordinary `catinclude` page.
2. `/llms.txt`, discovered from the page's metadata.
3. `/openapi.json`.
4. `/api/agent/search?text=template&sort=modified&language=en&page=1&pagelen=5`.
5. The same search with `pagelen=100`, then continuation pages 2, 3 and 4.

No investigator supplied these endpoint URLs to the participant. The agent noticed
that returned modification dates were not chronological, sorted its inspected
results locally, and reported the newest three among **400 unique inspected
matches**, explicitly stating that a remaining fifth page prevented verification
of the global top three. Its timestamps and human-page citations matched the
returned resources. The qualified list was not the global top three.

Independent evaluator checks reproduced the defect with a three-item request:

| API result position | Resource | Returned modified timestamp |
| --- | --- | --- |
| 1 | All dispatch rules (13087) | 2025-09-05T07:32:35+00:00 |
| 2 | Templates (13290) | 2022-06-14T07:36:02+00:00 |
| 3 | mod_authentication dispatch rules (13564) | 2026-09-30T08:55:46+00:00 |

A separate public model query using `asort=-modified` and `zsort=id` returned:

| Expected position | Resource | Modified timestamp (UTC) |
| --- | --- | --- |
| 1 | mod_development (12900) | 2026-10-20T21:29:36Z |
| 2 | Set up a local development environment (13595) | 2026-10-07T10:45:10Z |
| 3 | Choose a development tool (14809) | 2026-10-07T10:36:56Z |

Code inspection supports the explanation: the agent model supplies its chosen
order through the search `sort` term, while full-text search also adds relevance
ordering. Search provides `asort` for ordering ahead of ordinary sort terms.
See [m_agent_content](../../apps/zotonic_mod_agent_discovery/src/models/m_agent_content.erl),
[search_query](../../apps/zotonic_mod_search/src/support/search_query.erl) and
[z_search_terms](../../apps/zotonic_core/src/support/z_search_terms.erl).
This is an API contract problem, not a failure to discover or call the API.
It also shows why argument-shape tests and successful HTTP responses alone were
insufficient to validate the phase 3 sorting contract.

### Text-B: no visible route to the API

The agent tried the HTML search with `text=template` and `q=template`, followed the
Template topic, and inspected the sitemap index and its URL set. The search page
did not yield usable results through this text-only interface, and the topic/body
text did not provide modification timestamps. Sitemap entries supplied timestamps,
but the displayed URL set was truncated and did not establish the full-text match
set. The agent stopped after six requests and explicitly reported the limitation.

This does not prove that an unrestricted browser or another agent would fail.
It identifies an access-mode gap in the current pilot's visible navigation.

## Measures and limits

The [machine-readable trace](agent-discovery-phase4-first-pass.json) records task
prompts, exact requested/final URLs, statuses, payload sizes, fetch durations,
response hashes, answer summaries and the independent recency check. Every
successful participant fetch returned HTTP 200. No credentials were used.

Summed fetch durations were 0.167 s, 0.132 s, 0.130 s, 0.493 s and 0.919 s for the
five runs above. These are local network/server timings, not total agent elapsed
time or production performance estimates. Bytes are complete received response
bodies before display extraction/truncation; they exclude headers and failed
sandbox attempts. Generation token counts and cost were not measured.

Two similar HTTP runs selected identical paths; they are instances of the same
model, not independent providers. The evaluator used the same assistant family.
There were only two tasks, no randomized variant order, no HTML-only ablation,
and no repeated recency trials. We therefore make no statistical adoption,
accuracy, latency or token-saving claim.

## Follow-up

The sorting defect was subsequently fixed and the recency trial repeated with
fresh agents. See [repeat results](agent-discovery-phase4-recency-repeat.md).
The observations below remain the historical pre-fix record.

## Subsequent policy decision

The user ruled out discovery links in human-facing pages after this trial.
The visible-fragment recommendation below is historical and is **withdrawn**.
No visible or hidden body links will be added. Header/head discovery and dedicated
documents remain; limited discovery in text-only clients is an accepted constraint.

## Next small iteration (recommendations at the time of the trial)

1. **Correct and regression-test explicit ordering with nonempty full-text
   queries.** Test newest, oldest, modified and ID order, including equal-key
   tie-breaks and pagination. Verify returned values, not just generated arguments.
2. **Expose the existing `_agent_discovery_link.tpl` fragment in the pilot's
   visible resource layout.** Repeat the extracted-text recency task with fresh
   agents, keeping question and budget unchanged. This separates a discovery fix
   from a tool-capability limitation.
3. Repeat the HTTP recency task after the ordering fix. Compare requests and
   bytes against this trace, while keeping the snapshot and expected answers fixed.
4. Only then broaden to the planned discovery variants, other agent/browser
   implementations, relationship tasks and repeated trials.

These fixes were identified and recorded, not applied during this measurement
pass. Survey blocks remain excluded from Markdown. Additional JSON-LD and Turtle
support remains deferred until the SPARQL merge.
