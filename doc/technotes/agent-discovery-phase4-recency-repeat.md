# Agent discovery: recency repeat after the sorting fix

Date: 10 November 2026. Follow-up to the
[first phase 4 pass](agent-discovery-phase4-first-pass.md), on local `zotonicwww2`.

## Changes before the repeat

The facade's validated order now uses Zotonic search's `asort` term, including
the resource-ID tie-breaker. This places it before the relevance ordering added
by full-text search. The earlier ordinary `sort` term allowed relevance to take
precedence. Public query parameters and response fields are unchanged.

Discovery remains outside human-facing page bodies and footers. The optional
`_agent_discovery_link.tpl` template was removed, and the plan, module description
and guide no longer recommend visible links. No hidden body links replace it.
HTTP headers, HTML head links, `llms.txt`, the catalog, OpenAPI, API help and
representation endpoints remain. The pilot had not included the fragment, so
its ordinary page navigation was unchanged for this comparison.

## Verification before interpreting the trial

- All 13 content tests passed, including a new regression that combines the
  facade's actual ordering term with an earlier relevance term through the real
  SQL-term combiner. It verifies primary sort **and ID tie-breaker** before rank
  for id, newest, oldest and modified.
- On the live site, each of the four modes was checked against actual resource
  values for 100 results matching `template`.
- Pagination was consistent for each mode: two pages of three equaled one page
  of six. The newest/oldest samples included 80/42 repeated publication-date
  keys respectively, exercising ID ordering within ties.
- Modification timestamps in PostgreSQL retain fractional seconds; current public
  timestamp serialization exposes seconds. Independent checks used full database
  precision, so apparently equal displayed timestamps were not mistaken for exact
  database ties. There were no exact modified-time ties in that 100-result sample.
- The corrected public recency query returned the same three expected resources
  established independently in the first pass.

No production data was edited and no temporary content or authentication session
was needed for these checks. The module remained enabled on the local site.

## Repeat design

Two fresh agents, with no previous conversation or trial results, received exactly
the same recency question, starting ordinary page and eight-request budget as
before:

`https://zotonicwww2.test:8443/en/docs/13301/catinclude`

> Find the THREE most recently modified public pages on this site matching a
> search for 'template'. Give each title, exact last modification timestamp, and
> ordinary human page URL, newest first. If you cannot verify the ordering or
> timestamps, say so rather than guessing.

The inherited Codex model, HTTP helper and extracted-text rules were unchanged.
Prompts did not mention APIs, the sorting fix or discovery documents. The exact
model/build was not available from the tooling. This is one repeat per access
mode, not a statistically controlled benchmark.

## Results

| Run | Requests | Body bytes received | Discovery/API use | Outcome |
| --- | ---: | ---: | --- | --- |
| HTTP, before fix | 8 | 342,934 | Unaided; 5 search calls | Inspected 400 matches; could not establish global top three |
| HTTP, after fix | 7 | 228,240 | Unaided; 1 search call | Correct top three, then independently checked each ordinary page |
| Extracted text, before fix | 6 | 555,798 | No API use | Could not verify recency |
| Extracted text, after fix | 7 | 281,312 | No API use | Could not verify recency; explicitly declined to guess |

The HTTP agent followed:

1. Ordinary `catinclude` page.
2. Its advertised `/llms.txt` link.
3. `/openapi.json`.
4. `/api/agent/search?text=template&sort=modified&language=en&page=1&pagelen=3`.
5. Each of the three returned ordinary pages, checking titles, canonical links
   and existing page modification metadata.

It returned the correct newest-first answer:

| Title | Modification timestamp (UTC) | Human page |
| --- | --- | --- |
| mod_development | 2026-10-20T21:29:36Z | `/en/docs/12900/mod_development` |
| Set up a local development environment | 2026-10-07T10:45:10Z | `/en/docs/13595/set-up-a-local-development-environment` |
| Choose a development tool | 2026-10-07T10:36:56Z | `/en/docs/14809/choose-a-development-tool` |

The answer was available after four requests; the agent spent three more checking
sources. It used existing page JSON-LD during verification. No new JSON-LD or
Turtle support was introduced.

The extracted-text agent found the working HTML search parameter `qs=template`,
compared ordinary results with an attempted `sort=-modified`, followed the
Template topic and visited a template model page. The body text exposed neither
modification timestamps nor a documented recency-sort control, and “Loading more
results…” did not supply an ordinary pagination link. It did not discover the
API and did not fabricate timestamps or a global ordering.

That result is an **accepted access-mode limitation under the no-body-links
policy**, not a reason to add visible or hidden links for agents. It also differs
in navigation from the earlier text run, illustrating normal agent variability.

## Evidence and next steps

The [repeat trace](agent-discovery-phase4-recency-repeat.json) records all 14
participant requests, response hashes, statuses, sizes, timings, final findings
and validation summary. All participant requests returned HTTP 200. Summed fetch
times were 0.5128 s (HTTP) and 0.7699 s (text); these exclude model reasoning and
are not end-to-end latency measures. Body-byte totals are before extraction or
display truncation. Generation tokens and cost were not measured.

The repeat supports two narrow conclusions: header/head discovery can work
without prompting when structured metadata is useful, and the explicit-sort fix
removes the observed recency-task blocker. It does not establish broad adoption
rates or general cost savings from two single-model runs.

Future phase 4 trials should retain the no-body-links policy, vary tasks and
agent implementations, and distinguish HTTP-capable agents from clients that
hide discovery metadata. Survey blocks remain excluded from Markdown. Additional
JSON-LD and Turtle remain deferred until the SPARQL merge.
