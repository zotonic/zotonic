# Phase 4: hidden discovery links in actual text readers

Date: 11 November 2026. Repository revision: `d09962f64`.
Continues the [agent discovery plan](agent-discovery-and-access.md).

## Decision

**Do not add hidden discovery links to the shared end-of-body include on the
basis of this experiment.** Real readers behave differently, and main-content
selection can discard this position even when its links are visible. CSS-only
hiding sometimes survives inside an article, but this is not a portable discovery
mechanism and says nothing about search-engine acceptance.

No live pages, site resources, application templates, configuration or ACLs were
changed. No documentation import was triggered. Survey blocks remain excluded
from Markdown; additional JSON-LD and Turtle remain deferred until the SPARQL
merge. This experiment does not authorize a production body-link change.

## Method

Unlike the earlier agent trials' custom text-extraction simulation, this trial ran
published extraction libraries and **Jina Reader's actual hosted service**.
Only synthetic HTML was sent to Jina. No local-site content, cookies, credentials
or private URLs were uploaded, and no public test page was published.

The fixture is a four-paragraph archive article with a title, a normal footer,
and one candidate link labelled “Machine-readable API documentation”. Its absolute
destination is `https://example.com/reader-trial/api-help`. The same nine treatments
were placed at the end of the article, after the main content and footer just
before the body closes, and inside the footer: 27 cases. A head-only `link`
element and an absent-link negative control bring the total to 29.

Each case was evaluated by seven configurations (203 extraction results):

| Reader/configuration | Version and settings |
| --- | --- |
| Jina hosted Reader | `POST https://r.jina.ai/`, raw `html` and synthetic base `url`, JSON response, no preset or authentication; deployed build not reported |
| Mozilla Readability → Turndown | Readability 0.6.0, jsdom 31.0.0, default article extraction; Turndown 7.2.5 converts extracted HTML to Markdown |
| Trafilatura | 2.3.1, Markdown output, `include_links=True` |
| Trafilatura recall | Same, plus `favor_recall=True` |
| Turndown body | 7.2.5, whole body, no article selection |
| html2text body | 2025.4.15, whole body, `body_width=0` to avoid splitting URLs |
| markdownify body | 1.2.3, whole body, defaults; BeautifulSoup 4.15.0 |

The last three are general converters, not browser reader modes. The jsdom
pipeline parses HTML without rendering or executing scripts. The hosted test
uses Jina's documented raw-HTML input, not its remote-URL browser-fetch path.
These distinctions matter when CSS, rendering and boilerplate filtering differ.
See [Mozilla's API documentation](https://github.com/mozilla/readability#readme)
and [Jina's raw-HTML input documentation](https://github.com/jina-ai/reader/blob/main/cookbooks.md#raw-html).

Retention means that the complete destination URL occurs in the returned text or
Markdown. Label retention is recorded separately. All results retained the
article's opening sentence; all negative controls lacked both the label and URL.
All 29 recorded hosted results returned HTTP 200. An initial successful visible
control preflight is excluded from the matrix. One intermediate negative-control
response had null `data` and exposed a harness error; its raw response was not
saved. The parser was corrected and that case rerun successfully. It is not
counted as evidence of link removal or as a recorded service error status.

## Results inside the article

“Yes” means the destination survived. Trafilatura's two configurations had the
same results. The three whole-body converters agreed except for `template`.

| Treatment | Jina hosted raw HTML | Readability → Turndown | Trafilatura, both modes | Whole-body converters |
| --- | --- | --- | --- | --- |
| Visible paragraph | Yes | Yes | No | All yes |
| HTML `hidden` attribute | No | No | No | All yes |
| Inline `display:none` | No | No | No | All yes |
| CSS class with `display:none` | Yes | Yes | No | All yes |
| Visually hidden CSS class | Yes | Yes | No | All yes |
| Visually hidden class plus `aria-hidden="true"` | No | No | No | All yes |
| HTML comment | No | No | No | All no |
| Inert `template` element | Yes | No | Yes | html2text/markdownify yes; Turndown no |
| Closed `details` element | Yes | Yes | No | All yes |

The visually hidden class uses absolute positioning, a one-pixel box, negative
margin, clipping and hidden overflow. It does not use `display:none`.

Readability 0.6.0 checks inline display/visibility styles, `hidden` and
`aria-hidden`; it does not resolve stylesheet rules in this jsdom configuration.
This explains why equivalent inline and class-based `display:none` produce
different results. Jina's observed pattern is similar, but this experiment does
not identify the hosted service's internal implementation or promise identical
behavior for a fetched, rendered page.

Trafilatura discarded even the visible standalone link paragraph, while retaining
the article prose. Consequently its losses cannot all be attributed to hiding.
The surprising `template` retention is a parser/extraction result, not a proposed
use of inert markup for discovery. No treatment survived every tested pipeline.

## End-of-body and footer placement

**Jina, Readability and both Trafilatura configurations discarded every candidate
at both positions, including the visible control.** Moving a link outside the
article can therefore matter more than how it is hidden.

Turndown, html2text and markdownify retained visible and hidden anchor paragraphs
at both positions. All discarded comments. html2text and markdownify retained
`template` content; Turndown did not. All retained the closed-details link.

The head-only link did not appear in any returned body text. This does not test
HTTP Link headers, client metadata APIs, or whether an agent with raw-HTML access
could discover it. The body converters intentionally receive only the body.

Zotonicwww2's `base.tpl` calls `{% all include "_html_body.tpl" %}` near the body
end. The body-end fixture models that position, but is not a complete copy of the
site's layout. These measurements support rejecting a universal-retention claim;
they are not a numerical estimate of discovery success on all Zotonic pages.

## Browser appearance check

An isolated headless Chrome 157.0.8103.39 process rendered all 29 fixtures at
1200 × 900 with network requests blocked. It used a temporary browser context,
not the user's browser profile. Removing the `hidden`, inline/class display-none,
and visually hidden candidate blocks changed body height by zero pixels. The
visible paragraph changed it by 34 pixels at the article/body-end positions.
The CSS computed styles and layout measurements are saved in the results.

Chrome's `innerText` excluded display-none/hidden links but included clipped,
visually hidden links, including the `aria-hidden` variant. Thus `innerText` is
not the same thing as visible pixels or the accessibility tree. This is a browser
diagnostic, not an eighth reader or an accessibility audit. Closed `details`
kept its summary visible, so it does not satisfy the no-visible-content constraint.
Neither comments nor inert templates rendered their candidate text.

## SEO interpretation and next steps

Reader retention is not a test of Google or Bing classification. Google's policy
distinguishes manipulative hidden text/links from legitimate hidden content,
including content intended to improve screen-reader users' experience. It does
not provide a general exemption for bot-only API links.
[Google's hidden-text policy](https://developers.google.com/search/docs/essentials/spam-policies#hidden-text-and-link-abuse)

There is no demonstrated approach here that simultaneously guarantees retention,
zero visible footprint and search-engine acceptance. Keep header/head discovery
and dedicated documents. Links in Zotonic's own Markdown response remain an
option for clients that already request it; that alone would not solve initial
discovery from ordinary HTML.

The useful next experiment, if pursued, is the same controlled matrix through
remote-URL extraction on a separately hosted disposable fixture, with explicit
reader/rendering configurations. Firecrawl, actual chat-product fetch tools and
search-engine indexing were not tested. Do not substitute this raw-HTML Jina
result or the local Readability library result for those measurements. No new
agent adoption trial was run: retaining a URL does not prove an agent will use it.

## Evidence and reproduction

[Machine-readable results](agent-discovery-phase4-text-readers.json) contain
per-case URL/label retention, hosted status and elapsed time, version metadata,
fixture hashes, browser diagnostics, and exact extracted outputs deduplicated by
SHA-256. The synthetic HTML is reproducible with
[fixtures.py](agent-discovery-reader-trial/fixtures.py). Supporting scripts are
kept in [agent-discovery-reader-trial](agent-discovery-reader-trial/).
No site build or import is needed.

From the repository root, use a disposable directory (the original was
`/tmp/zotonic-reader-trial`). Install the versions above into its Python virtual
environment and npm prefix, plus `playwright-core` for the browser diagnostic;
the complete observed dependency versions are in the JSON. Then run:

```sh
python3 doc/technotes/agent-discovery-reader-trial/fixtures.py /tmp/zotonic-reader-trial/fixtures.json
NODE_PATH=/tmp/zotonic-reader-trial/node_modules node doc/technotes/agent-discovery-reader-trial/readers.cjs /tmp/zotonic-reader-trial/fixtures.json /tmp/zotonic-reader-trial/js-results.json
/tmp/zotonic-reader-trial/venv/bin/python doc/technotes/agent-discovery-reader-trial/readers.py /tmp/zotonic-reader-trial/fixtures.json /tmp/zotonic-reader-trial/py-results.json
python3 doc/technotes/agent-discovery-reader-trial/hosted.py /tmp/zotonic-reader-trial/fixtures.json /tmp/zotonic-reader-trial/jina-results.json
NODE_PATH=/tmp/zotonic-reader-trial/node_modules node doc/technotes/agent-discovery-reader-trial/browser.cjs /tmp/zotonic-reader-trial/fixtures.json /tmp/zotonic-reader-trial/browser-results.json
```

The hosted step sends synthetic data to Jina and can encounter service/rate-limit
changes. It saves each response before stopping on a non-200 result. The browser
script uses the macOS Chrome executable path; adjust that path on other systems.
To collect a fresh complete run, save `pip freeze` from the virtual environment
as `requirements.txt` and `npm ls --prefix /tmp/zotonic-reader-trial --all --json`
as `npm-versions.json` in that directory, then run `collect.py` with the directory
and desired output JSON path as arguments. The collector also supports the
original 28-case batch plus the separately rerun `jina-control.json`.
