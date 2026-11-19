"""Generate isolated reader fixtures; never install them in a Zotonic site."""
import json
from pathlib import Path

BASE = "https://example.com/reader-trial/"
TARGET = BASE + "api-help"
LABEL = "Machine-readable API documentation"
LINK = f'<a href="{TARGET}">{LABEL}</a>'
CSS = """.concealed { display: none; }
.visually-hidden { position: absolute; width: 1px; height: 1px;
padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0);
white-space: nowrap; border: 0; }"""
PARAGRAPHS = [
    "A community archive describes buildings, photographs, and the people who contributed them. Each record has a stable address and a short description. Visitors can follow references between records to understand how the collection developed over time.",
    "The catalogue separates publication dates from later corrections. An editor can update a description when new evidence arrives, while retaining the original publication date. This distinction helps readers compare accounts without confusing an editorial correction with a newly published item.",
    "Translations belong to individual records. A visitor may request a language that is not available for a particular document. The archive then displays the original text and identifies its language, rather than pretending that the fallback is a translation.",
    "References have a direction. A photograph can refer to the building it depicts, and the building page can list the photographs that refer back to it. These relationships provide a useful route through a collection even when the records use different terminology.",
]
VARIANTS = {
    "visible": f"<p>{LINK}</p>",
    "hidden": f"<p hidden>{LINK}</p>",
    "display-none-inline": f'<p style="display:none">{LINK}</p>',
    "display-none-class": f'<p class="concealed">{LINK}</p>',
    "visually-hidden": f'<p class="visually-hidden">{LINK}</p>',
    "visually-hidden-aria": f'<p class="visually-hidden" aria-hidden="true">{LINK}</p>',
    "comment": f"<!-- {LINK} -->",
    "template": f"<template>{LINK}</template>",
    "details": f"<details><summary>Alternative formats</summary><p>{LINK}</p></details>",
}


def fixtures():
    article = '<article><h1>Exploring a community archive</h1>' + ''.join(
        f"<p>{paragraph}</p>" for paragraph in PARAGRAPHS)
    cases = []
    for placement in ("article", "body-end", "footer"):
        for variant, fragment in VARIANTS.items():
            content = article + (fragment if placement == "article" else "") + "</article>"
            content += "<footer>Community archive</footer>"
            if placement == "body-end":
                content += fragment
            elif placement == "footer":
                content = content.replace("Community archive</footer>", "Community archive" + fragment + "</footer>")
            cases.append(dict(id=f"{placement}/{variant}", placement=placement,
                              variant=variant, head="", content=content))
    cases.append(dict(id="head/link", placement="head", variant="link",
                      head=f'<link rel="service-doc" href="{TARGET}" title="{LABEL}" type="text/html">',
                      content=article + "</article>"))
    cases.append(dict(id="control/absent", placement="none", variant="absent",
                      head="", content=article + "</article>"))
    for case in cases:
        case["html"] = ('<!doctype html><html lang="en"><head><meta charset="utf-8">'
                        '<title>Exploring a community archive</title><style>' + CSS + '</style>'
                        + case.pop("head") + '</head><body><main>'
                        + case.pop("content") + '</main></body></html>')
        # The end-of-body candidate must be outside main, as the module hook is.
        if case["placement"] in ("body-end", "footer"):
            case["html"] = case["html"].replace('</article>', '</article></main>').replace('</main></body>', '</body>')
    return cases


if __name__ == "__main__":
    import sys
    Path(sys.argv[1]).write_text(json.dumps(fixtures(), indent=2) + "\n")
