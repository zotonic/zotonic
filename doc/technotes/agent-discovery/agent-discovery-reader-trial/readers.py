"""Run real extraction libraries against the same synthetic HTML inputs."""
import importlib.metadata
import json
import sys
from pathlib import Path

from bs4 import BeautifulSoup
import html2text
import markdownify
import trafilatura

cases = json.loads(Path(sys.argv[1]).read_text())
rows = []
text_converter = html2text.HTML2Text()
text_converter.body_width = 0  # Preserve link destinations without line wrapping.
for case in cases:
    html = case["html"]
    body = str(BeautifulSoup(html, "html.parser").body)
    outputs = {
        "trafilatura": trafilatura.extract(html, output_format="markdown", include_links=True),
        "trafilatura-recall": trafilatura.extract(html, output_format="markdown", include_links=True, favor_recall=True),
        "html2text-body": text_converter.handle(body),
        "markdownify-body": markdownify.markdownify(body),
    }
    for reader, output in outputs.items():
        rows.append(dict(id=case["id"], reader=reader, output=output or ""))
Path(sys.argv[2]).write_text(json.dumps(dict(
    versions={name: importlib.metadata.version(name) for name in
              ("trafilatura", "html2text", "markdownify", "beautifulsoup4")},
    rows=rows), indent=2) + "\n")
