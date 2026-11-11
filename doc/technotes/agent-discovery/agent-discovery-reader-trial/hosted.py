"""Send only synthetic fixtures to Jina's raw-HTML endpoint; no credentials."""
import json
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

cases = json.loads(Path(sys.argv[1]).read_text())
destination = Path(sys.argv[2])
rows = []
for case in cases:
    request = urllib.request.Request(
        "https://r.jina.ai/",
        data=json.dumps({"html": case["html"], "url": "https://example.com/reader-trial/page"}).encode(),
        headers={"Content-Type": "application/json", "Accept": "application/json"})
    started = time.monotonic()
    try:
        with urllib.request.urlopen(request, timeout=45) as response:
            status, raw = response.status, response.read().decode()
    except urllib.error.HTTPError as error:
        status, raw = error.code, error.read().decode()
    payload = json.loads(raw)
    rows.append(dict(id=case["id"], reader="jina-hosted-raw-html", status=status,
                     seconds=round(time.monotonic() - started, 4),
                     output=(payload.get("data") or {}).get("content", ""), response=payload))
    destination.write_text(json.dumps(dict(rows=rows), indent=2) + "\n")
    print(case["id"], status, flush=True)
    if status != 200:
        break
