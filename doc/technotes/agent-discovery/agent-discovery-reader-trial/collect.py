"""Bundle outputs by hash and validate controls; input directory is disposable."""
import hashlib
import json
import sys
from pathlib import Path

from fixtures import LABEL, TARGET

source, destination = map(Path, sys.argv[1:])
cases = json.loads((source / "fixtures.json").read_text())
outputs, rows, versions = {}, [], {}
names = ["js-results", "py-results", "jina-results"]
hosted_ids = {r["id"] for r in json.loads((source / "jina-results.json").read_text())["rows"]}
if "control/absent" not in hosted_ids:
    names.append("jina-control")
for name in names:
    result = json.loads((source / f"{name}.json").read_text())
    versions.update(result.get("versions", {}))
    for row in result["rows"]:
        output = row.pop("output")
        digest = hashlib.sha256(output.encode()).hexdigest()
        outputs[digest] = output
        row.pop("response", None)
        row.update(output_sha256=digest, retains_url=TARGET in output,
                   retains_label=LABEL in output)
        assert "community archive describes buildings" in output, (name, row["id"])
        assert row.get("status", 200) == 200
        rows.append(row)
assert len(rows) == len(cases) * 7
assert len({(r["id"], r["reader"]) for r in rows}) == len(rows)
assert not any(r["retains_url"] or r["retains_label"] for r in rows if r["id"] == "control/absent")
browser = json.loads((source / "browser-results.json").read_text())
for case in cases:
    case["sha256"] = hashlib.sha256(case.pop("html").encode()).hexdigest()
destination.write_text(json.dumps(dict(
    date="2026-11-11", revision="d09962f64", target=TARGET, label=LABEL,
    versions=versions, fixtures=cases, rows=rows, outputs=outputs, browser=browser,
    python_packages=(source / "requirements.txt").read_text().splitlines(),
    npm_packages=json.loads((source / "npm-versions.json").read_text()),
    hosted_mode="Jina Reader POST raw HTML, Accept application/json; no preset or key; service build not reported"
), indent=2) + "\n")
readers = list(dict.fromkeys(r["reader"] for r in rows))
print(" | ".join(["Fixture"] + readers))
for case in cases:
    matches = {(r["reader"]): r for r in rows if r["id"] == case["id"]}
    print(" | ".join([case["id"]] + ["yes" if matches[r]["retains_url"] else "no" for r in readers]))
