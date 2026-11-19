#!/usr/bin/env python3
"""Check Claude discovery links against the canonical repository skills."""

from pathlib import Path
import sys


def check_links(root):
    source = root / ".agents/skills"
    discovery = root / ".claude/skills"
    errors = []
    for directory in (source, discovery):
        if not directory.is_dir():
            errors.append(f"Missing directory: {directory.relative_to(root)}")
    if errors:
        return errors

    skills = {path.name for path in source.iterdir() if (path / "SKILL.md").is_file()}
    links = {path.name: path for path in discovery.iterdir()}
    for name in sorted(skills - links.keys()):
        errors.append(f"Missing Claude symlink: .claude/skills/{name}")
    for name in sorted(links.keys() - skills):
        errors.append(f"Stale Claude entry (no canonical skill): .claude/skills/{name}")
    for name in sorted(skills & links.keys()):
        link = links[name]
        label = f".claude/skills/{name}"
        if not link.is_symlink():
            errors.append(f"Expected a symlink, not a file or directory: {label}")
            continue
        target = Path("../../.agents/skills") / name
        if link.readlink() != target:
            errors.append(f"Incorrect target for {label}: expected {target}")
        try:
            if link.resolve(strict=True) != (source / name).resolve(strict=True):
                errors.append(f"Link resolves to a different skill: {label}")
            # Verify the discovered skill can actually be read, not just resolved.
            (link / "SKILL.md").read_text(encoding="utf-8")
        except (OSError, RuntimeError, UnicodeError) as error:
            errors.append(f"Broken or unreadable skill link: {label} ({error})")
    return errors


if __name__ == "__main__":
    repository = Path(__file__).resolve().parents[2]
    failures = check_links(repository)
    if failures:
        for failure in failures:
            print(f"ERROR: {failure}", file=sys.stderr)
        sys.exit(1)
    print("All canonical skills have matching, valid Claude symlinks.")
