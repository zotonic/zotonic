# Shared Zotonic skills

`skills/<name>/SKILL.md` is the canonical source for each repository skill.
Keep supporting references and resources alongside it. Edit skills here so all
agents use the same instructions.

- Codex and GitHub Copilot discover skills in `.agents/skills/`.
- Claude Code discovers them through `.claude/skills/<name>` symlinks pointing
  to `../../.agents/skills/<name>`.
- Root `CLAUDE.md` and `.github/copilot-instructions.md` provide short entry
  points and guidance on selecting skills.

Read a skill's description to decide when to use it, then load its `SKILL.md`
and any references needed for the task. `agents/openai.yaml`, where present,
provides Codex metadata; shared instructions belong in `SKILL.md`.

For site inspection and validation, start with the
[LLM site development guide](../doc/guides/llm-site-development.md). It covers
local and remote setups using SSH, HTTP, MQTT, and browser tools. Use the
[development tools skill](skills/zotonic-development-tools/SKILL.md) for the
detailed agent workflow.

## Adding or renaming a skill

1. Create `skills/<name>/SKILL.md` with YAML `name` and `description` fields.
   Use a lowercase, hyphenated name matching the directory.
2. Add supporting files using relative links. Keep one canonical copy.
3. From the repository root, add Claude's discovery link:

   ```sh
   mkdir -p .claude/skills
   ln -s ../../.agents/skills/<name> .claude/skills/<name>
   ```

4. When renaming or removing a skill, update its discovery link and references.
   Commit symlinks as symlinks, not copied directories.
5. Verify each `.claude/skills/<name>` resolves to the corresponding canonical
   directory and that `.claude/skills/<name>/SKILL.md` is readable. Check that
   every canonical skill has a discovery link and no stale links remain.

Run the same check used by CI from the repository root:

```sh
python3 .github/scripts/check_skill_links.py
```

The Test workflow runs this as a separate job. Missing links, stale entries,
copied files or directories, incorrect targets, and broken links fail the check.

Checkouts must preserve Git symlinks for Claude's automatic discovery. If a
platform checks them out as text files, enable symlink support and recreate the
links; the canonical skills remain readable under `.agents/skills/`.
