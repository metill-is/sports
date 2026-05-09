# Claude Audit — sports

**Date:** 2026-05-09
**Project:** /Users/brynjolfurjonsson/sports
**Active model:** claude-opus-4-7 (1M context) — detected from session env

## Summary

12 issues found: **3 critical**, **5 warnings**, **4 info**.
Context budget: ~9.5k tokens always-loaded (~0.95% of 1M), ~14.5k peak when path-scoped rules activate. Well under the 50k warn threshold for Opus 4.7.

The setup is in healthy shape overall. Most findings are residue from the
Plan 7 cutover (2026-04-30) — files that referenced the old `_targets.R` /
`run.R` driver and the pre-monorepo `Sports/{sport}/{country}/` layout
weren't fully cleaned up.

## Critical Issues

### 1. `validate-pipeline-output.sh` is dead code

**Location:** [.claude/hooks/validate-pipeline-output.sh](.claude/hooks/validate-pipeline-output.sh) registered against `Bash(Rscript:*)` in [.claude/settings.json:39-49](.claude/settings.json:39-49).

**Problem:** The hook fires on **every** `Rscript` invocation (no `if` field
narrows scope), but the script body short-circuits at line 10:

```bash
if ! echo "$COMMAND" | grep -q "run.R"; then
  exit 0
fi
```

`run.R` was removed in the Plan 7 cutover (2026-04-30) when the pipeline
moved to `scripts/0N_*.R` entry points. So the hook now fires, finds no
match, and exits early on every `Rscript` call — adding ~10 ms of
overhead per invocation while doing zero validation. The downstream check
for `--step fit` and "stale posterior" warnings is also dead (those
patterns don't appear in the new scripts' output).

**Fix:** Either (a) delete the hook entry from `settings.json` and the
script file, or (b) rewrite the body to match the new `scripts/0N_*.R`
output (e.g. `grep -q 'scripts/0[1-5]_'` plus updated FAIL/ERROR
patterns from the new freshness predicates). Recommendation: delete —
the entry-script outputs already surface `[FAIL]`/`[ERROR]` to the
user directly, so this hook adds little value.

### 2. `.claude/rules/sports-per-sport.md` is entirely stale

**Location:** [.claude/rules/sports-per-sport.md](.claude/rules/sports-per-sport.md), 99 lines.

**Problem:** The rule's `paths: ["Sports/**"]` glob matches **zero files**
in the project — the `Sports/`, `handball/`, `basketball/`, and
`football/` directories don't exist post-monorepo. The rule never
loads. Worse, the content itself describes the pre-Plan-1 era:
references to `handball/iceland/`, `handball/other/`, `football/italy/`,
`football/england/`, the old `R/lengjan/` shared modules, and 12-country
`handball/{country}/` per-country directories — none of which exist in
the current repo (only `_legacy/` carries that history).

The current scope is documented in CLAUDE.md line 5 as "3 active
Icelandic leagues only" and per-league details have moved into
`config/leagues.yml` and the per-league `R/ingest-{ksi,kki,hsi}-*.R`
scrapers.

**Fix:** Delete the file. If per-sport notes become useful again, write
fresh content scoped to current paths (e.g. `R/ingest-*.R`).

### 3. CLAUDE.md is 474 lines (>300 line FAIL threshold)

**Location:** [CLAUDE.md](CLAUDE.md).

**Problem:** Every line is paid as context on every turn. At ~15 tok/line,
CLAUDE.md alone is ~7.1k tokens — for Opus 4.7's 1M context that's
trivial in absolute terms, but the file is also the maintenance
surface, and 474 lines is hard to keep current. The file is
content-rich (no obvious dead sections), but it blends seven distinct
concerns: status table, directory tree, local-only subsystem,
quick-reference commands, metill-platform integration, legacy
archival, and conventions for five subsystems (R, columns, Stan,
model, decide, publish, CI, placer).

**Fix:** Extract the long-tail content into `.claude/rules/` files with
`@path` imports back from CLAUDE.md. Best candidates for extraction:

| Section | Current lines | Extract to |
|---|---|---|
| `### CI / GitHub Actions` (the V8 / sysreqs / workflow_run write-up) | ~75 | `.claude/rules/ci-conventions.md` (paths: `.github/workflows/**`) |
| `### Publish layer` (the football extracts tree explanation) | ~95 | `.claude/rules/publish-layer.md` (paths: `R/publish-*.R`, `R/extract-*.R`) |
| `### Model layer` + `### Decide layer` | ~50 | `.claude/rules/model-decide.md` (paths: `R/model-*.R`, `R/decide-*.R`) |
| `## Directory structure` | ~95 | Could compress (LLMs derive this from `find`) or extract to a `repo-map.md` |

Extracting these four to rules with proper path scoping would cut
CLAUDE.md to ~150 lines (well under the 200-line target) and ensure each
rule loads only when relevant code is in scope. Auto-fix is feasible but
worth confirming before applying — the extractions affect readability
for humans browsing the repo.

## Warnings

### 1. CLAUDE.md still says "Mid-migration"

**Location:** [CLAUDE.md:9](CLAUDE.md:9).

**Problem:** The Status section opens with "Mid-migration" but the table
immediately following shows all seven plans as ✅ Complete (Plan 7
finished 2026-04-30). New readers (human or AI) will misjudge the
maturity of the codebase.

**Fix:** Replace "Mid-migration." with "Migration complete (Plan 7,
2026-04-30)." or similar. Single-line edit.

### 2. `/bet` skill references `decide_one` (older wrapper) instead of `decide_league`

**Location:** [.claude/skills/bet/SKILL.md:97](.claude/skills/bet/SKILL.md:97).

**Problem:** The Reference section says
`R/decide-pipeline.R::decide_one()`, but the canonical public entry per
CLAUDE.md:245 and `R/decide-pipeline.R` source is `decide_league()`.
`decide_one()` still exists as a wrapper (it calls `decide_league()`
internally) but the documentation pointing readers to the wrapper instead
of the public entry is misleading.

**Fix:** One-line change — `decide_one()` → `decide_league()`.

### 3. `.mcp.json` and `r-tools.R` use case-mismatched path

**Location:** [.mcp.json:8](.mcp.json:8) and
[.claude/r-tools.R:7](.claude/r-tools.R:7) (also lines 1-5 comment).

**Problem:** Both files reference `/Users/brynjolfurjonsson/Sports/`
(capital S), but the actual project directory is
`/Users/brynjolfurjonsson/sports/` (lowercase). Works on macOS due to
case-insensitive APFS, but breaks on case-sensitive file systems
(some Linux setups, the GitHub-hosted Ubuntu runners' home directory,
or APFS volumes formatted case-sensitive). Could silently fail if
the project is ever cloned on a case-sensitive volume.

**Fix:** Lowercase `Sports` → `sports` in both files. The MCP
`r-sports` server then resolves correctly regardless of filesystem
case sensitivity.

### 4. `pre-compact-context.sh` doesn't list the new git-hygiene skills

**Location:** [.claude/hooks/pre-compact-context.sh:21](.claude/hooks/pre-compact-context.sh:21).

**Problem:** The hook's Skills line names only `/bet`, `/sports-update`,
`/add-league`, `/place-bets`. Commit `fb16280` (today) added
`/sync-main` and `/wrap-up-session`, plus the project-local
`.claude/rules/git-hygiene.md`. After a SessionStart `compact` event or
PreCompact, the assistant won't see these surfaces in the preserved
context.

**Fix:** Append `/sync-main` and `/wrap-up-session` to the Skills line,
and add a new Rules line listing the .claude/rules/ files. Keep it
short — the hook is a context-survival aid, not a manifest.

### 5. `.claude/rules/git-hygiene.md` has no `paths:` frontmatter (always-loaded)

**Location:** [.claude/rules/git-hygiene.md](.claude/rules/git-hygiene.md), 100 lines.

**Problem:** No frontmatter means the rule is always-loaded — paid every
turn, not just on git-related work. ~1.5k tokens per turn for a rule
that's only useful when running git commands. Over 200 turns on Opus
4.7 that's ~300k tokens of always-on git advice.

**Fix:** Either narrow to git operations explicitly:

```yaml
---
paths:
  - ".github/**"
  - "**/.gitignore"
  - "scripts/**"
---
```

…or accept it as always-on (because cron-collisions can happen during
any session) and explicitly document the choice. Given that the rule's
own §1 says "anything in `data/` is either committed by cron or
locally generated and short-lived" — i.e. it's relevant during data
work too — always-on is defensible. If left always-on, no fix needed
beyond noting the trade-off.

## Info

### 1. CLAUDE.md doesn't use `@path` imports

Tied to Critical #3 — recommended remediation is to add `@path`
imports for the extracted sections.

### 2. No `.claude/agents/` directory

Project relies on built-in subagent types (general-purpose, Explore, Plan,
verifier, etc.) and the per-feature subagents under
`feature-dev:code-*`. No custom agent definitions. Acceptable — adding
custom agents has cost (descriptions compete for context); only worth
it for repeated multi-step workflows that don't fit a skill.

### 3. No `.claude-plugin/plugin.json`

The project isn't packaged as a Claude Code plugin. Correct — it's a
standalone monorepo for one user, not something to distribute via the
plugin marketplace.

### 4. Memory and CLAUDE.md boundary is mostly clean

Memory directory has 13 entries (~351 lines total), all under 60
lines each. MEMORY.md is 41 lines (well under the 200-line truncation
limit). Most recent entries are 2026-05-09 (today's git-hygiene
patterns); oldest is 2026-04-10 (Icelandic focus). No clearly stale
entries. Boundary with Obsidian (Knowledge topics under
`Sports/Knowledge/`) is appropriately maintained — operational
context lives in memory, conceptual depth lives in Obsidian.

## Context Budget Estimate

| Source | Lines | Tokens (est.) |
|---|---|---|
| CLAUDE.md | 474 | ~7,110 |
| git-hygiene.md (always-loaded) | 100 | ~1,500 |
| Skill descriptions (6 skills, 952 chars) | — | ~238 |
| MEMORY.md (always loaded, ≤200) | 41 | ~615 |
| **Always-loaded total** | — | **~9,463** |
| Plus path-scoped: r-conventions.md (R files) | 18 | ~270 |
| Plus path-scoped: sports-betting.md (decide/placer/config) | 208 | ~3,120 |
| Plus path-scoped: stan-conventions.md (Stan files) | 14 | ~210 |
| **Peak (working on betting code)** | — | **~13,063** |
| sports-per-sport.md (paths match nothing — never loads) | 99 | 0 |

For Opus 4.7's 1M context, ~9.5k always-on is **0.95%** — well within
the 50k warn threshold. Cost is amortised over 200+ turns/session, so
the practical concern is maintenance burden of a 474-line CLAUDE.md
rather than per-turn cost.

## Best-Practice Scoresheet

- **CLAUDE.md ≤ 200 lines:** ❌ FAIL — 474 lines (Phase 1a)
- **CLAUDE.md uses `@path` imports for long content:** ❌ FAIL — none used
- **CLAUDE.local.md exists:** N/A INFO — optional, not present
- **Skills with side effects use `context: fork`:** ✅ PASS — `/sports-update` and `/add-league` set `context: fork`; `/bet` and `/place-bets` are intentionally unforked (need conversation context for confirmations); `/sync-main` and `/wrap-up-session` are mechanical and inherit main context (acceptable)
- **Skills use `disable-model-invocation: true` where appropriate:** ✅ PASS — all six skills are intentionally model-invocable per the canonical `feedback_model_invocable_skills.md` memory note
- **Skill descriptions ≤ 250 chars, start with "Use when":** ✅ PASS — longest is `/wrap-up-session` at 218 chars; all six skills start with "Use when" (or "Use at" for wrap-up)
- **Skills with `context: fork` specify `agent`:** ⚠️ INFO — `/sports-update` and `/add-league` set `context: fork` but no `agent:` field; defaults to general-purpose. Acceptable because both are heavy-orchestration skills that benefit from the full general-purpose toolset; not a problem unless a more specialised agent type is wanted
- **Hook `if` field used to narrow scope:** ⚠️ WARNING — `Bash(Rscript:*)` matcher on `validate-pipeline-output.sh` has no `if` (would have been valuable, but the hook is dead anyway — see Critical #1)
- **`compact` SessionStart hook exists:** ✅ PASS — registered in settings.json:15-26, calls `pre-compact-context.sh`
- **MCP servers in `.mcp.json` match `allowed-tools` refs:** ✅ PASS — only `r-sports` defined; no skill references unconfigured MCP tools
- **Plugin manifest schema valid:** N/A — not a plugin
- **Output styles have `name` + `description`:** N/A — no project output styles
- **`.claude/rules/` used for path-scoped instructions:** ✅ PASS — five rules present, four use `paths:`
- **Rules use `paths:` for conditional loading:** ⚠️ MIXED — 4 of 5 rules use `paths:`; `git-hygiene.md` is always-loaded (Warning #5); `sports-per-sport.md` has stale `paths:` that match nothing (Critical #2)
- **`.claude/agents/` definitions valid:** N/A — no custom agents
- **Memory stays operational, not conceptual:** ✅ PASS — boundary with Obsidian is correctly maintained
- **Reasoning-heavy skills set `effort: high` on Opus:** ✅ PASS — `/sports-update` and `/add-league` set `effort: high`; `/bet`, `/place-bets`, `/sync-main`, `/wrap-up-session` are procedural rather than reasoning-heavy (acceptable to leave default)
- **Light side-effect skills pin `model: claude-haiku-4-5`:** ⚠️ INFO — `/sync-main` and `/wrap-up-session` are mechanical git operations; pinning Haiku would cut per-invocation cost. Not strictly required, but a defensible cost optimisation
- **Context budget within model threshold:** ✅ PASS — ~9.5k always-loaded, ~13k peak; threshold is 50k for Opus 4.7

## Auto-Fixable Items

The following can be applied with confidence:

1. **Delete `validate-pipeline-output.sh` and remove from settings.json** (Critical #1) — dead code, no behavior change
2. **Delete `.claude/rules/sports-per-sport.md`** (Critical #2) — never loads, content fully obsolete
3. **CLAUDE.md line 9: "Mid-migration." → "Migration complete (Plan 7, 2026-04-30)."** (Warning #1) — single-line correction
4. **`/bet` SKILL.md line 97: `decide_one()` → `decide_league()`** (Warning #2) — single-line correction
5. **`.mcp.json` line 8 and `r-tools.R` lines 1-7: `/Sports/` → `/sports/`** (Warning #3) — case correction
6. **`pre-compact-context.sh` line 21: append `/sync-main`, `/wrap-up-session`** (Warning #4) — additive

The CLAUDE.md extraction (Critical #3) is best done interactively —
each section's `paths:` scope is a judgment call.

## Manual-Action Items

- **CLAUDE.md extraction** (Critical #3) — 4 sections to extract,
  ~250 lines reclaimed from the always-loaded budget. Worth a focused
  session.
- **`git-hygiene.md` paths decision** (Warning #5) — pick "always-on"
  or scope to `.github/**`. Document the choice in a comment.
