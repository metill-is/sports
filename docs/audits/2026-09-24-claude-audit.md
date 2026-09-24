# Claude Audit — sports

**Date:** 2026-09-24
**Project:** /Users/brynjolfurjonsson/sports (main @ e9d60192e)
**Model / window:** no `model` pinned in project or global settings; this session runs Opus 5.5 (1M). Both window verdicts are reported.
**Baseline:** the cross-project config review applied in 98502a08a (2026-09-23 11:10Z). This audit checks what that left behind and what has changed since.

## Summary

25 findings: **1 FAIL**, **10 WARNING**, **14 INFO**.

- The one FAIL is a state-claim in an always-loaded rule that is false on disk: the ledger pre-commit hook is not active in this clone.
- Two regressions since 98502a08a: an arbitrary-code runner (`Bash(python3 -)`) is back in `settings.local.json`, and the Obsidian MCP server is now pending approval in terminal sessions.
- Two memory facts about money and deploy state are wrong (football `kelly_frac`, metill-platform PR #54).
- Context budget: ~7.5k tokens from the project, ~20.5k in total with the global layer. That passes on 1M and would be a WARNING on 200k.

---

## Pass 1 — CLAUDE.md

Files: `CLAUDE.md` (177 lines, 11,673 B). There is no `CLAUDE.local.md`. `_legacy/**/CLAUDE.md` (4 files) is excluded through `claudeMdExcludes`.

Checks that passed:
- 1a: 177 lines, at or under the 200-line limit.
- 1b: every required section is present: overview and scope, quick reference, conventions, Obsidian Output with `Handoff:`, and the Things 3 area ID.
- 1c: no TODO, FIXME or HACK comments. Every repo-rooted path resolves. All 9 named functions resolve (`ingest_league`, `rebuild_duckdb`, `commit_ledger_changes`, `sync_recs`, `fit_league`, `decide_league`, `pipeline_health`, `void_bet`, `wc_correct_knockout_dates`). `Rscript`, `R`, `gh`, `git` and `quarto` are all installed.
- 1f: the Metill vault, `Sports/Sports Handoff.md` (last modified 2026-09-24 07:56) and all 5 Knowledge topic `_MOC.md` files exist.
  **Correction (made while applying fixes):** `Sports/Sessions/` (17 notes) and `Sports/log.md` also exist and are in active use (entries from today). The Vault Guide says there is deliberately *no* `Sports/Sessions/`, and that Sports sessions and log entries route to the vault root. So this is a guide-vs-practice conflict, not a pass. It is left for the owner to settle: update the guide, or migrate the notes.

```
[INFO] CLAUDE.md:1c — "Eight CI workflows commit to main throughout the day" (also git-hygiene.md:3 and :116)
  world-cup.yml has no schedule: key (dispatch-only; last scheduled run 2026-09-02T09:36Z),
  so seven workflows commit on a schedule.
  Fix: "Seven scheduled CI workflows (plus the dispatch-only world-cup.yml)".

[INFO] CLAUDE.md:1c — Scope blockquote lists the World Cup pipeline alongside the live leagues
  Fix: mark it dormant (dispatch-only since 2026-09) so it isn't read as an active surface.

[INFO] CLAUDE.md:1c — "## Status" (Plan 7, 2026-04-30) is history, and "## Directory structure" holds no structure
  Fix: drop Status or fold it into one clause; rename the heading to "Source registry".

[INFO] CLAUDE.md:1d — AGENTS.md is now an untracked symlink to CLAUDE.md (Codex port, see I11)
  CLAUDE.md edits now also change Codex's instructions.
  Fix: none needed. Be aware of it when wording Claude-specific instructions.

[INFO] CLAUDE.md:1e — the autoplace bullet (lines 29-38) repeats git-hygiene.md ledger layers 1–2
  Both files are always loaded, so commit_ledger_changes / sync_recs rescue / 2026-06-10 incident /
  background-git warning are paid for twice in every session. The CI-isolation sentence is also
  near-verbatim in sports-betting.md "Local-only enforcement".
  Fix: cut the CLAUDE.md bullet to kill switch + health check + log path, plus a pointer to git-hygiene.md.

[INFO] CLAUDE.md:1e — 5 extra worktrees carry stale CLAUDE.md copies
  .claude/worktrees/{angry-jackson,crazy-rhodes} are detached HEADs from 2026-09-14;
  .worktrees/fix-kki-thor-alias is detached; sharp-jepsen and fix-alias-followups are on branches.
  Fix: triage with /wrap-up-session.

[INFO] vault:1f — Sports/Knowledge/"WC Accountability — Upstream Contract.md" sits outside the topic folders
  Fix: file it under Publish Pipeline/ or add a row to the CLAUDE.md topic table.
```

## Pass 2 — Config

Inventory:
- Hooks: 2, both `type: command`. Both scripts exist and are executable, both are referenced, and both are 25 lines or fewer.
- Skills: 9.
- Agents, output styles, plugin: none.
- `.mcp.json`: 1 server (`r-sports`, connected).
- Claude Code's own permission validator (`claude -p … < /dev/null`) printed no warnings.

```
[WARNING] 2 settings.local.json — Bash(python3 -) re-added after 98502a08a removed the arbitrary-code runners
  This is the rule Claude Code saves when you approve a python3 heredoc. Such a heredoc can
  subprocess `place_bets.R --live` and never match the text-based ask rule
  Bash(Rscript *place_bets.R* --live*). This is the bypass that 98502a08a closed.
  The file was modified 2026-09-24 07:20, after the review; the backup diff shows the addition.
  The same edit added two one-off rules:
  Bash(grep -n 'max\(recs…' R/placer-load.R) and Bash(echo "grep exit=$?").
  Fix: delete all three entries.

[WARNING] 3.75c obsidian MCP — pending approval in terminal `claude` sessions
  98502a08a changed enabledMcpjsonServers from ["obsidian"] + enableAllProjectMcpServers to ["r-sports"].
  `obsidian` is defined in ~/.mcp.json, so `claude mcp list` in ~/sports now reports
  "⏸ Pending approval". Desktop sessions are unaffected because they also load claude_desktop_config.json.
  CLAUDE.md "Obsidian Output" ("Prefer MCP write_note") and /done depend on this server.
  Fix: enabledMcpjsonServers: ["r-sports", "obsidian"].

[WARNING] 3b sports-update — three stale paths in the verification table and the reference list
  :111 beliefs.parquet → the files are part-0.parquet
  :113 data/publish/{sport}/iceland/{karla,kvenna}/*.json → the layout is {karla,kvenna}-{slug}/*.json
  :176 R/publish-{football,basketball,handball}-iceland.R → do not exist; see R/publish-iceland-league.R, R/publish-pipeline.R
  The skill runs as a fork and reports freshness to the caller from these globs.
  Fix: correct the three paths.

[WARNING] 3b sports-update — context: fork without agent:
  The body is fork-aware (Step 5: "cannot ask the user"), but it names no subagent type.
  Fix: add agent: general-purpose (or a dedicated pipeline agent).

[WARNING] 3b wrap-up-session — dead cross-reference
  :41-42 points to "the cherry-pick-from-stash pattern" in .claude/rules/git-hygiene.md.
  No such section exists there, and none existed before 98502a08a either.
  Fix: inline the pattern (git checkout stash@{N} -- <paths> onto a branch) or drop the pointer.

[WARNING] 3b wrap-up-session — destructive git operations with no user gate, and the skill is model-invocable
  `git stash drop` and `git branch -D` run on the model's own judgement ("drop after confirming
  pop succeeded"). It carries no disable-model-invocation and has no AskUserQuestion step.
  Fix: add a step that lists the stashes and branches to drop, then AskUserQuestion before any drop or -D.
  (Keep the skill model-invocable if you like; the gate is what matters.)

[WARNING] 3b sync-main / wrap-up-session — overlapping triggers
  Both descriptions lead with "origin/main moved under local work because of cron commits".
  Fix: lead wrap-up-session with its own trigger (end of session; leftover stashes, branches,
  worktrees or PRs) and drop the cron clause, which sync-main owns.

[INFO] 2b PostToolUse Edit|Write → check-stan-syntax.sh has no `if` field
  It spawns bash + jq on every edit and filters to *.stan inside the script. It blocks with
  exit 2 on a stanc failure, which returns the error to Claude.
  Fix (optional): narrow it with an `if` permission-rule filter on *.stan paths.

[INFO] 2b No SessionStart `compact` matcher
  This is deliberate: 98502a08a removed it because the re-injected payload was stale.
  Fix: none. Optionally set the health-banner matcher to startup|resume|compact (about one line of cost).

[INFO] 3b All 8 model-listed project skill descriptions lead with "Use when…" (trigger first)
  They are short (98–285 chars) and fine. Stating what the skill does first would match BP-SKILLS-04.

[INFO] 3c Listing budget — 39 model-listed skills, 20,324 chars ≈ 5.6k tokens; none over 1,536
  The project's 8 skills total 1,525 chars. The five longest overall (all global): docs 983,
  html-artifacts 982, metill-ehf 981, pptx 964, xlsx 954.
  metill-ehf and vault-health-auditor are each listed twice (a personal copy and a claude.ai-synced
  copy), which wastes about 1.9k chars. That is a global issue, not a project one.
```

Other checks that passed:
- place-bets: its chat confirmation plus the global `ask` rule `Bash(Rscript *place_bets.R* --live*)` both verified.
- wc-refresh: `disable-model-invocation: true`, with `allowed-tools` narrowed.
- Every frontmatter field is in the canonical list.
- Every SKILL.md is 187 lines or fewer.
- `enabledMcpjsonServers` matches `.mcp.json`.

## Pass 3 — Context budget

```
CONTEXT BUDGET ESTIMATE (always loaded in a ~/sports session)
  CLAUDE.md files:        177 lines / 11,673 B   (~2.9k tokens)
  @path imports:            0
  Always-loaded rules:    143 lines /  7,502 B   (~1.9k)   git-hygiene.md
  Skill descriptions:  20,324 chars              (~5.6k)   project share: 1,525 chars (~0.4k)
  Agent descriptions:       0 (project)
  Memory (MEMORY.md):      80 lines /  9,103 B   (~2.3k)
  Prompt hooks:             0 (SessionStart command hook adds about one banner line)
  ─────────────────────────────────
  Project-owned:          ~7.5k tokens
  + global CLAUDE.md 27,114 B (~6.8k) + global obsidian-routing.md 4,084 B (~1.0k) + global skills (~5.2k)
  All-in:                ~20.5k tokens
```

| Window | Project-owned 7.5k | All-in 20.5k |
| --- | --- | --- |
| 1M (Opus 5.5, this session) | PASS (warn 50k) | PASS |
| 200k | PASS (warn 15k) | **WARNING** (>15k; driven by the global layer) |

```
[INFO] context-budget — path-scoped co-load peak
  Detail: reading R/decide-kelly.R loads sports-betting (14,248 B) + model-decide (7,657) +
  settle-health (6,316) + r-conventions (943) ≈ 29 KB ≈ 7.3k tokens.
  Fix: none required on 1M. It becomes the first trim target on a 200k model.
```

- 6.5: no skill sets `effort:` or `model:`, so N/A.
- Compared with the 2026-05-09 audit: that one measured ~9.5k always loaded, and the project-owned figure is now ~7.5k.

## Phase 4 — Rules

| Rule | Scope | Size | Globs match |
| --- | --- | --- | --- |
| backtest.md | 5 paths | 4.7 KB | yes |
| ci-conventions.md | `.github/workflows/**` | 12.5 KB | yes |
| **git-hygiene.md** | **unscoped** | 7.5 KB | — |
| model-decide.md | 9 paths | 7.7 KB | yes |
| publish-layer.md | 6 paths | 17.0 KB | yes |
| r-conventions.md | `**/*.R`, `**/*.r` | 0.9 KB | `**/*.r` matches nothing |
| settle-health.md | 7 paths | 6.3 KB | yes |
| sports-betting.md | 8 paths | 14.2 KB | yes |
| stan-conventions.md | `**/*.stan` | 0.7 KB | yes |

All use `paths:` (none use the legacy `globs:`). git-hygiene.md is unscoped on purpose, since no file path triggers a git operation.

```
[FAIL] rules git-hygiene.md:18-49 — "Three enforcement layers are active": layer 3 (pre-commit hook) is not
  In .git/config, core.hooksPath = /Users/brynjolfurjonsson/sports/.git/hooks (absolute path).
  That directory holds only *.sample files. The tracked tools/git-hooks/pre-commit never fires,
  here or in any of the 5 worktrees (they inherit the setting). tools/install-hooks.sh sets
  tools/git-hooks, so something has since overwritten it. The next commit on something else can
  once again leave dirty ledger rows behind (the 121710d incident class).
  Fix: bash tools/install-hooks.sh, then `git config --get core.hooksPath` should print tools/git-hooks.
  Consider having the pipeline-doctor check core.hooksPath so a silent revert shows up.

[WARNING] rules sports-betting.md:188-199 — "Skill reference" is stale
  It says "The four skills … are model-invocable and intentionally unforked (see test-skill-conventions.R)".
  There are 9 skills, sports-update sets context: fork, and the test guards only bet and place-bets.
  Fix: replace the table with a pointer to CLAUDE.md "Skills" and state the real invariant
  (bet and place-bets must not fork).

[INFO] rules r-conventions.md — `**/*.r` matches no file (every R file is .R)
[INFO] rules publish-layer.md — `R/publish-pipeline.R` is already covered by `R/publish-*.R`
```

## Phase 5 — Memory

MEMORY.md: 80 lines, 9.1 KB, well under the 200-line / 25 KB load limit. 69 topic files, all with valid frontmatter. Every index entry resolves. The 2 unindexed files are the ones the header says are deliberately unindexed.

```
[WARNING] memory project_kelly_frac_cut_2026_05_02.md (+ its MEMORY.md line) — wrong for football
  The memory says football is quarter-Browne (male 0.05, female 0.025).
  config/leagues.yml:524-538 has had football at half-Browne (0.10 / 0.05) since 2026-06-05,
  after the current_pool correction (PR #33). Basketball and handball are still quarter-Browne,
  as the memory says.
  Fix: update the table and the index hook ("quarter-Browne for BB/HB; football half-Browne since 2026-06-05").

[WARNING] memory project_bb_hb_parity_2026_09.md (+ its MEMORY.md line) — "metill-platform PR #54 open"
  gh shows it MERGED 2026-09-05T17:07Z, and the file's own line 82 says so, but the
  description and line 39 still say open.
  Fix: update the description and the index line to "both merged 2026-09-05".

[INFO] memory — conceptual content with no vault counterpart
  project_methodology_optimisation_verdict_2026_06_13.md (9.0 KB of methodology argument).
  A grep for its key terms finds 0 vault notes. The (S,D) and approximate-inference verdicts
  do have vault notes (8 and 2).
  Fix: move the reasoning to Sports/Knowledge/Betting Optimisation/ and keep a 3-line operational verdict in memory.

[INFO] memory — two memory-to-memory links don't resolve
  gotcha_trycatch_sibling_handler.md → [[project_bb_hb_metill_parity_2026_09]] (the name is project-bb-hb-parity-2026-09)
  project_autoplace_ledger_sync_incident_2026_06_10.md → [[project-autoplace-installed-2026-06-05]] (the name uses underscores)

[INFO] memory — 24 files are more than 90 days old by mtime (5 are feedback, which doesn't age)
  Spot-checked: autoplace still installed (launchctl lists is.metill.sports.autoplace), OK;
  kelly_frac stale (above).
  project_world_cup_2026 still describes "what remains", but the tournament is over and the workflow is dispatch-only.
  project_icelandic_focus.md duplicates the CLAUDE.md Scope blockquote.
```

## Out-of-scope observations

```
[INFO] I11 Untracked Codex port (AGENTS.md → CLAUDE.md, .agents/skills/, .codex/), created 2026-09-23 17:11
  It was generated from the pre-98502a08a backup: 8 of 9 skills match the backup with
  s/Claude/Codex/ applied, not the current skills. 4 skills contain mangled `.Codex/rules/…` paths.
  .codex/hooks.json brings back the PreCompact hook that 98502a08a removed.
  Fix: regenerate it from current main, then commit it or add it to .gitignore.
```

---

## Best-practice scoresheet

```
CLAUDE.md ≤ 200 lines:                          PASS    — 177 lines / 11.7 KB
CLAUDE.md uses @path imports:                   N/A     — under 200 lines; long content lives in path-scoped rules
CLAUDE.local.md exists:                         N/A     — optional; absent
Side-effect skills gated:                       WARNING — wrap-up-session drops stashes / deletes branches ungated; place-bets and wc-refresh gated
Skill descriptions ≤ 1,536; what + when:        PASS    — project max 285 chars; INFO: all trigger-first
Skills with context: fork specify agent:        WARNING — sports-update has no agent:
Hook `if` field narrows scope:                  INFO    — Stan hook fires on every Edit/Write; filters internally
compact SessionStart hook exists:               INFO    — removed deliberately in 98502a08a (stale payload)
MCP servers match allowed-tools refs:           PASS    — no MCP refs; r-sports enabled (obsidian pending in CLI: see Pass 2)
Plugin manifest schema valid:                   N/A     — not a plugin
Output styles have name + description:          N/A     — none
.claude/rules/ used for path-scoped content:    PASS    — 8 of 9 rules scoped
Rules use paths: for conditional loading:       PASS    — 1 deliberate unscoped rule; `**/*.r` matches nothing
.claude/agents/ definitions valid:              N/A     — none
Memory stays operational, not conceptual:       INFO    — one 9 KB methodology verdict with no vault counterpart
Frontmatter effort:/model: with stated reason:  N/A     — none set
Context budget within window threshold:         PASS    — 1M: ~20.5k all-in (warn 50k); 200k would WARN (global layer)
State-claims in always-loaded docs are true:    FAIL    — ledger pre-commit hook claimed active; core.hooksPath disables it
```

---

## Fixes applied (2026-09-24, same session)

The proposal `foreground-edit-before-background-launch` was skipped, as the owner asked.

**Tracked (commit 2189fe3d2):**
- **FAIL, ledger hook:** re-ran `bash tools/install-hooks.sh`, so `core.hooksPath` is now `tools/git-hooks` in main and all worktrees. `git hook run pre-commit` exits 0 on a clean ledger.
  - Mutation test in a scratch repo: a dirty-ledger commit is blocked (exit 1) with `tools/git-hooks`, and goes through (exit 0) with the absolute `.git/hooks` value. That is the bug, reproduced.
  - git-hygiene.md now says how to check the hook is live, and `health-banner.sh` prints a WARNING at session start when `core.hooksPath` drifts. The guard was tested against the scratch repo (warns) and the real repo (quiet).
  - Who wrote the bad value is still unknown. metill-platform and esbvaktin carry the same absolute value, which is harmless there (no tracked hook dir; esbvaktin's pre-commit-framework hook lives in `.git/hooks`).
- **sports-update:** the three paths fixed; `agent: general-purpose` added.
- **wrap-up-session:** gains an AskUserQuestion gate before `stash drop`, `branch -D` and `worktree remove`; a worktree-based stash-rescue recipe replaces the dead pointer; the description no longer overlaps sync-main.
- **sports-betting.md:** the "Skill reference" section now states the invariant the test actually enforces.
- **CLAUDE.md** (177 → 172 lines) **and git-hygiene.md:** "seven scheduled workflows"; the WC pipeline marked dormant; Status and Directory structure folded into "Source registry and design"; the autoplace bullet cut to a pointer.
- **settings.json:** the Stan hook is split into two handlers, `if: Edit(**/*.stan)` and `if: Write(**/*.stan)` (`if` takes exactly one rule, per the hooks docs). Verified live with `claude -p`: a broken `.stan` file under `Stan/` got the stanc blocking error, from one handler only.
- **Rule globs:** removed `**/*.r` and the redundant `R/publish-pipeline.R`.
- The three config-reading test files pass: skill-conventions, placer-ci-isolation, publish-refactor-hygiene (0 failed, 0 errors).

**Local and untracked:**
- `settings.local.json`: removed `Bash(python3 -)` and the two one-off rules; added `obsidian` to `enabledMcpjsonServers`. `claude mcp list` now shows obsidian `✔ Connected`.
- Codex port: `.agents/skills/*/SKILL.md` and `.codex/hooks/*.sh` are now symlinks into `.claude/` (the metill-platform precedent), so they can no longer drift. Removed the stale PreCompact wiring and the `pre-compact-context.sh` copy, which was byte-identical to the git-tracked original at 98502a08a^. It is still untracked and not ignored, as in metill-platform.
- Worktree commits `58e515356` (Víkingur Reykjavík rendering, plus tests) and `8313435b4` (team-names health check, 200 lines) were reachable only through detached worktree HEADs and were never merged. They are now pinned as `rescue/vikingur-rendering-2026-09-03` and `rescue/team-names-health-2026-09-03`.

**Memory:**
- kelly_frac: football is half-Browne since 39921152f.
- BB/HB parity: PR #54 merged; all 8 cells live (basketball since 2026-09-15/17).
- Two dangling links fixed.
- World Cup memory rewritten as a closed record. The `hm2026` route shipped; the WC schemas never did.
- Icelandic focus cut to its non-CLAUDE.md content. **An extra wrong claim was found:** `--league` does *not* override `active: false`, because `01_ingest_results.R` stops.
- Methodology verdict cut to its operational core. **An extra wrong claim was found:** PR #40 had merged (2026-06-16) but the memory said it was open.
- Pre-edit copies were backed up to the session scratchpad.

**Vault (Metill):**
- New note `Sports/Knowledge/Betting Optimisation/Historical/methodology-verdict-2026-06-13.md`. Its re-opening gates are taken from spec §3.3 and §4.5, not paraphrased.
- Rows added to the Betting Optimisation MOC: the new note, plus the unlisted 2026-06-21 bet-sizing audit.
- The WC Accountability note stays in place (its inbound links are path-qualified, one in the append-only `log.md`) and is now linked from the Publish Pipeline MOC.

**Not applied:**
- Global duplicate skill listings (metill-ehf, vault-health-auditor): these live outside the project.
- The "Use when…" description style: optional, INFO only.
- A `compact` SessionStart hook: its removal was deliberate.
- Removing worktrees: this needs the owner's confirmation (wrap-up-session's own new gate).
  - Removable: `.worktrees/fix-kki-thor-alias` (detached at a commit on main, clean) and `.worktrees/fix-alias-followups` (PR #92 squash-merged as d7da31da1).
  - Keep: `sharp-jepsen-e3a666` (PR #94 open), plus the two detached 2026-09-03 worktrees, whose work is now safe on the rescue branches.
