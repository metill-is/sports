---
name: wire-league
description: Use when Lengjan has started listing a league we already model and a discovery proposal needs turning into a config edit. Reads data/discovery/proposals.json, drafts the leagues.yml competition entry + team_names, verifies via a dry decide, and presents the diff for review.
argument-hint: "[comp_id]"
context: fork
effort: high
---

# /wire-league — Turn a discovery proposal into a config edit

The `discover-leagues.yml` workflow writes `data/discovery/proposals.json` when
Lengjan starts offering a competition we model but do not yet scrape, and the
`discovery` health row WARNs about it. This skill turns one proposal into a
reviewed `config/leagues.yml` edit. It does NOT place bets and does NOT add a new
modelled league (that is `/add-league`).

## Step 1: Read the proposal

```bash
cat data/discovery/proposals.json
```

Pick the target competition (by `comp_id` if one was passed as an argument).
Note its `sport`, `country`, `inferred_sex`, `inferred_division`, `lengjan_name`,
and `proposed_team_names`.

## Step 2: Draft the `leagues.yml` edit

Under the matching league's `lengjan:` block:

1. Append a `competitions` entry:
   `- { id: "<comp_id>", name: "<lengjan_name>", sex: <inferred_sex> }`
2. Add the `team_names[[inferred_sex]]` entries from `proposed_team_names`:
   - `confidence: high` → add as-is.
   - `confidence: medium`/`low` or `canonical_guess: null` → add only if you can
     confirm the canonical team from the division's `results`; otherwise leave a
     comment that it is a pattern-guess to verify on first odds. A wrong guess is
     fail-safe (`decide_league` warn-skips an unmatched name), but do not invent.

Icelandic strings: edit `config/leagues.yml` directly (it is YAML/UTF-8, not R
source), mirroring the existing women's-Lengjudeild block (comp 4670) as the
template.

## Step 3: Verify the wiring

```bash
# Config still parses:
Rscript -e 'devtools::load_all(); stopifnot("<comp_id>" %in% vapply(load_leagues()[["<league_key>"]]$lengjan$competitions, function(c) c$id, character(1)))'

# Team-name map is still invertible:
Rscript -e 'devtools::load_all(); validate_team_names_config(load_leagues()[["<league_key>"]])'

# Dry decide produces candidates once odds exist (no ledger writes):
Rscript scripts/02_scrape_odds.R --league <league_key>
Rscript -e 'devtools::load_all(); print(decide_league("<league_key>", sex = "<inferred_sex>", write = FALSE))'
```

If `decide_league` warns "no beliefs for <team>", a team-name guess is wrong —
fix the mapping and re-run. No bet is ever placed by this skill.

## Step 4: Present for review

Show the `git diff config/leagues.yml` and the dry-decide output. The human
reviews and commits. The `discovery` health WARN self-clears on the next health
snapshot once the comp_id is in config.
