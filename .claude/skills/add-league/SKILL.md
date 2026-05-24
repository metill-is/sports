---
name: add-league
description: Use when adding a new league to the monorepo. Walks through leagues.yml, Stan model, ingest source, team_names invariant, and verification.
argument-hint: "[sport] [country]"
context: fork
effort: high
---

# /add-league — Add a new league to the monorepo

The post-Plan-6 monorepo simplifies the old four-step "create directory + bets.yml +
team_names CSV + symlinks" dance into a single source of truth: `config/leagues.yml`.
Most additions touch three files.

> The current scope is the three active Icelandic leagues. When extending beyond
> Iceland, the dead `_legacy/` paths in `_legacy/{sports,lengjan-odds,…}` are
> useful as historical reference for ingest scrapers and team_names CSVs.

## Step 1: Gather information

Ask the user (or infer from arguments):

1. **Sport** — `basketball`, `handball`, `football`
2. **Country** — lowercase (`norway`, `denmark`, …)
3. **League key** — by convention `{sport}_{country}` (e.g. `basketball_norway`)
4. **Sexes** — any subset of `[male, female]`
5. **Federation source** — does an `R/ingest-{federation}-{sport}.R` already
   cover this league? If not, a new scraper is needed (see Step 4).
6. **Lengjan odds + bet placement** — does Lengjan list this league? If yes,
   you'll need competition IDs and a `team_names` map (Step 5).
7. **Stan model** — reuse an existing one (e.g. `2d_student_t_scalarsigma.stan`
   for basketball, `2d_student_t.stan` for handball, `bivariate_poisson_no_inflation.stan`
   for football) or write a new one. The path is relative to `Stan/`.

## Step 2: Add a `config/leagues.yml` entry

Append a block following the schema enforced by `config/leagues.schema.json`.
Mirror the structure of an active league. A minimal entry:

```yaml
basketball_norway:
  sport: basketball
  country: norway
  sexes: [male]
  active: true
  data_source:
    results: nbbf_basketball         # must match a registered ingest source
    schedule: nbbf_basketball
    odds: lengjan_odds
  lengjan:
    competitions:
      - { id: "1234", name: "BLNO", sex: male }
    team_names:
      Bærum: Bærum Basket
      # ... canonical pipeline name → Lengjan display name
  stan_model: basketball_norway/2d_student_t_scalarsigma.stan
  betting:
    kelly_frac: 0.10                  # conservative for new leagues
    ev_threshold: 0.0
    markets:
      moneyline: true
      spread: true
      total: true
    scoring:
      has_ties: false
      tie_threshold: 0
    min_bet: 200
    max_age_hours: 48
```

Validate the entry parses cleanly:

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(); print(names(load_leagues()))'
```

## Step 3: Add the Stan model

Create the directory and place a `.stan` file at the path you set in
`stan_model:`:

```bash
mkdir -p Stan/basketball_norway
# Either copy a reusable model:
cp Stan/basketball_iceland/2d_student_t_scalarsigma.stan Stan/basketball_norway/
# Or author a new one — see .claude/rules/stan-conventions.md
```

If reusing an existing model verbatim, a single per-league directory still
makes sense — different leagues will diverge over time (priors, additional
parameters), and per-key directories keep changes isolated.

## Step 4: (If new federation) add the ingest scraper

If `data_source.results` references a source that doesn't already exist in
`R/ingest.R`'s dispatcher, write a new scraper following the patterns in:

- `R/ingest-ksi-football.R` (paginated server-rendered HTML)
- `R/ingest-kki-basketball.R` (XLSX from a federation site)
- `R/ingest-hsi-handball.R` (chromote-driven JS site)

Each scraper exports a function that returns a tibble matching the `results`
schema in `R/storage-schemas.R`. Register the new source string in `R/ingest.R::ingest_league()`.

Add a test under `tests/testthat/test-ingest-{federation}-{sport}.R` —
either a real-network test (skip in CI) or an HTML/XLSX fixture-driven test.

## Step 5: (If betting) populate `team_names`

The `team_names` invariant is **load-bearing**: a recommendation with a team
not keyed in `lengjan.team_names` will fail
`validate_team_names_config()` pre-flight, before the placer opens a Chrome
session.

Direction: `{canonical_pipeline_name: lengjan_display_name}`.

To populate: after a first odds scrape (`Rscript scripts/02_scrape_odds.R --league {key}`)
the canonical names are visible in `data/facts/odds/`; the Lengjan display
names are in the same rows. Diff them and add any rows where they differ.

> Known limitation: the schema is sex-agnostic. Teams that appear in both
> male and female schedules under different Lengjan names (e.g. `Fram` /
> `Fram kv`) cannot be represented in a single map. For now, leave shared
> teams out and document in a comment — they'll error out at validate time
> with a clear "missing team_names for: …" message. See
> [project_team_names_schema](../../../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_team_names_schema.md).

## Step 6: Run the data + fit pipeline

Ingest first (cheap, tests the scraper):

```bash
Rscript scripts/00_active_competitions.R
Rscript scripts/01_ingest_results.R --league {key}
Rscript scripts/02_scrape_odds.R --league {key}
```

Verify Parquet writes:

```bash
Rscript -e 'sports::rebuild_duckdb(); con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE); print(DBI::dbGetQuery(con, "SELECT sport, country, COUNT(*) FROM results GROUP BY 1,2"))'
```

Then a smoke-test fit:

```bash
# Detached (recommended for any fit):
LOG=/tmp/fit-{key}.log
nohup Rscript scripts/03_fit.R --league {key} --force > "$LOG" 2>&1 & disown
echo "PID $! — log: $LOG"
```

## Step 7: (Optional) wire up publish

Football iceland has the full publisher (11–12 JSONs per cell across
`publish_divisions`, driven by `config/leagues.yml::football_iceland.publish_divisions`
— see [project_publish_divisions_config](../../../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_publish_divisions_config.md));
basketball + handball are scaffolds (meta + next_games only). To add full
publishing for a new league, mirror `R/publish-football-iceland.R` under a new
file and register it in `R/publish-pipeline.R::publish_one()`. New cells for an
existing league require adding entries under `publish_divisions` _and_ a
matching `DIVISIONS` entry in `metill-platform/app/routes/ithrottir.py` —
forgetting the consumer side produces a 404 with no signal.

If only the scaffold is wanted (typical for new leagues until the
metill-platform page is designed), no code change is needed — the
dispatcher uses sport-level routing.

## Checklist

- [ ] `config/leagues.yml` entry parses (`load_leagues()` succeeds)
- [ ] `Stan/{league_key}/{model}.stan` exists and matches `stan_model:` path
- [ ] (If new federation) `R/ingest-{federation}-{sport}.R` written + registered
- [ ] (If new federation) ingest test added
- [ ] `lengjan.team_names` populated for any teams that appear in odds scrape
- [ ] Dry-run lists expected targets
- [ ] `scripts/01_ingest_results.R --league {key}` succeeds (results in Parquet)
- [ ] `scripts/02_scrape_odds.R --league {key}` succeeds (odds rows in Parquet)
- [ ] First fit converges (no divergences, ESS > 400, R̂ < 1.01)
- [ ] (If betting) `scripts/04_decide.R --league {key}` produces non-zero
      recommendations or a clear "no candidates passed EV threshold" message

## Reference

- Single source of truth: `config/leagues.yml` (validated against
  `config/leagues.schema.json`)
- Ingest dispatcher: `R/ingest.R::ingest_league()`
- Storage schemas: `R/storage-schemas.R`
- Stan conventions: `.claude/rules/stan-conventions.md`
- Model + decide layers: `.claude/rules/model-decide.md`
- Publish layer: `.claude/rules/publish-layer.md`
- Betting conventions: `.claude/rules/sports-betting.md`
- CI conventions: `.claude/rules/ci-conventions.md`
