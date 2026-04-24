---
name: add-league
description: Use when adding a new league to the pipeline. Guides through config, directory setup, team mappings, and verification.
argument-hint: "[sport] [country]"
context: fork
effort: high
---

# /add-league — Add a new league

You are adding a new league to the Sports unified pipeline. Follow this checklist.

## Step 1: Gather information

If not provided in arguments, ask the user:

1. **Sport**: basketball, handball, or football
2. **Country**: lowercase name (e.g., `romania`, `croatia`)
3. **Pipeline type**: Infer from sport:
   - Basketball → `shared` (needs `config_module`)
   - Handball → `handball_other` (uses shared handball/other pipeline)
   - Football → `football` (self-contained per-league scripts)
4. **Data source**: Infer from sport:
   - Basketball → `baskethotel` (only Iceland) or manual
   - Handball → `livesport_handball`
   - Football → `livesport_football`
5. **Betting**: Will this league have a betting pipeline? (requires Lengjan odds coverage)
6. **Sex**: `[male]`, `[female]`, or `[male, female]`

## Step 2: Add to `leagues.yml`

Read `Sports/config/leagues.yml` and add a new entry:

```yaml
{sport}_{country}:
  sport: {sport}
  country: {country}
  dir: {sport}/{country}
  sex: [{sex}]
  stan_model: {model}  # 2d_student_t.stan for basketball/handball, bivariate_poisson_inflated_diagonal_corrmodel.stan for football
  pipeline: {pipeline_type}
  data_source: {data_source}
  has_bets: {true/false}
```

Place it in the correct section (grouped by sport, betting/no-betting).

## Step 3: Create league directory

```bash
cd /Users/brynjolfurjonsson/sports/Sports

# Create directory structure
mkdir -p {sport}/{country}
touch {sport}/{country}/.here    # CRITICAL: here::here() needs this

# If betting is enabled:
mkdir -p {sport}/{country}/config
mkdir -p {sport}/{country}/history
mkdir -p {sport}/{country}/R
```

### Create `.here` marker

The `.here` file must exist — without it, `here::here()` resolves to `Sports/` instead of the league directory.

## Step 4: Create `config/bets.yml` (if betting)

Only if `has_bets: true`. Use this template, adjusting for sport:

```yaml
sport: "{sport}"
country: "{country}"
sex: [{ sex }]

bankroll:
  kelly_frac: 0.10 # Start conservative for new leagues
  kelly_frac_male: 0.10 # Optional per-sex override (omit if you don't need it)
  kelly_frac_female: 0.10 # Optional per-sex override (omit if you don't need it)
  max_match_kelly: 1.0 # Stage 1 per-match ceiling (keep 1.0 unless you have a reason)
  ev_threshold: 0.00 # Min EV edge to enter optimiser
  min_bet_amount: 200
  bet_digits: 0
  currency: "kr"
  # NOTE: do NOT add cur_pool — current pool is computed from Sports/config/bankroll.yml::initial_pool

scoring:
  has_ties: { true for handball/football, false for basketball }
  tie_threshold: { 0.5 for handball, 0 for football, 0 for basketball }

markets:
  outcome: true
  handicap: { true for football, false for handball }
  totals: true

predictions:
  path: "results"
  max_age_hours: 48

odds:
  source: "lengjan-odds"
  lengjan_odds_path: "../../../lengjan-odds/data/{sport}_{country}"
  booker: "Lengjan"

deduplication:
  use_gsheets: false

history:
  enabled: true
  path: "history"
```

## Step 5: Create `R/run_bets.R` (if betting)

```r
box::use(../../../R/bets/run[run_betting_pipeline])
cfg <- yaml::read_yaml(here::here("config", "bets.yml"))
run_betting_pipeline(cfg)
```

Adjust the `../../../` path based on directory depth from `Sports/R/bets/`.

## Step 6: Create team name mapping (if Lengjan odds)

If using `lengjan-odds` source, create:
`~/sports/lengjan-odds/config/team_names_{sport}_{country}.csv`

```csv
out, in
pipeline_name, lengjan_dom_name
```

**Column convention**: `out` = pipeline/model team name, `in` = name as it appears on Lengjan's DOM. Accepted alternatives: `pipeline` / `lengjan` (newer style). No other column names are recognised — `resolve_match_ids` silently produces an empty lookup if the header is wrong.

Populate by comparing Lengjan team names (`lengjan-odds/data/{sport}_{country}/odds_1x2.csv` after the first scrape) with model team names (`Sports/{sport}/{country}/data/{sex}/schedule.csv`).

**Required even if no names differ today** — promotion/relegation introduces new teams mid-season, and bet placement fails silently the first time a mismatched name appears.

## Step 7: Handball-specific: create symlinks

For `handball_other` pipeline, create symlinks so the shared code can find data:

```bash
cd /Users/brynjolfurjonsson/sports/Sports/handball/other
mkdir -p data results    # parents only, not per-country subdirs
ln -sf ../../{country}/data data/{country}
ln -sf ../../{country}/results results/{country}

# If betting:
mkdir -p odds           # parent only, NOT odds/{country}
ln -sf ../../{country}/odds odds/{country}
```

Symlink relative paths are **two levels up**, not three. From `handball/other/odds/{country}`, the target `../../{country}/odds` resolves to `handball/{country}/odds`. Verify with `ls -l handball/other/odds/` — existing entries should look like `denmark -> ../../denmark/odds`.

These symlinks let `handball/other/R/` code access per-country data via `here("data", country, ...)`.

## Step 8: Add to lengjan-odds (if scraping odds)

If this sport/country needs odds scraped:

1. Add competition to `lengjan-odds/config/competitions.yml` **with a `team_names:` key pointing at the CSV from Step 6**:

   ```yaml
   {sport}_{country}:
     sport: <id>               # 1=Football, 2=Basketball, 6=Handball
     country: "<code>"
     team_names: "team_names_{sport}_{country}.csv"   # REQUIRED — both scraper and lengjan-bets read this key
     leagues:
       League Name:
         competition: "<id>"
   ```

   **Critical invariant**: the `team_names:` key is what activates the CSV. A CSV on disk without this key is dead code — `lengjan-bets` cannot resolve match IDs and bet placement fails with `no_match_id`. Older CLAUDE.md copy calls this "optional" — that's true for the scraper but not for bet placement.

2. Update `active_competitions.json` if using schedule-aware scraping

## Step 9: Verify with dry-run

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript run.R --league {sport}_{country} --dry-run
```

This should show the league in the plan without errors.

## Step 10: Test data step

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript run.R --league {sport}_{country} --step data
```

If data download succeeds, the league is correctly wired.

## Checklist summary

- [ ] `leagues.yml` entry added
- [ ] Directory created with `.here` marker
- [ ] `config/bets.yml` created (if betting)
- [ ] `R/run_bets.R` created (if betting)
- [ ] `history/` directory created (if betting)
- [ ] Team name mapping CSV created (if Lengjan)
- [ ] Symlinks in `handball/other/` (if handball)
- [ ] `lengjan-odds` competition config (if scraping)
- [ ] Dry-run passes
- [ ] Data step works
