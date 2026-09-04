# Runbook: stale odds

**Symptom.** `pipeline_health()` reports `odds_freshness` `FAIL` (or `WARN`) for
football_iceland, and/or fly.metill.is shows stale recommendations.

`odds_freshness` is **match-proximity-aware** (not a flat staleness clock).
Lengjan posts Icelandic-football odds with a ~2-day median lead, and routine
between-match gaps run 2–3 days, so absolute age is meaningless on its own.
The check escalates only relative to the next fixture (`odds_lead_days = 3`):

| state | meaning |
|---|---|
| `OK` | odds cover a fixture on/after today, **or** the next fixture is > `odds_lead_days` away (a benign lull — Lengjan simply hasn't posted yet), **or** the only upcoming fixtures sit in (sex, division) cells Lengjan has never priced |
| `WARN` | next fixture is within `odds_lead_days` but no upcoming odds are scraped yet (approaching; usually self-heals when Lengjan posts) |
| `FAIL` | a fixture is **today** and no odds covering today-or-later were scraped — a genuine pipeline stall |

So a `WARN` is normally not actionable; a `FAIL` means a fixture is upon us with
no odds at all.

The expectation is **division-scoped** (since 2026-06-10): KSÍ schedules span
divisions Lengjan never prices — the lone 4. deild fixture on 2026-06-09
false-FAILed the check all day and alert-emailed twice. A fixture only carries
an odds expectation when its (sex, division) cell has ever produced a
decide-layer candidate (`R/health.R::.covered_divisions`; candidates rather
than raw odds because odds rows carry Lengjan display names, candidates are
post-join canonical). With no candidate history at all, the check
conservatively expects odds everywhere (cold start). Note the healthcheck cron
also moved 07:00 → 12:00 UTC the same day: Lengjan sometimes posts same-day
odds late morning, so the pre-scrape slot false-FAILed on match days.

## Diagnose

1. **Check whether the season has actually started.** Basketball and handball
   resumed for 2026/27 (Olisdeild opened early September, Bonusdeild opens
   2026-09-29/30 for three of four cells and 2026-10-08 for Bonusdeild karla).
   They are no longer on seasonal pause, and both are configured
   `betting.enabled: false` -- so they produce **no odds rows at all, by
   design**, and `odds_freshness` has nothing to say about them. An absent
   odds row for basketball or handball is correct, not a fault. Only
   `football_iceland` is bet.

## Fix

- **On the placer host (local), run the scrape directly** — it's the faster,
  more deterministic fix: `Rscript scripts/02_scrape_odds.R`, then commit + push
  the refreshed `data/facts/odds/`. Re-dispatching the workflow can take ~30 min
  on a cold CI cache (the V8/chromote rebuild in `ci-conventions.md`), whereas
  the local scrape runs in seconds against a warm toolchain.
- Transient (timeout / Lengjan briefly down) or to confirm a lull is self-healing
  when you're *not* on the placer host: re-run
  `gh workflow run scrape-odds.yml --repo metill-is/sports` and check it
  returns a non-zero row count.
- Lengjan markup changed: the DOM odds parser
  (`parse_actual_odds_from_dom()` in `R/placer-place.R`) or
  `R/ingest-lengjan-odds.R` selectors need updating; fixtures live in
  `tests/testthat/test-placer-place.R`. Local-only — never wire the placer into CI.

## Verify

`Rscript scripts/07_healthcheck.R` -> `odds_freshness` is `OK` (odds now cover an
upcoming fixture). The next `decide-publish.yml` run republishes fresh JSONs.
