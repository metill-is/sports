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

1. **Check the seasonal pause first.** Only `football_iceland` is active right
   now; `basketball_iceland` / `handball_iceland` are off-season until autumn
   2026 and produce no `odds_freshness` row at all (no upcoming fixture), which
   is correct, not a fault.
2. **Is it just a lull?** A `WARN` with "next fixture in 2–3d, no upcoming odds
   scraped" during a between-match gap is benign and self-heals when Lengjan
   posts. Confirm the next fixture really is a few days out
   (`data/facts/schedules/`) — if so, stop here.
3. **Did the odds scrape run?** `gh run list --repo metill-is/sports --workflow scrape-odds.yml --limit 6`.
   The scrape **exits clean** when in-season leagues yield 0 rows (a between-rounds
   gap is benign — staleness escalation is owned by this check, not the scrape).
   So a green scrape that logged "wrote 0 rows … likely between rounds" is
   expected during a lull; only a *red* scrape run signals a scraper fault.
4. **Real stall (`FAIL`, fixture today, no odds):** inspect the latest scrape
   run's logs for timeouts ("could not parse", chromote errors) vs a Lengjan
   markup change, and confirm the fixture is one Lengjan actually prices.

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
