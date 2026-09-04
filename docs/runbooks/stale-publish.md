# Runbook: stale or missing publish output

**Symptom.** `publish_freshness` FAIL for one or more cells, or
`publish_format` WARN. Run `/pipeline-doctor` or
`Rscript scripts/07_healthcheck.R` to see the rows.

The check emits one row per (league, sex, division) in `publish_divisions` for
every active league. `PAUSED` means the cell has no upcoming fixture and is
genuinely off-season -- not a fault.

## Diagnose

Take these in order. The first step is the one that would have caught B4.

1. **Is there an extract partition at all?**

   ```bash
   ls data/beliefs/extracts/
   ls data/beliefs/extracts/sport=<sport>/country=iceland/sex=<sex>/
   ```

   No `sport=<s>` partition for the failing cell means the publisher had
   nothing to read: the fit ran but the extractor did not, or neither ran.
   The FAIL row says so in its value. This is the state basketball and handball
   were in from the Plan-7 cutover to 2026-09 -- publishing nothing, with a
   green pipeline, because no composed check read `data/publish/`.

2. **Are you on a branch that is behind `main`?** Check this BEFORE touching a
   threshold. `data/publish/` is committed by CI roughly four times a day, so a
   feature branch a couple of days old shows every football cell as stale for
   branch reasons, not pipeline reasons.

   ```bash
   git -C ~/sports fetch origin
   git -C ~/sports log --oneline -1              -- data/publish/
   git -C ~/sports log --oneline -1 origin/main  -- data/publish/
   ```

   Measured 2026-09-04 on `feat/bb-hb-metill-parity`: the branch tree was
   commit `39bfbfe8f` (2026-09-02T22:27Z) while `origin/main` was at
   `d3d57c526` (2026-09-04T17:33Z) -- all nine football cells FAILed at "45h
   old" and every one of them was a branch artefact. **Tuning
   `publish_max_age_hours` to silence that disarms the check.**

3. **Did the publish step actually run?**

   ```bash
   gh run list --workflow decide-publish.yml --limit 6
   ```

   `scripts/05_publish.R` exits non-zero when ANY target failed, so a red run
   means at least one cell aborted -- read which from the printed `failed`
   frame. The other cells still published; per-target isolation is deliberate.

4. **Missing artefact rather than stale?** The value names the missing
   basenames. Compare against `sport_publish_profile(<sport>)$surfaces`; note
   that surface list is NOT a file list (five football entries are payload
   features, and `cup_bracket` is `bracket.json`).

5. **`publish_format` WARN.** The value carries BOTH numbers -- the published
   `n_rounds` and the one derived from the division's configured
   `expected_meetings`. A disagreement means the federation changed the
   competition format. Re-measure from `data/facts/results` and update
   `config/leagues.yml`; do not loosen the check.

## Fix

- Extract partition missing: re-run the fit for that cell
  (`Rscript scripts/03_fit.R --league <key> --sex <sex>`), which writes the
  extracts the publisher reads. See [failed-fit.md](failed-fit.md) if it aborts.
- Partition present, publish stale: `Rscript scripts/05_publish.R --league <key>`.
- Schema abort during publish: [schema-abort.md](schema-abort.md).
- Branch artefact: sync the branch (`/sync-main`). Nothing to fix in the
  pipeline.

## Verify

```bash
Rscript scripts/07_healthcheck.R
```

The cell's `publish_freshness` row reads `OK` with a fresh age and the expected
artefact count. `PAUSED` is also a pass for an off-season cell.
