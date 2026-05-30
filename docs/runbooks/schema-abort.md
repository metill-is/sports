# Runbook: schema / value validation abort

**Symptom.** A workflow is red with one of:
- `Value validation failed for table "..."` (storage value guard, 2026-05-30)
- `Schema validation failed for table "..."` (Arrow type guard)
- a publish JSON schema failure from `validate_publish_dir()`

## Diagnose

The error names the offending location:

- **Value guard** (`R/storage-validate.R`) rejects impossible *values* that are
  structurally valid int32/float64: a negative or absurd score (above the
  per-sport cap), `odds <= 1` / non-finite, or a recommendation `p` outside
  `[0, 1]`. The message names the table, column, and first offending value.
- **Arrow type guard** (`validate_against_schema`) rejects a wrong column type
  or a missing column.
- **Publish schema** rejects a JSON that does not match
  `config/publish-schemas/<sport>/<file>.schema.json`.

## Fix

- Real record-breaking score that tripped the cap: bump the relevant value in
  `R/storage-validate.R::score_caps()` with a `# WHY` note (the maintainer is a
  statistician — sanity-check the bound), then re-run.
- Scraper glitch (a flipped/garbage score, a 0.x odds): the value guard did its
  job — fix the scraper (`R/ingest-*.R`) so the bad row never lands, then
  re-ingest.
- Publish schema mismatch: fix the publisher (`R/publish-*.R`) or, if the
  contract genuinely changed, update the schema **and** the metill-platform
  mirror (`scripts/validate_publish.py`) together.

## Verify

Re-run the failing step; the write / publish completes. `devtools::test()` for
the affected layer stays green.
