# Runbook: metill-platform desync

**Symptom.** fly.metill.is shows stale or missing football data even though
`data/publish/football/iceland/.../*.json` in this repo is fresh.

## Diagnose

The consumer repo `metill-is/metill-platform` runs `pull-sports-data.yml`
hourly: it clones this repo, rsyncs `data/publish/` into `data/ithrottir/`,
validates the JSON (`scripts/validate_publish.py`), commits if changed, and a
push triggers Fly.io auto-deploy.

1. Did the hourly pull run? `gh run list --repo metill-is/metill-platform --workflow pull-sports-data.yml --limit 6`.
2. Did its publish-schema validation fail (stops before deploy)? Check the logs.
3. **New division/cell?** A freshly added `publish_divisions` cell renders 404
   with no error signal unless the consumer's `DIVISIONS` dict + sitemap were
   updated too — the cross-repo sync the publish layer warns about.

## Fix

- Force a refresh without waiting for cron:
  `gh workflow run pull-sports-data.yml --repo metill-is/metill-platform`.
- New cell: add the matching `DIVISIONS[<slug>]` entry in
  `metill-platform/app/routes/ithrottir.py` and the sitemap row in
  `app/routes/pages.py` (see `.claude/rules/publish-layer.md` -> "To add a new
  cell").

## Verify

The platform's `pull-sports-data.yml` run is green and fly.metill.is renders the
fresh data.
