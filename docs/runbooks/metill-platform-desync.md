# Runbook: metill-platform desync

**Symptom.** fly.metill.is shows stale or missing football data even though
`data/publish/football/iceland/.../*.json` in this repo is fresh.

## Diagnose

The consumer repo `metill-is/metill-platform` runs `pull-sports-data.yml`
7×/day (see *Cadence and propagation* below): it clones this repo, rsyncs `data/publish/` into `data/ithrottir/`,
validates the JSON (`scripts/validate_publish.py`), commits if changed, and a
push triggers Fly.io auto-deploy.

1. Did the latest pull run? `gh run list --repo metill-is/metill-platform --workflow pull-sports-data.yml --limit 6`.
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

## Cadence and propagation

Moved from CLAUDE.md on 2026-09-23.

The `metill-is/metill-platform` repo runs `pull-sports-data.yml` **7×/day**
(`25 7-12,19 * * *` — clustered on 07–12 UTC where real change lands, cut from
hourly on 2026-09-02): clones `metill-is/sports`, rsyncs `data/publish/` into
`data/ithrottir/`, commits **if the change is semantic**. Since 2026-09-02 it
gates the commit on `scripts/ci_semantic_diff.py`, which ignores pure build
stamps (`generated_at`) — so a republish that moves no number no longer commits
or deploys. A push to metill-platform triggers Fly.io auto-deploy.

Sports-side workflow:
1. decide-publish.yml writes data/publish/{...}/*.json
2. metill-platform's pull-sports-data.yml **polls** and picks it up on its
   next slot (it is not triggered by this push) — so expect up to a few hours'
   propagation in-window, not seconds. `gh workflow run pull-sports-data.yml
   --repo metill-is/metill-platform --ref main` forces it immediately.
3. metill-platform's commit deploys to fly.metill.is
