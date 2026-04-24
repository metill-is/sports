# sports

Bayesian sports-prediction and automated-betting monorepo for Icelandic football, basketball, and handball.

**Status:** mid-migration. See [`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md) for the end-state design and [`docs/superpowers/plans/`](docs/superpowers/plans/) for the implementation plans.

## Local-only subsystem

`R/placer/` places bets against Lengjan (games.lotto.is) using credentials in `.Renviron` (see `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret is configured for it.

## Layout

- `config/leagues.yml` — single source of truth for league metadata
- `R/` — package source (storage, config, model, decide, placer, publish, research — added across Plans 1–4)
- `Stan/` — per-league Stan models (populated in Plan 2)
- `data/` — Parquet stores (facts, beliefs, decisions, publish), hive-partitioned
- `scripts/etl/` — one-time migration of legacy CSVs to Parquet
- `_legacy/` — subtree-merged histories of the four predecessor repos

## Development

```bash
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'
```
