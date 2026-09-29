# Runbook: unmapped Lengjan team name

**Symptom.** `pipeline_health()` reports `unmapped_team_names` `WARN` or `FAIL`
for a league. Lengjan is pricing a team under a display name that
`config/leagues.yml::<league>.lengjan.team_names` has no rendering for, so
`normalise_lengjan_team_names()` cannot rewrite it to the canonical
(federation) name, the decide-layer join against `beliefs_latest` finds
nothing, and `decide_league()` warn-and-skips every fixture involving that
team. Those fixtures are **silently unbettable**.

## Why this is a health check and not a test

`config/leagues.yml` deliberately expects unmapped names to appear:

> further LD3 sides will surface unmapped as their odds first appear -- fill
> them opportunistically

A new side entering a division we scrape is routine and harmless. A build-time
assertion would be flaky. So severity is set by whether we already **model** the
team:

| status | meaning | action |
|---|---|---|
| `OK` | every Lengjan name in the last `unmapped_window_days` (14) has a rendering | none |
| `WARN` | an unmapped name, but no `beliefs_latest` fixture identifies it — a genuinely new or unmodelled side | fill in opportunistically; a wrong guess fails safe (decide warn-and-skips, no mis-placed bet) |
| `FAIL` | an unmapped name that **does** resolve to a canonical we hold a fitted model for | fix now — every bet on that team is being lost |

## Diagnose

The `value` string already names the fix. A `FAIL` reads:

```
1 unmapped: Vikingur Reykjavik (2026-08-23..2026-09-02, 3 fixtures)
  -> modelled as "Vikingur R." (team_names.male)
```

That is: the Lengjan rendering, the scraped-date span it has been appearing
over, how many fixtures it touches, and the exact `team_names` sub-map plus
canonical key the rendering belongs under.

The resolution comes from `R/health.R::.identify_unmapped_canonical()`: for a
fixture carrying the unmapped name, it inverts the fixture's *already-mapped*
opponent to canonical and looks in `data/beliefs/latest` for the same
`match_date` with that opponent on the same side. The counterpart's name is the
canonical. No hit means no such fixture is modelled at all.

To see the raw evidence:

```bash
Rscript -e '
suppressMessages(devtools::load_all())
od <- read_table("odds", filter = list(sport = "football", country = "iceland"))
od$scraped_date <- as.Date(od$scraped_date)
nm <- "<the unmapped rendering>"
print(unique(od[od$home_team == nm | od$away_team == nm,
                c("scraped_date", "match_date", "home_team", "away_team")]))
'
```

A **rename** shows as a clean handover — the old rendering stops on the same
day the new one starts. The 2026-07-17 case:

```
Vikingur Rvk         first=2026-04-15  last=2026-07-17
Vikingur Reykjavik   first=2026-07-17  last=2026-09-02
```

## Fix

Add the new rendering to the canonical entry in `config/leagues.yml`. Where a
team has more than one acceptable rendering, the value becomes a **list**, with
the **primary first** — the primary is what the placer types into the bet slip,
so it must be the rendering Lengjan currently shows:

```yaml
    team_names:
      male:
        Vikingur R.:
          - Vikingur Reykjavik   # current, since 2026-07-17
          - Vikingur Rvk         # pre-rename; kept so historical odds still join
```

Keep the retired rendering. `tn_renderings()` points every rendering at the one
canonical, so old odds partitions keep joining, and a revert on Lengjan's side
needs no further edit. Do **not** create a second canonical entry for the new
rendering: `check_team_names_injective()` will abort `load_leagues()` because
two canonicals would claim overlapping renderings.

## Verify

```bash
Rscript -e '
suppressMessages(devtools::load_all())
res <- check_unmapped_team_names(load_leagues(), here::here("data"),
                                 Sys.time(), health_thresholds())
print(as.data.frame(res))
'
```

The row should read `OK`. Then re-run the decide layer and confirm the fixtures
now produce candidates:

```bash
Rscript scripts/04_decide.R --league football_iceland
```

The check is self-clearing off the live config, so no state file needs
resetting. A retired rendering ages out of the window on its own once Lengjan
stops using it.

## Related

- `.claude/rules/model-decide.md` — team-name normalisation in the decide layer
- `.claude/rules/sports-betting.md` — `no_match_id` (the placer-side twin of
  this fault, caught by `validate_team_names_config()` pre-flight)
- [stale-odds.md](stale-odds.md) — the other reason recommendations go missing
