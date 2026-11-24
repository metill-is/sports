# Runbook: EURO 2028 qualifying odds on draw night

**When.** The qualifying draw is on Sunday 6 December 2026, 18:00 CET (17:00 Belfast), at ICC Belfast; the technical part takes about 40 minutes. UEFA publishes the twelve groups at the end; its fixture list follows "after the draw" (regulations, Article 24.03), possibly days later. Neither is needed in advance: odds come from the groups alone.

**What it produces.** For all 54 entrants: win the group, finish second, qualify directly, take a reserved host place (Article 14.04), enter the March 2028 play-offs (as a runner-up or via the Nations League), and qualify at all; per team and per group slot ("QF1 winner"), under both readings of Article 15 d), with how often the readings differ.

**Cost (measured 2026-11-24).** Fit: 78 s plus 11 s of draws on one thread (`ml run`). Odds: 62 s and 1.44 GB at 20,000 simulations for both readings (`ml light`), once the 54 rows are typed.

## Before the night (by 5 December)

1. **Facts unchanged?** Re-read UEFA's draw procedure (link in `data/nt/euro2028/README.md`). If the pots, the quarter-final pairings or the draw conditions moved, edit `data/nt/euro2028/*.csv` and run `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-nt-euro2028.R")'`.

2. **Fit current?** No international window falls between 17 November 2026 and March 2027, so the fit to 17 November stands unless martj42 corrected a result. Compare martj42's head with the fit's:

   ```bash
   git ls-remote https://github.com/martj42/international_results.git HEAD
   ```

   ```bash
   Rscript -e 'readRDS("/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/fit/nt_fit_2026-11-18.rds")$source$martj42_commit'
   ```

   The fit of 24 November used `ca84241bb3eed854a1956a2fab442fe4daad40dd`. If the heads differ, or the fit file is gone (`~/.claude-reviews/` has no backup), refit from that commit's file (or the new head's):

   ```bash
   curl -sSL -o /Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/data/results.csv https://raw.githubusercontent.com/martj42/international_results/ca84241bb3eed854a1956a2fab442fe4daad40dd/results.csv
   ```

   ```bash
   LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 ml run -- Rscript scripts/nt/euro2028_fit.R /Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/data/results.csv /Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/fit/nt_fit_2026-11-18.rds source=ca84241bb3eed854a1956a2fab442fe4daad40dd
   ```

   The fit is repeatable on one machine (one thread, R-INLA's online optimisation off, seeded draws).

3. **Rehearse** on a simulated draw typed the way UEFA prints it (`/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/rehearsal/groups.csv`, made 2026-11-24):

   ```bash
   LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 ml light -- Rscript scripts/nt/euro2028_odds.R fit=/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/fit/nt_fit_2026-11-18.rds groups=/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/rehearsal/groups.csv out=/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/rehearsal/odds
   ```

## On the night

1. **Type the groups** into `data/nt/euro2028/groups.csv`, one row per team, in UEFA's spelling, from UEFA's draw page:

   ```csv
   group,pot,team
   A,1,Spain/France winner
   A,2,Türkiye
   G,1,QF3 defeated
   ```

   - Groups A-F have four teams (pots 1-4), G-L five (pots 1-5). The `pot` column is optional; when present it is checked against the seeding and catches a team typed in the wrong row.
   - The eight Nations League quarter-finalists are drawn as placeholders. Write them by pairing (`Spain/France winner`, `Norway/England defeated team`, in either order; `loser` also works) or by number (`QF1 winner`, numbered as in `data/nt/euro2028/quarter_finals.csv`: 1 Spain/France, 2 Norway/England, 3 Netherlands/Portugal, 4 Belgium/Germany). UEFA's own numbering may differ, so the pairing form is safer; a label giving both (`QF1 winners (Netherlands/Portugal)`) is refused when the two disagree.
   - UEFA spellings Türkiye, Czechia and Bosnia-Herzegovina map to the results file's names; the results file's own spelling also works.

2. **Run the odds:**

   ```bash
   LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 ml light -- Rscript scripts/nt/euro2028_odds.R fit=/Users/brynjolfurjonsson/.claude-reviews/euro2028-draw/fit/nt_fit_2026-11-18.rds out=docs/reports/2026-12-06-euro2028-draw
   ```

   - An error naming teams means a typing error: a team missing, repeated, unknown or in the wrong pot, or a quarter-finalist typed as itself instead of as a placeholder.
   - A **warning** that the groups break draw conditions means the typed groups cannot be UEFA's draw (a team in the wrong group, most likely). Check the named groups against UEFA's page. If the file is right, UEFA changed a condition: update `data/nt/euro2028/draw_conditions.csv`. One exception: a winter warning that counts a Norway/England placeholder, or a host warning that counts an England placeholder, may be UEFA reading those conditions differently from us (the procedure does not say how they apply to placeholders); the odds are still computed, so note it and carry on.

3. **Read `odds.md`** in the output folder. It starts with Iceland, then has one table per group, the quarter-finalists, the reserved host places and play-off format, and the **Article 15 d) comparison**: how often the "repeat" (published explanations) and "once" (the regulation's literal wording) readings change a group table, the direct qualifiers or the play-off field. Which reading to use is the owner's decision; the default is "repeat". `teams.csv`, `teams_once.csv` and `slots.csv` hold the numbers; `odds.rds` holds them for later steps.

4. **Commit** `data/nt/euro2028/groups.csv` and the output folder on a branch. Publishing on metill.is needs a publish contract and the owner's approval; neither exists yet.

5. **When UEFA publishes the fixture list**, save it as `data/nt/euro2028/fixtures_2027.csv` (`group,match_date,home_team,away_team,venue`, with `venue` `home` or `neutral`, placeholders as above) and rerun step 2. The script uses that file whenever it exists. Venues matter (home advantage); dates do not.

## What the odds assume

- Strengths are the fit's posterior at its cutoff; they do not move between windows. Refit after each window (March, June, September, October, November 2027).
- Each Nations League quarter-final is two legs, extra time in the second leg at a third of that leg's rate, then a coin-toss shootout. The same applies to two-legged play-off ties; single-leg play-off matches use extra time and a shootout too.
- Disciplinary points (Article 15 j, Article 23) are not modelled: a random draw settles ties that reach them.
- Without a fixture list, Belarus, Ukraine and Israel play home matches at neutral venues (UEFA decision for the first two; every competitive Israel home match since June 2025 was in Hungary). All other home matches are at home.
- The play-off draw keeps hosts apart and Northern Ireland away from hosts when possible (Article 16.03), each such allocation equally likely. Further conditions the UEFA Executive Committee may add are not modelled.
- Code: `R/nt-euro2028.R` (facts, loader, host places, play-offs), `R/nt-euro2028-draw.R` (draw simulator and condition check), `R/nt-qualifying.R` (groups, Article 15 and 23 rankings).

## Before the draw: what the draw itself may bring

`scripts/nt/euro2028_draw_mc.R` simulates the draw under the published procedure and conditions, then qualifying per draw: odds averaged over the draw, Iceland's likely groups and opponents, and the Article 15 d) comparison across every simulated campaign. Heavy (about 3 s per draw at 1,000 simulations): `ml run`. The run of 24 November is recorded in `docs/reports/2026-11-24-euro2028-draw-readiness.md`.
