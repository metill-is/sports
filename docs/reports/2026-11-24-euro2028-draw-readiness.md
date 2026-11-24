# EURO 2028 qualifying: ready for the draw of 6 December 2026

Branch `claude/serene-neumann-dabd96`, 24 November 2026. The qualifying simulator of 22 November (PR metill-is/sports#110) now takes UEFA's drawn groups as they will be published, resolves the Nations League quarter-final placeholders, applies the reserved host places and plays the play-offs, so every entrant's qualification odds can be produced within minutes of the draw. The draw-night commands are in [`docs/runbooks/euro2028-draw-night.md`](../runbooks/euro2028-draw-night.md).

## Sources, read 2026-11-24

| Source | Used for |
| --- | --- |
| UEFA, *Qualifying draw procedure, European Qualifiers 2026-28* (PDF created 17 Nov 2026) | Pots and the interim overall 2026/27 Nations League ranking (the pot list's 1-54 numbering), draw conditions, procedure |
| *Regulations of the UEFA European Football Championship 2026-28*, Articles 13.02, 14, 16, 17, 22, 23 (documents.uefa.com, per-article pages; the `-2026-28-Online` address works) | Reserved host places, play-off field, draw and matches, extra time, overall ranking |
| UEFA.com, 20 Nov 2026 | Nations League quarter-final ties (two legs, the second-named side at home in the second leg) and every league-phase group placing |

The facts are in `data/nt/euro2028/` with a README citing each. Three points sharpen the brief: the excessive-travel rule allows **one** such pair per group (it is not a ban); each group of five holds at most one quarter-final defeated placeholder or League A/B or B/C play-off team, and with 16 play-off teams and 4 placeholders this condition binds hard; Belarus and Ukraine play home matches on neutral ground (the procedure says so; the loader also treats Israel that way, as every competitive Israel home match since June 2025 was in Hungary).

## What was built

| Brief item | Where | Status |
| --- | --- | --- |
| 1. Input format and loader | `nt_read_euro2028_draw()`, `nt_euro2028_draw()`; `groups.csv` (`group,pot,team`), `fixtures_2027.csv` (`venue` as home/neutral or 1/0); the Euro 2024 fixture now stores `venue` | Done |
| 2. UEFA-to-martj42 names, every drawn team in the fit | `nt_uefa_to_martj42()` (Türkiye, Czechia, Bosnia-Herzegovina and variants); the loader aborts on a team absent from the fit, missing, repeated or in the wrong pot, and warns when the groups break a draw condition | Done |
| 3. Quarter-final placeholders | `nt_simulate_qualifying(quarter_finals = )`: each tie simulated per posterior draw as two legs (`nt_simulate_two_legged()`, extra time at the end of the second leg) | Done |
| 4. Reserved host places; play-offs | `nt_simulate_euro_playoffs()`: Article 14.04, then Article 16 (8 or 12 teams, Nations League selection including the League D and ranking steps, pots, host and Northern Ireland separation) and Articles 17 and 22 (single-leg paths, home-and-away ties) | Done: the regulations were readable |
| 5. Article 15 d) measured | `nt_compare_h2h_readings()`; run on 500 simulated draws below and on the real groups on draw night | Done: no difference in 500,000 simulated campaigns; the reading is the owner's call |
| 6. Monte Carlo of the draw | `nt_simulate_uefa_draw()` (the published procedure with an exact lookahead), `nt_check_uefa_draw()`; `scripts/nt/euro2028_draw_mc.R` | Done |
| 7. Draw-night runbook | `docs/runbooks/euro2028-draw-night.md`, `scripts/nt/euro2028_odds.R`, `scripts/nt/euro2028_fit.R` | Done, rehearsed |

Without placeholders the qualifying simulator gives bit-identical outcomes to the code of PR metill-is/sports#110 (a 54-team, 2,000-simulation run under both readings), and the Euro 2024 validation still reproduces all ten tables and the overall order.

## The fit

The owner-approved v2 specification (12-year window, quarterly grid, estimated walk, pooled venue effects, no friendly term), now fitted repeatably (`nt_fit_inla(reproducible = TRUE)`: one thread, R-INLA's online optimisation off, seeded draws). martj42 at commit `ca84241bb3eed854a1956a2fab442fe4daad40dd`, matches before 18 November 2026: 11,337 matches, 217 teams, 78 s to fit and 11 s for 2,000 draws. Team-level sd 0.72, walk sd 0.0195 per square-root month, attack/defence split sd 0.10, in line with the 15 November report. Iceland rank 27th of the 54 entrants by posterior mean strength (Spain 1st, San Marino 54th). No international window falls between 17 November and March 2027, so this fit is the draw-night fit unless martj42 corrects a result.

## Draw-night rehearsal

One simulated draw, typed the way UEFA prints it (UEFA spellings, placeholders as `Belgium/Germany defeated team`; [`rehearsal/groups.csv`](2026-11-24-euro2028-draw-readiness/rehearsal/groups.csv), output in [`rehearsal/odds.md`](2026-11-24-euro2028-draw-readiness/rehearsal/odds.md)), through `scripts/nt/euro2028_odds.R` with the real fit: 62 s and 1.44 GB peak for 20,000 simulations under both readings (31 s and 1.32 GB at 10,000), identical output on a rerun. In that draw Iceland were in group B with Denmark, Austria and Moldova: group winner 7.7%, direct 25.9%, play-offs 73.7% (69.7% through the Nations League), **qualify 44.5%**.

## Before the draw: 500 simulated draws

`scripts/nt/euro2028_draw_mc.R`: 500 draws under the published procedure and conditions, then 1,000 qualifying campaigns per draw under both readings of Article 15 d), seed 2028. The draws took 60 s, the campaigns 1,173 s (2.3 s per draw). Output: [`draw_mc.md`](2026-11-24-euro2028-draw-readiness/draw_mc.md), with every entrant's odds averaged over the draw in `team_odds.csv`, Iceland's opponents in `focus_opponents.csv` and Iceland's odds in each draw in `focus_by_draw.csv`.

**Iceland** (pot 3), averaged over the draw: win the group 8.8%, qualify directly 29.7%, play-offs 67.7% (56.6% through the Nations League), **qualify 46.5%**. The draw moves that a good deal. Over the 500 draws the 10th to 90th percentile runs from 15.2% to 44.8% for direct qualification, and from 34.7% to 58.3% for qualifying at all.

- **Group size.** Iceland land in a group of five (G-L) in 71.6% of draws. The size barely matters: qualify 46.1% in a group of five, 47.4% in a group of four.
- **Likely pot-1 opponents.** The Norway/England defeated team (15.2% of draws), the Spain/France defeated team (12.6%), Italy (12.0%) and Greece (11.6%). Each quarter-final winner appears in only 2.2% to 4.4% of draws, because the winners go to groups A-F.
- **Pot 2.** Spread evenly, from 4.6% (Hungary) to 11.8% (Turkey).
- **Best simulated draw:** 68.0%, with Greece, Bosnia and Herzegovina, San Marino and Andorra.
- **Worst simulated draw:** 25.7%, with the Spain/France defeated team, Switzerland, Luxembourg and Azerbaijan. All five worst draws pair a quarter-final defeated team with Switzerland.

**Article 15 d).** In 500,000 simulated campaigns, which is 6,000,000 group tables, the two readings never ranked a group differently. So no direct qualifier, host place or play-off place changed either. The rehearsal draw (20,000 campaigns) shows the same.

This is expected, not a fault of the comparison. "Once" is the regulation's single re-application of a) to c), so the readings can differ only in one case: four or more teams finish level on points, and the first re-application still leaves two of them level.

A direct count with the same fit and ranking code ([`h2h_precondition.R`](2026-11-24-euro2028-draw-readiness/h2h_precondition.R), 200,000 campaigns per group) puts four or more teams level on points in:

- 0.023% of campaigns for the rehearsal's group H;
- none for its group B, a group of four;
- 0.15% for a stress-test group of five evenly matched teams.

None of those ties made the readings differ. The unit tests build a four-way tie that does separate the readings, so the comparison detects a difference when there is one.

## Decisions for the owner

1. **Article 15 d)**: "repeat" (published Euro 2024 explanations; the default) or "once" (the regulation's wording). Measured above, the choice does not move any probability: the readings never differed in 500,000 simulated campaigns. It would matter only if a real group ended with four or more teams level on points. The draw-night run repeats the comparison on the real groups.
2. **Publishing**: the odds exist as files only. A publish contract and a metill.is page need the owner's approval.

## Not modelled, or assumed

- Strengths are frozen at the fit; refit after each window.
- Disciplinary points (Article 15 j, Article 23) are a random draw.
- The play-off draw gives every allocation that keeps hosts apart (and Northern Ireland away from hosts, if possible) the same chance; further Executive Committee conditions (16.03) are not modelled.
- A draw condition on a quarter-finalist applies to both placeholders of its tie. The procedure says so for the prohibited pair (Spain) and the travel pairs; for the winter rule (Norway) and host separation (England) this is our reading, the only one under which the condition holds whoever wins.
- The draw simulator follows the written procedure; UEFA's software is not public, so the probabilities of particular groups are a model of it.
