# International handball: the nations model, its backtest and the championship forecasts

24 November 2026. Owner's words the same day: "We can very well handle setting up a
INLA version of the international handball model and deploy it in time for the
tournament. We'll do that." The same day he agreed that its results and forecasts
may be published on metill.is.

## What was built

- **Data.** Every senior national-team match of both sexes from 2012 to today
  (championships with their qualifiers, the Olympics, continental championships and
  games, the EHF Euro Cup, friendlies), with the score after 60 minutes for matches
  that went to extra time. Read from the folder named by `IH_DATA_DIR`.
- **Model** (`R/ih-model.R`): the handball v2 (margin, total) model of
  `docs/reports/2026-11-15-hb-bb-v2/` on R-INLA, unchanged except for a venue term,
  with nations as teams.
- **Simulator** (`R/ih-simulate.R`): preliminary round, main round with carried-over
  results, knockouts with extra time and penalties, under the EHF and IHF rules of
  `tournament-facts.md`.
- **Publisher** (`R/ih-publish.R`): the five files of metill-platform's handball
  championship contract (`data/hm2027-handbolti/README.md` there, schema_version 1),
  to `data/publish/handball_euro/kvenna/` (EURO) and
  `data/publish/handball_world_cup/karla/` (WC).

## The model

A nation's state in a season (July to June) is `b[nation] + u[nation, season]`: a
long-run level and an AR(1) season level, constant within a season (the static form
the handball leagues use). Two such states per nation, one in each half:

- margin = home − away ~ normal(venue × (hfa + ha[host]) + s[home] − s[away], σm)
- total = home + away ~ normal(level[season] + t[home] + t[away], σt)

Fitted with `fit_v2_inla()` on one thread with `internal.opt = FALSE`, draws seeded,
4,000 joint draws per half, the total half's draws shuffled before pairing (as the
league engine pairs them).

Changes from the league engine, and why:

1. **Venue.** `fit_v2_inla()` takes an optional venue per row: 1 when the listed home
   side plays at home, 0 on neutral ground, −1 when the listed away side is the host.
   League fits pass nothing and are unchanged (a test pins that a venue of 1 on every
   row is the default fit). Qualifiers and the EHF Euro Cup are home and away (1);
   friendlies are the listed home side's home match (1), except in friendly
   tournaments named after their host (Golden League, Posten Cup), where only the
   host is at home; championship matches are neutral except for a host nation
   (`config/intl-handball/hosts.csv`: EURO, world championship and Olympics hosts
   since 2012). Continental championships are treated as neutral.
2. **Priors.** Nations differ far more than clubs: the club level's 1 % bound is
   30 goals instead of 10 and the season level's 12 instead of 8. Everything else is
   `v2_priors("handball")`.
3. **Training on 60 minutes.** The response is the score after 60 minutes; a match
   that went to extra time without a readable 60-minute score keeps its indices and
   leaves the likelihood. Awarded results and walkovers are left out.

## The simulator

- Scores: one posterior draw per simulation for every match of it; the margin and the
  total drawn from the two halves and rounded with matching parity
  (`v2_sample_scores()`), so a level margin is a draw.
- Groups ranked on 2-1-0 points; then points, goal difference and goals among the
  teams level on points; then goal difference and goals in all the group's matches;
  then Fair Play (EHF) or lots, drawn at random. Neither body says whether the
  head-to-head table is recomputed for teams still level after one is separated;
  the simulator's switch `h2h_reapply` defaults to the EHF text read literally (not
  recomputed). Tests pin a case where the readings differ.
- Main round: the qualifiers' results against each other are carried over; each team
  plays the qualifiers of the other groups feeding its main-round group.
- Knockouts: a match level after 60 minutes goes to 2 × 5 minutes of extra time, drawn
  as a short continuation of the match (mean and variance scaled by 10/60), a second
  if level again, then penalties as a coin flip.
- Host advantage applies to a host nation playing in its own country (by venue city).
- Played results are kept, matched by the two teams within each stage.

## Backtest

Eight championships, 2022 to 2026: the women's EURO 2022 and 2024 and world
championships 2023 and 2025, the men's world championships 2023 and 2025 and EURO
2024 and 2026 (682 matches). Each model is fitted once on every match before the
championship's first day and predicts every match of it, pairings as played. Scored
on the result after 60 minutes (a knockout match level after 60 minutes is a draw):
the 1X2 log score and ranked probability score (RPS), and the log score of the
60-minute margin and total. Lower is better throughout.

- **v2**: this model.
- **legacy**: the international model as it was (`2d_student_t.stan`, per-team random
  walks over match rounds, a friendly effect), its data preparation reproduced in
  `legacy.R` with two changes in its favour: friendlies are flagged correctly and
  matches carry their true dates. 4 chains × 1,000 iterations.
- **base**: the draw rate of neutral-ground championship matches in the four years
  before, the rest split evenly between the sides.

The legacy model cannot score a match with a nation it never trained on: it scored
643 of the 682 matches (all but 7 at the women's WC 2023, 19 at the women's WC 2025
and 13 at the men's WC 2025). The paired differences below are over the matches both
models scored.

| | log 1X2 | RPS | log margin | log total |
|---|---|---|---|---|
| v2 (682 matches) | 0.551 | 0.1230 | 3.057 | 3.355 |
| legacy (643) | 0.569 | 0.1269 | 3.068 | 3.375 |
| base (682) | 0.882 | 0.2489 | | |
| **v2 − legacy, paired (643)** | **−0.009 (se 0.009)** | **−0.002 (se 0.003)** | **−0.006 (se 0.012)** | **−0.017 (se 0.014)** |
| v2 − base, paired (682) | −0.331 (se 0.018) | −0.126 (se 0.007) | | |

v2 is a little better than the legacy model on all four scores, and none of the four
differences is more than 1.2 standard errors from zero: on this evidence the two
models forecast equally well. v2 is chosen because it scores every match (the
legacy model has no prediction for a nation new to it), fits in about 45 seconds
against the legacy model's 9 to 16 minutes, and is the engine the handball leagues
already run on. By championship, v2's 1X2 log score is better at three of the eight
(the women's EURO 2022, −0.018 (se 0.023), the women's WC 2023, −0.046 (se 0.021), and
the men's EURO 2024, −0.059 (se 0.026)), within 0.003 at four, and worse at the men's
WC 2025, +0.032 (se 0.031).
Per-championship numbers: `backtest_scores.csv` and `backtest_paired.csv`.

**Calibration** (`backtest_calibration.csv`). When v2 makes a side favourite at
50 to 95 %, that side wins about as often as it says (54 % at 55 %, 65 % at 65 %,
80 % at 75 %, 89 % at 85 %, 90 % at 93 %, 99 % at 98 %). Both models predict too few draws after 60 minutes: 43 of the 682
matches were drawn, v2 expected 29 and the legacy model 28 (on its 643), and the gap
is in the matches it called close (8 % and 11 % drawn where it gave 5 % and 7 %).

To re-run: `IH_DATA_DIR=<match table folder> IH_RESULTS_DIR=<per-match output folder>
ml run -- Rscript docs/reports/2026-11-24-intl-handball/backtest.R --models
v2,base,legacy`, then the same `IH_RESULTS_DIR` with `summarise.R`. The per-match
predictions stay outside this repository; the three CSV files here are the record.

## Forecast

**Women's EURO 2026**, as of 24 November 2026 (fitted on 4,131 women's matches up
to that day, 20,000 simulations of the championship; published to
`data/publish/handball_euro/kvenna/`).

| Nation | Main round | Semi-final | Final | Champion |
|---|---|---|---|---|
| Norway | 100 % | 96 % | 83 % | 68 % |
| Denmark | 100 % | 64 % | 39 % | 11 % |
| France | 99 % | 74 % | 31 % | 11 % |
| Germany | 99 % | 51 % | 14 % | 3.9 % |
| Netherlands | 98 % | 47 % | 13 % | 3.4 % |
| Hungary | 96 % | 23 % | 10 % | 1.9 % |
| Sweden | 95 % | 24 % | 4.8 % | 0.9 % |

Iceland, in group A in Oradea: 19 % to reach the main round (3 % to win the group,
16 % second, 30 % third, 51 % fourth). Match by match: Slovenia–Iceland on 3 December
59 % / 6 % / 35 %, Montenegro–Iceland on 5 December 65 % / 6 % / 29 %, Iceland–Hungary
on 7 December 6 % / 2 % / 92 %.

Norway at 68 % is the model's own figure: its rating is 4.7 goals above Denmark's,
the next best, and it is 96 % to reach the semi-finals. No bookmaker's outright price
was compared.

<!-- WC2027 -->

## Not done, and open points

- **Live updates during the championship.** The forecast is refreshed by re-running
  `scripts/ih/run_tournament.R` on new results. A scheduled job that does this four
  times a day from 3 to 21 December and pushes the five files to `main` is written but
  not installed: installing it needs the owner's approval. Until then a refresh is a
  manual run.
- **Too few draws.** Both models under-predict 60-minute draws by about a third (see
  Calibration). A draw is worth a point each in the group tables, so this mostly
  shifts close group races a little. Not corrected here. The match probabilities come
  from the same v2 functions the handball leagues use; whether the leagues show the
  same gap was not checked.
- **Head-to-head reading.** Neither body says whether the head-to-head comparison is
  recomputed for teams still level once one is separated. The simulator reads the EHF
  text literally (`h2h_reapply = "none"`); the other readings are one switch away.
- **Fair Play** (the EHF's last criterion before lots) is not modelled; lots are drawn.
- **Continental championships** (Africa, Asia, Pan-America) are treated as neutral
  ground; their hosts are not in `hosts.csv`.
- **Nations outside Europe** play few matches against European sides, so their
  strengths rest on few links; this matters for the world championship more than the
  EURO.
- **Late friendlies.** Teams play friendlies up to the first day; a re-run around
  2 December takes them in.
