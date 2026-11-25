# International handball: the nations model, its backtest and the championship forecasts

24 November 2026. Owner's words the same day: "We can very well handle setting up a
INLA version of the international handball model and deploy it in time for the
tournament. We'll do that." The same day he agreed that its results and forecasts
may be published on metill.is.

Updated 25 November: the venue rule (training now gives home advantage as the
simulator does; see Venues), the backtest re-run under it, the EURO forecast
refreshed and the men's WC 2027 forecast added.

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
   row is the default fit). As in the simulator, a nation is at home only in a match
   played in its own country (since 25 November; see Venues below):
   - Qualifiers played home and away, and the EHF Euro Cup: 1.
   - Qualification tournaments played at one venue (Olympic qualification
     tournaments, first stages, continental qualifiers): at home only for the
     nation of the venue's country (`config/intl-handball/qualifier_venues.csv`).
   - Championship matches: at home only for a host playing in its own country
     (`hosts.csv`: EURO, world championship and Olympics hosts since 2012;
     `championship_venues.csv`: where hosts played at co-hosted championships).
   - Friendlies are the listed home side's home match (1), except in friendly
     tournaments named after their host (Golden League, Posten Cup), where only the
     host is at home.
   - Continental championships are treated as neutral.
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
| v2 (682 matches) | 0.550 | 0.1227 | 3.057 | 3.355 |
| legacy (643) | 0.569 | 0.1269 | 3.068 | 3.375 |
| base (682) | 0.882 | 0.2489 | | |
| **v2 − legacy, paired (643)** | **−0.010 (se 0.009)** | **−0.002 (se 0.003)** | **−0.006 (se 0.012)** | **−0.017 (se 0.014)** |
| v2 − base, paired (682) | −0.332 (se 0.018) | −0.126 (se 0.007) | | |

v2 is a little better than the legacy model on all four scores, and none of the four
differences is more than 1.2 standard errors from zero: on this evidence the two
models forecast equally well. v2 is chosen because it scores every match (the
legacy model has no prediction for a nation new to it), fits in about 45 seconds
against the legacy model's 9 to 16 minutes, and is the engine the handball leagues
already run on. By championship, v2's 1X2 log score is better at three of the eight
(the women's EURO 2022, −0.019 (se 0.025), the women's WC 2023, −0.047 (se 0.022), and
the men's EURO 2024, −0.060 (se 0.025)), within 0.003 at four, and worse at the men's
WC 2025, +0.030 (se 0.030).
Per-championship numbers: `backtest_scores.csv` and `backtest_paired.csv`. The
numbers are those of the venue rule of 25 November (see Venues).

**Calibration** (`backtest_calibration.csv`). When v2 makes a side favourite at
50 to 95 %, that side wins about as often as it says (54 % at 55 %, 65 % at 65 %,
79 % at 75 %, 89 % at 85 %, 91 % at 92 %, 99 % at 98 %). Both models predict too few draws after 60 minutes: 43 of the 682
matches were drawn, v2 expected 29 and the legacy model 28 (on its 643), and the gap
is in the matches it called close (9 % and 11 % drawn where it gave 5 % and 7 %).

### Venues (25 November)

A review of the first version found that training and the simulator gave home
advantage differently. The simulator gives it to a host only in its own country, by
venue city; training gave it to a host in every match of its championship, to the
listed home side when two hosts met, and to every listed home side at a
qualification tournament played at one venue (112 of those matches had the host
listed away). Training now follows the simulator: a nation is at home only in a
match played in its own country.

- Qualification tournaments at one venue are found from the match table's structure
  (a short single round robin whose pairs meet nowhere else in the stage, with its
  play-off and placement matches): 603 matches in 95 tournaments. Their venue
  countries (`config/intl-handball/qualifier_venues.csv`, 8 at neutral venues) and
  the venues of the 198 matches hosts played at co-hosted championships
  (`championship_venues.csv`; 35 host appearances outside the host's own country)
  come from the English Wikipedia articles and EHF pages each row cites; 15 rows were
  checked against the pages by hand.
- The championship being forecast trains on its own schedule's venues.

The fitted home advantage rises from about 1.0 goal to 1.2 for women and from
1.1–1.2 to 1.5 for men (the fits before each backtest championship), as the
neutral matches no longer dilute it. The forecasts barely change: paired over the
682 backtest matches, the new rule minus the old is −0.0008 (se 0.0010) in the 1X2
log score, −0.0003 (se 0.0004) in RPS and −0.0005 (se 0.0013) in the margin log
score. The table above is the new rule's.

To re-run: `IH_DATA_DIR=<match table folder> IH_RESULTS_DIR=<per-match output folder>
ml run -- Rscript docs/reports/2026-11-24-intl-handball/backtest.R --models
v2,base,legacy`, then the same `IH_RESULTS_DIR` with `summarise.R`. The per-match
predictions stay outside this repository; the three CSV files here are the record.

## Forecast

**Women's EURO 2026**, as of 25 November 2026 (fitted on 4,136 women's matches up
to that day, 20,000 simulations of the championship; published to
`data/publish/handball_euro/kvenna/`).

| Nation | Main round | Semi-final | Final | Champion |
|---|---|---|---|---|
| Norway | 100 % | 96 % | 83 % | 67 % |
| Denmark | 100 % | 63 % | 38 % | 11 % |
| France | 99 % | 73 % | 31 % | 11 % |
| Germany | 99 % | 52 % | 16 % | 4.3 % |
| Netherlands | 98 % | 46 % | 13 % | 3.3 % |
| Hungary | 96 % | 24 % | 10 % | 2.0 % |
| Sweden | 95 % | 24 % | 4.9 % | 1.2 % |

Iceland, in group A in Oradea: 19 % to reach the main round (3 % to win the group,
16 % second, 30 % third, 51 % fourth). Match by match: Slovenia–Iceland on 3 December
59 % / 6 % / 35 %, Montenegro–Iceland on 5 December 64 % / 6 % / 30 %, Iceland–Hungary
on 7 December 6 % / 2 % / 92 %.

Norway at 67 % is the model's own figure: its rating is 4.6 goals above Denmark's,
the next best, and it is 96 % to reach the semi-finals. No bookmaker's outright price
was compared. Against the forecast of 24 November (the old venue rule), no nation's
chance of reaching the main round moved by more than 1.4 points.

**Men's WC 2027**, as of 25 November 2026 (fitted on 5,623 men's matches, the last
on 8 November; 20,000 simulations; published to
`data/publish/handball_world_cup/karla/`, which replaces the placeholder of
metill.is's `/hm2027-handbolti/`). Germany is the only host and is at home in every
match.

| Nation | Main round | Quarter-final | Semi-final | Final | Champion |
|---|---|---|---|---|---|
| Denmark | 100 % | 96 % | 74 % | 56 % | 39 % |
| Germany | 100 % | 90 % | 65 % | 34 % | 18 % |
| France | 100 % | 83 % | 47 % | 30 % | 15 % |
| Sweden | 99 % | 83 % | 46 % | 18 % | 7.7 % |
| Iceland | 100 % | 69 % | 33 % | 17 % | 6.2 % |
| Egypt | 100 % | 68 % | 36 % | 12 % | 3.9 % |
| Spain | 100 % | 53 % | 18 % | 8.7 % | 3.1 % |

Iceland, in group H in Magdeburg: 99.7 % to reach the main round, 84 % to win the
group. Match by match: Iceland–Bahrain on 15 January 91 % / 3 % / 6 %, Japan–Iceland
on 17 January 2 % / 1 % / 97 %, Iceland–North Macedonia on 19 January 82 % / 4 % /
14 %. The men's friendlies of December and January are not in yet; the forecast is
re-run before the championship.

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
