# International handball: the nations model, its backtest and the championship forecasts

24 November 2026. Owner's words the same day: "We can very well handle setting up a
INLA version of the international handball model and deploy it in time for the
tournament. We'll do that." and "we can also plan to deploy results and forecasts to
production with the scraped data as it's still a personal project".

<!-- NUMBERS: filled from backtest_*.csv after the backtest run. -->

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

<!-- BACKTEST -->

## Forecast

<!-- FORECAST -->

## Not done, and open points

<!-- OPEN -->
