# Euro 2024 qualifying fixture

Validation data for the UEFA group-ranking code in `R/nt-qualifying.R`, built
on 2026-11-22 by `scripts/nt/build_euro2024_qualifying_fixture.R`.

| File | Content | Source |
| --- | --- | --- |
| `matches.csv` | All 230 group matches (date, group, home, away, score, `venue`: 1 when the listed home side played at home, 0 at a neutral venue; martj42's `neutral` flag inverted) | martj42 `international_results`, `results.csv` (CC0-1.0), master at commit `1f1a7dc5a1` (2026-11-19). Every score was checked against Wikipedia's tables (below): all 230 agree. |
| `tables.csv` | Final table of each of the ten groups, in official order, with Wikipedia's tie-break notes | English Wikipedia, `Template:UEFA Euro 2024 qualifying group tables`, revision 1363707721 (2026-07-11), which cites UEFA's standings |
| `overall.csv` | Overall European Qualifiers ranking (1 to 53) with the statistics after discarding results against sixth-placed teams | Same template, section `Overall` |
| `nations_league_rank.csv` | 2022-23 Nations League overall ranking (September 2022) of the 53 teams, criterion k | English Wikipedia, `UEFA Euro 2024 qualifying`, revision 1380834391 (2026-10-18), seeding pots |
| `disciplinary.csv` | Disciplinary points of Bosnia and Herzegovina (20) and Latvia (29) in the matches the overall ranking takes into account (pass as `disciplinary_overall`) | The note in `overall.csv` that their overall rank was decided on fair play |

Known discrepancy: the overall table gives Iceland 7 goals scored after the
discard; Wikipedia's own scores give 6 (17 in the group table, 11 of them
against sixth-placed Liechtenstein). The tests pin this.

Rules: the Euro 2024 group tie-breakers and overall-ranking criteria were
taken from Wikipedia's transcription of the 2022-24 regulations (sections
"Tiebreakers for group ranking" and "Criteria for overall ranking" of the
article above), because UEFA's 2022-24 regulation pages now return 404. They
are the same criteria as Articles 15 and 23 of the 2026-28 regulations,
except that Euro 2024 discarded results against sixth place (groups of five
and six) where Euro 2028 discards those against fifth place (groups of four
and five).
