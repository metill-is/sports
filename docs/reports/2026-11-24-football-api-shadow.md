# Football odds: DOM scraper against the JSON API (shadow runs and team-name audit)

2026-11-24. Spec `docs/superpowers/specs/2026-09-23-hb-bb-lengjan-odds-staged-betting-design.md`,
WS3 (Milestone B, scraping half). How to run both tools and the proposed
judging rule: [`docs/runbooks/odds-source-cutover.md`](../runbooks/odds-source-cutover.md).

The Icelandic season is over, so no configured football competition is on
Lengjan. The shadow runs below therefore use throwaway league definitions for
English competitions that list handicaps and totals. They test the
comparison machinery and the two scrapers' agreement on the same site; they
do not replace the Icelandic runs that the cutover rule asks for.

## Shadow runs

Each run: `Rscript scripts/0Nc_compare_odds_sources.R --competition <id> --country england --max-detail 10 --out <dir>`
(one `current-program` request, two rounds of `markets` requests, one
competition page and at most 10 detail pages). The RDS and CSV output stays
outside the repo.

| Run | Competition | Code | DOM rows | DOM pass | API rows | API-only | Shared matches | Verdict |
|---|---|---|---|---|---|---|---|---|
| 1 | Premier League (296), 2026-11-24 22:45 UTC | this branch | 110 | 110 | 566 | 456 | 5 of 5 DOM, 20 priced on the API | **pass** |
| 2 | Championship (291), 23:05 UTC | this branch | 114 | 90 | 460 | 370 | 4 of 5 DOM, 16 priced on the API | fail: 24 rows `name_whitespace` |
| 2, re-judged | the same snapshots, API names squished | (offline) | 114 | 114 | 460 | 346 | 5 of 5 DOM | **pass** |
| 3 | Premier League (296), 23:13 UTC | cutover branch | 110 | 110 | 566 | 456 | 5 of 5 DOM, 20 priced on the API | **pass** |
| 4 | Championship (291), 23:15 UTC | cutover branch | 114 | 114 | 460 | 346 | 5 of 5 DOM, 16 priced on the API | **pass** |

### Run 1 in detail

- **Every DOM row passes.** 15 moneyline, 75 handicap and 20 total rows
  from 5 detail pages, all at the API's price to the cent. No detail page
  was mispaired (each visit's event id matched the API event with the same
  teams and date) and no key was duplicated.
- **Prices did not move** between the two API snapshots (566 rows, 0
  changed), and the DOM pass took 54 seconds. So this run did not exercise
  the "either snapshot" part of the rule.
- **API-only rows, by cause:**
  - `extra_line_on_api`, 30 rows: on each shared match the API lists two
    more handicap lines (three outcomes each) than the detail page renders.
  - `match_not_on_dom`, 426 rows: 15 priced matches that the DOM competition
    page did not list (next section).
- The API listed 35 events. Six are one-market specials (a player
  match-up, four Arsenal v Man.City props and one Hull v Coventry) whose
  market is not one the scrape keeps, and nine (5 December onwards) had no
  markets open yet; neither kind gives an odds row.
- **Stray whitespace was not exercised:** both fixtures with `"Newcastle "`
  were among the 15 the DOM did not list.

### Runs 2 to 4: stray whitespace

- **Run 2's only failure is the known one.** The API lists the Championship
  match on 25 November as `"Stoke "` v Millwall; the site and the DOM show
  `Stoke`. All 24 DOM rows of that match fail with `name_whitespace`, and
  the API's 30 rows for it are API-only for the same reason. Every other
  DOM row passes to the cent; prices did not move between the snapshots
  (460 rows, 0 changed).
- **Squishing the API's names clears it.** Re-judging the same two
  snapshots with `stringr::str_squish()` on the API's team names, which is
  what the cutover branch does in the API parser, passes all 114 DOM rows,
  with the 30 extra handicap rows as `extra_line_on_api`. This used the run's
  saved snapshots and made no request.
- **Run 3 ran the cutover branch's code** against the Premier League. It
  passes like run 1, but none of the 5 matches the DOM listed has a padded
  name, so it does not exercise the fix.
- **Run 4 confirms it live.** The cutover branch's code against the
  Championship again, ten minutes after run 2: all 114 DOM rows pass, the
  24 rows of Stoke v Millwall included, and no API team name carries stray
  whitespace.
- Prices did not move between snapshots in any run (0 changed of 566, 460,
  566 and 460), so no run needed the "either snapshot" part of the rule.

### The DOM sees at most 5 matches per competition page

The competition page gave the DOM scraper 5 match links while the API priced
20 matches. The scraper takes every link the page renders after a 3-second
settle; it has no cap of its own. The 2026 football odds history shows the
same ceiling on configured competitions (teams mapped to divisions through
`team_names` and the results table):

| Division (male) | Scrapes | Most matches in one scrape | Scrapes at exactly 5 |
|---|---|---|---|
| Besta deild | 324 | 5 | 55 % |
| Lengjudeild | 259 | 5 | 32 % |
| 2. deild | 124 | 6 | 13 % |
| 3. deild | 87 | 6 | 16 % |

Besta deild and Lengjudeild rounds have 6 matches. In 14 of the 16 complete
6-match Besta deild karla rounds of 2026, the sixth match was first seen
on its own and later: a median 1.5 days before kickoff, against 3.3 days for
the first five (Lengjudeild: 8 of 10 rounds). It was not lost: 99.2 % of
Besta deild karla fixtures were seen at some point, which fits the page
showing the sixth match only once an earlier one had kicked off and left the
list. The cause is not
confirmed; a list the site renders lazily to the headless window's height
would produce it. The API lists every event, so the cutover removes the
delay.

## Team-name audit

`Rscript scripts/0Nc_audit_team_names.R --league football_iceland`, with the
API test fixture, the live program and the archived programs of a local
archive copy (one program per 6 hours, 2026-11-16 to 2026-11-24).

- **DOM names.** All 70 distinct names the DOM scraper wrote to
  `data/facts/odds` for football in 2026 (2026-04-10 to 2026-10-24) map to a
  canonical team: 49 male, 21 female. None is unmapped, whitespace-padded or
  mapped only under the other sex.
- **API names.** The four participant names of configured competitions seen
  on the API (the fixture's events in competitions 20442 and 27524) map:
  `KA`, `Stjarnan`, `Valur kv`, `Víkingur Rvk kv`.
- **Unseen renderings.** Every configured rendering has been seen, by one
  scraper or the other, except the female fallback `Tindastóll kv`. Its
  primary, `Tindastoll kv`, is seen. Several `team_names` comments still say
  a rendering is a pattern guess to verify, although it has since been seen;
  that is comment drift, not a mapping gap.
- **Competition ids.** Only 2 of the 12 configured competition ids are
  confirmed on the API (20442 and 27524, both from the fixture). The archived
  and live programs hold no Icelandic football, because the season had
  ended before the archive began. The other 10 ids are carried over from the
  DOM scraper's configuration, which reads the same site, so they are
  expected to hold; the first Icelandic shadow run confirms them.
- **Stray whitespace.** 21 of 2,635 distinct API participant names carry
  stray spaces, 12 of them English football (for example `"Newcastle "`,
  `"Stoke "`, `"Norwich "`) and one Icelandic handball (`"Víkingur "`). The
  site and the DOM scraper show these names trimmed, and football's
  `team_names` lists only trimmed renderings, so an untrimmed API name would
  fall through the mapping. The cutover branch squishes participant names in
  the API parser. Handball's `Víkingur` mapping already lists both forms, so
  it needs no config change.

## What this means for the cutover

Across the four live runs (two competitions, 448 DOM rows, 10 distinct
matches seen by the DOM, each twice) every DOM row has an API row with the same key at the same price,
once the API's team names are squished as the cutover branch does. No run
had a mispaired detail page, a duplicate key, a price mismatch, or a line,
market or selection the API lacked or held suspended. The API is a superset
of the DOM: every match the competition page lists (the DOM sees at most 5),
and two more handicap lines per match.

Not yet tested, and covered by the cutover rule's Icelandic runs:

- **Icelandic competitions.** 10 of the 12 configured competition ids are
  not yet confirmed on the API, and women's names (`kv` suffix) have only
  the fixture's two.
- **Moving prices.** No run saw a price change between its snapshots, so
  the bracketing allowance is untested.
- **Downstream volume.** After the switch, football's odds rows include
  each round's sixth match from the start and the extra handicap lines. With
  betting paused this only changes paper recommendations, but a reviewer of
  the first post-cutover decide run should expect more candidate rows.

Next: shadow runs on Icelandic events (the Lengjubikar or the cup), with
`--league football_iceland` on the cutover branch, until the rule's 3
passing runs on 2 matchdays are met; then the owner's go for the branch.
