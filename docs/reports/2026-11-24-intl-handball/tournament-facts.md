# Tournament facts: Women's EHF EURO 2026 and Men's IHF World Championship 2027

Researched 2026-11-24 from official sources (EHF and IHF sites, regulations, official schedule PDFs). Wikipedia used only as a cross-check, and it gets one venue fact wrong (see Discrepancies).

The groups and schedules are in this repository: `config/intl-handball/euro2026w/` and `config/intl-handball/wc2027m/` (`groups.csv`, `schedule.csv`; 65 and 112 matches). They were built from the EHF match data and the IHF match pages; the build checked that every preliminary pairing is inside its group, that no main-round pairing is between two teams of the same preliminary group, that the WC has 112 matches and that the WC group positions reproduce the IHF day template.

Page numbers for regulations are the **printed** numbers, which sit in the page footer. I checked several pages of each PDF: in both documents the PDF page number is the printed number + 1.

---

## 1. Women's EHF EURO 2026

### 1.1 Dates, hosts, venues

| Fact | Value | Source |
|---|---|---|
| Dates | 3–20 December 2026 | Draw article (EHF): https://ehfcl.eurohandball.com/news/en/womens-ehf-euro-2026-draw-reveals-preliminary-round-opponents/ ; regulations Enclosure 2 (p. 99) |
| Hosts | Romania, Türkiye, Czechia, Slovakia, Poland. EHF calls it the first EURO with five host countries | Draw article |
| Teams | 24 | Regulations Art. 7.1 (p. 22) |
| Draw | 16 April 2026, Katowice | Draw article |
| Group A | Oradea (ROU), Oradea Arena | Schedule PDF v9; EHF match API |
| Group B | Cluj-Napoca (ROU), BT Arena | same |
| Group C | Antalya (TUR), Antalya Sports Hall | same |
| Group D | Brno (CZE), T-Mobile Arena (PDF says "T-Arena") | same |
| Group E | Katowice (POL), Spodek Arena | same |
| Group F | Bratislava (SVK), Tipos Arena – Ondrej Nepela Arena | same |
| Main Round I (from groups A, B, C) | Cluj-Napoca, BT Arena, 10/12/14/15 Dec | same |
| Main Round II (from groups D, E, F) | Katowice, Spodek Arena, 11/13/15/16 Dec | same |
| Final weekend | Katowice, Spodek Arena: semi-finals and 5/6 on 18 Dec; bronze and final on 20 Dec; rest days 17 and 19 Dec | same |
| Hosts pre-allocated before the draw | Hungary to A (Oradea), Romania to B, Türkiye to C, Czechia to D, Poland to E, Slovakia to F | https://ehfcl.eurohandball.com/news/en/all-you-need-to-know-about-women-s-ehf-euro-2026-draw-on-16-april/ |

Time zones: Oradea and Cluj-Napoca are EET (UTC+2), Antalya is TRT (UTC+3), and Brno, Katowice and Bratislava are CET (UTC+1). I derived these from each match's UTC vs local time in the EHF API. They agree with the EHF article "Preliminary round across three time zones": https://ehfcl.eurohandball.com/news/en/ehf-confirms-throw-off-times-for-women-s-ehf-euro-2026/

Main sources:

- **EHF match API** behind https://ehfeuro.eurohandball.com/women/2026/matches/ : `https://ehfeuro.eurohandball.com/umbraco/api/competitionmatchesapi/GetInitMatches?contentId=117758&competitionId=geUe4-8vreQ-I8HeX-A38w`, paged with `GetMatches?...&matchId=<id>&futureMatches=true`. Read 2026-11-24. It returned 71 final-tournament records: 65 matches plus 6 "carried-over" placeholders (see 1.4).
- **Schedule PDF v9** ("W EURO26 matchshedule_vertical_v9", created 2026-10-01), linked from the matches page: https://ehfeuro.eurohandball.com/media/kmnlbjo1/match-schedule-illustrated-version-vertical-womens-ehf-euro-2026.pdf . Its times are printed in **CET**.
- **Regulations**: "EHF EURO Regulations – Women's EHF EURO 2026", 105 pp., PDF created 2026-05-05: https://www.eurohandball.com/media/ipvbgerz/womens-ehf-euro-2026-regulations.pdf (listed at https://www.eurohandball.com/en/what-we-do/regulations/national-team-competitions/).

### 1.2 Groups and draw positions

Positions are from the regulations' Enclosure 2 match template (p. 99: match 1 = A3–A4, 2 = A1–A2, 3 = A1–A3, 4 = A2–A4, 5 = A4–A1, 6 = A3–A2, and the same pattern for B–F) matched against the API pairings. They coincide with the order the EHF lists the teams and with the seeding pots (position = pot).

| Group (city) | 1 | 2 | 3 | 4 |
|---|---|---|---|---|
| A (Oradea) | Hungary | Montenegro | Slovenia | Iceland |
| B (Cluj-Napoca) | Norway | Romania | Switzerland | North Macedonia |
| C (Antalya) | Denmark | Spain | Türkiye | Greece |
| D (Brno) | Netherlands | Czechia | Austria | Croatia |
| E (Katowice) | France | Poland | Faroe Islands | Ukraine |
| F (Bratislava) | Sweden | Germany | Slovakia | Serbia |

Pots (draw article): 1 Norway, Denmark, Hungary, France, Sweden, Netherlands; 2 Germany, Montenegro, Poland, Romania, Spain, Czechia; 3 Türkiye, Slovakia, Slovenia, Switzerland, Austria, Faroe Islands; 4 Croatia, Serbia, Iceland, North Macedonia, Ukraine, Greece.

### 1.3 Schedule

The full schedule is in `euro2026w_schedule.csv`. Match numbers are the EHF match-ID suffixes (e.g. 202612010101**001**), and local times come from the API.

Group A (Oradea Arena, local EET):

| No | Date | Local | Home | Away |
|---|---|---|---|---|
| 1 | Thu 3 Dec | 18:30 | Slovenia | Iceland |
| 2 | Thu 3 Dec | 21:00 | Hungary | Montenegro |
| 4 | Sat 5 Dec | 18:30 | Montenegro | Iceland |
| 3 | Sat 5 Dec | 21:00 | Hungary | Slovenia |
| 6 | Mon 7 Dec | 18:30 | Slovenia | Montenegro |
| 5 | Mon 7 Dec | 21:00 | Iceland | Hungary |

Match days by group: A and C play 3/5/7 Dec; B, D and F play 4/6/8 Dec; E plays 5/7/9 Dec.

Basic times: preliminary round 18:30 & 21:00 local in A/B/C and 18:00 & 20:30 in D/E/F. The exception is Group F on Sun 6 Dec: 15:30 Sweden–Slovakia and 18:00 Germany–Serbia, from the API and PDF v9. The older PDF of 22 April 2026 had 18:00 Germany–Serbia and 20:30 Sweden–Slovakia, so it is superseded. Main Round I is 16:00 / 18:30 / 21:00 local (EET). Main Round II is 15:30 / 18:00 / 20:30 CET. Fixtures in both main rounds have not yet been assigned to slots.

Final weekend (CET):

- 18 Dec: 5/6 placement at 15:00 (3rd MR I v 3rd MR II); semi-finals at 17:45 and 20:30, order assigned after the main round
- 20 Dec: bronze match at 15:15; final at 18:00

Main-round fixture pattern, with the order as listed by the EHF:

- MR I: 10 Dec 1C–2B, 2A–2C, 1A–1B · 12 Dec 1C–1B, 1A–2C, 2A–2B · 14 Dec 1A–2B, 2A–1C, 1B–2C · 15 Dec 1A–1C, 2A–1B, 2B–2C
- MR II: 11 Dec 1F–2E, 2D–2F, 1D–1E · 13 Dec 1F–1E, 1D–2F, 2D–2E · 15 Dec 1D–2E, 2D–1F, 1E–2F · 16 Dec 1D–1F, 2D–1E, 2E–2F

### 1.4 Format (regulations Art. 9)

- **Structure (9.2–9.3, p. 23):** six preliminary groups of 4 → main round → 5/6 placement match, semi-finals and final.
- **Preliminary round (9.8–9.11, p. 24):** single round robin of 2×30 min with **no extra time**. Points are 2 for a win, 1 for a draw and 0 for a defeat.
- **Advancing (9.17–9.19, pp. 26–27):** 1st and 2nd of each group go to the main round, which has two groups of six:
  - MR I = 1st/2nd of A, B and C (Cluj-Napoca)
  - MR II = 1st/2nd of D, E and F (Katowice)
- **Main round matches (9.20, p. 27):** each team plays only the opponents it did not meet in the preliminary round, i.e. 4 matches.
- **Carried-over results (9.23, p. 27):** "The preliminary round results including points, goals scored, and goals conceded from the matches between the opponents who reached the main round are kept and remain valid for the ranking of the main round."
- **Main round table (9.24, p. 27):** points and goals from 9.23 plus each main-round match.
- The EHF match database models each carry-over as a dateless main-round "match" (nos. 66–71: "1./A PR v 2./A PR", etc.). I left these out of the schedule CSV, because they are the preliminary-round result counted again, not a new match.
- **Non-qualifiers:**
  - 3rd-placed preliminary teams go home ranked 13–18 by preliminary points. Ties: (a) goal difference in all group matches, (b) goals scored, (c) Fair Play ranking, (d) the highest-ranked opponent they met in the preliminary round, by the EURO final ranking (9.15–9.16, p. 26).
  - 4th-placed teams are ranked 19–24 by the same method (9.13–9.14, pp. 25–26).
  - There is no President's Cup and there are no placement matches below 5th.
- **Places 7–12 (9.31–9.33, pp. 29–30):** 4th, 5th and 6th of the main-round groups go home. Places 7/8 go to the two 4th-placed teams, 9/10 to the 5th-placed teams and 11/12 to the 6th-placed teams, ranked by main-round points. Ties: goal difference in all group matches, goals scored, Fair Play ranking, then the highest-ranked opponent met in the preliminary round. The lettering in the PDF runs a, b, e, c.
- **5/6 match (9.26–9.30, p. 29):** 3rd MR I v 3rd MR II.
- **Semi-finals (9.34–9.35, p. 30):** "The teams ranked first (1st) in the main round groups play against the teams ranked second (2nd) in the other main round group":
  - SF: 1st MR I v 2nd MR II
  - SF: 1st MR II v 2nd MR I (the API lists it as "MR 2. I v MR 1. II")
  - Losers play the bronze match (9.36) and winners the final (9.37).
- **Extra time, for the 5/6 match, semi-finals, bronze match and final only (9.28–9.30, 9.38–9.42, pp. 29–31):** if level after 60 min, there is a 5-min break, then 2×5 min extra time (1-min half-time). If still level, a second 2×5 min extra time follows after another 5-min break. If still level, penalty throws (9.43, pp. 31–32):
  - Five nominated throwers each, alternating. If still level, further rounds of five, and the second round is decided "once there is a difference in goals after both teams have executed the same number of throws".
  - Note: 9.30 cites 9.43 correctly, but 9.42 cross-references "article 9.36", a typo for 9.43.

### 1.5 Tie-breaks (verbatim)

**Preliminary round, 9.12 (pp. 24–25):** "If two or more teams are equal on points in the preliminary round, their ranking is determined as follows:"

*During the preliminary round matches:* a) Superior goal difference from all group matches; b) Higher number of goals scored in all group matches; c) Alphabetical order.

*After the completion of the preliminary round matches:*

> a) Higher number of points obtained in the group matches played amongst the teams in question
> b) Superior goal difference from the group matches played amongst the teams in question
> c) Higher number of goals scored in the group matches played amongst the teams in question
> d) Superior goal difference from all group matches (achieved by subtraction)
> e) Higher number of goals scored in all group matches
> f) If the ranking of one of these teams is determined, the criteria are consecutively followed until the ranking of all teams is determined.
> g) If this procedure does not lead to a decision after completion of all preliminary round matches, the Fair Play Ranking decides about the final group ranking. [Yellow card 1 pt, two-minute suspension 2 pts, red card 8 pts; fewer points ranks higher is implied, not stated]
> h) In case the number of points in the Fair Play Ranking are identical, a draw must be carried out by the EHF […]

**Main round, 9.25 (pp. 28–29):** identical wording, with "main round" in place of "preliminary round". During the round: GD, goals, alphabetical. After the round: H2H points, H2H GD, H2H goals, overall GD, overall goals, the (f) clause, Fair Play, then a draw.

**Carried-over results in the main round.** Under 9.23/9.24 the main-round table includes the carried preliminary result between the two teams from the same group. "Group matches played amongst the teams in question" therefore naturally includes that carried match, and "all group matches" means the main-round table (carried plus main-round matches). The regulations do not spell this out. The fact that the EHF's own fixture list carries these results as main-round "matches" 66–71 supports this reading.

**Three or more teams level:** see Discrepancies item E1. Clause (f) does not say whether the head-to-head mini-table is recomputed among the teams still level.

---

## 2. Men's IHF World Championship 2027 (Germany)

### 2.1 Dates, hosts, venues

| Fact | Value | Source |
|---|---|---|
| Dates | 13–31 January 2027, Germany (30th edition), 32 teams | IHF draw article: https://www.ihf.info/media-center/news/draw-munich-sets-stage-2027-ihf-mens-world-championship |
| Draw | 10 June 2026, Munich | same |
| Group A, Group C | Munich, SAP Garden | same; schedule PDF; ihf.info match pages |
| Group B, Group D | Stuttgart, Porsche-Arena | same |
| Group E, Group G | Kiel. Named "MERKUR Ostseehalle" in the schedule PDF, ihf.info match pages and handball2027.com; the IHF draw article calls it "Wunderino Arena" (same hall, renamed) | same |
| Group F, Group H | Magdeburg, GETEC Arena | same |
| Main Round I (A/B) and II (C/D) | Cologne, LANXESS Arena | schedule PDF; IHF schedule news ("Main Round Group II in Cologne") |
| Main Round III (E/F) and IV (G/H) | Hanover, ZAG Arena | schedule PDF |
| President's Cup (8 fourth-placed teams) | Magdeburg, GETEC Arena, 20–25 Jan, placement matches 27 Jan | schedule PDF |
| Quarter-finals | Cologne: 26 Jan (QF1, QF2) and 27 Jan (QF3, QF4) | schedule PDF; draw article |
| SF, 5–8 semis | Cologne, Fri 29 Jan | same |
| Final, bronze, 5/6, 7/8 | Cologne, Sun 31 Jan | same |
| Pre-draw allocations | Germany and Croatia to Munich; Italy and France to Stuttgart; Sweden and Denmark to Kiel; Portugal and Iceland to Magdeburg | draw article; https://www.ihf.info/media-center/news/ihf-confirms-team-allocations-2027-mens-world-championship |

All venues use CET (UTC+1).

Main sources:

- **Schedule PDF V1** (16 Jun 2026), linked from https://www.ihf.info/media-center/news/match-schedule-confirmed-2027-ihf-mens-world-championship (Dropbox link on that page).
- **Schedule PDF V2** (15 Oct 2026, same fixtures and times as V1) from the organiser site: https://www.handball2027.com/sites/staging.handballworldcup2027.com/files/2026-10/Spielplan_Schedule_WM27.pdf (linked at https://www.handball2027.com/de/wm-spielplan-modus).
- **ihf.info match pages** (pairings, home/away, venues, IHF match ids; no times entered, all "00:00"): https://www.ihf.info/competitions/men/308/30th-ihf-mens-handball-world-championship-2027-germany/263667/matches/2027-01-13 (and the other dates).
- **Regulations:** "Regulations for IHF Competitions (Indoor Handball)", edition 21 August 2024, the version linked on https://www.ihf.info/regulations-documents on 2026-11-24: https://www.ihf.info/sites/default/files/2024-12/Regulations%20for%20IHF%20Competitions_Indoor%20Handball_E.pdf . Chapter II covers Men's and Women's World Championships. I found no 2027-specific tournament manual or regulations online.

### 2.2 Groups and draw positions

The IHF lists the teams in this order (draw article and ihf.info standings), and the order is position 1–4. I confirmed it for all 8 groups against the regulations' day template (Ch. II §2.2.1, p. 27: Day 1 1–3, 2–4; Day 2 4–1, 2–3; Day 3 3–4, 1–2). Every official fixture, including its home/away order, matches, and `make_csvs.py` asserts this.

| Group (city) | 1 | 2 | 3 | 4 |
|---|---|---|---|---|
| A (Munich) | Germany | Serbia | Tunisia | Uruguay |
| B (Stuttgart) | Egypt | Italy | Cape Verde | Saudi Arabia |
| C (Munich) | Croatia | Spain | Chile | Türkiye |
| D (Stuttgart) | Argentina | France | Brazil | Kuwait |
| E (Kiel) | Sweden | Norway | Greece | Qatar |
| F (Magdeburg) | Portugal | Faroe Islands | Poland | Algeria |
| G (Kiel) | Denmark | Slovenia | United States of America | Angola |
| H (Magdeburg) | Iceland | North Macedonia | Bahrain | Japan |

Team-name variants: ihf.info match pages write "Turkiye" and the IHF articles write "Türkiye"; the CSV uses "Türkiye". The schedule PDF writes "USA"; the CSV uses "United States of America" as ihf.info does.

### 2.3 Schedule

The full schedule is in `wc2027m_schedule.csv`. `match_no` is the **ihf.info match id** (e.g. 307599), because the IHF publishes no sequential match numbers.

Group H (GETEC Arena, Magdeburg, CET), from schedule PDF V1/V2 and ihf.info:

| IHF id | Date | CET | Home | Away |
|---|---|---|---|---|
| 307597 | Fri 15 Jan | 18:00 | North Macedonia | Japan |
| 307599 | Fri 15 Jan | 20:30 | Iceland | Bahrain |
| 307601 | Sun 17 Jan | 18:00 | North Macedonia | Bahrain |
| 307603 | Sun 17 Jan | 20:30 | Japan | Iceland |
| 307605 | Tue 19 Jan | 18:00 | Bahrain | Japan |
| 307607 | Tue 19 Jan | 20:30 | Iceland | North Macedonia |

Match days by group: A and B play 13/15/17 Jan; C, D, E and F play 14/16/18 Jan; G and H play 15/17/19 Jan.

Preliminary-round times:

- 13 Jan: opening match Serbia–Uruguay at 17:30, then the opening ceremony and Germany–Tunisia at 20:30
- Otherwise 18:00 and 20:30
- Group D (France) shows "18:00/18:30" and "20:30/21:00", pending French broadcasters (IHF schedule news). Still unresolved in V2.

Basic times for the other rounds (IHF still to assign fixtures to slots):

- MR I, III, IV: 15:30 / 18:00 / 20:30
- MR II: 15:30/16:00 – 18:00/18:30 – 20:30/21:00
- Germany plays at 20:30 if it qualifies (MR I on 20/22/24 Jan; QF1 or QF2)
- President's Cup: 20–25 Jan at 11:00 and 13:30; 27 Jan at 10:30, 13:00, 15:30 and 18:00
- QFs: 26 Jan 17:30 and 20:30; 27 Jan 17:30/18:00 and 20:30/21:00
- Final weekend: 11:30/12:00, 14:30/15:00, 17:30/18:00, 20:30/21:00

Main-round fixtures (schedule PDF = regulations §2.2.2 template, p. 28):

- MR I (Cologne): 20 Jan 1A–3B, 1B–2A, 3A–2B · 22 Jan 2A–3B, 2B–1A, 3A–1B · 24 Jan 1B–1A, 2B–2A, 3B–3A
- MR II (Cologne): 21 Jan 1C–3D, 1D–2C, 3C–2D · 23 Jan 2C–3D, 2D–1C, 3C–1D · 25 Jan 1D–1C, 2D–2C, 3D–3C
- MR III (Hanover): 20 Jan 1E–3F, 1F–2E, 3E–2F · 22 Jan 2E–3F, 2F–1E, 3E–1F · 24 Jan 1F–1E, 2F–2E, 3F–3E
- MR IV (Hanover): 21 Jan 1G–3H, 1H–2G, 3G–2H · 23 Jan 2G–3H, 2H–1G, 3G–1H · 25 Jan 1H–1G, 2H–2G, 3H–3G

President's Cup (Magdeburg), from the schedule PDF:

- 20 Jan 4A–4B, 4E–4F · 21 Jan 4C–4D, 4G–4H · 22 Jan 4A–4E, 4B–4F · 23 Jan 4C–4G, 4D–4H · 24 Jan 4F–4A, 4B–4E · 25 Jan 4H–4C, 4D–4G
- 27 Jan placements 25/26, 27/28, 29/30, 31/32 (1st v 1st … 4th v 4th of the two PC groups)

### 2.4 Format (regulations Ch. II, plus the 2027 schedule)

- **Preliminary round:** 8 groups of 4, single round robin (§2.2.1, p. 27). Points 2/1/0 (§2.3.1, p. 30). No extra time in group matches: extra time applies only to knockout matches (§2.3.2, p. 31).
- **Main round (§2.2.1–2.2.2, pp. 27–28):** "The top three (3) teams each of preliminary round groups A/B, C/D, E/F and G/H are put together in groups MRI, MRII, MRIII and MRIV of the main round, respectively." That makes 4 groups of 6. Each team plays the 3 teams from the other preliminary group.
- **Carried-over results (p. 28 and §2.3.1, pp. 30–31):** "The results that teams achieved in the preliminary group against teams who have also advanced to the main round are carried over to the main round." Also: "For the main round, only those results from the preliminary round are taken into consideration that were obtained against teams that also qualified for the main round. This means that points achieved as well as goals scored and conceded against these teams are carried over […]". Each main-round team therefore starts with 2 carried results and plays 3 new matches.
- **Quarter-finals:** top 2 of each MR group (§2.2.2, p. 28). Pairings (§2.2.4, p. 29, and printed on the 2027 schedule):
  - QF1 = 1 MRI – 2 MRIII
  - QF2 = 1 MRIII – 2 MRI
  - QF3 = 1 MRII – 2 MRIV
  - QF4 = 1 MRIV – 2 MRII
- **Semi-finals:** SF1 = W QF1 – W QF3; SF2 = W QF2 – W QF4 (§2.2.6, p. 30). Final = W SF1 – W SF2; bronze = L SF1 – L SF2 (§2.2.7).
- **Places 5–8 (§2.2.5, pp. 29–30):** played "only in years prior to the Olympic Games". 2027 qualifies, and the schedule has them.
  - PM1 = L QF1 – L QF3; PM2 = L QF2 – L QF4 (29 Jan)
  - 5/6 = W PM1 – W PM2; 7/8 = L PM1 – L PM2 (31 Jan)
- **Places 9–24 (§2.3.3, p. 32):** no matches. 3rd in MR → 9–12, 4th → 13–16, 5th → 17–20, 6th → 21–24, ranked within each band by:
  - a) MR points; b) MR goal difference; c) MR goals scored
  - d) preliminary-round points, then preliminary-round GD, then preliminary-round goals
  - e) drawing of lots
  - "MR" here means the main-round table, including carried results.
- **President's Cup (places 25–32):**
  - The 4th-placed teams play two groups of 4, single round robin, 3 matches each. No results carry over, because all four come from different preliminary groups.
  - 2027 composition, from the schedule: one group of 4A, 4B, 4E, 4F and one of 4C, 4D, 4G, 4H.
  - This differs from the regulations' default (PCI = 4A–4D, PCII = 4E–4H, §2.2.1/§2.2.3, pp. 27–29). It is allowed by §2.4.1 (p. 33): "A different allocation of teams to compose the main round and President's Cup groups or a different pairing of teams in the match scheme may be implemented."
  - Placements: 25/26 1PCI–1PCII, 27/28 2nd v 2nd, 29/30, 31/32 (§2.2.3, p. 29).
  - In the CSV I labelled the 4A group "PC I" and the 4C group "PC II". That is an inference: no official 2027 document names them. ihf.info only shows "PCI1 v PCII1" etc. for the placements.
- **Draws in knockout matches (§2.3.2, p. 31):**
  - President's Cup placement matches (25–32): "in case of a draw at the end of the regular playing time, a direct 7 m shoot-out" (no extra time).
  - All other knockout matches (QF, SF, 5–8, bronze, final): after a 5-min break, overtime of 2×5 min (1-min half-time). If still tied, a second 2×5 min after another 5-min break. If still tied, a 7 m shoot-out per Rule 2:2 of the Rules of the Game.
  - The COC may order a direct shoot-out for specific matches if this is published with the announcement or schedule. I found none for 2027.

### 2.5 Tie-breaks (verbatim, §2.3.1, p. 31; applies to preliminary round, main round and President's Cup groups)

> If two (2) or more teams have gained the same number of points after the group matches have been completed, their ranking is decided based on the following:
> - results in points between the teams concerned
> - goal differences in the matches between the teams concerned
> - greater number of goals scored in the matches between the teams concerned
>
> If the teams are still equal, a decision is made between those teams with equal number of points as follows:
> - goal difference is subtracted in all matches
> - greater number of goals scored in all matches
>
> If a ranking is still not possible, a drawing of lots shall decide. The IHF representative on site shall execute the drawing of lots, if possible in the presence of the responsible team officials. […]

Order: points → H2H points → H2H GD → H2H goals → overall GD → overall goals → lots. There is no Fair Play step and no interim "during the round" rule.

In the main round, the carried preliminary results are part of the group table (see 2.4), so a carried match between two tied teams is one of "the matches between the teams concerned", and "all matches" means the main-round table. That reading is implied rather than stated. The regulations say nothing about re-applying head-to-head when 3+ teams are level (see E2).

---

## 3. "Believed" facts checked

| Believed | Verdict |
|---|---|
| W EURO 2026: 3–20 Dec 2026 | Correct (draw article; regulations Encl. 2) |
| Hosted by Poland, Romania, Czechia, Slovakia, Türkiye | Correct |
| Iceland in Group A in Oradea with Hungary, Montenegro, Slovenia | Correct. Iceland is A4 (pot 4). Venue: Oradea Arena, match days 3/5/7 Dec |
| Men's WC 2027: 13–31 Jan 2027 in Germany | Correct |
| Iceland in Group H in Magdeburg with North Macedonia, Bahrain, Japan | Correct. Iceland is H1 (pre-allocated to Magdeburg, pot 1). Venue: GETEC Arena, 15/17/19 Jan |

No believed fact is wrong.

---

## 4. Discrepancies / unverified

- **E1. EHF tie-break with 3+ teams level is ambiguous.** Clause (f) ("If the ranking of one of these teams is determined, the criteria are consecutively followed until the ranking of all teams is determined") does not say whether the mini-table (a)–(c) is recomputed among the remaining tied teams after one team is separated, or whether ranking simply continues to the next criterion. The literal reading is to continue down the list. I found no EHF clarification. A simulator should make this a switch.
- **E2. IHF tie-break with 3+ teams level is not specified.** The IHF text has no re-application clause at all. It is unspecified whether the head-to-head is recomputed for a sub-group once one team is separated.
- **E3. Women's EURO match numbering for the final weekend.** In the EHF API, 61/62 are the semi-finals, 63 is 5/6, 64 is bronze and 65 is the final. Regulations Enclosure 2 (p. 99) numbers 61 as 5/6, 62/63 as the semi-finals, 64 bronze and 65 final. The CSV uses the API numbering. Home/away for the second semi-final also differs: the API has 2 MR I v 1 MR II, and PDF v9 has 1.II – 2.I.
- **E4. EHF schedule versions.** The 22 Apr 2026 horizontal PDF (https://tickets.eurohandball.com/fileadmin/fm_de/EHF2026W/Schedule/260422_EHF2026-W_Match_Schedule_horizontal.pdf) has Group F on 6 Dec at 18:00 Germany–Serbia and 20:30 Sweden–Slovakia. PDF v9 and the API have 15:30 Sweden–Slovakia and 18:00 Germany–Serbia. The CSV follows the API. The EHF reserves the right to change match order for TV.
- **E5. Unpublished times.**
  - EHF main-round and semi-final fixture-to-slot allocation is not published (set after the preliminary and main rounds).
  - IHF Group D times are still "18:00/18:30" and "20:30/21:00" (CSV keeps both).
  - All IHF main-round, President's Cup, QF and final-weekend fixture-to-slot allocations are unpublished.
  - ihf.info shows 00:00 for every match.
- **E6. Wikipedia error.** The Wikipedia article on the 2027 WC (https://en.wikipedia.org/wiki/2027_World_Men%27s_Handball_Championship) puts main-round Groups II and IV in Cologne and I and III in Hanover. The official schedule (V1 and V2) and the IHF news ("Main Round Group II in Cologne") put MR I and II in Cologne and MR III and IV in Hanover. The QF dates are consistent with the official version: MR I and III finish 24 Jan and play QFs on 26 Jan.
- **E7. Kiel arena name.** "Wunderino Arena" (IHF draw article, Wikipedia) vs "MERKUR Ostseehalle" (IHF match pages, schedule PDFs, organiser site). Same venue; city Kiel either way.
- **E8. President's Cup group labels for 2027** (PC I = 4A/4B/4E/4F, PC II = 4C/4D/4G/4H) are my inference. The composition itself is official (schedule PDF), but the naming is not.
- **E9. IHF regulations edition.** The regulations used are the 21 Aug 2024 edition, the current link on the IHF regulations page. I found no 2027-specific manual or official announcement online, so any event-specific deviation in such a document (other than the PC grouping visible in the schedule) is unverified.
- **E10. Not read.** Rule 2:2 of the IHF Rules of the Game (7 m shoot-out procedure). The EHF penalty procedure is in regulations 9.43.
- **E11. EHF regulation typos (no effect on order).** 9.33 is lettered a, b, e, c. 9.42 points to "article 9.36" for penalty throws instead of 9.43.
