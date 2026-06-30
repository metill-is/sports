# WC Third-Place (Bronze) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a true 3rd-place (bronze) outcome to the WC 2026 forecast — the two semi-final losers play a neutral match — so the published podium has gold/silver/bronze, and the interactive `/hm2026` bracket shows a pinnable bronze node.

**Architecture:** The forward bracket model in `~/sports` is *analytic* (it propagates exact winner distributions, no Monte Carlo). The bronze final is the mirror of the existing Final computation, using each semi's **loser** distribution (`participants − winner`) against the same neutral win-matrix `W`. The 3rd-place match (FIFA match_no 103, already left as a gap before the Final at 104) lives in a *separate* `structure$third_place` row — NOT the main `bracket` tibble — so the hot propagation loop and pin-walker (which parse a `"W"` feeder prefix) stay untouched. Two repos: `~/sports` (model + publish) ships the new `bracket.json` / `tournament_placements.json` fields; `metill-platform` (consumer) renders them.

**Tech Stack:** R (testthat 3, devtools), the `~/sports` WC forecast package; FastAPI + Jinja2 + vanilla JS (`cup-bracket.js`) on `metill-platform`.

## Global Constraints

- **Bronze = neutral knockout match** between the two SF losers, scored with the same `W[a][b]` matrix as the Final. Documented assumption: real 3rd-place games are often rotated dead rubbers; the model treats it as a normal neutral match (consistent with the Final).
- **Match numbering:** the bronze is `match_no = 103L`, `round = "Third"`, `feeder_a = "L101"`, `feeder_b = "L102"` (loser of SF 101, loser of SF 102). `"L"` is a NEW feeder prefix meaning "loser of".
- **Do not add 103 to `structure$bracket`** — it would feed `as.integer(sub("W", "", "L101"))` → `NA` and break `wc_forward_bracket` + `.wc_knockout_pins`. It lives in `structure$third_place`.
- **Loser distribution formula:** `loser(m) = (participants_a(m) + participants_b(m)) − winner(m)`, clamped to ≥ 0. Same on the R and JS sides.
- **Correctness identity (the load-bearing test):** for every team, `P(Third) + P(Fourth) == P(reach SF) − P(reach Final)`, and `ΣP(Third) ≈ 1`, `ΣP(Fourth) ≈ 1`.
- **Backward compatibility:** the platform renderer must tolerate an *old* `bracket.json` with no match 103 (deploy-ordering safety). New fields added to existing JSON files are safe from the `rsync --delete` cron (only whole new *files* risk blanking — see `metill-platform/.claude/rules/deployment.md`).
- **Deploy ordering:** push `~/sports` main FIRST (so the pull-sports-data cron carries the new `bracket.json` shape), platform SECOND.
- **Copy:** Icelandic product strings in [[copy-voice]] — bronze round label `"3. sæti"`; podium medals `gull` / `silfur` / `brons`.

---

# PHASE A — `~/sports` model + publish

All paths in Phase A are under `/Users/brynjolfurjonsson/sports`. Run R via `devtools::load_all()` / `devtools::test(filter = "...")`. Test constructors live in `tests/testthat/helper-wc.R` (`make_sim_inputs`, `make_wc_fixtures`, `make_certain_occ`, `make_group_results_scored`).

### Task 1: `structure$third_place` row

**Files:**
- Modify: `R/wc-structure.R` (the `wc_structure()` return list, ~line 109–117)
- Test: `tests/testthat/test-wc-structure.R`

**Interfaces:**
- Produces: `structure$third_place` — a 1-row tibble `match_no` (103L), `round` ("Third"), `feeder_a` ("L101"), `feeder_b` ("L102"). `structure$bracket` is UNCHANGED (still 31 rows, no 103).

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-wc-structure.R`:

```r
test_that("wc_structure exposes a third_place row separate from the main bracket", {
  s <- wc_structure()
  expect_false(103L %in% s$bracket$match_no) # not in the hot-loop bracket
  tp <- s$third_place
  expect_s3_class(tp, "tbl_df")
  expect_equal(nrow(tp), 1L)
  expect_equal(tp$match_no, 103L)
  expect_equal(tp$round, "Third")
  expect_equal(tp$feeder_a, "L101")
  expect_equal(tp$feeder_b, "L102")
})
```

- [ ] **Step 2: Run it to confirm it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-structure.R")'`
Expected: FAIL — `s$third_place` is NULL (`$ operator` / `nrow(NULL)` error).

- [ ] **Step 3: Add the `third_place` row to `wc_structure()`**

In `R/wc-structure.R`, just before the final `list(...)` return, add:

```r
  third_place <- tibble::tibble(
    match_no = 103L, round = "Third", feeder_a = "L101", feeder_b = "L102"
  )
```

and add `third_place = third_place,` to the returned `list(...)` (next to `bracket = bracket,`). Update the roxygen `@return` to mention `third_place`.

- [ ] **Step 4: Run the test to confirm it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-structure.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports add R/wc-structure.R tests/testthat/test-wc-structure.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(wc): add third_place (match 103) row to structure"
```

### Task 2: compute Third/Fourth in `wc_forward_bracket`

**Files:**
- Modify: `R/wc-simulate.R` (`wc_forward_bracket`, lines 155–204)
- Test: `tests/testthat/test-wc-simulate.R`

**Interfaces:**
- Consumes: `structure$bracket` (to find the two SF match_nos and their feeders), an optional `third_pin` (team index forcing the bronze winner — used by Task A4).
- Produces: `placement` now also carries `round_name` `"Third"` and `"Fourth"` rows (per team), factor levels extended to `c("R32","R16","QF","SF","Final","Champion","Third","Fourth")`. New signature: `wc_forward_bracket(W, occ_a, occ_b, bracket, teams, pins = list(), third_pin = NULL)`.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-wc-simulate.R`:

```r
test_that("wc_forward_bracket emits Third/Fourth consistent with the SF-loser pool", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 100L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 6L)
  bm <- out$bracket_model

  fb <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams)
  third <- fb$placement[fb$placement$round_name == "Third", ]
  fourth <- fb$placement[fb$placement$round_name == "Fourth", ]

  # exactly one bronze winner and one fourth-placed team across the field
  expect_equal(sum(third$probability), 1, tolerance = 1e-9)
  expect_equal(sum(fourth$probability), 1, tolerance = 1e-9)

  # per-team identity: Third + Fourth == reach(SF) - reach(Final) (the SF-loser pool)
  pool <- fb$reach$SF - fb$reach$Final
  third <- third[match(teams, third$team), ]
  fourth <- fourth[match(teams, fourth$team), ]
  expect_equal(third$probability + fourth$probability, pool, tolerance = 1e-9)
  expect_true(all(third$probability >= -1e-12))
})

test_that("wc_forward_bracket third_pin forces the bronze winner", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 80L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 6L)
  bm <- out$bracket_model
  x <- which(teams == "Brazil")
  fb <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams, third_pin = x)
  third <- fb$placement[fb$placement$round_name == "Third", ]
  expect_equal(third$probability[third$team == "Brazil"], 1, tolerance = 1e-9)
  expect_equal(sum(third$probability), 1, tolerance = 1e-9)
})
```

- [ ] **Step 2: Run to confirm it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-simulate.R")'`
Expected: FAIL — no `"Third"` rows in `placement` (empty subset).

- [ ] **Step 3: Implement Third/Fourth in `wc_forward_bracket`**

In `R/wc-simulate.R`, change the signature to add `third_pin = NULL`. After the `champ <- winner[[final_m]]` line (191), before building `placement`, insert:

```r
  # ---- Third place (bronze): the two SF losers play a neutral match ----------
  # loser(m) = (the two teams that played m) - (winner of m), clamped >= 0.
  sf_no <- bracket$match_no[bracket$round == "SF"]
  loser_of <- function(mno) {
    row <- bracket[bracket$match_no == mno, ]
    fa <- as.integer(sub("W", "", row$feeder_a)); fb <- as.integer(sub("W", "", row$feeder_b))
    part <- winner[[fa]] + winner[[fb]]
    l <- part - winner[[mno]]
    l[l < 0] <- 0
    l
  }
  lA <- loser_of(sf_no[1]); lB <- loser_of(sf_no[2])
  bronze <- lA * as.vector(Wm %*% lB) + lB * as.vector(Wm %*% lA)
  sb <- sum(bronze)
  if (sb > 0) bronze <- bronze / sb * sum(lA) # scale to the SF-loser pool mass (~1)
  if (!is.null(third_pin)) {
    other <- (lA + lB)
    bronze <- numeric(nt); bronze[third_pin] <- 1
  }
  fourth <- (lA + lB) - bronze
  fourth[fourth < 0] <- 0
```

Then extend the `placement` assembly and factor levels:

```r
  placement <- do.call(rbind, c(
    lapply(rounds, function(r) {
      tibble::tibble(team = teams, round_name = r, probability = reach[[r]])
    }),
    list(
      tibble::tibble(team = teams, round_name = "Champion", probability = champ),
      tibble::tibble(team = teams, round_name = "Third", probability = bronze),
      tibble::tibble(team = teams, round_name = "Fourth", probability = fourth)
    )
  ))
  placement$round_name <- factor(placement$round_name,
    levels = c(rounds, "Champion", "Third", "Fourth")
  )
```

(Leave `reach` and `winner` in the returned list unchanged.)

- [ ] **Step 4: Run to confirm it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-simulate.R")'`
Expected: PASS (including the existing monotone/pins test).

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports add R/wc-simulate.R tests/testthat/test-wc-simulate.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(wc): analytic Third/Fourth from SF-loser distributions in wc_forward_bracket"
```

### Task 3: publish Third/Fourth + the bronze node

**Files:**
- Modify: `R/wc-publish.R` — `tournament_placements.json` summary (~line 220–231) and `bracket.json` matches (~line 279–284)
- Test: `tests/testthat/test-wc-publish.R`

**Interfaces:**
- Consumes: `sim_out$placement_probs` (now with Third/Fourth rows from A2), `structure$third_place` (A1).
- Produces: `tournament_placements.json` `records` gains `round_name` "Third"/"Fourth" automatically; `summary` gains `p_bronze` per team. `bracket.json` `matches` gains the 103 "Third" row.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-wc-publish.R`:

```r
test_that("publish writes Third/Fourth placements and a bronze bracket node", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 60L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 3L)
  root <- withr::local_tempdir()
  publish_world_cup(out, s, si$team, fit_date = as.Date("2026-06-30"),
    generated_at = "2026-06-30T00:00:00Z", n_draws = 60L, root = root)

  tp <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "tournament_placements.json"),
    simplifyVector = FALSE
  )
  rounds <- vapply(tp$records, function(r) r$round_name, character(1))
  expect_true("Third" %in% rounds)
  expect_true("Fourth" %in% rounds)
  expect_true(!is.null(tp$summary[[1]]$p_bronze))

  br <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "bracket.json"),
    simplifyVector = FALSE
  )
  thirds <- Filter(function(m) m$round == "Third", br$matches)
  expect_length(thirds, 1L)
  expect_equal(thirds[[1]]$match_no, 103L)
  expect_equal(thirds[[1]]$feeder_a, "L101")
})
```

(Adjust `publish_world_cup(...)` arg names to match the real signature — check the top of `R/wc-publish.R`; the existing `test-wc-publish.R` tests already call it, copy their call shape.)

- [ ] **Step 2: Run to confirm it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-publish.R")'`
Expected: FAIL — no "Third" in records / no 103 in matches / `p_bronze` NULL.

- [ ] **Step 3: Implement the publisher changes**

In `R/wc-publish.R`:

(a) Append the bronze node to `bracket.json` matches — replace the `matches = lapply(seq_len(nrow(structure$bracket)), ...)` block so it iterates `rbind(structure$bracket, structure$third_place)`:

```r
    matches = local({
      allm <- rbind(structure$bracket, structure$third_place)
      lapply(seq_len(nrow(allm)), function(i) {
        list(
          match_no = allm$match_no[i], round = allm$round[i],
          feeder_a = allm$feeder_a[i], feeder_b = allm$feeder_b[i]
        )
      })
    }),
```

(b) Add `p_bronze` to the tournament_placements `summary`. After the `champ <- pp[pp$round_name == "Champion", ...]` block, build a bronze lookup and include it:

```r
  bronze <- pp[pp$round_name == "Third", c("team", "probability")]
  bz <- stats::setNames(bronze$probability, bronze$team)
  summary_payload <- lapply(seq_len(nrow(champ)), function(i) {
    list(
      team = champ$team[i], team_is = is_name(champ$team[i]),
      p_champion = rnd(champ$probability[i]),
      p_bronze = rnd(unname(bz[champ$team[i]]))
    )
  })
```

- [ ] **Step 4: Run to confirm it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-publish.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports add R/wc-publish.R tests/testthat/test-wc-publish.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(wc): publish Third/Fourth placements + bronze (103) bracket node"
```

### Task 4: pin a played bronze result

**Files:**
- Modify: `R/wc-knockout.R` (`.wc_knockout_pins`, after the main loop) and `R/wc-simulate.R` (`simulate_world_cup`, the `kp`/`fwd` block ~lines 553–556)
- Test: `tests/testthat/test-wc-knockout.R` and `tests/testthat/test-wc-simulate.R`

**Interfaces:**
- Consumes: `structure$bracket` (SF feeders), `structure$third_place`, `knockout_results`, `shootout_winners`.
- Produces: `.wc_knockout_pins(...)` returns `pins[["103"]]` (bronze winner index) and a `played` record with `match_no = 103L` when both SFs are decided and the bronze result is on record. `simulate_world_cup` passes `pins[["103"]]` as `third_pin` to `wc_forward_bracket`, so a played bronze collapses `Third` to the winner.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-wc-knockout.R`:

```r
test_that(".wc_knockout_pins resolves the bronze (103) once both SFs are decided", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  # Drive both SFs to a known state by feeding every match on one path.
  # Build results so that SF 101 and SF 102 are decided, then the bronze.
  # Helper: walk the bracket giving slot-a the win at each round.
  res <- wc_bronze_test_results(s, teams, occ) # defined in helper below
  kp <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, res$kr,
    shootout_winners = NULL, third_place = s$third_place)
  expect_false(is.null(kp$pins[["103"]]))
  expect_equal(kp$pins[["103"]], res$bronze_winner_idx)
  thirdrec <- Filter(function(p) p$match_no == 103L, kp$played)
  expect_length(thirdrec, 1L)
})
```

Add the helper to `tests/testthat/helper-wc.R` (it constructs a fully-decided knockout where slot-a always wins, so the SF losers and bronze are deterministic):

```r
# Fully-decided knockout (slot-a wins every match) for the given one-hot occ,
# returning the results tibble plus the deterministic bronze winner index.
wc_bronze_test_results <- function(structure, teams, occ) {
  tidx <- stats::setNames(seq_along(teams), teams)
  winner <- list()
  rows <- list()
  add <- function(w, l) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      match_date = as.Date("2026-07-10"),
      home_team = teams[w], away_team = teams[l], home_score = 2L, away_score = 0L
    )
  }
  r32 <- which(structure$bracket$round == "R32")
  for (k in seq_along(r32)) {
    a <- which(occ$occ_a[k, ] >= 0.9995); b <- which(occ$occ_b[k, ] >= 0.9995)
    winner[[as.character(structure$bracket$match_no[r32[k]])]] <- a
    add(a, b)
  }
  for (i in which(structure$bracket$round != "R32")) {
    m <- structure$bracket$match_no[i]
    fa <- sub("W", "", structure$bracket$feeder_a[i]); fb <- sub("W", "", structure$bracket$feeder_b[i])
    a <- winner[[fa]]; b <- winner[[fb]]
    winner[[as.character(m)]] <- a
    add(a, b)
  }
  # SF losers: loser of 101 and 102 (slot-b of each SF's winner pairing)
  sf <- structure$bracket$match_no[structure$bracket$round == "SF"]
  loser_sf <- function(mno) {
    i <- which(structure$bracket$match_no == mno)
    fa <- sub("W", "", structure$bracket$feeder_a[i]); fb <- sub("W", "", structure$bracket$feeder_b[i])
    setdiff(c(winner[[fa]], winner[[fb]]), winner[[as.character(mno)]])
  }
  l1 <- loser_sf(sf[1]); l2 <- loser_sf(sf[2])
  add(l1, l2) # bronze: l1 beats l2
  list(kr = dplyr::bind_rows(rows), bronze_winner_idx = l1)
}
```

- [ ] **Step 2: Run to confirm it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-wc-knockout.R")'`
Expected: FAIL — `.wc_knockout_pins` has no `third_place` arg / no `pins[["103"]]`.

- [ ] **Step 3: Implement the bronze pin**

In `R/wc-knockout.R`, add `third_place = NULL` to the `.wc_knockout_pins` signature. After the main `for` loop (before `list(pins = pins, played = played)`), insert:

```r
  # ---- Bronze (3rd place): SF losers, resolved only once both SFs are pinned --
  if (!is.null(third_place) && nrow(third_place) == 1L) {
    sf <- bracket$match_no[bracket$round == "SF"]
    loser_sf <- function(mno) {
      i <- which(bracket$match_no == mno)
      fa <- as.integer(sub("W", "", bracket$feeder_a[i]))
      fb <- as.integer(sub("W", "", bracket$feeder_b[i]))
      wsf <- winner_of[[as.character(mno)]]
      parts <- c(winner_of[[as.character(fa)]], winner_of[[as.character(fb)]])
      if (is.null(wsf) || length(parts) < 2L) return(NA_integer_)
      setdiff(parts, wsf)
    }
    lA <- loser_sf(sf[1]); lB <- loser_sf(sf[2])
    if (!is.na(lA) && !is.na(lB)) {
      hit <- which(rk == .wc_pair_key(teams[lA], teams[lB]))
      if (length(hit) > 0L) {
        row <- knockout_results[hit[[1L]], , drop = FALSE]
        w_name <- .wc_knockout_winner_of(row, shootout_winners)
        if (!is.na(w_name)) {
          wi <- tidx[[w_name]]; li <- if (wi == lA) lB else lA
          home_wins <- identical(row$home_team[[1L]], w_name)
          pins[["103"]] <- wi
          played[[length(played) + 1L]] <- list(
            match_no = 103L, winner = wi, loser = li,
            winner_score = as.integer(if (home_wins) row$home_score[[1L]] else row$away_score[[1L]]),
            loser_score = as.integer(if (home_wins) row$away_score[[1L]] else row$home_score[[1L]]),
            shootout = isTRUE(row$home_score[[1L]] == row$away_score[[1L]])
          )
        }
      }
    }
  }
```

In `R/wc-simulate.R`, thread it through `simulate_world_cup`: change the `kp <- .wc_knockout_pins(...)` call to pass `third_place = structure$third_place`, and the `fwd <- wc_forward_bracket(...)` call to pass `third_pin = kp$pins[["103"]]`:

```r
  kp <- .wc_knockout_pins(
    structure$bracket, teams, occ_a, occ_b, knockout_results, shootout_winners,
    third_place = structure$third_place
  )
  fwd <- wc_forward_bracket(W, occ_a, occ_b, structure$bracket, teams,
    pins = kp$pins, third_pin = kp$pins[["103"]]
  )
```

- [ ] **Step 4: Add the simulate-level conditioning test, then run all WC tests**

Add to `tests/testthat/test-wc-simulate.R`:

```r
test_that("simulate_world_cup collapses Third onto a played bronze result", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  fx <- make_group_results_scored(s)
  si <- make_sim_inputs(teams, n_draws = 40L)
  res <- wc_bronze_test_results(s, teams, occ)
  out <- simulate_world_cup(si$team, si$scalar, fx, s,
    pairing_seed = 5L, knockout_results = res$kr)
  third <- out$placement_probs[out$placement_probs$round_name == "Third", ]
  expect_equal(third$probability[res$bronze_winner_idx], 1, tolerance = 1e-9)
  expect_true(any(vapply(out$bracket_model$played, function(p) p$match_no == 103L, logical(1))))
})
```

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "wc")'`
Expected: PASS (all wc-* tests).

Note: `make_group_results_scored` only fixes the group stage; `wc_bronze_test_results` supplies the full knockout chain so the SFs are pinned and the bronze resolves. If the group occupancy in `out` differs from `occ`, align by building `occ` from `out$bracket_model$occ_a/occ_b` instead — verify slot-a/-b certainty as the existing conditioning test does (test-wc-simulate.R:186–191).

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports add R/wc-knockout.R R/wc-simulate.R tests/testthat/
git -C /Users/brynjolfurjonsson/sports commit -m "feat(wc): pin a played bronze (103) result onto the forecast"
```

### Task 5: regenerate JSON, full check, push upstream

**Files:** none new — regenerates `data/publish/world_cup/karla/{tournament_placements,bracket}.json`.

- [ ] **Step 1: Full package test + check**

Run: `Rscript -e 'devtools::test(".")'` then `Rscript -e 'devtools::check(".", error_on = "warning")'`
Expected: 0 failures; check clean (or pre-existing notes only).

- [ ] **Step 2: Regenerate the published WC JSON**

Run the daily forecast entrypoint to rebuild the publish tree from the current fit:
`Rscript scripts/wc/forecast.R` (or `scripts/wc/refresh_now.sh` if a fresh martj42 pull is wanted).
Expected: writes `data/publish/world_cup/karla/*.json`.

- [ ] **Step 3: Eyeball the new fields**

```bash
python3 -c "import json; d=json.load(open('/Users/brynjolfurjonsson/sports/data/publish/world_cup/karla/tournament_placements.json')); print('Third' in {r['round_name'] for r in d['records']}, 'p_bronze' in d['summary'][0])"
python3 -c "import json; d=json.load(open('/Users/brynjolfurjonsson/sports/data/publish/world_cup/karla/bracket.json')); print([m for m in d['matches'] if m['round']=='Third'])"
```
Expected: `True True` and a single `match_no 103` Third node.

- [ ] **Step 4: Commit + push upstream FIRST**

```bash
git -C /Users/brynjolfurjonsson/sports add data/publish/world_cup/karla/
git -C /Users/brynjolfurjonsson/sports commit -m "data(wc): publish true podium (gold/silver/bronze)"
git -C /Users/brynjolfurjonsson/sports pull --rebase && git -C /Users/brynjolfurjonsson/sports push
```

---

# PHASE B — `metill-platform` consumer

All paths in Phase B are under `/Users/brynjolfurjonsson/metill-platform`. Do this on a feature branch (`git -C ... checkout -b wc-bronze`). Verify with `uv run --extra dev pytest tests/` + the `preview_*` workflow (NOT after Phase A is pushed strictly — the renderer must tolerate the old JSON too).

### Task 6: teach `cup-bracket.js` the `"L"` (loser) feeder + render the Third node

**Files:**
- Modify: `app/static/js/cup-bracket.js` (`propagate()` ~73–102; `childrenOf()` ~116–130; round-label dict)
- Verify: `preview_*` (no JS unit harness)

**Interfaces:**
- Consumes: `bracket.json` `matches` now including `{match_no:103, round:"Third", feeder_a:"L101", feeder_b:"L102"}`.
- Produces: a rendered "3. sæti" node between the SFs and the Final, fed by the two SF losers, that re-propagates on what-if pins.

- [ ] **Step 1: Verify the played→completed mapping (read-only)**

Read how `cup-bracket.js` receives `played`/`completed` and how `app/routes/hm2026.py` passes `bracket.json` to it (the Explore map noted the JS reads `B.completed` while the publisher writes `played` — confirm whether the route or template renames it). Note the exact field so the bronze `played` record (match 103) surfaces the same way the other knockouts do. Do not edit yet.

- [ ] **Step 2: Add the `loser` dict + prefix dispatch in `propagate()`**

In `app/static/js/cup-bracket.js` `propagate()`, initialise `const winner = {}, loser = {}, slots = {};`. In the non-leaf branch, replace the two feeder lookups with prefix-aware dispatch:

```javascript
} else {
  const srcA = m.feeder_a[0] === "L" ? loser : winner;
  const srcB = m.feeder_b[0] === "L" ? loser : winner;
  dA = srcA[+m.feeder_a.slice(1)];
  dB = srcB[+m.feeder_b.slice(1)];
}
```

After `winner[m.match_no] = wd;`, add the loser distribution:

```javascript
const lo = new Float64Array(nt);
for (let t = 0; t < nt; t++) lo[t] = Math.max(0, dA[t] + dB[t] - wd[t]);
loser[m.match_no] = lo;
```

Return `loser` alongside the rest: `return { winner, slots, scenarioP, loser };`

- [ ] **Step 3: Accept `"L"` feeders in `childrenOf()`**

Change the non-leaf return (line ~129) so loser-feeders also resolve to their SF match node (for connector drawing):

```javascript
return ["feeder_a", "feeder_b"].map(k => {
  const f = m[k];
  return (f[0] === "W" || f[0] === "L") ? mIndex[+f.slice(1)] : null;
});
```

- [ ] **Step 4: Add the round label**

Find the round-label dict (the Explore map called it `RLABEL`/`RWIN`/`RREACH`) and add a `Third: "3. sæti"` entry (and any reach/win label variant the SF/Final use), so the new column header and feeder chips read in Icelandic.

- [ ] **Step 5: Verify in the browser preview**

Start the preview at the worktree, open `/hm2026`, and confirm: (a) a "3. sæti" node renders between the SFs and the Final; (b) clicking a what-if pin still re-propagates without console errors; (c) with the *current* committed `bracket.json` (which has 103 once Phase A is pulled, or lacks it before) the page still renders — test BOTH by temporarily pointing at an old bracket.json if needed.

Run: `preview_start` (point at the worktree checkout) → `preview_eval window.location='/hm2026'` → `preview_console_logs` (expect no errors) → `preview_screenshot`.

- [ ] **Step 6: Commit**

```bash
git -C /Users/brynjolfurjonsson/metill-platform add app/static/js/cup-bracket.js
git -C /Users/brynjolfurjonsson/metill-platform commit -m "feat(hm2026): render the 3rd-place (bronze) bracket node"
```

### Task 7: surface the podium (gull / silfur / brons) on `/hm2026`

**Files:**
- Modify: `app/routes/hm2026.py` (compute a podium view from `tournament_placements.json`) and `app/templates/ithrottir_hm2026.html` (a small podium panel)
- Test: `tests/` (route test) + preview

**Interfaces:**
- Consumes: `tournament_placements.json` records (`round_name` Champion/Final/Third) → gold = Champion, silver = Final − Champion, bronze = Third.
- Produces: a podium panel listing the top contenders by gold/silver/bronze.

- [ ] **Step 1: Write the failing route test**

In `tests/` (mirror an existing hm2026 route test), assert the `/hm2026` page renders a podium with a bronze figure:

```python
async def test_hm2026_shows_bronze_podium(client):
    r = await client.get("/hm2026")
    assert r.status_code == 200
    assert "brons" in r.text.lower()  # podium panel present
```

- [ ] **Step 2: Run to confirm it fails**

Run: `uv run --extra dev pytest tests/ -k hm2026 -q`
Expected: FAIL (`brons` not in page).

- [ ] **Step 3: Compute the podium in the route + render the panel**

In `app/routes/hm2026.py`, build a `podium` list from `tournament_placements.json` (gold = `p_champion` from summary; silver = `Final` record prob − `p_champion`; bronze = `Third` record prob / summary `p_bronze`), sorted by gold, top ~8, and pass to the template. In `ithrottir_hm2026.html`, add a compact podium panel (three columns gull/silfur/brons) above or beside the bracket, in [[copy-voice]] Icelandic. Reuse the existing fact-pack/table styling — no new CSS system.

- [ ] **Step 4: Run to confirm it passes + preview**

Run: `uv run --extra dev pytest tests/ -k hm2026 -q` → PASS.
Then `preview_screenshot` of `/hm2026` showing the podium.

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/metill-platform add app/routes/hm2026.py app/templates/ithrottir_hm2026.html tests/
git -C /Users/brynjolfurjonsson/metill-platform commit -m "feat(hm2026): podium panel (gull/silfur/brons) from true placements"
```

### Task 8: ship

- [ ] **Step 1: Full platform test + lint**

Run: `uv run --extra dev pytest tests/` and `uv run --extra dev ruff check .`
Expected: green.

- [ ] **Step 2: Open the PR (platform) AFTER `~/sports` main is pushed**

```bash
git -C /Users/brynjolfurjonsson/metill-platform pull --rebase origin main
git -C /Users/brynjolfurjonsson/metill-platform push -u origin wc-bronze
gh pr create --repo metill-is/metill-platform --title "WC bronze: true podium + 3rd-place bracket node" --body "..."
```

Confirm the pull-sports-data cron has carried the new `bracket.json`/`tournament_placements.json` (with match 103 + Third/Fourth) into `data/ithrottir/world_cup/karla/` before/at merge, so the renderer has real data. The renderer is backward-compatible, so a brief ordering gap degrades gracefully (no bronze node until the data lands).

---

## Self-Review

- **Spec coverage:** Level 1 (true podium probabilities) = A2 + A3 + B2. Level 2 (pinnable bracket node) = A1 + A4 + B1. The "no 3rd-place match in the sim" gap is fully closed.
- **Type consistency:** `third_pin` (A2) ← `kp$pins[["103"]]` (A4); `structure$third_place` (A1) used by A3/A4; loser formula identical in R (`participants − winner`) and JS. `round_name` strings "Third"/"Fourth" consistent across A2/A3/B2.
- **Open verification points flagged inline:** the played→completed field mapping (B1 Step 1); aligning the bronze test's occupancy with `simulate_world_cup`'s actual occupancy (A4 Step 4); `publish_world_cup` arg names (A3 Step 1).
