# Football v2 prototype: walk-forward evaluation tables and the two external
# benchmarks that exist for 2026 (production's stored forecasts, the Lengjan
# line).

fv2_match_key <- function(d) {
  paste(d$sex, d$match_date, d$home_team, d$away_team, sep = "|")
}

#' Read every walk-forward output for the given variants.
fv2_read_wf <- function(out_dir, variants = NULL) {
  files <- list.files(out_dir, pattern = "^wf_.*_(male|female)_[0-9]{4}\\.rds$", full.names = TRUE)
  parts <- lapply(files, readRDS)
  pred <- do.call(rbind, lapply(parts, `[[`, "pred"))
  hyper <- do.call(rbind, lapply(parts, `[[`, "hyper"))
  if (!is.null(variants)) {
    pred <- pred[pred$variant %in% variants, , drop = FALSE]
    hyper <- hyper[hyper$variant %in% variants, , drop = FALSE]
  }
  pred$key <- fv2_match_key(pred)
  list(pred = pred, hyper = hyper)
}

#' Mean scores per group. Log scores are reported as losses (lower is
#' better) to match how the 1X2 log loss is usually quoted.
fv2_score_table <- function(pred, by = "variant") {
  home_win <- as.numeric(pred$home_score > pred$away_score)
  agg <- function(v) tapply(v, pred[by], mean)
  idx <- interaction(pred[by], drop = TRUE, lex.order = TRUE)
  out <- unique(pred[by])
  out <- out[do.call(order, out), , drop = FALSE]
  key <- interaction(out, drop = TRUE, lex.order = TRUE)
  pick <- function(v) as.numeric(tapply(v, idx, mean)[as.character(key)])
  out$n <- as.integer(tapply(home_win, idx, length)[as.character(key)])
  out$logloss_1x2 <- -pick(pred$ls_1x2)
  out$scoreline <- -pick(pred$ls_score)
  out$rps <- pick(pred$rps)
  out$p_home <- pick(pred$p_home)
  out$obs_home <- pick(home_win)
  rownames(out) <- NULL
  out
}

#' Paired score differences of each variant against a reference, on the
#' matches both predicted. Positive means the variant scored better.
#'
#' The interval treats matches as independent (match-clustered); `se_week`
#' clusters by sex and cutoff week instead, for matches that share a fit.
fv2_compare <- function(pred, ref = "base", by = NULL, metric = c("ls_1x2", "ls_score")) {
  metric <- match.arg(metric)
  base <- pred[pred$variant == ref, , drop = FALSE]
  others <- setdiff(unique(pred$variant), ref)
  rows <- list()
  for (v in others) {
    d <- pred[pred$variant == v, , drop = FALSE]
    i <- match(d$key, base$key)
    d <- d[!is.na(i), , drop = FALSE]
    b <- base[i[!is.na(i)], , drop = FALSE]
    diff <- d[[metric]] - b[[metric]]
    week <- paste(d$sex, d$cutoff)
    groups <- if (is.null(by)) list(all = seq_len(nrow(d))) else split(seq_len(nrow(d)), d[by], drop = TRUE)
    for (g in names(groups)) {
      k <- groups[[g]]
      p <- fv2_paired(diff[k])
      rows[[length(rows) + 1L]] <- data.frame(
        variant = v, ref = ref, metric = metric, group = g,
        n = p$n, diff = p$mean, se = p$se, lo = p$lo, hi = p$hi,
        se_week = fv2_paired(diff[k], week[k])$se
      )
    }
  }
  do.call(rbind, rows)
}

#' Production's last stored pre-match 1X2 probabilities and the Lengjan line
#' for priced 2026 matches (the decide layer's candidates table).
fv2_priced_matches <- function(root = here::here("data")) {
  cand <- read_table("candidates", root = root, filter = list(sport = "football", country = "iceland"))
  cand <- as.data.frame(cand[cand$market == "moneyline" & !is.na(cand$p) & !is.na(cand$odds), ])
  cand$run_date <- as.Date(cand$run_date)
  cand <- cand[cand$run_date <= cand$match_date, ]
  key <- paste(cand$sex, cand$match_date, cand$home_team, cand$away_team)
  cand <- cand[as.numeric(cand$run_date) == stats::ave(as.numeric(cand$run_date), key, FUN = max), ]
  cand <- cand[!duplicated(cand[c("sex", "match_date", "home_team", "away_team", "run_date", "outcome")]), ]
  w <- stats::reshape(
    cand[c("sex", "match_date", "home_team", "away_team", "run_date", "outcome", "p", "odds")],
    idvar = c("sex", "match_date", "home_team", "away_team", "run_date"),
    timevar = "outcome", direction = "wide"
  )
  cols <- c("p.home", "p.draw", "p.away", "odds.home", "odds.draw", "odds.away")
  w <- w[stats::complete.cases(w[cols]), ]
  odds <- w[c("odds.home", "odds.draw", "odds.away")]
  prop <- fv2_devig(odds, "proportional")
  pow <- fv2_devig(odds, "power")
  p <- as.matrix(w[c("p.home", "p.draw", "p.away")])
  p <- p / rowSums(p)
  out <- data.frame(
    w[c("sex", "match_date", "home_team", "away_team", "run_date")],
    overround = rowSums(1 / odds),
    line_home = prop[, 1], line_draw = prop[, 2], line_away = prop[, 3],
    linepow_home = pow[, 1], linepow_draw = pow[, 2], linepow_away = pow[, 3],
    prod_home = p[, 1], prod_draw = p[, 2], prod_away = p[, 3]
  )
  out$key <- fv2_match_key(out)
  rownames(out) <- NULL
  out
}

#' Production's stored pre-round forecasts for every published division
#' (the publisher's lookahead-free round history), mapped back to matches.
#'
#' The store is per team and round; a match is recovered from the home
#' team's record where that team played exactly one match in the round and
#' the fit predates the match.
fv2_round_history <- function(results, sex, root = here::here("data"),
                              slugs = c(BD = "bd", LD1 = "ld", LD2 = "2deild", LD3 = "3deild")) {
  folder <- if (sex == "male") "karla" else "kvenna"
  out <- list()
  for (code in names(slugs)) {
    f <- file.path(
      root, "beliefs", "round_predictions_history", "football", "iceland",
      paste0(folder, "-", slugs[[code]]), "round_predictions_history.json"
    )
    if (!file.exists(f)) next
    rec <- jsonlite::fromJSON(f)$records
    rec <- rec[rec$n_matches == 1L, , drop = FALSE]
    m <- results[results$division == code & results$season %in% rec$season, , drop = FALSE]
    i <- match(paste(m$season, m$round, m$home_team), paste(rec$season, rec$round, rec$team))
    j <- match(paste(m$season, m$round, m$away_team), paste(rec$season, rec$round, rec$team))
    ok <- !is.na(i) & !is.na(j)
    m <- m[ok, , drop = FALSE]
    home <- rec[i[ok], , drop = FALSE]
    away <- rec[j[ok], , drop = FALSE]
    consistent <- abs(home$p_win - away$p_loss) < 0.02 & as.Date(home$fit_date) <= m$match_date
    out[[code]] <- data.frame(
      sex = sex, m[c("match_date", "home_team", "away_team")],
      fit_date = as.Date(home$fit_date),
      prodround_home = home$p_win, prodround_draw = home$p_draw, prodround_away = home$p_loss
    )[consistent, , drop = FALSE]
  }
  out <- do.call(rbind, out)
  s <- out$prodround_home + out$prodround_draw + out$prodround_away
  out[c("prodround_home", "prodround_draw", "prodround_away")] <- out[c("prodround_home", "prodround_draw", "prodround_away")] / s
  out$key <- fv2_match_key(out)
  rownames(out) <- NULL
  out
}

#' Join external 1X2 forecasts to one variant's walk-forward predictions and
#' score everything on the common matches.
#'
#' @param ext Data frame with `key` and columns `<name>_home/_draw/_away`
#'   for each name in `sources`.
fv2_benchmark <- function(pred, ext, sources, variant = "base") {
  d <- pred[pred$variant == variant, , drop = FALSE]
  i <- match(d$key, ext$key)
  d <- d[!is.na(i), , drop = FALSE]
  e <- ext[i[!is.na(i)], , drop = FALSE]
  d$ls_model <- d$ls_1x2
  for (s in sources) {
    d[[paste0("ls_", s)]] <- fv2_log_score_1x2(
      e[[paste0(s, "_home")]], e[[paste0(s, "_draw")]], e[[paste0(s, "_away")]],
      d$home_score, d$away_score
    )
    d[[paste0("phome_", s)]] <- e[[paste0(s, "_home")]]
  }
  d
}
