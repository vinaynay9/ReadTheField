#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

repo_root <- normalizePath(Sys.getenv("READTHEFIELD_REPO_ROOT", "."))
options(READTHEFIELD_REPO_ROOT = repo_root)
Sys.setenv(READTHEFIELD_REPO_ROOT = repo_root)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

source(file.path(repo_root, "R", "simulation", "bootstrap_simulation.R"))

pwi_path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")
if (!file.exists(pwi_path)) {
  stop("Missing player_week_identity.parquet: ", pwi_path)
}

pwi <- read_parquet(pwi_path)

pick_rows <- function(position, max_rows = 25) {
  feature_paths <- list(
    QB = file.path(repo_root, "data", "processed", "qb_player_weekly_features.parquet"),
    RB = file.path(repo_root, "data", "processed", "rb_weekly_features.parquet"),
    WR = file.path(repo_root, "data", "processed", "wr_weekly_features.parquet"),
    TE = file.path(repo_root, "data", "processed", "te_weekly_features.parquet"),
    K = file.path(repo_root, "data", "processed", "k_weekly_features.parquet")
  )
  feature_path <- feature_paths[[position]]
  candidates <- pwi %>%
    filter(.data$position == .env$position) %>%
    select(player_id, season, week, position)

  if (!is.null(feature_path) && file.exists(feature_path)) {
    feature_df <- tryCatch(read_parquet(feature_path), error = function(e) NULL)
    if (!is.null(feature_df) && all(c("player_id", "season", "week") %in% names(feature_df))) {
      candidates <- inner_join(
        candidates,
        feature_df %>% select(player_id, season, week) %>% distinct(),
        by = c("player_id", "season", "week")
      )
    }
  }

  rows <- candidates %>%
    arrange(desc(season), desc(week)) %>%
    slice(1:max_rows)
  if (nrow(rows) == 0) {
    stop("No PWI rows for position: ", position)
  }
  rows
}

assert_report <- function(res, position) {
  if (is.null(res) || !is.list(res) || isTRUE(res$ok) != TRUE) {
    stop("Simulation failed for position: ", position)
  }
  payload <- if (!is.null(res$data)) res$data else res
  if (is.null(payload$summary_tables) || length(payload$summary_tables) == 0) {
    stop("summary_tables empty for position: ", position)
  }
  required <- get_report_required_summary_v1(position)
  missing <- setdiff(required, names(payload$summary_tables))
  if (length(missing) > 0) {
    stop("Missing summary_tables for position ", position, ": ", paste(missing, collapse = ", "))
  }
  forbidden <- c("rushing_tds", "receiving_tds")
  present_forbidden <- intersect(forbidden, names(payload$summary_tables))
  if (length(present_forbidden) > 0) {
    stop("Forbidden summary_tables present for position ", position, ": ", paste(present_forbidden, collapse = ", "))
  }
  percentile_cols <- c("p10", "p25", "p40", "p50", "p60", "p75", "p90")
  for (stat in required) {
    rows <- payload$summary_tables[[stat]]
    if (is.null(rows) || !is.data.frame(rows)) {
      stop("summary_tables entry missing for stat ", stat, " position ", position)
    }
    missing_cols <- setdiff(percentile_cols, names(rows))
    if (length(missing_cols) > 0) {
      stop("summary_tables stat ", stat, " missing columns: ", paste(missing_cols, collapse = ", "))
    }
  }
  TRUE
}

positions <- c("QB", "RB", "WR", "TE", "K")

for (pos in positions) {
  rows <- pick_rows(pos)
  passed <- FALSE
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    res <- simulate_player_game_v1(
      player_id = row$player_id,
      season = row$season,
      week = row$week,
      n_sims = 200,
      availability_policy = "played_only",
      seed = 4242,
      schema_version = "v1",
      mode = "historical_replay"
    )
    ok <- TRUE
    tryCatch({
      assert_report(res, pos)
    }, error = function(e) {
      ok <<- FALSE
    })
    if (ok) {
      cat("PASS:", pos, "player_id=", row$player_id, "season=", row$season, "week=", row$week, "\n")
      passed <- TRUE
      break
    }
  }
  if (!passed) {
    stop("No valid simulation rows found for position: ", pos)
  }
}

cat("ALL REPORT CONTRACT TESTS PASSED\n")
