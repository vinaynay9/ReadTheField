# Smoke Test for QB Simulation
#
# This script tests the QB simulation pipeline end-to-end without calling refresh.
# It loads cached QB features, fits models, and runs a QB simulation.
#
# Exit codes:
#   0: All tests passed
#   1: Test failed (error occurred)

if (file.exists("README.md") && file.exists("R") && file.exists("scripts")) {
  # Already in project root
} else {
  script_path <- commandArgs(trailingOnly = FALSE)
  if (length(script_path) > 0) {
    script_file <- sub("--file=", "", script_path[grep("--file=", script_path)])
    if (length(script_file) > 0 && file.exists(script_file)) {
      project_root <- dirname(dirname(normalizePath(script_file)))
      setwd(project_root)
    }
  }
}

cat("Loading simulation bootstrap...\n")
tryCatch({
  source("R/simulation/bootstrap_simulation.R")
  cat("  Bootstrap loaded successfully\n")
}, error = function(e) {
  cat("ERROR: Failed to load bootstrap:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 1: Loading cached QB weekly features...\n")
tryCatch({
  qb_features <- read_qb_weekly_features_cache() |>
    as.data.frame()
  if (nrow(qb_features) == 0) {
    stop("QB weekly features cache is empty. Run scripts/refresh_weekly_cache.R first.")
  }
  cat("  Loaded", nrow(qb_features), "rows\n")

  required_cols <- c(
    "player_id", "season", "week",
    "target_pass_attempts_qb",
    "target_pass_yards_qb",
    "target_pass_tds_qb",
    "target_interceptions_qb_thrown",
    "target_sacks_qb_taken"
  )
  missing_cols <- setdiff(required_cols, names(qb_features))
  if (length(missing_cols) > 0) {
    stop("QB weekly features missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  qb_stats <- read_qb_weekly_stats_cache() |>
    as.data.frame()
  if (nrow(qb_stats) == 0) {
    stop("QB weekly stats cache is empty.")
  }
  cat("  Loaded raw QB weekly stats with", nrow(qb_stats), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to load QB weekly features or stats:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 2: Fitting QB models...\n")
tryCatch({
  train_df <- qb_features |>
    dplyr::filter(season >= 2010, season <= 2023)
  if (nrow(train_df) < 1000) {
    stop("QB training window too small: ", nrow(train_df))
  }

  qb_models <- fit_qb_models(train_df, min_rows = 200)
  qb_targets <- get_qb_v1_targets()
  qb_regimes <- get_qb_regimes()
  required_keys <- c()
  for (target in qb_targets) {
    for (regime in qb_regimes) {
      required_keys <- c(required_keys, get_qb_model_key(target, regime))
    }
  }
  if (is.null(qb_models$models) || any(!required_keys %in% names(qb_models$models))) {
    stop("QB models failed validation.")
  }

  cat("  Models fitted and validated\n")
}, error = function(e) {
  cat("ERROR: Failed to fit QB models:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 3: Finding test QB player-week...\n")
tryCatch({
  test_season <- 2024
  test_games <- NULL
  test_week <- NA_integer_

  for (wk in 8:12) {
    feat_cols <- get_qb_features_by_week(wk)
    candidates <- qb_features[
      qb_features$season == test_season &
        qb_features$week == wk &
        !is.na(qb_features$player_id),
      , drop = FALSE
    ]
    if (nrow(candidates) == 0) next
    available_cols <- feat_cols[sapply(feat_cols, function(f) !all(is.na(candidates[[f]])))]
    complete_mask <- stats::complete.cases(candidates[, available_cols, drop = FALSE])
    candidates <- candidates[complete_mask, , drop = FALSE]
    if (nrow(candidates) > 0) {
      test_games <- candidates
      test_week <- wk
      break
    }
  }

  if (is.null(test_games) || nrow(test_games) == 0) {
    stop("No QB games found for season ", test_season, " with complete feature set.")
  }
  test_player_id <- test_games$player_id[1]
  test_player_name <- test_games$player_name[1]
  test_team <- test_games$team[1]
  test_opponent <- test_games$opponent[1]

  cat("  Test player:", test_player_name, "(gsis_id:", test_player_id, ")\n")
  cat("  Test game: Season", test_season, "Week", test_week,
      test_team, "vs", test_opponent, "\n")
}, error = function(e) {
  cat("ERROR: Failed to find test QB player-week:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 4: Running QB simulation...\n")
tryCatch({
  result <- simulate_player_game(
    gsis_id = test_player_id,
    season = test_season,
    week = test_week,
    n_sims = 500,
    mode = "historical_replay"
  )

  if (is.null(result) || is.null(result$draws) || nrow(result$draws) != 500) {
    stop("QB simulation draws invalid.")
  }

  required_outcomes <- c(
    "passing_attempts",
    "passing_yards",
    "passing_tds",
    "interceptions_thrown",
    "qb_sacks_taken",
    "qb_rush_attempts",
    "qb_rush_yards"
  )
  missing_outcomes <- setdiff(required_outcomes, names(result$draws))
  if (length(missing_outcomes) > 0) {
    stop("Missing QB outcomes in draws: ", paste(missing_outcomes, collapse = ", "))
  }

  cat("  QB simulation completed successfully\n")
  cat("  Draws:", nrow(result$draws), "rows\n")
}, error = function(e) {
  cat("ERROR: QB simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nAll QB smoke tests passed!\n")
quit(status = 0)
