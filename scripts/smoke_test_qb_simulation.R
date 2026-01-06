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
    "game_id",
    "target_pass_attempts_qb",
    "target_pass_yards_qb",
    "target_pass_tds_qb",
    "target_interceptions_qb_thrown",
    "target_sacks_qb_taken",
    "target_qb_rush_tds",
    "target_qb_rush_tds_roll1",
    "target_qb_rush_tds_roll3",
    "target_qb_rush_tds_roll5"
  )
  missing_cols <- setdiff(required_cols, names(qb_features))
  if (length(missing_cols) > 0) {
    stop("QB weekly features missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (any(is.na(qb_features[["game_id"]]))) {
    stop("QB weekly features contain missing game_id values.")
  }

  qb_stats <- read_qb_weekly_stats_cache() |>
    as.data.frame()
  if (nrow(qb_stats) == 0) {
    stop("QB weekly stats cache is empty.")
  }
  cat("  Loaded raw QB weekly stats with", nrow(qb_stats), "rows\n")

  # Recompute roll3 sacks for a random sample and compare.
  set.seed(42)
  sample_rows <- qb_features |>
    dplyr::filter(week >= 4, !is.na(target_sacks_qb_taken_roll3)) |>
    dplyr::sample_n(size = min(30, nrow(qb_features)))

  pick_col <- function(df, candidates) {
    for (candidate in candidates) {
      if (candidate %in% names(df)) {
        return(df[[candidate]])
      }
    }
    rep(NA_real_, nrow(df))
  }

  roll_mean <- function(x, n) {
    if (length(x) < n || any(is.na(tail(x, n)))) return(NA_real_)
    mean(tail(x, n))
  }

  for (i in seq_len(nrow(sample_rows))) {
    row <- sample_rows[i, ]
    stats_player <- qb_stats |>
      dplyr::filter(player_id == row$player_id, season == row$season) |>
      dplyr::arrange(week)
    stats_player$sacks_taken <- as.numeric(pick_col(stats_player, c("sacks_suffered", "sacks_taken", "passing_sacks")))
    idx <- which(stats_player$week == row$week)[1]
    prior <- stats_player$sacks_taken[seq_len(idx - 1)]
    recomputed <- roll_mean(prior, 3)
    if (is.na(recomputed) || abs(recomputed - row$target_sacks_qb_taken_roll3) > 1e-6) {
      stop("QB roll3 sacks mismatch for player ", row$player_id,
           " season ", row$season, " week ", row$week)
    }
  }
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
    "qb_rush_yards",
    "qb_rush_tds"
  )
  missing_outcomes <- setdiff(required_outcomes, names(result$draws))
  if (length(missing_outcomes) > 0) {
    stop("Missing QB outcomes in draws: ", paste(missing_outcomes, collapse = ", "))
  }

  if (all(is.na(result$draws$qb_rush_tds))) {
    stop("QB simulation draws missing rushing TDs (all NA).")
  }

  cat("  QB simulation completed successfully\n")
  cat("  Draws:", nrow(result$draws), "rows\n")
}, error = function(e) {
  cat("ERROR: QB simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 5: Rushing-heavy QB sanity check...\n")
tryCatch({
  rush_td_candidates <- qb_features[
    qb_features$season >= 2020 &
      !is.na(qb_features$target_qb_rush_tds) &
      qb_features$target_qb_rush_tds > 0,
    , drop = FALSE
  ]
  if (nrow(rush_td_candidates) == 0) {
    stop("No QB rows with rushing TDs found for sanity check.")
  }
  rush_row <- rush_td_candidates[1, ]
  rush_result <- simulate_player_game(
    gsis_id = rush_row$player_id,
    season = rush_row$season,
    week = rush_row$week,
    n_sims = 500,
    mode = "historical_replay"
  )
  if (is.null(rush_result$draws) || all(is.na(rush_result$draws$qb_rush_tds))) {
    stop("Rushing TD draws are missing for rushing-heavy QB test.")
  }
  if (!any(rush_result$draws$qb_rush_tds > 0, na.rm = TRUE)) {
    stop("Rushing TD draws are all zero for rushing-heavy QB test.")
  }
  rush_td_prob <- mean(rush_result$draws$qb_rush_tds >= 1, na.rm = TRUE)
  if (!is.finite(rush_td_prob) || rush_td_prob < 0.01) {
    stop("Rushing TD draw probability too low for rushing-heavy QB test.")
  }
  cat("  Rushing-heavy QB simulation completed successfully\n")
}, error = function(e) {
  cat("ERROR: Rushing-heavy QB test failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nAll QB smoke tests passed!\n")
quit(status = 0)
