# Smoke Test for K Simulation
#
# This script tests the K simulation pipeline end-to-end without calling refresh.
# It loads cached K features, fits models, and runs a K simulation.
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

cat("\nTest 1: Loading cached K weekly features...\n")
tryCatch({
  k_features <- read_k_weekly_features_cache() |>
    as.data.frame()
  if (nrow(k_features) == 0) {
    stop("K weekly features cache is empty. Run scripts/refresh_weekly_cache.R first.")
  }
  cat("  Loaded", nrow(k_features), "rows\n")

  required_cols <- c(
    "player_id", "season", "week",
    "target_fg_attempts_k",
    "target_fg_made_k",
    "target_pat_attempts_k",
    "target_pat_made_k"
  )
  missing_cols <- setdiff(required_cols, names(k_features))
  if (length(missing_cols) > 0) {
    stop("K weekly features missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  k_stats <- read_k_weekly_stats_cache() |>
    as.data.frame()
  if (nrow(k_stats) == 0) {
    stop("K weekly stats cache is empty.")
  }
  cat("  Loaded raw K weekly stats with", nrow(k_stats), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to load K weekly features or stats:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 2: Fitting K models...\n")
tryCatch({
  train_df <- k_features |>
    dplyr::filter(season >= 2010, season <= 2023)
  if (nrow(train_df) < 500) {
    stop("K training window too small: ", nrow(train_df))
  }

  k_models <- fit_k_models(train_df, min_rows = 200)
  k_targets <- get_k_v1_targets()
  k_regimes <- get_k_regimes()
  required_keys <- c()
  for (target in k_targets) {
    for (regime in k_regimes) {
      required_keys <- c(required_keys, get_k_model_key(target, regime))
    }
  }
  if (is.null(k_models$models) || any(!required_keys %in% names(k_models$models))) {
    stop("K models failed validation.")
  }

  cat("  Models fitted and validated\n")
}, error = function(e) {
  cat("ERROR: Failed to fit K models:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 3: Finding test K player-week...\n")
tryCatch({
  test_season <- 2024
  test_games <- NULL
  test_week <- NA_integer_

  for (wk in 8:12) {
    feat_cols <- get_k_features_by_week(wk)
    candidates <- k_features[
      k_features$season == test_season &
        k_features$week == wk &
        !is.na(k_features$player_id),
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
    stop("No K games found for season ", test_season, " with complete feature set.")
  }
  test_player_id <- test_games$player_id[1]
  test_player_name <- test_games$player_name[1]
  test_team <- test_games$team[1]
  test_opponent <- test_games$opponent[1]

  cat("  Test player:", test_player_name, "(gsis_id:", test_player_id, ")\n")
  cat("  Test game: Season", test_season, "Week", test_week,
      test_team, "vs", test_opponent, "\n")
}, error = function(e) {
  cat("ERROR: Failed to find test K player-week:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 4: Running K simulation...\n")
tryCatch({
  result <- simulate_player_game(
    gsis_id = test_player_id,
    season = test_season,
    week = test_week,
    n_sims = 400,
    mode = "historical_replay"
  )

  if (is.null(result) || is.null(result$draws) || nrow(result$draws) != 400) {
    stop("K simulation draws invalid.")
  }

  required_outcomes <- c("fg_made", "pat_made")
  missing_outcomes <- setdiff(required_outcomes, names(result$draws))
  if (length(missing_outcomes) > 0) {
    stop("Missing K outcomes in draws: ", paste(missing_outcomes, collapse = ", "))
  }

  cat("  K simulation completed successfully\n")
  cat("  Draws:", nrow(result$draws), "rows\n")
}, error = function(e) {
  cat("ERROR: K simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nAll K smoke tests passed!\n")
quit(status = 0)
