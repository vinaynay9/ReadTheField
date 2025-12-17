# Smoke Test for RB Simulation
#
# This script tests the RB simulation pipeline end-to-end without calling refresh.
# It loads cached features, fits models, and runs a simulation for a known player-week.
#
# Exit codes:
#   0: All tests passed
#   1: Test failed (error occurred)

# Set working directory to project root
if (file.exists("README.md") && file.exists("R") && file.exists("scripts")) {
  # Already in project root
} else {
  # Try to detect project root
  script_path <- commandArgs(trailingOnly = FALSE)
  if (length(script_path) > 0) {
    script_file <- sub("--file=", "", script_path[grep("--file=", script_path)])
    if (length(script_file) > 0 && file.exists(script_file)) {
      project_root <- dirname(dirname(normalizePath(script_file)))
      setwd(project_root)
    }
  }
}

# Source bootstrap
cat("Loading simulation bootstrap...\n")
tryCatch({
  source("R/simulation/bootstrap_simulation.R")
  cat("  Bootstrap loaded successfully\n")
}, error = function(e) {
  cat("ERROR: Failed to load bootstrap:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Test 1: Load cached features
cat("\nTest 1: Loading cached RB weekly features...\n")
tryCatch({
  rb_features <- read_rb_weekly_features_cache()
  if (nrow(rb_features) == 0) {
    cat("ERROR: RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R first.\n")
    quit(status = 1)
  }
  cat("  Loaded", nrow(rb_features), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to load RB weekly features:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Test 2: Filter to stable training window (2010:2023)
cat("\nTest 2: Filtering to training window (2010:2023)...\n")
tryCatch({
  train_window <- rb_features[
    rb_features$season >= 2010 & 
    rb_features$season <= 2023 &
    !is.na(rb_features$target_carries) &
    !is.na(rb_features$target_rushing_yards) &
    !is.na(rb_features$target_receptions) &
    !is.na(rb_features$target_receiving_yards) &
    !is.na(rb_features$target_total_touchdowns),
    , drop = FALSE
  ]
  if (nrow(train_window) < 200) {
    cat("ERROR: Insufficient training data. Found", nrow(train_window), "rows, need at least 200.\n")
    quit(status = 1)
  }
  cat("  Training window has", nrow(train_window), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to filter training window:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Test 3: Fit models
cat("\nTest 3: Fitting RB models...\n")
tryCatch({
  rb_models <- fit_rb_models(train_window, min_rows = 200)
  
  # Check all models exist (baseline or fitted)
  required_models <- c("carries_model", "rushing_yards_model", "receiving_yards_model",
                      "receptions_model", "total_touchdowns_model")
  missing <- required_models[sapply(required_models, function(m) is.null(rb_models[[m]]))]
  if (length(missing) > 0) {
    cat("ERROR: Missing models:", paste(missing, collapse = ", "), "\n")
    quit(status = 1)
  }
  
  # Check for baseline models
  baseline <- sapply(rb_models[required_models], function(m) 
    !is.null(m$type) && m$type == "baseline")
  if (any(baseline)) {
    cat("  WARNING: Some models are baseline:", paste(names(baseline)[baseline], collapse = ", "), "\n")
  } else {
    cat("  All models fitted successfully\n")
  }
}, error = function(e) {
  cat("ERROR: Failed to fit models:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Test 4: Find a known player-week for simulation
cat("\nTest 4: Finding test player-week...\n")
tryCatch({
  # Find a player with sufficient history in 2024
  test_season <- 2024
  test_week <- 8
  
  # Look for players with games in test_season, test_week
  test_games <- rb_features[
    rb_features$season == test_season & 
    rb_features$week == test_week &
    !is.na(rb_features$player_id) &
    !is.na(rb_features$player_name),
    , drop = FALSE
  ]
  
  if (nrow(test_games) == 0) {
    cat("ERROR: No games found for season", test_season, "week", test_week, "\n")
    cat("  Try a different season/week or run refresh_weekly_cache.R\n")
    quit(status = 1)
  }
  
  # Pick first player
  test_player <- test_games$player_name[1]
  test_player_id <- test_games$player_id[1]
  test_team <- test_games$team[1]
  test_opponent <- test_games$opponent[1]
  
  cat("  Test player:", test_player, "(ID:", test_player_id, ")\n")
  cat("  Test game: Season", test_season, "Week", test_week, 
      test_team, "vs", test_opponent, "\n")
}, error = function(e) {
  cat("ERROR: Failed to find test player-week:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Test 5: Run simulation
cat("\nTest 5: Running simulation...\n")
tryCatch({
  result <- simulate_player_game(
    player_name = test_player,
    season = test_season,
    week = test_week,
    n_sims = 1000,  # Smaller for smoke test
    mode = "historical_replay"
  )
  
  # Validate result structure
  if (is.null(result)) {
    cat("ERROR: Simulation returned NULL\n")
    quit(status = 1)
  }
  
  if (is.null(result$draws) || nrow(result$draws) != 1000) {
    cat("ERROR: Simulation draws invalid. Expected 1000 rows, got", 
        if (is.null(result$draws)) "NULL" else nrow(result$draws), "\n")
    quit(status = 1)
  }
  
  required_outcomes <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
  missing_outcomes <- setdiff(required_outcomes, names(result$draws))
  if (length(missing_outcomes) > 0) {
    cat("ERROR: Missing outcomes in draws:", paste(missing_outcomes, collapse = ", "), "\n")
    quit(status = 1)
  }
  
  cat("  Simulation completed successfully\n")
  cat("  Draws:", nrow(result$draws), "rows\n")
  
  # Check for baseline models
  if (!is.null(result$diagnostics$baseline_models) && 
      length(result$diagnostics$baseline_models) > 0) {
    cat("  WARNING: Some models used baseline:", 
        paste(result$diagnostics$baseline_models, collapse = ", "), "\n")
  }
  
}, error = function(e) {
  cat("ERROR: Simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nAll smoke tests passed!\n")
quit(status = 0)

