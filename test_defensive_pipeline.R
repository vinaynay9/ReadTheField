# Complete Defensive Features Pipeline Test
# Tests all components from cache building to model integration

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})

cat("================================================================\n")
cat("DEFENSIVE FEATURES PIPELINE TEST\n")
cat("================================================================\n\n")

passed <- 0
failed <- 0

test <- function(name, expr) {
  cat(name, "... ")
  result <- tryCatch({
    expr
    cat("PASS\n")
    passed <<- passed + 1
    TRUE
  }, error = function(e) {
    cat("FAIL:", conditionMessage(e), "\n")
    failed <<- failed + 1
    FALSE
  })
  return(result)
}

# Test 1: Load required libraries and functions
test("Loading defensive game stats builder", {
  source("R/data/build_team_defense_game_stats.R")
  if (!exists("build_team_defense_game_stats")) {
    stop("build_team_defense_game_stats not found")
  }
})

test("Loading defensive features builder", {
  source("R/features/build_team_defense_features.R")
  if (!exists("build_team_defense_features")) {
    stop("build_team_defense_features not found")
  }
})

test("Loading rolling helpers", {
  source("R/utils/rolling_helpers.R")
  if (!exists("lagged_roll_mean")) {
    stop("lagged_roll_mean not found")
  }
})

test("Loading regime system", {
  source("R/utils/rb_regime_v1.R")
  if (!exists("get_rb_features_by_regime")) {
    stop("get_rb_features_by_regime not found")
  }
})

# Test 2: Validate rolling helpers work correctly
test("Testing lagged_roll_mean correctness", {
  test_data <- c(10, 20, 30, 40, 50, 60)
  result <- lagged_roll_mean(test_data, window = 5)
  # Week 6 should be mean of weeks 1-5: (10+20+30+40+50)/5 = 30
  if (is.na(result[6]) || abs(result[6] - 30) > 0.01) {
    stop("lagged_roll_mean incorrect: expected 30, got ", result[6])
  }
  # Weeks 1-5 should be NA (insufficient history)
  if (any(!is.na(result[1:5]))) {
    stop("lagged_roll_mean should be NA for first 5 weeks")
  }
})

# Test 3: Check regime contracts include defensive features
test("Checking regime contracts for defensive features", {
  contracts <- get_rb_features_by_regime()
  
  # Late and standard should have defensive features
  late_def <- grep("^opp_", contracts$late, value = TRUE)
  std_def <- grep("^opp_", contracts$standard, value = TRUE)
  
  if (length(late_def) == 0) {
    stop("Late regime missing defensive features")
  }
  if (length(std_def) == 0) {
    stop("Standard regime missing defensive features")
  }
  
  # Check for required features
  required_def <- c("opp_rush_yards_allowed_roll5", "opp_yards_per_rush_allowed_roll5",
                   "opp_points_allowed_roll5", "opp_sacks_roll5", "opp_tfl_roll5")
  
  if (!all(required_def %in% contracts$late)) {
    missing <- setdiff(required_def, contracts$late)
    stop("Late regime missing: ", paste(missing, collapse = ", "))
  }
  
  if (!all(required_def %in% contracts$standard)) {
    missing <- setdiff(required_def, contracts$standard)
    stop("Standard regime missing: ", paste(missing, collapse = ", "))
  }
  
  # Early and mid should NOT have defensive features (not enough history)
  early_def <- grep("^opp_", contracts$early, value = TRUE)
  mid_def <- grep("^opp_", contracts$mid, value = TRUE)
  
  if (length(early_def) > 0) {
    stop("Early regime incorrectly includes defensive features")
  }
  if (length(mid_def) > 0) {
    stop("Mid regime incorrectly includes defensive features")
  }
})

# Test 4: Check defensive cache exists and is valid
test("Checking defensive cache file", {
  defense_cache_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  
  if (!file.exists(defense_cache_path)) {
    stop("defense_weekly_features.parquet does not exist. Run scripts/refresh_weekly_cache.R first.")
  }
  
  def_features <- arrow::read_parquet(defense_cache_path)
  
  if (nrow(def_features) == 0) {
    stop("Defensive cache is empty")
  }
  
  # Check required columns
  required_cols <- c("defense_team", "season", "week", 
                    "opp_rush_yards_allowed_roll5", "opp_yards_per_rush_allowed_roll5",
                    "opp_points_allowed_roll5", "opp_sacks_roll5", "opp_tfl_roll5")
  
  missing <- setdiff(required_cols, names(def_features))
  if (length(missing) > 0) {
    stop("Missing columns in defensive cache: ", paste(missing, collapse = ", "))
  }
  
  cat(" (", nrow(def_features), " rows)")
})

# Test 5: Validate Week 1 leakage check
test("Validating Week 1 values are NA (no leakage)", {
  defense_cache_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  def_features <- arrow::read_parquet(defense_cache_path)
  
  week1_rows <- def_features[def_features$week == 1, ]
  if (nrow(week1_rows) == 0) {
    cat(" (no Week 1 data)")
    return()
  }
  
  rolling_cols <- grep("_roll[0-9]+$", names(def_features), value = TRUE)
  week1_nonNA <- sum(!is.na(week1_rows[, rolling_cols, drop = FALSE]))
  
  if (week1_nonNA > 0) {
    stop("Found ", week1_nonNA, " non-NA rolling features in Week 1 (LEAKAGE DETECTED)")
  }
})

# Test 6: Check RB features include defensive features
test("Checking RB features include defensive features", {
  rb_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
  
  if (!file.exists(rb_features_path)) {
    stop("rb_weekly_features.parquet does not exist. Run scripts/refresh_weekly_cache.R first.")
  }
  
  rb_features <- arrow::read_parquet(rb_features_path)
  
  required_def_cols <- c("opp_rush_yards_allowed_roll5", "opp_yards_per_rush_allowed_roll5",
                        "opp_points_allowed_roll5", "opp_sacks_roll5", "opp_tfl_roll5")
  
  missing <- setdiff(required_def_cols, names(rb_features))
  if (length(missing) > 0) {
    stop("Missing defensive columns in RB features: ", paste(missing, collapse = ", "))
  }
  
  cat(" (", nrow(rb_features), " rows)")
})

# Test 7: Check defensive feature population in mid-season weeks
test("Checking defensive feature population (weeks 6+)", {
  rb_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
  rb_features <- arrow::read_parquet(rb_features_path)
  
  week6plus <- rb_features[rb_features$week >= 6, ]
  
  if (nrow(week6plus) == 0) {
    cat(" (no data for weeks 6+)")
    return()
  }
  
  def_cols <- c("opp_rush_yards_allowed_roll5", "opp_yards_per_rush_allowed_roll5",
               "opp_points_allowed_roll5", "opp_sacks_roll5", "opp_tfl_roll5")
  
  na_rates <- sapply(def_cols, function(col) {
    sum(is.na(week6plus[[col]])) / nrow(week6plus) * 100
  })
  
  high_na <- names(na_rates)[na_rates > 50]
  if (length(high_na) > 0) {
    stop("High NA rates (>50%) in weeks 6+: ", paste(high_na, collapse = ", "))
  }
  
  avg_na_rate <- mean(na_rates)
  cat(" (avg NA rate: ", round(avg_na_rate, 1), "%)")
})

# Test 8: Validate no duplicate team-week combinations in defensive cache
test("Checking for duplicate team-week combinations", {
  defense_cache_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  def_features <- arrow::read_parquet(defense_cache_path)
  
  duplicates <- duplicated(def_features[, c("defense_team", "season", "week")])
  if (sum(duplicates) > 0) {
    stop("Found ", sum(duplicates), " duplicate team-week combinations")
  }
})

# Test 9: Validate yards_per_rush_allowed is reasonable
test("Validating yards_per_rush_allowed values", {
  defense_cache_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  def_features <- arrow::read_parquet(defense_cache_path)
  
  ypr_vals <- def_features$opp_yards_per_rush_allowed_roll5
  ypr_vals <- ypr_vals[!is.na(ypr_vals)]
  
  if (length(ypr_vals) == 0) {
    cat(" (no non-NA values)")
    return()
  }
  
  # YPR should typically be between 2.0 and 7.0
  if (any(ypr_vals < 0)) {
    stop("Found negative yards per rush values")
  }
  if (any(ypr_vals > 10)) {
    stop("Found implausibly high yards per rush values (>10)")
  }
  
  mean_ypr <- mean(ypr_vals)
  cat(" (mean: ", round(mean_ypr, 2), " yds/rush)")
})

# Test 10: Check that simulation can use defensive features
test("Checking simulation defensive context capture", {
  source("R/simulation/run_rb_simulation.R")
  
  # Check that the defensive_context section in run_rb_simulation captures all features
  script_content <- readLines("R/simulation/run_rb_simulation.R")
  def_context_line <- grep("def_features <- c\\(", script_content, value = TRUE)
  
  if (length(def_context_line) == 0) {
    stop("defensive_context setup not found in run_rb_simulation.R")
  }
  
  # Check that all required features are captured
  required <- c("opp_rush_yards_allowed_roll5", "opp_yards_per_rush_allowed_roll5",
               "opp_points_allowed_roll5", "opp_sacks_roll5", "opp_tfl_roll5")
  
  for (feat in required) {
    if (!any(grepl(feat, script_content, fixed = TRUE))) {
      stop("Feature ", feat, " not captured in defensive context")
    }
  }
})

# Summary
cat("\n================================================================\n")
cat("TEST SUMMARY\n")
cat("================================================================\n")
cat("Passed:", passed, "\n")
cat("Failed:", failed, "\n")

if (failed == 0) {
  cat("\n✓ ALL TESTS PASSED\n")
  cat("\nDefensive features pipeline is fully operational!\n\n")
  cat("Next steps:\n")
  cat("  1. If cache doesn't exist: source('scripts/refresh_weekly_cache.R')\n")
  cat("  2. Run simulation: source('scripts/run_rb_simulation_cli.R')\n")
  cat("  3. Check output shows defensive context values\n")
} else {
  cat("\n✗ SOME TESTS FAILED\n")
  cat("\nPlease fix the issues above before proceeding.\n")
}

cat("================================================================\n")

