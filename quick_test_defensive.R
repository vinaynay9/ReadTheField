# Quick test of defensive features setup
library(dplyr)

cat("Testing defensive feature setup...\n\n")

# Test 1: Load regime system
cat("1. Loading regime system...\n")
source("R/utils/rb_regime_v1.R")
cat("   SUCCESS\n\n")

# Test 2: Check feature contracts
cat("2. Checking feature contracts...\n")
contracts <- get_rb_features_by_regime()

cat("   Late regime features (weeks 6-7):\n")
late_def <- grep("^opp_", contracts$late, value = TRUE)
if (length(late_def) > 0) {
  cat("     Defensive features:", paste(late_def, collapse = ", "), "\n")
  cat("     PASS: Defensive features included\n")
} else {
  cat("     FAIL: No defensive features\n")
}

cat("\n   Standard regime features (weeks 8+):\n")
std_def <- grep("^opp_", contracts$standard, value = TRUE)
if (length(std_def) > 0) {
  cat("     Defensive features:", paste(std_def, collapse = ", "), "\n")
  cat("     PASS: Defensive features included\n")
} else {
  cat("     FAIL: No defensive features\n")
}

# Test 3: Check that defensive builders exist
cat("\n3. Checking defensive feature builders...\n")
if (file.exists("R/data/build_team_defense_game_stats.R")) {
  cat("   PASS: build_team_defense_game_stats.R exists\n")
} else {
  cat("   FAIL: build_team_defense_game_stats.R missing\n")
}

if (file.exists("R/features/build_team_defense_features.R")) {
  cat("   PASS: build_team_defense_features.R exists\n")
} else {
  cat("   FAIL: build_team_defense_features.R missing\n")
}

# Test 4: Check rolling helpers
cat("\n4. Checking rolling helpers...\n")
if (file.exists("R/utils/rolling_helpers.R")) {
  source("R/utils/rolling_helpers.R")
  # Test lagged_roll_mean
  test_data <- c(10, 20, 30, 40, 50, 60)
  result <- lagged_roll_mean(test_data, window = 5)
  # Week 6 should be mean of weeks 1-5: (10+20+30+40+50)/5 = 30
  if (!is.na(result[6]) && abs(result[6] - 30) < 0.01) {
    cat("   PASS: lagged_roll_mean working correctly\n")
    cat("     Test: mean([10,20,30,40,50]) = ", result[6], " (expected 30)\n")
  } else {
    cat("   FAIL: lagged_roll_mean not working correctly\n")
  }
} else {
  cat("   FAIL: rolling_helpers.R missing\n")
}

cat("\nAll setup checks complete!\n")
cat("\nNext steps:\n")
cat("  1. Run: source('scripts/refresh_weekly_cache.R')\n")
cat("  2. Run: source('scripts/validate_defensive_features.R')\n")
cat("  3. Run: source('scripts/run_bijan_simulation.R')\n")

