# Validate Defensive Features Integration
#
# This script validates that defensive features are correctly:
# 1. Cached in defense_weekly_features.parquet
# 2. Joined into RB weekly features
# 3. Used by models
# 4. Time-aware (no leakage)

if (basename(getwd()) == "scripts") {
  setwd("..")
}

cat("=================================================================\n")
cat("Defensive Features Validation\n")
cat("=================================================================\n\n")

# Test 1: Check defensive cache exists
cat("Test 1: Checking defensive cache file exists...\n")
defense_cache_path <- file.path("data", "processed", "defense_weekly_features.parquet")

if (file.exists(defense_cache_path)) {
  cat("  PASS: defense_weekly_features.parquet exists\n")
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    def_features <- arrow::read_parquet(defense_cache_path)
    cat("  Rows:", nrow(def_features), "\n")
    cat("  Columns:", paste(names(def_features), collapse = ", "), "\n")
    
    # Test 2: Check Week 1 leakage
    cat("\nTest 2: Checking Week 1 values are NA (no leakage)...\n")
    week1_rows <- def_features[def_features$week == 1, ]
    if (nrow(week1_rows) > 0) {
      rolling_cols <- grep("_roll[0-9]+$", names(def_features), value = TRUE)
      week1_nonNA <- sum(!is.na(week1_rows[, rolling_cols, drop = FALSE]))
      if (week1_nonNA == 0) {
        cat("  PASS: All Week 1 rolling features are NA\n")
      } else {
        cat("  FAIL: Found", week1_nonNA, "non-NA rolling features in Week 1\n")
      }
    } else {
      cat("  SKIP: No Week 1 data found\n")
    }
    
    # Test 3: Check mid-season values exist
    cat("\nTest 3: Checking mid-season defensive values exist...\n")
    mid_season <- def_features[def_features$week >= 6 & def_features$week <= 10, ]
    if (nrow(mid_season) > 0) {
      rolling_cols <- grep("_roll[0-9]+$", names(def_features), value = TRUE)
      mid_season_na_rate <- sum(is.na(mid_season[, rolling_cols, drop = FALSE])) / 
                           (nrow(mid_season) * length(rolling_cols))
      cat("  Mid-season (weeks 6-10) NA rate:", round(mid_season_na_rate * 100, 1), "%\n")
      if (mid_season_na_rate < 0.5) {
        cat("  PASS: Most mid-season defensive features are populated\n")
      } else {
        cat("  WARN: High NA rate in mid-season defensive features\n")
      }
    } else {
      cat("  SKIP: No mid-season data found\n")
    }
    
    # Test 4: Check no duplicate team-week combinations
    cat("\nTest 4: Checking for duplicate team-week combinations...\n")
    duplicates <- duplicated(def_features[, c("defense_team", "season", "week")])
    if (sum(duplicates) == 0) {
      cat("  PASS: No duplicate team-week combinations\n")
    } else {
      cat("  FAIL: Found", sum(duplicates), "duplicate team-week combinations\n")
    }
    
  } else {
    cat("  SKIP: arrow package not available\n")
  }
} else {
  cat("  FAIL: defense_weekly_features.parquet not found\n")
  cat("  ACTION: Run scripts/refresh_weekly_cache.R to build defensive cache\n")
}

# Test 5: Check RB features include defensive features
cat("\nTest 5: Checking RB features include defensive features...\n")
rb_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")

if (file.exists(rb_features_path)) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    rb_features <- arrow::read_parquet(rb_features_path)
    
    expected_def_cols <- c("def_rush_yards_defense_allowed_roll5", "def_yards_per_rush_defense_allowed_roll5",
                          "def_points_defense_allowed_roll5", "def_sacks_defense_forced_roll5", "def_tackles_for_loss_defense_forced_roll5")
    found_def_cols <- intersect(expected_def_cols, names(rb_features))
    
    if (length(found_def_cols) == length(expected_def_cols)) {
      cat("  PASS: All expected defensive features found in RB features\n")
      cat("  Columns:", paste(found_def_cols, collapse = ", "), "\n")
      
      # Check population rate
      cat("\nTest 6: Checking defensive feature population in RB features...\n")
      week6plus <- rb_features[rb_features$week >= 6, ]
      if (nrow(week6plus) > 0) {
        na_rates <- sapply(found_def_cols, function(col) {
          sum(is.na(week6plus[[col]])) / nrow(week6plus) * 100
        })
        cat("  NA rates for weeks 6+ (should be low):\n")
        for (col in names(na_rates)) {
          cat("    ", col, ":", round(na_rates[col], 1), "%\n")
        }
        if (all(na_rates < 50)) {
          cat("  PASS: Defensive features well-populated for weeks 6+\n")
        } else {
          cat("  WARN: Some defensive features have high NA rates\n")
        }
      }
      
    } else {
      cat("  FAIL: Missing defensive features:", 
          paste(setdiff(expected_def_cols, found_def_cols), collapse = ", "), "\n")
      cat("  ACTION: Run scripts/refresh_weekly_cache.R to rebuild RB features with defensive context\n")
    }
  }
} else {
  cat("  FAIL: rb_weekly_features.parquet not found\n")
}

# Test 7: Check regime feature contracts include defensive features
cat("\nTest 7: Checking regime feature contracts...\n")
if (file.exists("R/utils/rb_regime_v1.R")) {
  source("R/utils/rb_regime_v1.R")
  
  feature_contracts <- get_rb_features_by_regime()
  
  # Late and standard regimes should have defensive features
  late_has_def <- any(grepl("^def_", feature_contracts$late))
  standard_has_def <- any(grepl("^def_", feature_contracts$standard))
  
  if (late_has_def && standard_has_def) {
    cat("  PASS: Late and standard regimes include defensive features\n")
    cat("  Late regime defensive features:\n")
    cat("    ", paste(grep("^def_", feature_contracts$late, value = TRUE), collapse = ", "), "\n")
    cat("  Standard regime defensive features:\n")
    cat("    ", paste(grep("^def_", feature_contracts$standard, value = TRUE), collapse = ", "), "\n")
  } else {
    cat("  FAIL: Late or standard regimes missing defensive features\n")
  }
  
  # Early and mid regimes should NOT have defensive features
  early_has_def <- any(grepl("^def_", feature_contracts$early))
  mid_has_def <- any(grepl("^def_", feature_contracts$mid))
  
  if (!early_has_def && !mid_has_def) {
    cat("  PASS: Early and mid regimes correctly exclude defensive features (not enough history)\n")
  } else {
    cat("  WARN: Early or mid regimes include defensive features (may cause NA issues)\n")
  }
  
} else {
  cat("  FAIL: rb_regime_v1.R not found\n")
}

cat("\n=================================================================\n")
cat("Validation Complete\n")
cat("=================================================================\n")

