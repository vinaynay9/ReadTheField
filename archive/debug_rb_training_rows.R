# scripts/debug_rb_training_rows.R
# Purpose: single source of truth for RB training row diagnostics

cat("RB TRAINING ROW DIAGNOSTICS\n")
cat("=====================================\n\n")

library(dplyr)
library(arrow)

# Load feature cache
features_path <- "data/processed/rb_weekly_features.parquet"

if (!file.exists(features_path)) {
  stop("rb_weekly_features.parquet not found. Run refresh_weekly_cache.R first.")
}

features <- read_parquet(features_path)

cat("Total rows in rb_weekly_features:", nrow(features), "\n")
cat("Unique players:", n_distinct(features$player_id), "\n")

# Check for rb_regime column
if ("rb_regime" %in% names(features)) {
  cat("rb_regime column: PRESENT\n")
  cat("Regime distribution:\n")
  regime_counts <- table(features$rb_regime, useNA = "ifany")
  for (regime in names(regime_counts)) {
    cat(sprintf("  %-10s %6d rows\n", regime, regime_counts[regime]))
  }
  cat("\n")
} else {
  cat("rb_regime column: MISSING (regime-based modeling will fail)\n\n")
}

# Load RB v1 schema
if (file.exists("R/utils/rb_schema_v1.R")) {
  source("R/utils/rb_schema_v1.R", local = TRUE)
  target_cols <- get_rb_v1_targets()
} else {
  # Fallback to canonical RB v1 targets
  target_cols <- c(
    "target_carries",
    "target_receptions",
    "target_rush_tds",
    "target_rec_tds"
  )
}

# Required columns for modeling
feature_cols <- grep("_roll", names(features), value = TRUE)

cat("Feature columns:", length(feature_cols), "\n")
cat("RB v1 target columns:", paste(target_cols, collapse = ", "), "\n\n")
cat("NOTE: RB v1 does NOT include yardage targets (target_rush_yards, target_rec_yards)\n\n")

# Check NA rates
cat("NA COUNTS\n")
cat("-------------------------------------\n")
for (col in c(feature_cols, target_cols)) {
  if (col %in% names(features)) {
    cat(sprintf("%-25s %6d\n", col, sum(is.na(features[[col]]))))
  }
}

# Feature completeness (rows with all features non-NA)
cat("\nFEATURE COMPLETENESS\n")
cat("-------------------------------------\n")
feature_complete <- features %>%
  filter(if_all(all_of(feature_cols), ~ !is.na(.)))
cat("Rows with all features non-NA:", nrow(feature_complete), "\n\n")

# Rows usable per target (per-target, no intersection)
cat("USABLE ROWS PER TARGET (per-target, not intersection)\n")
cat("-------------------------------------\n")

for (tgt in target_cols) {
  if (!tgt %in% names(features)) {
    cat(sprintf("%-20s MISSING COLUMN\n", tgt))
    next
  }
  
  # Per-target: only this target and features must be non-NA
  usable <- features %>%
    filter(
      !is.na(.data[[tgt]]),
      if_all(all_of(feature_cols), ~ !is.na(.))
    )
  
  cat(sprintf(
    "%-20s %6d rows\n",
    tgt,
    nrow(usable)
  ))
}

cat("\nNOTE: Each target trains independently on its own valid subset.\n")
cat("No intersection across targets is required.\n")

cat("\nDONE.\n")
