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
cat("Unique players:", n_distinct(features$player_id), "\n\n")

# Required columns for modeling
feature_cols <- grep("_roll", names(features), value = TRUE)
target_cols <- c(
  "target_carries",
  "target_rush_yards",
  "target_receptions",
  "target_rec_yards",
  "target_rush_tds",
  "target_rec_tds"
)

cat("Feature columns:", length(feature_cols), "\n")
cat("Target columns:", paste(target_cols, collapse = ", "), "\n\n")

# Check NA rates
cat("NA COUNTS\n")
cat("-------------------------------------\n")
for (col in c(feature_cols, target_cols)) {
  if (col %in% names(features)) {
    cat(sprintf("%-25s %6d\n", col, sum(is.na(features[[col]]))))
  }
}

# Rows usable per target
cat("\nUSABLE ROWS PER TARGET\n")
cat("-------------------------------------\n")

for (tgt in target_cols) {
  if (!tgt %in% names(features)) next
  
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

# Combined usable rows
combined <- features %>%
  filter(
    if_all(all_of(c(feature_cols, target_cols)), ~ !is.na(.))
  )

cat("\nROWS USABLE FOR ALL MODELS:", nrow(combined), "\n")

cat("\nDONE.\n")
