# Refresh Weekly Cache
#
# Builds Layer 1/2 caches and Layer 3 RB rolling features using nflreadr.
# This script is the only place that performs nflreadr downloads.

if (basename(getwd()) == "scripts") {
  setwd("..")
}

if (!exists("build_player_week_identity") || !exists("build_rb_weekly_stats") || !exists("read_rb_weekly_features_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R")
  } else {
    stop("Missing R/data/build_weekly_player_layers.R")
  }
}

if (!exists("assemble_rb_weekly_features")) {
  if (file.exists("R/assemble/assemble_rb_training_data.R")) {
    source("R/assemble/assemble_rb_training_data.R")
  } else {
    stop("Missing R/assemble/assemble_rb_training_data.R")
  }
}

if (!exists("lagged_roll_mean") || !exists("lagged_roll_sum")) {
  if (file.exists("R/utils/rolling_helpers.R")) {
    source("R/utils/rolling_helpers.R")
  } else {
    stop("Missing R/utils/rolling_helpers.R required for rolling features")
  }
}

current_year <- as.integer(format(Sys.Date(), "%Y"))
seasons_to_refresh <- 1999:current_year

cat("Refreshing weekly RB caches for seasons", min(seasons_to_refresh), "through", max(seasons_to_refresh), "...\n")

identity <- build_player_week_identity(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
if (nrow(identity) == 0) {
  stop("Player identity cache built with zero rows; ensure nflreadr returned data and try again.")
}
cat("  Built player week identity cache with", nrow(identity), "rows\n")

rb_stats <- build_rb_weekly_stats(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
if (nrow(rb_stats) == 0) {
  stop("RB weekly stats cache built with zero rows; ensure nflreadr returned RB rows.")
}
cat("  Built RB weekly stats cache with", nrow(rb_stats), "rows\n")

cat("  Computing RB rolling features...\n")
rb_features <- assemble_rb_weekly_features(rb_stats)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write RB weekly features. Install with install.packages('arrow').")
}

dir.create(dirname(rb_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(rb_features, rb_weekly_features_path)

if (nrow(rb_features) == 0) {
  stop("RB weekly features cache empty after feature computation.")
}

cat("  Built RB weekly features cache with", nrow(rb_features), "rows\n")
cat("\nWeekly caches refreshed. Files:")
cat("\n  -", player_week_identity_path)
cat("\n  -", rb_weekly_stats_path)
cat("\n  -", rb_weekly_features_path, "\n")

