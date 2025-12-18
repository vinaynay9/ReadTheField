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

# Build player directory cache (required for name resolution)
if (!exists("build_player_directory")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R")
  } else {
    stop("Missing R/data/build_weekly_player_layers.R")
  }
}

cat("  Building player directory cache...\n")
player_dir <- build_player_directory(write_cache = TRUE)
if (nrow(player_dir) == 0) {
  stop("Player directory cache built with zero rows; ensure nflreadr returned data.")
}
cat("  Built player directory cache with", nrow(player_dir), "players\n")

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

# Build defensive weekly features (opponent context)
cat("  Computing defensive weekly features...\n")

if (!exists("build_team_defense_game_stats")) {
  if (file.exists("R/data/build_team_defense_game_stats.R")) {
    source("R/data/build_team_defense_game_stats.R")
  } else {
    stop("Missing R/data/build_team_defense_game_stats.R")
  }
}

if (!exists("build_team_defense_features")) {
  if (file.exists("R/features/build_team_defense_features.R")) {
    source("R/features/build_team_defense_features.R")
  } else {
    stop("Missing R/features/build_team_defense_features.R")
  }
}

tryCatch({
  def_game_stats <- build_team_defense_game_stats(seasons = seasons_to_refresh)
  
  if (nrow(def_game_stats) > 0) {
    def_features <- build_team_defense_features(def_game_stats)
    
    defense_weekly_path <- file.path("data", "processed", "defense_weekly_features.parquet")
    dir.create(dirname(defense_weekly_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(def_features, defense_weekly_path)
    
    cat("  Built defensive weekly features cache with", nrow(def_features), "rows\n")
    
    # Validation: Check for Week 1 leakage
    week1_def <- def_features[def_features$week == 1, ]
    if (nrow(week1_def) > 0) {
      rolling_def_cols <- grep("_roll[0-9]+$", names(def_features), value = TRUE)
      week1_nonNA <- sum(!is.na(week1_def[, rolling_def_cols, drop = FALSE]))
      if (week1_nonNA > 0) {
        stop("Week 1 defensive rolling features contain non-NA values. This indicates leakage.")
      }
    }
    cat("  Validated: Week 1 defensive features are NA (leakage-safe)\n")
    
  } else {
    warning("No defensive game stats available. Defensive features will be missing.")
  }
}, error = function(e) {
  warning(paste("Failed to build defensive features:", e$message, "Defensive features will be missing."))
})

# Get cache paths for output
player_directory_path <- file.path("data", "cache", "player_directory.parquet")
player_week_identity_path <- file.path("data", "cache", "player_week_identity.parquet")
rb_weekly_stats_path <- file.path("data", "cache", "rb_weekly_stats.parquet")
rb_weekly_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")

cat("\nWeekly caches refreshed. Files:")
cat("\n  -", player_directory_path)
cat("\n  -", player_week_identity_path)
cat("\n  -", rb_weekly_stats_path)
cat("\n  -", rb_weekly_features_path)
if (file.exists(defense_weekly_features_path)) {
  cat("\n  -", defense_weekly_features_path)
}
cat("\n")

