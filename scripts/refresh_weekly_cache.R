# Refresh Weekly Cache
#
# Builds Layer 1/2 caches and Layer 3 RB rolling features using nflreadr.
# This script is the only place that performs nflreadr downloads.

if (basename(getwd()) == "scripts") {
  setwd("..")
}

if (!exists("build_player_week_identity") || !exists("build_rb_weekly_stats") ||
    !exists("build_wr_weekly_stats") || !exists("build_te_weekly_stats") ||
    !exists("read_rb_weekly_features_cache") || !exists("read_wr_weekly_features_cache") ||
    !exists("read_te_weekly_features_cache")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R")
  } else {
    stop("Missing R/data/build_weekly_player_layers.R")
  }
}
if (!exists("build_player_dim")) {
  if (file.exists("R/data/build_player_dim.R")) {
    source("R/data/build_player_dim.R")
  } else {
    stop("Missing R/data/build_player_dim.R")
  }
}

if (!exists("assemble_rb_weekly_features")) {
  if (file.exists("R/assemble/assemble_rb_training_data.R")) {
    source("R/assemble/assemble_rb_training_data.R")
  } else {
    stop("Missing R/assemble/assemble_rb_training_data.R")
  }
}

if (!exists("assemble_wr_weekly_features")) {
  if (file.exists("R/positions/WR/assemble_wr_training_data.R")) {
    source("R/positions/WR/assemble_wr_training_data.R")
  } else {
    stop("Missing R/positions/WR/assemble_wr_training_data.R")
  }
}

if (!exists("assemble_te_weekly_features")) {
  if (file.exists("R/positions/TE/assemble_te_training_data.R")) {
    source("R/positions/TE/assemble_te_training_data.R")
  } else {
    stop("Missing R/positions/TE/assemble_te_training_data.R")
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
resolve_max_season <- function(default_year) {
  max_season <- NA_integer_
  cache_dir <- file.path("data", "cache")
  candidates <- c(
    file.path(cache_dir, "schedules.rds"),
    list.files(cache_dir, pattern = "schedule", full.names = TRUE)
  )
  candidates <- unique(candidates[file.exists(candidates)])
  for (path in candidates) {
    obj <- tryCatch({
      if (grepl("\\.rds$", path, ignore.case = TRUE)) {
        readRDS(path)
      } else if (grepl("\\.parquet$", path, ignore.case = TRUE) && requireNamespace("arrow", quietly = TRUE)) {
        arrow::read_parquet(path)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (!is.null(obj) && nrow(obj) > 0 && "season" %in% names(obj)) {
      max_season <- suppressWarnings(max(as.integer(obj$season), na.rm = TRUE))
      if (is.finite(max_season)) {
        break
      }
    }
  }
  if (!is.finite(max_season)) {
    max_season <- default_year
  } else if (max_season < default_year) {
    max_season <- default_year
  }
  max_season
}
max_season <- resolve_max_season(current_year)
seasons_to_refresh <- 1999:max_season

cat("Refreshing weekly RB caches for seasons", min(seasons_to_refresh), "through", max(seasons_to_refresh), "...\n")

# Rolling window diagnostics (no mutation)
log_roll_window_summary <- function(df, label, expected_windows = NULL, disallowed_windows = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    cat("  Rolling windows in", label, ": (no rows)\n")
    return(invisible(NULL))
  }
  rolling_cols <- grep("_roll[0-9]+$", names(df), value = TRUE)
  if (length(rolling_cols) == 0) {
    cat("  Rolling windows in", label, ": (none)\n")
    return(invisible(NULL))
  }
  roll_sizes <- sort(unique(as.integer(gsub(".*_roll([0-9]+)$", "\\1", rolling_cols))))
  cat("  Rolling windows in", label, ":", paste0("roll", roll_sizes, collapse = ", "), "\n")
  if (!is.null(expected_windows)) {
    missing <- setdiff(expected_windows, roll_sizes)
    if (length(missing) > 0) {
      warning(label, " missing expected rolling windows: ",
              paste0("roll", missing, collapse = ", "), call. = FALSE)
    }
  }
  if (!is.null(disallowed_windows)) {
    present <- intersect(disallowed_windows, roll_sizes)
    if (length(present) > 0) {
      warning(label, " contains disallowed rolling windows: ",
              paste0("roll", present, collapse = ", "), call. = FALSE)
    }
  }
  expected_first <- list("1" = 2L, "3" = 3L, "5" = 5L)
  for (roll_size in roll_sizes) {
    cols <- rolling_cols[grepl(paste0("_roll", roll_size, "$"), rolling_cols)]
    has_vals <- apply(!is.na(df[, cols, drop = FALSE]), 1, any)
    first_non_na <- if (any(has_vals)) suppressWarnings(min(df$week[has_vals], na.rm = TRUE)) else NA_integer_
    expected_week <- expected_first[[as.character(roll_size)]]
    expected_week <- if (is.null(expected_week)) roll_size else expected_week
    cat("    - roll", roll_size, ": first non-NA week=",
        ifelse(is.na(first_non_na), "NA", first_non_na),
        " (expected >=", expected_week, ")\n", sep = "")
  }
  invisible(NULL)
}

validate_roll_na_weeks <- function(df, label, roll_size, weeks) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  cols <- grep(paste0("_roll", roll_size, "$"), names(df), value = TRUE)
  if (length(cols) == 0) return(invisible(NULL))
  rows <- df$week %in% weeks
  if (!any(rows)) return(invisible(NULL))
  non_na <- sum(!is.na(df[rows, cols, drop = FALSE]))
  if (non_na > 0) {
    warning(label, " roll", roll_size, " has non-NA values in weeks ",
            paste(weeks, collapse = ", "), ".", call. = FALSE)
  } else {
    cat("  Validated: ", label, " roll", roll_size, " NA for weeks ",
        paste(weeks, collapse = ", "), "\n", sep = "")
  }
  invisible(NULL)
}

# Build player directory cache (required for name resolution)
if (!exists("build_player_directory")) {
  if (file.exists("R/data/build_weekly_player_layers.R")) {
    source("R/data/build_weekly_player_layers.R")
  } else {
    stop("Missing R/data/build_weekly_player_layers.R")
  }
}

message("Building player directory cache (live if available, cached fallback)...")
player_dir <- build_player_directory(write_cache = TRUE)
stopifnot(exists("player_dir"), nrow(player_dir) > 1000)
cat("  Built player directory cache with", nrow(player_dir), "players\n")

cat("  Building player dimension cache...\n")
player_dim <- build_player_dim(seasons = seasons_to_refresh, write_cache = TRUE)
if (nrow(player_dim) == 0) {
  stop("Player dimension cache built with zero rows; ensure nflreadr returned data.")
}
cat("  Built player dimension cache with", nrow(player_dim), "rows\n")

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

wr_stats <- build_wr_weekly_stats(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
if (nrow(wr_stats) == 0) {
  stop("WR weekly stats cache built with zero rows; ensure nflreadr returned WR rows.")
}
cat("  Built WR weekly stats cache with", nrow(wr_stats), "rows\n")

te_stats <- build_te_weekly_stats(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
if (nrow(te_stats) == 0) {
  stop("TE weekly stats cache built with zero rows; ensure nflreadr returned TE rows.")
}
cat("  Built TE weekly stats cache with", nrow(te_stats), "rows\n")

# ------------------------------------------------------------------
# Build defensive weekly features FIRST (dependency for RB features)
# ------------------------------------------------------------------

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

def_game_stats <- build_team_defense_game_stats(seasons = seasons_to_refresh)
def_features <- build_team_defense_features(def_game_stats)

# Rolling window diagnostics (defense)
log_roll_window_summary(def_features, "defense_weekly_features", expected_windows = c(1, 3, 5), disallowed_windows = integer(0))
validate_roll_na_weeks(def_features, "defense_weekly_features", 1, 1)
validate_roll_na_weeks(def_features, "defense_weekly_features", 3, 1:2)
validate_roll_na_weeks(def_features, "defense_weekly_features", 5, 1:4)

# Canonicalize and validate defense_team contract
if (!"defense_team" %in% names(def_features)) {
  if ("team" %in% names(def_features)) {
    def_features$defense_team <- def_features$team
  } else if ("team_abbr" %in% names(def_features)) {
    def_features$defense_team <- def_features$team_abbr
  } else {
    stop(
      "Defensive weekly features missing team identifier. Expected `defense_team`, `team`, or `team_abbr`.",
      call. = FALSE
    )
  }
}

required_def_cols <- c("defense_team", "season", "week")
missing_def <- setdiff(required_def_cols, names(def_features))
if (length(missing_def) > 0) {
  stop(
    "Defensive weekly features missing required columns: ",
    paste(missing_def, collapse = ", "),
    call. = FALSE
  )
}

dupes_def <- duplicated(def_features[, c("defense_team", "season", "week")])
if (any(dupes_def)) {
  stop(
    "Duplicate rows found in defensive weekly features for (defense_team, season, week).",
    call. = FALSE
  )
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write defensive weekly features. Install with install.packages('arrow').")
}

defense_weekly_path <- file.path("data", "processed", "defense_weekly_features.parquet")
dir.create(dirname(defense_weekly_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(def_features, defense_weekly_path)

cat("  Built defensive weekly features cache with", nrow(def_features), "rows\n")

# Validation: Check for Week 1 leakage (diagnostic only)
week1_def <- def_features[def_features$week == 1, ]
if (nrow(week1_def) > 0) {
  rolling_def_cols <- grep("_roll[0-9]+$", names(def_features), value = TRUE)
  week1_nonNA <- sum(!is.na(week1_def[, rolling_def_cols, drop = FALSE]))
  if (week1_nonNA > 0) {
    warning("Week 1 defensive rolling features contained non-NA values. Review rolling window computation.", call. = FALSE)
  }
}
cat("  Validated: Week 1 defensive features are NA (leakage-safe)\n")

# ------------------------------------------------------------------
# Build team offense context (lagged team-level signals)
# ------------------------------------------------------------------
cat("  Building team offense context features...\n")
if (!exists("build_team_offense_context")) {
  if (file.exists("R/features/build_team_offense_context.R")) {
    source("R/features/build_team_offense_context.R")
  } else {
    stop("Missing R/features/build_team_offense_context.R")
  }
}
team_offense_context <- build_team_offense_context(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
if (nrow(team_offense_context) == 0) {
  stop("Team offense context cache built with zero rows; ensure nflreadr returned data.")
}
cat("  Built team offense context cache with", nrow(team_offense_context), "rows\n")

# ------------------------------------------------------------------
# Build QB rolling features (team-level QB context)
# ------------------------------------------------------------------
cat("  Building QB rolling features...\n")
if (!exists("build_qb_game_stats")) {
  if (file.exists("R/data/build_qb_game_stats.R")) {
    source("R/data/build_qb_game_stats.R")
  } else {
    stop("Missing R/data/build_qb_game_stats.R")
  }
}
if (!exists("build_qb_features")) {
  if (file.exists("R/features/build_qb_features.R")) {
    source("R/features/build_qb_features.R")
  } else {
    stop("Missing R/features/build_qb_features.R")
  }
}

qb_game_stats <- build_qb_game_stats(seasons = seasons_to_refresh, season_type = "REG")
qb_features <- build_qb_features(qb_game_stats)

required_qb_cols <- c(
  "team", "season", "week",
  "target_completion_pct_qb_roll3",
  "target_interceptions_qb_thrown_roll3"
)
missing_qb_cols <- setdiff(required_qb_cols, names(qb_features))
if (length(missing_qb_cols) > 0) {
  stop("QB rolling features missing required columns: ", paste(missing_qb_cols, collapse = ", "))
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write QB weekly features. Install with install.packages('arrow').")
}
qb_weekly_features_path <- file.path("data", "processed", "qb_weekly_features.parquet")
dir.create(dirname(qb_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(qb_features, qb_weekly_features_path)
cat("  Built QB rolling features cache with", nrow(qb_features), "rows\n")

# ------------------------------------------------------------------
# Build prior-season player aggregates (season - 1)
# ------------------------------------------------------------------
cat("  Building prior-season player stats cache...\n")
if (!exists("build_prior_season_player_stats")) {
  if (file.exists("R/features/build_prior_season_player_stats.R")) {
    source("R/features/build_prior_season_player_stats.R")
  } else {
    stop("Missing R/features/build_prior_season_player_stats.R")
  }
}
prior_season_stats <- build_prior_season_player_stats(
  seasons = seasons_to_refresh,
  season_type = "REG",
  write_cache = TRUE
)
cat("  Built prior-season stats cache with", nrow(prior_season_stats), "rows\n")

# ------------------------------------------------------------------
# NOW assemble RB rolling features (safe to depend on defense cache)
# ------------------------------------------------------------------

cat("  Computing RB rolling features...\n")
rb_features <- assemble_rb_weekly_features(rb_stats)

# Rolling window diagnostics (RB)
log_roll_window_summary(rb_features, "rb_weekly_features", expected_windows = c(1, 3, 5))
validate_roll_na_weeks(rb_features, "rb_weekly_features", 1, 1)
validate_roll_na_weeks(rb_features, "rb_weekly_features", 3, 1:2)
validate_roll_na_weeks(rb_features, "rb_weekly_features", 5, 1:4)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write RB weekly features. Install with install.packages('arrow').")
}

dir.create(dirname(rb_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(rb_features, rb_weekly_features_path)

if (nrow(rb_features) == 0) {
  stop("RB weekly features cache empty after feature computation.")
}

cat("  Built RB weekly features cache with", nrow(rb_features), "rows\n")

# ------------------------------------------------------------------
# Build WR rolling features
# ------------------------------------------------------------------

cat("  Computing WR rolling features...\n")
wr_features <- assemble_wr_weekly_features(wr_stats)

log_roll_window_summary(wr_features, "wr_weekly_features", expected_windows = c(1, 3, 5))
validate_roll_na_weeks(wr_features, "wr_weekly_features", 1, 1)
validate_roll_na_weeks(wr_features, "wr_weekly_features", 3, 1:2)
validate_roll_na_weeks(wr_features, "wr_weekly_features", 5, 1:4)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write WR weekly features. Install with install.packages('arrow').")
}

dir.create(dirname(wr_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(wr_features, wr_weekly_features_path)

if (nrow(wr_features) == 0) {
  stop("WR weekly features cache empty after feature computation.")
}

cat("  Built WR weekly features cache with", nrow(wr_features), "rows\n")

# ------------------------------------------------------------------
# Build TE rolling features
# ------------------------------------------------------------------

cat("  Computing TE rolling features...\n")
te_features <- assemble_te_weekly_features(te_stats)

log_roll_window_summary(te_features, "te_weekly_features", expected_windows = c(1, 3, 5))
validate_roll_na_weeks(te_features, "te_weekly_features", 1, 1)
validate_roll_na_weeks(te_features, "te_weekly_features", 3, 1:2)
validate_roll_na_weeks(te_features, "te_weekly_features", 5, 1:4)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write TE weekly features. Install with install.packages('arrow').")
}

dir.create(dirname(te_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(te_features, te_weekly_features_path)

if (nrow(te_features) == 0) {
  stop("TE weekly features cache empty after feature computation.")
}

cat("  Built TE weekly features cache with", nrow(te_features), "rows\n")

# Get cache paths for output
player_directory_path <- file.path("data", "cache", "player_directory.parquet")
player_week_identity_path <- file.path("data", "cache", "player_week_identity.parquet")
rb_weekly_stats_path <- file.path("data", "cache", "rb_weekly_stats.parquet")
rb_weekly_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
wr_weekly_stats_path <- file.path("data", "cache", "wr_weekly_stats.parquet")
wr_weekly_features_path <- file.path("data", "processed", "wr_weekly_features.parquet")
te_weekly_stats_path <- file.path("data", "cache", "te_weekly_stats.parquet")
te_weekly_features_path <- file.path("data", "processed", "te_weekly_features.parquet")
defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
player_dim_path <- file.path("data", "processed", "player_dim.parquet")
team_offense_context_path <- file.path("data", "processed", "team_offense_context.parquet")
qb_weekly_features_path <- file.path("data", "processed", "qb_weekly_features.parquet")
prior_season_player_stats_path <- file.path("data", "processed", "prior_season_player_stats.parquet")

cat("\nWeekly caches refreshed. Files:")
cat("\n  -", player_directory_path)
cat("\n  -", player_week_identity_path)
cat("\n  -", rb_weekly_stats_path)
cat("\n  -", rb_weekly_features_path)
cat("\n  -", wr_weekly_stats_path)
cat("\n  -", wr_weekly_features_path)
cat("\n  -", te_weekly_stats_path)
cat("\n  -", te_weekly_features_path)
if (file.exists(defense_weekly_features_path)) {
  cat("\n  -", defense_weekly_features_path)
}
if (file.exists(player_dim_path)) {
  cat("\n  -", player_dim_path)
}
if (file.exists(team_offense_context_path)) {
  cat("\n  -", team_offense_context_path)
}
if (file.exists(qb_weekly_features_path)) {
  cat("\n  -", qb_weekly_features_path)
}
if (file.exists(prior_season_player_stats_path)) {
  cat("\n  -", prior_season_player_stats_path)
}
cat("\n")
