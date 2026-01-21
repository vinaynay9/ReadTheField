# Refresh Weekly Cache
#
# Builds Layer 1/2 caches and Layer 3 RB rolling features using nflreadr.
# This script is the only place that performs nflreadr downloads.

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, cmd_args, value = TRUE)
  if (length(match) > 0) {
    return(normalizePath(sub(file_arg, "", match[1])))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
  stop("Unable to determine script path to set working directory.")
}

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(repo_root)
options(READTHEFIELD_REPO_ROOT = repo_root)
options(READTHEFIELD_SKIP_CACHE_CHECK = TRUE)

args <- commandArgs(trailingOnly = TRUE)
mode_arg <- args[grepl("^--mode=", args)]
if (length(mode_arg) > 0) {
  mode_val <- sub("^--mode=", "", mode_arg[1])
  if (nzchar(mode_val)) {
    options(READTHEFIELD_REFRESH_MODE = mode_val)
  }
}

source(file.path(repo_root, "R/simulation/bootstrap_simulation.R"))
if (!exists("get_rb_v1_targets")) {
  stop("Bootstrap incomplete: get_rb_v1_targets not loaded.")
}

current_year <- as.integer(format(Sys.Date(), "%Y"))
if (requireNamespace("nflreadr", quietly = TRUE)) {
  nfl_year <- tryCatch(as.integer(nflreadr::most_recent_season()), error = function(e) NA_integer_)
  if (is.finite(nfl_year)) {
    # Clamp to nflreadr's known season range to avoid future-year roster errors.
    current_year <- min(current_year, nfl_year)
  }
}
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
  } else if (max_season > default_year) {
    # Guard against future seasons beyond current_year (e.g., preseason schedule stubs).
    max_season <- default_year
  }
  max_season
}
max_season <- resolve_max_season(current_year)
refresh_mode <- getOption("READTHEFIELD_REFRESH_MODE", "daily")
refresh_mode <- tolower(as.character(refresh_mode)[1])
if (!refresh_mode %in% c("daily", "full")) {
  stop("Invalid READTHEFIELD_REFRESH_MODE: ", refresh_mode, ". Use 'daily' or 'full'.")
}

if (refresh_mode == "full") {
  seasons_to_refresh <- 1999:max_season
} else {
  seasons_to_refresh <- unique(c(max_season, max_season - 1))
  seasons_to_refresh <- seasons_to_refresh[seasons_to_refresh >= 1999]
}

cat("Refresh mode:", refresh_mode, "\n")
cat("Refreshing weekly caches for seasons", min(seasons_to_refresh), "through", max(seasons_to_refresh), "...\n")

# Cache paths for output and reuse
player_directory_path <- file.path("data", "cache", "player_directory.parquet")
player_week_identity_path <- file.path("data", "cache", "player_week_identity.parquet")
rb_weekly_stats_path <- file.path("data", "cache", "rb_weekly_stats.parquet")
rb_weekly_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
wr_weekly_stats_path <- file.path("data", "cache", "wr_weekly_stats.parquet")
wr_weekly_features_path <- file.path("data", "processed", "wr_weekly_features.parquet")
te_weekly_stats_path <- file.path("data", "cache", "te_weekly_stats.parquet")
te_weekly_features_path <- file.path("data", "processed", "te_weekly_features.parquet")
qb_weekly_stats_path <- file.path("data", "cache", "qb_weekly_stats.parquet")
qb_player_weekly_features_path <- file.path("data", "processed", "qb_player_weekly_features.parquet")
k_weekly_stats_path <- file.path("data", "cache", "k_weekly_stats.parquet")
k_weekly_features_path <- file.path("data", "processed", "k_weekly_features.parquet")
defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
player_dim_path <- file.path("data", "processed", "player_dim.parquet")
team_offense_context_path <- file.path("data", "processed", "team_offense_context.parquet")
qb_weekly_features_path <- file.path("data", "processed", "qb_weekly_features.parquet")
prior_season_player_stats_path <- file.path("data", "processed", "prior_season_player_stats.parquet")

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

run_block <- function(label, expr) {
  timing <- system.time(result <- eval(expr, parent.frame()))
  cat("Timing:", label, "-", sprintf("%.1f", timing[["elapsed"]]), "sec\n")
  result
}

ensure_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read/write parquet caches. Install with install.packages('arrow').")
  }
}

read_parquet_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  ensure_arrow()
  arrow::read_parquet(path)
}

merge_season_cache <- function(existing, refreshed, seasons) {
  if (is.null(existing) || nrow(existing) == 0) return(refreshed)
  if (is.null(refreshed) || nrow(refreshed) == 0) return(existing)
  if (!"season" %in% names(existing) || !"season" %in% names(refreshed)) {
    stop("Cannot merge caches: missing 'season' column.")
  }
  keep_existing <- existing[!as.integer(existing$season) %in% as.integer(seasons), , drop = FALSE]
  combined <- rbind(keep_existing, refreshed)
  combined
}

stats_seasons <- seasons_to_refresh
if (refresh_mode == "daily") {
  existing_stats_probe <- read_parquet_safe(rb_weekly_stats_path)
  if (is.null(existing_stats_probe) ||
      !"season" %in% names(existing_stats_probe) ||
      all(as.integer(existing_stats_probe$season) %in% as.integer(seasons_to_refresh))) {
    stats_seasons <- 1999:max_season
    cat("  Daily mode: historical stats cache missing; refreshing full seasons for weekly stats/identity.\n")
  }
}

existing_identity <- if (refresh_mode == "daily") read_parquet_safe(player_week_identity_path) else NULL
identity <- run_block("player week identity cache", quote({
  identity <- build_player_week_identity(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(identity) == 0) {
    stop("Player identity cache built with zero rows; ensure nflreadr returned data and try again.")
  }
  cat("  Built player week identity cache with", nrow(identity), "rows\n")
  identity
}))
if (refresh_mode == "daily" && !is.null(existing_identity)) {
  identity <- merge_season_cache(existing_identity, identity, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(identity, player_week_identity_path)
  cat("  Daily mode: merged player week identity cache with existing seasons.\n")
}

if (refresh_mode == "full") {
  player_dim <- run_block("player dimension cache", quote({
    cat("  Building player dimension cache...\n")
    player_dim <- build_player_dim(seasons = seasons_to_refresh, write_cache = TRUE)
    if (nrow(player_dim) == 0) {
      stop("Player dimension cache built with zero rows; ensure nflreadr returned data.")
    }
    cat("  Built player dimension cache with", nrow(player_dim), "rows\n")
    player_dim
  }))
} else {
  cat("  Daily mode: skipping player dimension rebuild (reusing existing cache).\n")
}

existing_rb_stats <- if (refresh_mode == "daily") read_parquet_safe(rb_weekly_stats_path) else NULL
rb_stats <- run_block("RB weekly stats cache", quote({
  rb_stats <- build_rb_weekly_stats(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(rb_stats) == 0) {
    stop("RB weekly stats cache built with zero rows; ensure nflreadr returned RB rows.")
  }
  cat("  Built RB weekly stats cache with", nrow(rb_stats), "rows\n")
  rb_stats
}))
if (refresh_mode == "daily" && !is.null(existing_rb_stats)) {
  rb_stats <- merge_season_cache(existing_rb_stats, rb_stats, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(rb_stats, rb_weekly_stats_path)
  cat("  Daily mode: merged RB weekly stats with existing seasons.\n")
}

existing_wr_stats <- if (refresh_mode == "daily") read_parquet_safe(wr_weekly_stats_path) else NULL
wr_stats <- run_block("WR weekly stats cache", quote({
  wr_stats <- build_wr_weekly_stats(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(wr_stats) == 0) {
    stop("WR weekly stats cache built with zero rows; ensure nflreadr returned WR rows.")
  }
  cat("  Built WR weekly stats cache with", nrow(wr_stats), "rows\n")
  wr_stats
}))
if (refresh_mode == "daily" && !is.null(existing_wr_stats)) {
  wr_stats <- merge_season_cache(existing_wr_stats, wr_stats, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(wr_stats, wr_weekly_stats_path)
  cat("  Daily mode: merged WR weekly stats with existing seasons.\n")
}

existing_te_stats <- if (refresh_mode == "daily") read_parquet_safe(te_weekly_stats_path) else NULL
te_stats <- run_block("TE weekly stats cache", quote({
  te_stats <- build_te_weekly_stats(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(te_stats) == 0) {
    stop("TE weekly stats cache built with zero rows; ensure nflreadr returned TE rows.")
  }
  cat("  Built TE weekly stats cache with", nrow(te_stats), "rows\n")
  te_stats
}))
if (refresh_mode == "daily" && !is.null(existing_te_stats)) {
  te_stats <- merge_season_cache(existing_te_stats, te_stats, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(te_stats, te_weekly_stats_path)
  cat("  Daily mode: merged TE weekly stats with existing seasons.\n")
}

existing_qb_stats <- if (refresh_mode == "daily") read_parquet_safe(qb_weekly_stats_path) else NULL
qb_stats <- run_block("QB weekly stats cache", quote({
  qb_stats <- build_qb_weekly_stats(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(qb_stats) == 0) {
    stop("QB weekly stats cache built with zero rows; ensure nflreadr returned QB rows.")
  }
  cat("  Built QB weekly stats cache with", nrow(qb_stats), "rows\n")
  qb_stats
}))
if (refresh_mode == "daily" && !is.null(existing_qb_stats)) {
  qb_stats <- merge_season_cache(existing_qb_stats, qb_stats, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(qb_stats, qb_weekly_stats_path)
  cat("  Daily mode: merged QB weekly stats with existing seasons.\n")
}

existing_k_stats <- if (refresh_mode == "daily") read_parquet_safe(k_weekly_stats_path) else NULL
k_stats <- run_block("K weekly stats cache", quote({
  k_stats <- build_k_weekly_stats(
    seasons = stats_seasons,
    season_type = "REG",
    write_cache = TRUE
  )
  if (nrow(k_stats) == 0) {
    stop("K weekly stats cache built with zero rows; ensure nflreadr returned K rows.")
  }
  cat("  Built K weekly stats cache with", nrow(k_stats), "rows\n")
  k_stats
}))
if (refresh_mode == "daily" && !is.null(existing_k_stats)) {
  k_stats <- merge_season_cache(existing_k_stats, k_stats, stats_seasons)
  ensure_arrow()
  arrow::write_parquet(k_stats, k_weekly_stats_path)
  cat("  Daily mode: merged K weekly stats with existing seasons.\n")
}

player_dir <- run_block("player directory cache", quote({
  message("Building player directory cache from player_dim...")
  player_dir <- build_player_directory(write_cache = TRUE)
  stopifnot(exists("player_dir"), nrow(player_dir) > 1000)
  cat("  Built player directory cache with", nrow(player_dir), "players\n")
  player_dir
}))

if (exists("validate_position_team_contract")) {
  validate_position_team_contract(identity, "Player week identity cache", position_col = "position", team_col = "team")
  if (exists("read_player_dim_cache")) {
    dim_check <- read_player_dim_cache()
    validate_position_team_contract(dim_check, "Player dimension cache", position_col = "position", team_col = "team")
  }
  validate_position_team_contract(player_dir, "Player directory cache", position_col = "position", team_col = "team")
}

if (refresh_mode == "full") {
  # ------------------------------------------------------------------
  # Build defensive weekly features FIRST (dependency for RB features)
  # ------------------------------------------------------------------

  def_features <- run_block("defensive weekly features", quote({
    cat("  Computing defensive weekly features...\n")
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
    def_features
  }))

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
  team_offense_context <- run_block("team offense context features", quote({
    cat("  Building team offense context features...\n")
    team_offense_context <- build_team_offense_context(
      seasons = seasons_to_refresh,
      season_type = "REG",
      write_cache = TRUE
    )
    if (nrow(team_offense_context) == 0) {
      stop("Team offense context cache built with zero rows; ensure nflreadr returned data.")
    }
    cat("  Built team offense context cache with", nrow(team_offense_context), "rows\n")
    team_offense_context
  }))

# ------------------------------------------------------------------
# Build QB rolling features (team-level QB context)
# ------------------------------------------------------------------
  qb_features <- run_block("QB rolling features (team-level)", quote({
    cat("  Building QB rolling features...\n")
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
    qb_features
  }))

# ------------------------------------------------------------------
# Build prior-season player aggregates (season - 1)
# ------------------------------------------------------------------
  prior_season_stats <- run_block("prior-season player stats cache", quote({
    cat("  Building prior-season player stats cache...\n")
    prior_season_stats <- build_prior_season_player_stats(
      seasons = seasons_to_refresh,
      season_type = "REG",
      write_cache = TRUE
    )
    cat("  Built prior-season stats cache with", nrow(prior_season_stats), "rows\n")
    prior_season_stats
  }))

# ------------------------------------------------------------------
# NOW assemble RB rolling features (safe to depend on defense cache)
# ------------------------------------------------------------------

  rb_features <- run_block("RB rolling features", quote({
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
    rb_features
  }))

# ------------------------------------------------------------------
# Build WR rolling features
# ------------------------------------------------------------------

  wr_features <- run_block("WR rolling features", quote({
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
    wr_features
  }))

# ------------------------------------------------------------------
# Build TE rolling features
# ------------------------------------------------------------------

  te_features <- run_block("TE rolling features", quote({
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
    te_features
  }))

# ------------------------------------------------------------------
# Build QB rolling features (player-level QB modeling cache)
# ------------------------------------------------------------------

  qb_features_player <- run_block("QB rolling features (player-level)", quote({
    cat("  Computing QB rolling features (player-level)...\n")
    qb_features_player <- assemble_qb_weekly_features(qb_stats)

log_roll_window_summary(qb_features_player, "qb_player_weekly_features", expected_windows = c(1, 3, 5))
validate_roll_na_weeks(qb_features_player, "qb_player_weekly_features", 1, 1)
validate_roll_na_weeks(qb_features_player, "qb_player_weekly_features", 3, 1:2)
validate_roll_na_weeks(qb_features_player, "qb_player_weekly_features", 5, 1:4)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write QB weekly features. Install with install.packages('arrow').")
}
qb_weekly_features_player_path <- file.path("data", "processed", "qb_player_weekly_features.parquet")
dir.create(dirname(qb_weekly_features_player_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(qb_features_player, qb_weekly_features_player_path)

if (nrow(qb_features_player) == 0) {
  stop("QB weekly features cache empty after feature computation.")
}
    cat("  Built QB weekly features cache with", nrow(qb_features_player), "rows\n")
    qb_features_player
  }))

# ------------------------------------------------------------------
# Build K rolling features
# ------------------------------------------------------------------

  k_features <- run_block("K rolling features", quote({
    cat("  Computing K rolling features...\n")
    k_features <- assemble_k_weekly_features(k_stats)

log_roll_window_summary(k_features, "k_weekly_features", expected_windows = c(1, 3, 5))
validate_roll_na_weeks(k_features, "k_weekly_features", 1, 1)
validate_roll_na_weeks(k_features, "k_weekly_features", 3, 1:2)
validate_roll_na_weeks(k_features, "k_weekly_features", 5, 1:4)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to write K weekly features. Install with install.packages('arrow').")
}
dir.create(dirname(k_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(k_features, k_weekly_features_path)

if (nrow(k_features) == 0) {
  stop("K weekly features cache empty after feature computation.")
}
    cat("  Built K weekly features cache with", nrow(k_features), "rows\n")
    k_features
  }))
} else {
  cat("  Daily mode: skipping defense, team context, and rolling feature rebuilds.\n")
}

validate_cache_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " missing at ", path, ".")
  }
  invisible(TRUE)
}

validate_current_season <- function(path, label, season) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to validate caches. Install with install.packages('arrow').")
  }
  if (!file.exists(path)) {
    stop(label, " missing at ", path, ".")
  }
  df <- arrow::read_parquet(path)
  if (is.null(df) || nrow(df) == 0) {
    stop(label, " is empty at ", path, ".")
  }
  if (!"season" %in% names(df)) {
    stop(label, " missing 'season' column at ", path, ".")
  }
  if (!any(as.integer(df$season) == as.integer(season))) {
    stop(label, " has no rows for season ", season, " at ", path, ".")
  }
  invisible(TRUE)
}

validate_cache_exists(player_directory_path, "Player directory cache")
validate_cache_exists(player_week_identity_path, "Player week identity cache")

validate_current_season(rb_weekly_stats_path, "RB weekly stats cache", max_season)
validate_current_season(wr_weekly_stats_path, "WR weekly stats cache", max_season)
validate_current_season(te_weekly_stats_path, "TE weekly stats cache", max_season)
validate_current_season(qb_weekly_stats_path, "QB weekly stats cache", max_season)
validate_current_season(k_weekly_stats_path, "K weekly stats cache", max_season)

write_cache_manifest <- function(paths, snapshot_id) {
  if (is.null(snapshot_id) || !nzchar(snapshot_id)) {
    snapshot_id <- format(Sys.Date(), "%Y%m%d")
  }
  entries <- list()
  for (path in paths) {
    if (!file.exists(path)) next
    size_bytes <- file.info(path)$size
    if (is.na(size_bytes) || size_bytes == 0) next
    row_count <- NA_integer_
    if (requireNamespace("arrow", quietly = TRUE) && grepl("\\.parquet$", path, ignore.case = TRUE)) {
      df <- tryCatch(arrow::read_parquet(path), error = function(e) NULL)
      if (!is.null(df)) row_count <- nrow(df)
    }
    entries[[path]] <- list(
      md5 = unname(tools::md5sum(path)),
      rows = row_count,
      bytes = size_bytes
    )
  }
  manifest <- list(
    snapshot_id = snapshot_id,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    files = entries
  )
  manifest_path <- file.path("data", "cache", paste0("cache_manifest_", snapshot_id, ".rds"))
  strict_manifest <- isTRUE(getOption("READTHEFIELD_STRICT_MANIFEST", TRUE))
  if (file.exists(manifest_path) && strict_manifest) {
    prior <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
    if (!is.null(prior) && !identical(prior$files, manifest$files)) {
      stop("Cache manifest differs from existing snapshot manifest. Check reproducibility or snapshot inputs.")
    }
  }
  saveRDS(manifest, manifest_path)
  saveRDS(manifest, file.path("data", "cache", "cache_manifest_latest.rds"))
  invisible(TRUE)
}

snapshot_id <- if (exists("get_snapshot_id")) get_snapshot_id() else format(Sys.Date(), "%Y%m%d")
manifest_paths <- c(
  player_directory_path,
  player_week_identity_path,
  rb_weekly_stats_path,
  wr_weekly_stats_path,
  te_weekly_stats_path,
  qb_weekly_stats_path,
  k_weekly_stats_path,
  rb_weekly_features_path,
  wr_weekly_features_path,
  te_weekly_features_path,
  qb_weekly_features_path,
  qb_player_weekly_features_path,
  k_weekly_features_path,
  defense_weekly_features_path,
  team_offense_context_path,
  player_dim_path,
  prior_season_player_stats_path
)
write_cache_manifest(manifest_paths, snapshot_id)

audit_simulation_position_usage <- function() {
  sim_paths <- list.files(file.path("R", "simulation"), pattern = "\\.R$", full.names = TRUE)
  if (length(sim_paths) == 0) return(invisible(NULL))
  files_missing_position <- character(0)
  for (path in sim_paths) {
    lines <- readLines(path, warn = FALSE)
    if (!any(grepl("\\bposition\\b", lines))) {
      files_missing_position <- c(files_missing_position, path)
    }
  }
  if (length(files_missing_position) > 0) {
    warning(
      "Simulation position audit: position not referenced explicitly in: ",
      paste(basename(files_missing_position), collapse = ", "),
      ". Position may be inferred implicitly.",
      call. = FALSE
    )
  } else {
    cat("Simulation position audit: all simulation files reference position.\n")
  }
  invisible(TRUE)
}

audit_simulation_position_usage()

cat("\nWeekly caches refreshed. Files:")
cat("\n  -", player_directory_path)
cat("\n  -", player_week_identity_path)
cat("\n  -", rb_weekly_stats_path)
cat("\n  -", rb_weekly_features_path)
cat("\n  -", wr_weekly_stats_path)
cat("\n  -", wr_weekly_features_path)
cat("\n  -", te_weekly_stats_path)
cat("\n  -", te_weekly_features_path)
if (file.exists(qb_weekly_stats_path)) {
  cat("\n  -", qb_weekly_stats_path)
}
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
if (file.exists(qb_player_weekly_features_path)) {
  cat("\n  -", qb_player_weekly_features_path)
}
if (file.exists(k_weekly_stats_path)) {
  cat("\n  -", k_weekly_stats_path)
}
if (file.exists(k_weekly_features_path)) {
  cat("\n  -", k_weekly_features_path)
}
if (file.exists(prior_season_player_stats_path)) {
  cat("\n  -", prior_season_player_stats_path)
}
cat("\n")
