# Build K Features (player-level rolling features)
#
# Uses nflreadr weekly player stats. All rolling windows are leakage-safe.
# K stats include FG/PAT attempts and makes. No imputation is performed.

suppressPackageStartupMessages({
  library(dplyr)
})

build_k_features <- function(k_weekly_stats) {
  if (is.null(k_weekly_stats) || nrow(k_weekly_stats) == 0) {
    stop("k_weekly_stats must be provided with at least one row")
  }

  if (!exists("lagged_roll_mean")) {
    if (file.exists("R/utils/rolling_helpers.R")) {
      source("R/utils/rolling_helpers.R", local = TRUE)
    } else {
      stop("Missing R/utils/rolling_helpers.R")
    }
  }

  required_cols <- c(
    "player_id", "player_name", "team", "season", "week",
    "opponent", "home_away"
  )
  missing <- setdiff(required_cols, names(k_weekly_stats))
  if (length(missing) > 0) {
    stop("Missing required columns in k_weekly_stats: ", paste(missing, collapse = ", "))
  }

  stats <- k_weekly_stats
  stats$season <- as.integer(stats$season)
  stats$week <- as.integer(stats$week)
  stats$home_away <- toupper(trimws(as.character(stats$home_away)))
  if ("is_home" %in% names(stats)) {
    stats$is_home <- as.integer(stats$is_home)
  } else {
    stats$is_home <- NA_integer_
  }
  if (!"game_date" %in% names(stats) && "gameday" %in% names(stats)) {
    stats$game_date <- stats$gameday
  }
  stats$game_date <- as.Date(stats$game_date)

  # nflreadr kicking columns (load_player_stats, stat_type="kicking"):
  # fg_att/fg_made/fg_pct/fg_long, pat_att/pat_made/pat_pct.
  stats$fg_attempts <- suppressWarnings(as.numeric(stats$fg_att))
  stats$fg_made <- suppressWarnings(as.numeric(stats$fg_made))
  stats$fg_pct <- suppressWarnings(as.numeric(stats$fg_pct))
  stats$fg_long <- suppressWarnings(as.numeric(stats$fg_long))
  stats$pat_attempts <- suppressWarnings(as.numeric(stats$pat_att))
  stats$pat_made <- suppressWarnings(as.numeric(stats$pat_made))
  stats$pat_pct <- suppressWarnings(as.numeric(stats$pat_pct))

  stats <- stats[order(stats$player_id, stats$season, stats$week), ]

  features <- stats %>%
    transmute(
      game_id = if ("game_id" %in% names(stats)) game_id else NA_character_,
      game_key = if ("game_key" %in% names(stats)) game_key else NA_character_,
      player_id,
      player_name,
      team,
      position = "K",
      opponent,
      home_away,
      is_home,
      season,
      week,
      gameday = game_date,
      defense_team = if ("defense_team" %in% names(stats)) defense_team else opponent,
      target_fg_attempts_k = fg_attempts,
      target_fg_made_k = fg_made,
      target_fg_pct_k = fg_pct,
      target_pat_attempts_k = pat_attempts,
      target_pat_made_k = pat_made,
      target_pat_pct_k = pat_pct,
      target_fg_long_k = fg_long
    )

  roll_cols <- c(
    "target_fg_attempts_k",
    "target_fg_made_k",
    "target_fg_pct_k",
    "target_pat_attempts_k",
    "target_pat_made_k",
    "target_pat_pct_k",
    "target_fg_long_k"
  )

  for (col in roll_cols) {
    features[[paste0(col, "_roll1")]] <- NA_real_
    features[[paste0(col, "_roll3")]] <- NA_real_
    features[[paste0(col, "_roll5")]] <- NA_real_
  }

  players <- unique(features$player_id)
  for (pid in players) {
    idx <- which(features$player_id == pid)
    player_data <- features[idx, ]
    seasons <- unique(player_data$season)
    for (season_val in seasons) {
      season_idx <- idx[player_data$season == season_val]
      season_data <- features[season_idx, ]
      for (col in roll_cols) {
        season_vals <- season_data[[col]]
        features[[paste0(col, "_roll1")]][season_idx] <- c(NA_real_, head(season_vals, -1))
        features[[paste0(col, "_roll3")]][season_idx] <- lagged_roll_mean(season_vals, window = 3)
        features[[paste0(col, "_roll5")]][season_idx] <- lagged_roll_mean(season_vals, window = 5)
      }
    }
  }

  features
}
