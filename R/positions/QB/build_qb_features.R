# Build QB Features (player-level rolling features)
#
# Uses nflreadr weekly player stats. All rolling windows are leakage-safe.
# QB stats are offense-caused (attempts, completions, yards, TDs, INTs thrown,
# sacks taken, rush attempts/yards). No imputation is performed.

suppressPackageStartupMessages({
  library(dplyr)
})

build_qb_player_features <- function(qb_weekly_stats) {
  if (is.null(qb_weekly_stats) || nrow(qb_weekly_stats) == 0) {
    stop("qb_weekly_stats must be provided with at least one row")
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
  missing <- setdiff(required_cols, names(qb_weekly_stats))
  if (length(missing) > 0) {
    stop("Missing required columns in qb_weekly_stats: ", paste(missing, collapse = ", "))
  }

  stats <- qb_weekly_stats
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

  pick_col <- function(df, candidates) {
    for (candidate in candidates) {
      if (candidate %in% names(df)) {
        return(df[[candidate]])
      }
    }
    rep(NA, nrow(df))
  }

  # nflreadr QB columns (2023 sample): attempts, completions, passing_yards,
  # passing_tds, passing_interceptions, sacks_suffered, passing_air_yards,
  # rushing_yards, rushing_tds.
  stats$pass_attempts <- suppressWarnings(as.numeric(pick_col(stats, c("attempts", "pass_attempts"))))
  stats$pass_completions <- suppressWarnings(as.numeric(pick_col(stats, c("completions", "pass_completions"))))
  stats$pass_yards <- suppressWarnings(as.numeric(pick_col(stats, c("passing_yards", "pass_yards"))))
  stats$pass_tds <- suppressWarnings(as.numeric(pick_col(stats, c("passing_tds", "pass_tds"))))
  stats$interceptions_thrown <- suppressWarnings(as.numeric(pick_col(stats, c("passing_interceptions", "interceptions"))))
  stats$sacks_taken <- suppressWarnings(as.numeric(pick_col(stats, c("sacks_suffered", "qb_sacks", "sacks_taken"))))
  stats$pass_air_yards <- suppressWarnings(as.numeric(pick_col(stats, c("passing_air_yards", "air_yards"))))
  stats$qb_rush_attempts <- suppressWarnings(as.numeric(pick_col(stats, c("carries", "rush_attempts", "rushing_attempts"))))
  stats$qb_rush_yards <- suppressWarnings(as.numeric(pick_col(stats, c("rushing_yards", "rush_yards"))))
  stats$qb_rush_tds <- suppressWarnings(as.numeric(pick_col(stats, c("rushing_tds", "rush_tds"))))

  stats$completion_pct <- ifelse(
    !is.na(stats$pass_attempts) & stats$pass_attempts > 0,
    stats$pass_completions / stats$pass_attempts,
    NA_real_
  )

  stats <- stats[order(stats$player_id, stats$season, stats$week), ]

  features <- stats %>%
    transmute(
      game_id = if ("game_id" %in% names(stats)) game_id else NA_character_,
      game_key = if ("game_key" %in% names(stats)) game_key else NA_character_,
      player_id,
      player_name,
      team,
      position = "QB",
      opponent,
      home_away,
      is_home,
      season,
      week,
      gameday = game_date,
      defense_team = if ("defense_team" %in% names(stats)) defense_team else opponent,
      target_pass_attempts_qb = pass_attempts,
      target_completions_qb = pass_completions,
      target_completion_pct_qb = completion_pct,
      target_pass_yards_qb = pass_yards,
      target_pass_tds_qb = pass_tds,
      target_interceptions_qb_thrown = interceptions_thrown,
      target_sacks_qb_taken = sacks_taken,
      target_air_yards_qb = pass_air_yards,
      target_qb_rush_attempts = qb_rush_attempts,
      target_qb_rush_yards = qb_rush_yards,
      target_qb_rush_tds = qb_rush_tds
    )

  roll_cols <- c(
    "target_pass_attempts_qb",
    "target_completions_qb",
    "target_completion_pct_qb",
    "target_pass_yards_qb",
    "target_pass_tds_qb",
    "target_interceptions_qb_thrown",
    "target_sacks_qb_taken",
    "target_air_yards_qb",
    "target_qb_rush_attempts",
    "target_qb_rush_yards",
    "target_qb_rush_tds"
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

  # Goal-line signal: QB rush TD rate over prior games (leakage-safe).
  rate_from_roll <- function(tds_roll, att_roll) {
    ifelse(
      !is.na(tds_roll) & !is.na(att_roll) & att_roll > 0,
      tds_roll / att_roll,
      ifelse(!is.na(tds_roll) & !is.na(att_roll) & att_roll == 0 & tds_roll == 0, 0, NA_real_)
    )
  }
  features$target_qb_rush_td_rate_roll3 <- rate_from_roll(
    features$target_qb_rush_tds_roll3,
    features$target_qb_rush_attempts_roll3
  )
  features$target_qb_rush_td_rate_roll5 <- rate_from_roll(
    features$target_qb_rush_tds_roll5,
    features$target_qb_rush_attempts_roll5
  )

  features
}
