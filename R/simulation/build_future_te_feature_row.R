# Build Future TE Feature Row
#
# Constructs a synthetic feature row for future games using the last completed
# historical game for that player. Preserves strict temporal causality by using
# only historical data and updating opponent/home-away from schedules.

#' Build synthetic TE feature row for future games
#'
#' Constructs a feature row for a future game by:
#'   1. Finding the last completed historical game for the player in the same season (week < target_week)
#'   2. Using that game's feature row as a template
#'   3. Updating opponent and home_away for the future game
#'   4. Preserving all rolling features from the last game (no recomputation)
#'
#' Enforces minimum 3-game history requirement (rolling features must be non-NA).
#' Maintains strict temporal causality by using only games before the target week.
#'
#' @param player_id Character, player_id (source of truth)
#' @param season Integer, target season (must match historical games)
#' @param week Integer, target week (must be > historical game weeks)
#' @param team Character, player's team abbreviation
#' @param opponent Character, opponent team abbreviation
#' @param home_away Character, "HOME" or "AWAY"
#' @param game_date Date, optional target game date (not used, kept for compatibility)
#' @return data.frame with one row containing synthetic features
build_future_te_feature_row <- function(player_id,
                                       season,
                                       week,
                                       team,
                                       opponent,
                                       home_away,
                                       game_date = NULL) {

  log_file <- "te_debug.log"
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }

  log_msg("=== build_future_te_feature_row ===")
  log_msg("INPUTS: player_id=", player_id, ", season=", season, ", week=", week)
  log_msg("INPUTS: team=", team, ", opponent=", opponent, ", home_away=", home_away)

  if (missing(player_id) || is.null(player_id) || length(player_id) == 0) {
    stop("player_id is required for future feature row construction")
  }
  player_id <- as.character(player_id)

  if (missing(season) || is.null(season) || is.na(season)) {
    stop("season is required for future feature row construction")
  }
  season <- as.integer(season)

  if (missing(week) || is.null(week) || is.na(week)) {
    stop("week is required for future feature row construction")
  }
  week <- as.integer(week)

  if (missing(team) || is.null(team) || length(team) == 0) {
    stop("team is required for future feature row construction")
  }
  team <- as.character(team)

  if (missing(opponent) || is.null(opponent) || length(opponent) == 0) {
    stop("opponent is required for future feature row construction")
  }
  opponent <- as.character(opponent)

  if (missing(home_away) || is.null(home_away) || length(home_away) == 0) {
    stop("home_away is required for future feature row construction")
  }
  home_away <- toupper(trimws(as.character(home_away)))
  if (!home_away %in% c("HOME", "AWAY")) {
    stop("home_away must be 'HOME' or 'AWAY', got: ", home_away)
  }

  if (!exists("read_te_weekly_features_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required for future feature row construction")
    }
  }

  te_features <- read_te_weekly_features_cache()
  if (nrow(te_features) == 0) {
    stop("TE weekly features cache is empty. Cannot build future feature row.")
  }

  historical_games <- te_features[
    te_features$player_id == player_id &
      te_features$season == season &
      !is.na(te_features$week) &
      te_features$week < week,
    , drop = FALSE
  ]

  if (nrow(historical_games) == 0) {
    stop("No games found for player_id '", player_id,
         "' in season ", season, " before week ", week,
         ". Cannot build future feature row. ",
         "Player must have played at least one game earlier in the same season.")
  }

  historical_games <- historical_games[order(
    historical_games$week,
    historical_games$gameday,
    decreasing = TRUE
  ), , drop = FALSE]

  last_game <- historical_games[1, ]

  rolling_cols <- grep("_roll[0-9]+$", names(last_game), value = TRUE)
  if (length(rolling_cols) == 0) {
    stop("Last historical game for player_id '", player_id,
         "' has no rolling features. Cannot build future feature row.")
  }

  has_rolling_features <- any(!is.na(last_game[, rolling_cols, drop = FALSE]))
  if (!has_rolling_features) {
    stop("Last historical game for player_id '", player_id,
         "' has all NA rolling features. Player must have at least 3 prior games. ",
         "Cannot build future feature row.")
  }

  synthetic_row <- last_game

  synthetic_row$season <- as.integer(season)
  synthetic_row$week <- as.integer(week)
  synthetic_row$team <- as.character(team)
  synthetic_row$opponent <- as.character(opponent)
  synthetic_row$home_away <- as.character(home_away)
  synthetic_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)

  if (is.na(synthetic_row$week) || synthetic_row$week != week) {
    stop("SYNTHETIC ROW WEEK INVARIANT VIOLATED: Requested week=", week,
         " but synthetic_row has week=", synthetic_row$week, ".")
  }

  if (!is.null(game_date) && !is.na(game_date)) {
    synthetic_row$game_date <- as.Date(game_date)
    synthetic_row$gameday <- as.Date(game_date)
  } else {
    synthetic_row$game_date <- NA
    synthetic_row$gameday <- NA
  }

  synthetic_row$game_id <- NA_character_
  synthetic_row$game_key <- NA_character_

  target_cols <- grep("^target_", names(synthetic_row), value = TRUE)
  for (col in target_cols) {
    synthetic_row[[col]] <- NA
  }

  if (!exists("get_passing_defense_all_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/passing_defense_features.R")
    }
  }

  required_features <- c(
    "targets_roll3", "targets_roll5",
    "receptions_roll3", "receptions_roll5",
    "rec_yards_roll3", "rec_yards_roll5",
    "air_yards_roll3", "air_yards_roll5",
    "target_share_roll1", "air_yards_share_roll1",
    get_passing_defense_all_features(),
    "is_home"
  )

  missing_features <- setdiff(required_features, names(synthetic_row))
  if (length(missing_features) > 0) {
    stop("Missing required feature columns in synthetic row: ",
         paste(missing_features, collapse = ", "))
  }

  synthetic_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)

  # Ensure numeric rolling/share features keep numeric types for prediction
  numeric_cols <- c(
    "targets_roll1", "targets_roll3", "targets_roll5",
    "receptions_roll1", "receptions_roll3", "receptions_roll5",
    "rec_yards_roll1", "rec_yards_roll3", "rec_yards_roll5",
    "air_yards_roll1", "air_yards_roll3", "air_yards_roll5",
    "target_share_roll1", "air_yards_share_roll1"
  )
  for (col in intersect(numeric_cols, names(synthetic_row))) {
    synthetic_row[[col]] <- as.numeric(synthetic_row[[col]])
  }

  rownames(synthetic_row) <- NULL

  log_msg("OUTPUTS: synthetic_row created with season=", synthetic_row$season,
          ", week=", synthetic_row$week)
  log_msg("OUTPUTS: team=", synthetic_row$team, ", opponent=", synthetic_row$opponent,
          ", is_home=", synthetic_row$is_home)

  synthetic_row
}
