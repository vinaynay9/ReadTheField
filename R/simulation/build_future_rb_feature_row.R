# Build Future RB Feature Row
#
# Constructs a synthetic feature row for future games using the last completed
# historical game for that player. Preserves strict temporal causality by using
# only historical data and updating opponent/home-away from schedules.
#
# Dependencies:
#   - R/data/build_weekly_player_layers.R (for read_rb_weekly_features_cache)
#   - R/data/load_schedules.R (for schedule data)
#
# Usage:
#   feature_row <- build_future_rb_feature_row(
#     player_id = "00-0031234",
#     season = 2025,
#     week = 15,
#     team = "ATL",
#     opponent = "TB",
#     home_away = "HOME"
#   )

#' Build synthetic RB feature row for future games
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
build_future_rb_feature_row <- function(player_id,
                                       season,
                                       week,
                                       team,
                                       opponent,
                                       home_away,
                                       game_date = NULL) {
  
  # Validate inputs
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
  
  # Load required caches
  if (!exists("read_rb_weekly_features_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required for future feature row construction")
    }
  }
  
  rb_features <- read_rb_weekly_features_cache()
  if (nrow(rb_features) == 0) {
    stop("RB weekly features cache is empty. Cannot build future feature row.")
  }
  
  # Find last completed historical game for this player
  # Filter to same season, week < target_week (strict temporal causality)
  # This ensures we use the most recent available features from the same season
  historical_games <- rb_features[
    rb_features$player_id == player_id &
    rb_features$season == season &
    !is.na(rb_features$week) &
    rb_features$week < week,
    , drop = FALSE
  ]
  
  if (nrow(historical_games) == 0) {
    stop("No games found for player_id '", player_id, 
         "' in season ", season, " before week ", week, 
         ". Cannot build future feature row. ",
         "Player must have played at least one game earlier in the same season.")
  }
  
  # Get the most recent historical game (last completed game in same season)
  # Order by week descending (most recent week before target)
  historical_games <- historical_games[order(
    historical_games$week,
    historical_games$gameday,
    decreasing = TRUE
  ), , drop = FALSE]
  
  last_game <- historical_games[1, ]
  
  # Validate that last game has rolling features (required for simulation)
  rolling_cols <- grep("_roll[0-9]+$", names(last_game), value = TRUE)
  if (length(rolling_cols) == 0) {
    stop("Last historical game for player_id '", player_id, 
         "' has no rolling features. Cannot build future feature row.")
  }
  
  # Check if last game has at least one non-NA rolling feature (3-game history requirement)
  has_rolling_features <- any(!is.na(last_game[, rolling_cols, drop = FALSE]))
  if (!has_rolling_features) {
    stop("Last historical game for player_id '", player_id, 
         "' has all NA rolling features. Player must have at least 3 prior games. ",
         "Cannot build future feature row.")
  }
  
  # Build synthetic feature row using last game as template
  synthetic_row <- last_game
  
  # Update game-specific fields for the future game
  synthetic_row$season <- season
  synthetic_row$week <- week
  synthetic_row$team <- team
  synthetic_row$opponent <- opponent
  synthetic_row$home_away <- home_away
  synthetic_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)
  
  if (!is.null(game_date) && !is.na(game_date)) {
    synthetic_row$game_date <- as.Date(game_date)
    synthetic_row$gameday <- as.Date(game_date)
  } else {
    synthetic_row$game_date <- NA
    synthetic_row$gameday <- NA
  }
  
  # Clear game identifiers (will be set by caller if needed)
  synthetic_row$game_id <- NA_character_
  synthetic_row$game_key <- NA_character_
  
  # IMPORTANT: Preserve all rolling features from last game
  # These represent the player's recent performance going into the future game
  # Do NOT recompute rolling features - use the last game's features as-is
  
  # Clear target columns (these are outcomes, not features)
  target_cols <- grep("^target_", names(synthetic_row), value = TRUE)
  for (col in target_cols) {
    synthetic_row[[col]] <- NA
  }
  
  # Validate required feature columns exist
  required_features <- c(
    "carries_roll3", "carries_roll5", "targets_roll3", "targets_roll5",
    "yards_per_carry_roll5", "yards_per_target_roll5", "catch_rate_roll5",
    "is_home"
  )
  missing_features <- setdiff(required_features, names(synthetic_row))
  if (length(missing_features) > 0) {
    stop("Missing required feature columns in synthetic row: ", 
         paste(missing_features, collapse = ", "))
  }
  
  # Ensure is_home is set correctly
  synthetic_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)
  
  rownames(synthetic_row) <- NULL
  
  synthetic_row
}

