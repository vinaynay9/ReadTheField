# Load Schedules
#
# Loads NFL schedule data from nflverse sources.
# Returns one row per game with game metadata.
#
# Dependencies: nflreadr
#
# Usage:
#   schedules <- load_schedules(seasons = 2021:2024)

#' Load NFL schedules for specified seasons
#'
#' Uses nflreadr to load schedule data from nflverse.
#' Returns a data.frame with one row per game.
#' If nflreadr fails, returns an empty data.frame with correct column schema.
#'
#' @param seasons Integer vector of NFL seasons to load (e.g., 2021:2024)
#' @return data.frame with columns:
#'   - game_id: character, unique game identifier
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - gameday: Date, game date
#'   - home_team: character, home team abbreviation
#'   - away_team: character, away team abbreviation
#'   - home_score: integer, home team final score (NA if game not played)
#'   - away_score: integer, away team final score (NA if game not played)
#'   - stadium: character, stadium name (optional)
#'   - surface: character, playing surface type (optional)
#' @examples
#' schedules <- load_schedules(2023)
#' schedules <- load_schedules(2021:2024)
load_schedules <- function(seasons) {
  
  # Validate input
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified, returning empty schedule")
    return(empty_schedule_df())
  }
  
  seasons <- as.integer(seasons)
  if (any(is.na(seasons))) {
    warning("Some seasons could not be converted to integer")
    seasons <- seasons[!is.na(seasons)]
  }
  
  if (length(seasons) == 0) {
    return(empty_schedule_df())
  }
  
  # Attempt to load from nflreadr
  tryCatch({
    # Check if nflreadr is available
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      warning("nflreadr package not installed. Returning empty schedule.")
      return(empty_schedule_df())
    }
    
    # Load schedules
    raw_schedules <- nflreadr::load_schedules(seasons = seasons)
    
    if (is.null(raw_schedules) || nrow(raw_schedules) == 0) {
      warning("nflreadr returned no schedule data for specified seasons")
      return(empty_schedule_df())
    }
    
    # Select and rename columns to match schema
    schedules <- data.frame(
      game_id = as.character(raw_schedules$game_id),
      season = as.integer(raw_schedules$season),
      week = as.integer(raw_schedules$week),
      gameday = as.Date(raw_schedules$gameday),
      home_team = as.character(raw_schedules$home_team),
      away_team = as.character(raw_schedules$away_team),
      home_score = as.integer(raw_schedules$home_score),
      away_score = as.integer(raw_schedules$away_score),
      stadium = as.character(raw_schedules$stadium),
      surface = as.character(raw_schedules$surface),
      stringsAsFactors = FALSE
    )
    
    # Sort by season, week, gameday
    schedules <- schedules[order(schedules$season, schedules$week, schedules$gameday), ]
    rownames(schedules) <- NULL
    
    return(schedules)
    
  }, error = function(e) {
    warning(paste("Failed to load schedules from nflreadr:", e$message))
    return(empty_schedule_df())
  })
}


#' Create empty schedule data.frame with correct schema
#'
#' Used as fallback when data loading fails.
#'
#' @return data.frame with zero rows and correct column types
empty_schedule_df <- function() {
  data.frame(
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    home_team = character(0),
    away_team = character(0),
    home_score = integer(0),
    away_score = integer(0),
    stadium = character(0),
    surface = character(0),
    stringsAsFactors = FALSE
  )
}

