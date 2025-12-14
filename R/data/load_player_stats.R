# Load Player Stats
#
# Loads NFL player-level offensive statistics from nflverse sources.
# This file specifically handles RB data extraction.
#
# Dependencies: nflreadr
#
# Usage:
#   rb_stats <- load_rb_stats(seasons = 2021:2024)

#' Load RB player statistics for specified seasons
#'
#' Uses nflreadr to load player weekly stats from nflverse.
#' Filters to RB position only.
#' Returns a data.frame with one row per player-game.
#' If nflreadr fails, returns an empty data.frame with correct column schema.
#'
#' @param seasons Integer vector of NFL seasons to load (e.g., 2021:2024)
#' @return data.frame with columns:
#'   - player_id: character, unique player identifier (gsis_id)
#'   - player_name: character, player display name
#'   - game_id: character, unique game identifier
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - team: character, player's team for that game
#'   - position: character, player position (always "RB")
#'   - carries: integer, rushing attempts
#'   - rush_yards: double, rushing yards
#'   - rush_tds: integer, rushing touchdowns
#'   - targets: integer, receiving targets
#'   - receptions: integer, receptions
#'   - rec_yards: double, receiving yards
#'   - rec_tds: integer, receiving touchdowns
#' @examples
#' rb_stats <- load_rb_stats(2023)
#' rb_stats <- load_rb_stats(2021:2024)
load_rb_stats <- function(seasons) {
  
  # Validate input
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified, returning empty RB stats")
    return(empty_rb_stats_df())
  }
  
  seasons <- as.integer(seasons)
  if (any(is.na(seasons))) {
    warning("Some seasons could not be converted to integer")
    seasons <- seasons[!is.na(seasons)]
  }
  
  if (length(seasons) == 0) {
    return(empty_rb_stats_df())
  }
  
  # Attempt to load from nflreadr
  tryCatch({
    # Check if nflreadr is available
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      warning("nflreadr package not installed. Returning empty RB stats.")
      return(empty_rb_stats_df())
    }
    
    # Load player stats (weekly)
    raw_stats <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
    
    if (is.null(raw_stats) || nrow(raw_stats) == 0) {
      warning("nflreadr returned no player stats for specified seasons")
      return(empty_rb_stats_df())
    }
    
    # Filter to RB only
    # nflverse uses "position" or "position_group" columns
    # Position filtering: only keep rows where position is "RB"
    if ("position" %in% names(raw_stats)) {
      rb_stats <- raw_stats[raw_stats$position == "RB", ]
    } else if ("position_group" %in% names(raw_stats)) {
      rb_stats <- raw_stats[raw_stats$position_group == "RB", ]
    } else {
      warning("No position column found in player stats")
      return(empty_rb_stats_df())
    }
    
    if (nrow(rb_stats) == 0) {
      warning("No RB data found after position filtering")
      return(empty_rb_stats_df())
    }
    
    # Map nflverse column names to our schema
    # nflverse player_stats columns:
    #   - player_id / gsis_id
    #   - player_name / player_display_name
    #   - recent_team
    #   - season, week
    #   - carries, rushing_yards, rushing_tds
    #   - targets, receptions, receiving_yards, receiving_tds
    
    # Determine correct column names (nflverse naming may vary)
    player_id_col <- if ("player_id" %in% names(rb_stats)) "player_id" else "gsis_id"
    player_name_col <- if ("player_name" %in% names(rb_stats)) "player_name" else "player_display_name"
    team_col <- if ("team" %in% names(rb_stats)) "team" else "recent_team"
    
    # Build game_id if not present
    # nflverse player_stats may not have game_id directly; construct from components
    if ("game_id" %in% names(rb_stats)) {
      game_ids <- as.character(rb_stats$game_id)
    } else {
      # Construct game_id placeholder - will need to join with schedules
      # Format: {season}_{week}_{away_team}_{home_team} is typical nflverse format
      # For now, create a temporary ID that can be matched later
      game_ids <- paste(rb_stats$season, rb_stats$week, rb_stats[[team_col]], sep = "_")
      warning("game_id not in player_stats; created temporary ID. Join with schedules for accurate game_id.")
    }
    
    # Extract statistics with safe column access
    safe_get <- function(df, col, default = NA) {
      if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
    }
    
    result <- data.frame(
      player_id = as.character(rb_stats[[player_id_col]]),
      player_name = as.character(rb_stats[[player_name_col]]),
      game_id = game_ids,
      season = as.integer(rb_stats$season),
      week = as.integer(rb_stats$week),
      team = as.character(rb_stats[[team_col]]),
      position = "RB",
      carries = as.integer(safe_get(rb_stats, "carries", 0)),
      rush_yards = as.double(safe_get(rb_stats, "rushing_yards", 0)),
      rush_tds = as.integer(safe_get(rb_stats, "rushing_tds", 0)),
      targets = as.integer(safe_get(rb_stats, "targets", 0)),
      receptions = as.integer(safe_get(rb_stats, "receptions", 0)),
      rec_yards = as.double(safe_get(rb_stats, "receiving_yards", 0)),
      rec_tds = as.integer(safe_get(rb_stats, "receiving_tds", 0)),
      stringsAsFactors = FALSE
    )
    
    # Replace NA counts with 0 (player played but had no carries/targets is 0, not NA)
    count_cols <- c("carries", "rush_tds", "targets", "receptions", "rec_tds")
    for (col in count_cols) {
      result[[col]] <- ifelse(is.na(result[[col]]), 0L, result[[col]])
    }
    
    # Yards can remain NA if truly missing, but typically should be 0
    result$rush_yards <- ifelse(is.na(result$rush_yards), 0, result$rush_yards)
    result$rec_yards <- ifelse(is.na(result$rec_yards), 0, result$rec_yards)
    
    # Sort by player, season, week
    result <- result[order(result$player_id, result$season, result$week), ]
    rownames(result) <- NULL
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Failed to load RB stats from nflreadr:", e$message))
    return(empty_rb_stats_df())
  })
}


#' Create empty RB stats data.frame with correct schema
#'
#' Used as fallback when data loading fails.
#'
#' @return data.frame with zero rows and correct column types
empty_rb_stats_df <- function() {
  data.frame(
    player_id = character(0),
    player_name = character(0),
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    team = character(0),
    position = character(0),
    carries = integer(0),
    rush_yards = double(0),
    rush_tds = integer(0),
    targets = integer(0),
    receptions = integer(0),
    rec_yards = double(0),
    rec_tds = integer(0),
    stringsAsFactors = FALSE
  )
}

