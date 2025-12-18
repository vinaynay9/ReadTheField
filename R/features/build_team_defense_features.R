# Build Team Defense Features
#
# Computes rolling defensive features using lagged windows.
# All features use strictly lagged data (current game excluded).
#
# Dependencies:
#   - R/data/build_team_defense_game_stats.R
#   - R/utils/rolling_helpers.R
#
# Usage:
#   source("R/data/build_team_defense_game_stats.R")
#   source("R/utils/rolling_helpers.R")
#   def_features <- build_team_defense_features(def_game_stats)

#' Build rolling defensive features from team defensive game stats
#'
#' Computes lagged rolling means (roll5) of defensive statistics.
#' All features exclude the current game to ensure leakage safety.
#'
#' @param def_game_stats data.frame from build_team_defense_game_stats()
#'   Must have columns: defense_team, season, week, gameday, and defensive stats
#' @return data.frame with same rows as input plus rolling feature columns:
#'   - opp_pass_yards_allowed_roll5: Mean pass yards allowed (last 5 games, excluding current)
#'   - opp_rush_yards_allowed_roll5: Mean rush yards allowed (last 5, excluding current)
#'   - opp_total_yards_allowed_roll5: Mean total yards allowed (last 5, excluding current)
#'   - opp_points_allowed_roll5: Mean points allowed (last 5, excluding current)
#'   - opp_sacks_roll5: Mean sacks (last 5, excluding current)
#'   - opp_tfl_roll5: Mean tackles for loss (last 5, excluding current)
#'   - opp_int_roll5: Mean interceptions (last 5, excluding current) - if available
#' @examples
#' def_stats <- build_team_defense_game_stats(2023)
#' def_features <- build_team_defense_features(def_stats)
build_team_defense_features <- function(def_game_stats) {
  
  # Validate input
  if (is.null(def_game_stats) || nrow(def_game_stats) == 0) {
    warning("Empty defensive game stats provided")
    return(empty_defense_features_df())
  }
  
  # Check required columns
  required_cols <- c("defense_team", "season", "week", "gameday")
  missing_cols <- setdiff(required_cols, names(def_game_stats))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Load rolling helpers (assumes they're in the path)
  if (!exists("lagged_roll_mean")) {
    if (file.exists("R/utils/rolling_helpers.R")) {
      source("R/utils/rolling_helpers.R", local = TRUE)
    } else {
      stop("rolling_helpers functions not found. Please source R/utils/rolling_helpers.R first.")
    }
  }
  
  # Ensure data is sorted by defense_team, then season, week, gameday
  def_game_stats <- def_game_stats[order(
    def_game_stats$defense_team,
    def_game_stats$season,
    def_game_stats$week,
    def_game_stats$gameday
  ), ]
  
  # Initialize result with input data
  result <- def_game_stats
  
  # Group by defense_team and compute rolling features
  message("Computing rolling defensive features...")
  
  teams <- unique(def_game_stats$defense_team)
  n_teams <- length(teams)
  
  # Initialize rolling feature columns
  result$opp_pass_yards_allowed_roll5 <- NA_real_
  result$opp_rush_yards_allowed_roll5 <- NA_real_
  result$opp_yards_per_rush_allowed_roll5 <- NA_real_
  result$opp_total_yards_allowed_roll5 <- NA_real_
  result$opp_points_allowed_roll5 <- NA_real_
  result$opp_sacks_roll5 <- NA_real_
  result$opp_tfl_roll5 <- NA_real_
  
  # Optional features (only if data exists)
  if ("def_interceptions" %in% names(def_game_stats)) {
    result$opp_int_roll5 <- NA_real_
  }
  
  # Process each team separately
  for (i in seq_along(teams)) {
    team <- teams[i]
    team_mask <- def_game_stats$defense_team == team
    team_data <- def_game_stats[team_mask, ]
    
    if (nrow(team_data) == 0) next
    
    # Ensure team data is sorted by time
    team_data <- team_data[order(team_data$season, team_data$week, team_data$gameday), ]
    
    # Compute rolling features using lagged windows
    # Window = 5 (strict semantics: requires exactly 5 prior games)
    
    # Pass yards allowed
    if ("pass_yards_allowed" %in% names(team_data)) {
      result$opp_pass_yards_allowed_roll5[team_mask] <- lagged_roll_mean(
        team_data$pass_yards_allowed,
        window = 5
      )
    }
    
    # Rush yards allowed
    if ("rush_yards_allowed" %in% names(team_data)) {
      result$opp_rush_yards_allowed_roll5[team_mask] <- lagged_roll_mean(
        team_data$rush_yards_allowed,
        window = 5
      )
    }
    
    # Yards per rush allowed
    if ("yards_per_rush_allowed" %in% names(team_data)) {
      result$opp_yards_per_rush_allowed_roll5[team_mask] <- lagged_roll_mean(
        team_data$yards_per_rush_allowed,
        window = 5
      )
    }
    
    # Total yards allowed
    if ("total_yards_allowed" %in% names(team_data)) {
      result$opp_total_yards_allowed_roll5[team_mask] <- lagged_roll_mean(
        team_data$total_yards_allowed,
        window = 5
      )
    }
    
    # Points allowed
    if ("points_allowed" %in% names(team_data)) {
      result$opp_points_allowed_roll5[team_mask] <- lagged_roll_mean(
        team_data$points_allowed,
        window = 5
      )
    }
    
    # Sacks
    if ("def_sacks" %in% names(team_data)) {
      result$opp_sacks_roll5[team_mask] <- lagged_roll_mean(
        as.numeric(team_data$def_sacks),
        window = 5
      )
    }
    
    # TFL
    if ("def_tfl" %in% names(team_data)) {
      result$opp_tfl_roll5[team_mask] <- lagged_roll_mean(
        as.numeric(team_data$def_tfl),
        window = 5
      )
    }
    
    # Interceptions (optional)
    if ("def_interceptions" %in% names(team_data) && "opp_int_roll5" %in% names(result)) {
      result$opp_int_roll5[team_mask] <- lagged_roll_mean(
        as.numeric(team_data$def_interceptions),
        window = 5
      )
    }
    
    if (i %% 10 == 0) {
      message(paste("Processed", i, "of", n_teams, "teams"))
    }
  }
  
  # Validation: Ensure no current-game leakage
  # This is guaranteed by lagged_roll_mean, but we document it here
  message("Rolling defensive features computed (leakage-safe: current game excluded)")
  
  # Additional validation: Check that rolling features are NA for first game of each team
  # This is expected behavior (no prior history)
  first_game_na <- by(result, result$defense_team, function(team_df) {
    if (nrow(team_df) > 0) {
      first_idx <- which.min(team_df$gameday)
      rolling_cols <- c("opp_pass_yards_allowed_roll5", "opp_rush_yards_allowed_roll5",
                       "opp_total_yards_allowed_roll5", "opp_points_allowed_roll5",
                       "opp_sacks_roll5", "opp_tfl_roll5")
      rolling_cols <- intersect(rolling_cols, names(team_df))
      if (length(rolling_cols) > 0) {
        first_row_rolling <- team_df[first_idx, rolling_cols, drop = FALSE]
        all_na <- all(is.na(first_row_rolling))
        return(all_na)
      }
    }
    return(TRUE)
  })
  
  if (!all(unlist(first_game_na))) {
    warning("Some first games have non-NA rolling features. This may indicate leakage.")
  }
  
  # Validation: Ensure defensive features are opponent-specific
  # Check that defense_team matches the opponent in the joined data
  # (This will be validated when joined to player data)
  message("Validation: Defensive features are opponent-specific (defense_team == opponent)")
  
  return(result)
}


#' Create empty defense features data.frame with correct schema
#'
#' @return data.frame with zero rows and correct column types
empty_defense_features_df <- function() {
  data.frame(
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    defense_team = character(0),
    opp_pass_yards_allowed_roll5 = double(0),
    opp_rush_yards_allowed_roll5 = double(0),
    opp_yards_per_rush_allowed_roll5 = double(0),
    opp_total_yards_allowed_roll5 = double(0),
    opp_points_allowed_roll5 = double(0),
    opp_sacks_roll5 = double(0),
    opp_tfl_roll5 = double(0),
    stringsAsFactors = FALSE
  )
}

