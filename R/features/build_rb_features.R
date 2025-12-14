# Build RB Features
#
# Computes rolling features for RB player-game data.
# All features use strictly lagged windows (no current-game leakage).
#
# Dependencies: R/utils/rolling_helpers.R
#
# Usage:
#   source("R/utils/rolling_helpers.R")
#   rb_data_with_features <- build_rb_features(rb_data)

#' Build rolling features for RB data
#'
#' Computes lagged rolling means and efficiency metrics for RB player-game data.
#' Features are computed per-player, sorted by season/week.
#' Returns NA for early-career rows with insufficient history.
#'
#' @param rb_data data.frame with columns:
#'   - player_id, season, week (for ordering)
#'   - carries, rush_yards, rush_tds
#'   - targets, receptions, rec_yards, rec_tds
#' @return data.frame with original columns plus:
#'   - carries_roll3: mean carries over last 3 games
#'   - carries_roll5: mean carries over last 5 games
#'   - targets_roll3: mean targets over last 3 games
#'   - targets_roll5: mean targets over last 5 games
#'   - rush_yards_roll3: mean rush yards over last 3 games
#'   - rec_yards_roll3: mean rec yards over last 3 games
#'   - yards_per_carry_roll5: ratio-of-sums yards/carries over last 5
#'   - yards_per_target_roll5: ratio-of-sums rec_yards/targets over last 5
#'   - catch_rate_roll5: ratio-of-sums receptions/targets over last 5
#'   - rush_tds_roll5: mean rush TDs over last 5 games
#'   - rec_tds_roll5: mean rec TDs over last 5 games
build_rb_features <- function(rb_data) {
  
  # Validate input
  if (is.null(rb_data) || nrow(rb_data) == 0) {
    warning("Empty rb_data provided, returning empty result")
    return(empty_rb_features_df())
  }
  
  required_cols <- c("player_id", "season", "week", "carries", "rush_yards", 
                     "rush_tds", "targets", "receptions", "rec_yards", "rec_tds")
  missing_cols <- setdiff(required_cols, names(rb_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Ensure data is sorted by player, then time
  rb_data <- rb_data[order(rb_data$player_id, rb_data$season, rb_data$week), ]
  
  # Initialize feature columns
  n <- nrow(rb_data)
  rb_data$carries_roll3 <- NA_real_
  rb_data$carries_roll5 <- NA_real_
  rb_data$carries_roll7 <- NA_real_
  rb_data$carries_roll10 <- NA_real_
  rb_data$targets_roll3 <- NA_real_
  rb_data$targets_roll5 <- NA_real_
  rb_data$targets_roll7 <- NA_real_
  rb_data$targets_roll10 <- NA_real_
  rb_data$rush_yards_roll3 <- NA_real_
  rb_data$rush_yards_roll7 <- NA_real_
  rb_data$rush_yards_roll10 <- NA_real_
  rb_data$rec_yards_roll3 <- NA_real_
  rb_data$rec_yards_roll7 <- NA_real_
  rb_data$rec_yards_roll10 <- NA_real_
  rb_data$yards_per_carry_roll5 <- NA_real_
  rb_data$yards_per_target_roll5 <- NA_real_
  rb_data$catch_rate_roll5 <- NA_real_
  rb_data$rush_tds_roll5 <- NA_real_
  rb_data$rec_tds_roll5 <- NA_real_
  
  # Get unique players

  players <- unique(rb_data$player_id)
  
  # Compute features per player
  for (pid in players) {
    idx <- which(rb_data$player_id == pid)
    
    if (length(idx) < 2) {
      # Not enough games for any rolling features
      next
    }
    
    # Extract player's data (already sorted by time)
    carries <- rb_data$carries[idx]
    rush_yards <- rb_data$rush_yards[idx]
    rush_tds <- rb_data$rush_tds[idx]
    targets <- rb_data$targets[idx]
    receptions <- rb_data$receptions[idx]
    rec_yards <- rb_data$rec_yards[idx]
    rec_tds <- rb_data$rec_tds[idx]
    
    # Compute rolling means (lagged) - multiple windows for trend analysis
    rb_data$carries_roll3[idx] <- lagged_roll_mean(carries, window = 3, min_obs = 1)
    rb_data$carries_roll5[idx] <- lagged_roll_mean(carries, window = 5, min_obs = 1)
    rb_data$carries_roll7[idx] <- lagged_roll_mean(carries, window = 7, min_obs = 1)
    rb_data$carries_roll10[idx] <- lagged_roll_mean(carries, window = 10, min_obs = 1)
    
    rb_data$targets_roll3[idx] <- lagged_roll_mean(targets, window = 3, min_obs = 1)
    rb_data$targets_roll5[idx] <- lagged_roll_mean(targets, window = 5, min_obs = 1)
    rb_data$targets_roll7[idx] <- lagged_roll_mean(targets, window = 7, min_obs = 1)
    rb_data$targets_roll10[idx] <- lagged_roll_mean(targets, window = 10, min_obs = 1)
    
    rb_data$rush_yards_roll3[idx] <- lagged_roll_mean(rush_yards, window = 3, min_obs = 1)
    rb_data$rush_yards_roll7[idx] <- lagged_roll_mean(rush_yards, window = 7, min_obs = 1)
    rb_data$rush_yards_roll10[idx] <- lagged_roll_mean(rush_yards, window = 10, min_obs = 1)
    
    rb_data$rec_yards_roll3[idx] <- lagged_roll_mean(rec_yards, window = 3, min_obs = 1)
    rb_data$rec_yards_roll7[idx] <- lagged_roll_mean(rec_yards, window = 7, min_obs = 1)
    rb_data$rec_yards_roll10[idx] <- lagged_roll_mean(rec_yards, window = 10, min_obs = 1)
    
    rb_data$rush_tds_roll5[idx] <- lagged_roll_mean(rush_tds, window = 5, min_obs = 1)
    rb_data$rec_tds_roll5[idx] <- lagged_roll_mean(rec_tds, window = 5, min_obs = 1)
    
    # Compute ratio-of-sums efficiency metrics (lagged)
    rb_data$yards_per_carry_roll5[idx] <- lagged_ratio_of_sums(
      rush_yards, carries, window = 5, min_obs = 1
    )
    rb_data$yards_per_target_roll5[idx] <- lagged_ratio_of_sums(
      rec_yards, targets, window = 5, min_obs = 1
    )
    rb_data$catch_rate_roll5[idx] <- lagged_ratio_of_sums(
      receptions, targets, window = 5, min_obs = 1
    )
  }
  
  rownames(rb_data) <- NULL
  return(rb_data)
}


#' Create empty RB features data.frame with correct schema
#'
#' @return data.frame with zero rows and correct feature columns
empty_rb_features_df <- function() {
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
    fumbles = integer(0),
    fumbles_lost = integer(0),
    carries_roll3 = double(0),
    carries_roll5 = double(0),
    carries_roll7 = double(0),
    carries_roll10 = double(0),
    targets_roll3 = double(0),
    targets_roll5 = double(0),
    targets_roll7 = double(0),
    targets_roll10 = double(0),
    rush_yards_roll3 = double(0),
    rush_yards_roll7 = double(0),
    rush_yards_roll10 = double(0),
    rec_yards_roll3 = double(0),
    rec_yards_roll7 = double(0),
    rec_yards_roll10 = double(0),
    yards_per_carry_roll5 = double(0),
    yards_per_target_roll5 = double(0),
    catch_rate_roll5 = double(0),
    rush_tds_roll5 = double(0),
    rec_tds_roll5 = double(0),
    stringsAsFactors = FALSE
  )
}

