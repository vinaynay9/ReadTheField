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

#' Build rolling features for RB data (season-bounded)
#'
#' Computes lagged rolling means and efficiency metrics for RB player-game data.
#' Features are computed per (player_id, season) group, with rolling windows
#' resetting at season boundaries. This ensures Week 1 of each season has NA
#' rolling features (no prior games in that season).
#'
#' Rolling windows are strictly lagged: the current game is never included.
#' Week 1 of any season will have NA for all rolling features.
#' Week 2 will use only Week 1 data, Week 3 will use Weeks 1-2, etc.
#'
#' @param rb_data data.frame with columns:
#'   - player_id, season, week (for ordering and grouping)
#'   - carries, rush_yards, rush_tds
#'   - targets, receptions, rec_yards, rec_tds
#' @return data.frame with original columns plus:
#'   - carries_roll3: mean carries over last 3 games (within season)
#'   - carries_roll5: mean carries over last 5 games (within season)
#'   - targets_roll3: mean targets over last 3 games (within season)
#'   - targets_roll5: mean targets over last 5 games (within season)
#'   - rush_yards_roll3: mean rush yards over last 3 games (within season)
#'   - rec_yards_roll3: mean rec yards over last 3 games (within season)
#'   - yards_per_carry_roll5: ratio-of-sums yards/carries over last 5 (within season)
#'   - yards_per_target_roll5: ratio-of-sums rec_yards/targets over last 5 (within season)
#'   - catch_rate_roll5: ratio-of-sums receptions/targets over last 5 (within season)
#'   - rush_tds_roll5: mean rush TDs over last 5 games (within season)
#'   - rec_tds_roll5: mean rec TDs over last 5 games (within season)
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
  
  # Ensure data is sorted by player, season, then week (season-bounded grouping)
  rb_data <- rb_data[order(rb_data$player_id, rb_data$season, rb_data$week), ]
  
  # Initialize feature columns (RB v1: roll3, roll5, roll7 only - no roll10)
  n <- nrow(rb_data)
  rb_data$carries_roll3 <- NA_real_
  rb_data$carries_roll1 <- NA_real_
  rb_data$carries_roll5 <- NA_real_
  rb_data$carries_roll7 <- NA_real_
  rb_data$targets_roll3 <- NA_real_
  rb_data$targets_roll1 <- NA_real_
  rb_data$targets_roll5 <- NA_real_
  rb_data$targets_roll7 <- NA_real_
  rb_data$rush_yards_roll1 <- NA_real_
  rb_data$rush_yards_roll3 <- NA_real_
  rb_data$rec_yards_roll1 <- NA_real_
  rb_data$rec_yards_roll3 <- NA_real_
  rb_data$rush_tds_roll1 <- NA_real_
  rb_data$rec_tds_roll1 <- NA_real_
  rb_data$yards_per_carry_roll5 <- NA_real_
  rb_data$yards_per_target_roll5 <- NA_real_
  rb_data$catch_rate_roll5 <- NA_real_
  rb_data$rush_tds_roll5 <- NA_real_
  rb_data$rec_tds_roll5 <- NA_real_
  
  # Compute features per (player_id, season) group to enforce season boundaries
  # This prevents cross-season leakage in rolling windows
  player_seasons <- unique(rb_data[, c("player_id", "season"), drop = FALSE])
  player_seasons <- player_seasons[order(player_seasons$player_id, player_seasons$season), ]
  
  for (i in seq_len(nrow(player_seasons))) {
    pid <- player_seasons$player_id[i]
    season_val <- player_seasons$season[i]
    
    # Get indices for this (player_id, season) group
    idx <- which(rb_data$player_id == pid & rb_data$season == season_val)
    
    if (length(idx) < 2) {
      # Not enough games in this season for any rolling features
      next
    }
    
    # Extract player-season's data (already sorted by week within season)
    # Verify ordering: should be strictly increasing weeks
    weeks_in_season <- rb_data$week[idx]
    if (any(diff(weeks_in_season) <= 0)) {
      stop("Weeks not strictly increasing for player ", pid, " season ", season_val,
           ". Data must be sorted by week within each (player_id, season) group.")
    }
    
    carries <- rb_data$carries[idx]
    rush_yards <- rb_data$rush_yards[idx]
    rush_tds <- rb_data$rush_tds[idx]
    targets <- rb_data$targets[idx]
    receptions <- rb_data$receptions[idx]
    rec_yards <- rb_data$rec_yards[idx]
    rec_tds <- rb_data$rec_tds[idx]
    
    # Compute rolling means (lagged) - RB v1: roll3, roll5, roll7 only
    # Rolling windows reset at season boundary (only use weeks within this season)
    # Strict window semantics: requires exactly N prior games (no partial windows)
    rb_data$carries_roll1[idx] <- c(NA_real_, head(carries, -1))
    rb_data$carries_roll3[idx] <- lagged_roll_mean(carries, window = 3)
    rb_data$carries_roll5[idx] <- lagged_roll_mean(carries, window = 5)
    rb_data$carries_roll7[idx] <- lagged_roll_mean(carries, window = 7)
    
    rb_data$targets_roll1[idx] <- c(NA_real_, head(targets, -1))
    rb_data$targets_roll3[idx] <- lagged_roll_mean(targets, window = 3)
    rb_data$targets_roll5[idx] <- lagged_roll_mean(targets, window = 5)
    rb_data$targets_roll7[idx] <- lagged_roll_mean(targets, window = 7)
    
    rb_data$rush_yards_roll1[idx] <- c(NA_real_, head(rush_yards, -1))
    rb_data$rush_yards_roll3[idx] <- lagged_roll_mean(rush_yards, window = 3)
    rb_data$rec_yards_roll1[idx] <- c(NA_real_, head(rec_yards, -1))
    rb_data$rec_yards_roll3[idx] <- lagged_roll_mean(rec_yards, window = 3)
    
    rb_data$rush_tds_roll1[idx] <- c(NA_real_, head(rush_tds, -1))
    rb_data$rush_tds_roll5[idx] <- lagged_roll_mean(rush_tds, window = 5)
    rb_data$rec_tds_roll1[idx] <- c(NA_real_, head(rec_tds, -1))
    rb_data$rec_tds_roll5[idx] <- lagged_roll_mean(rec_tds, window = 5)
    
    # Compute ratio-of-sums efficiency metrics (lagged)
    # Strict window semantics: requires exactly N prior games (no partial windows)
    rb_data$yards_per_carry_roll5[idx] <- lagged_ratio_of_sums(
      rush_yards, carries, window = 5
    )
    rb_data$yards_per_target_roll5[idx] <- lagged_ratio_of_sums(
      rec_yards, targets, window = 5
    )
    rb_data$catch_rate_roll5[idx] <- lagged_ratio_of_sums(
      receptions, targets, window = 5
    )
  }
  
  # Enforce Week 1 roll1 features are NA (season-boundary safety)
  rb_data <- rb_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_roll1"),
        ~ dplyr::if_else(week == 1, NA_real_, .)
      )
    )
  
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
    carries_roll1 = double(0),
    carries_roll3 = double(0),
    carries_roll5 = double(0),
    carries_roll7 = double(0),
    targets_roll1 = double(0),
    targets_roll3 = double(0),
    targets_roll5 = double(0),
    targets_roll7 = double(0),
    rush_yards_roll1 = double(0),
    rush_yards_roll3 = double(0),
    rec_yards_roll1 = double(0),
    rec_yards_roll3 = double(0),
    yards_per_carry_roll5 = double(0),
    yards_per_target_roll5 = double(0),
    catch_rate_roll5 = double(0),
    rush_tds_roll1 = double(0),
    rush_tds_roll5 = double(0),
    rec_tds_roll1 = double(0),
    rec_tds_roll5 = double(0),
    stringsAsFactors = FALSE
  )
}

