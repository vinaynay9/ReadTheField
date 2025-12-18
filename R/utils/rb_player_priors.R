# RB Player Priors - Cumulative Career Statistics
#
# Adds player-specific career priors to RB feature data.
# These are lagged cumulative means computed from all prior games (across seasons).
# Used for weeks 1-3 when rolling windows are not yet available.
#
# Design principles:
#   - Strictly causal: current game excluded (lagged computation)
#   - No minimum game requirement (priors, not strict windows)
#   - Cross-season aggregation (full career history)
#   - NA-safe ratio computation (guard against zero denominators)

#' Add RB player priors (lagged cumulative statistics)
#'
#' Computes player-specific cumulative averages from all prior games.
#' These features provide player role/usage signal in early season weeks
#' when rolling windows are not yet available.
#'
#' @param df data.frame with columns:
#'   - player_id (required)
#'   - season, week, gameday (for ordering)
#'   - carries, targets, receptions (volume stats)
#'   - rush_yards, rec_yards OR rushing_yards, receiving_yards (efficiency stats)
#' @return data.frame with added columns:
#'   - carries_cum_mean
#'   - targets_cum_mean
#'   - receptions_cum_mean
#'   - ypc_cum (yards per carry)
#'   - ypt_cum (yards per target)
#'   - catch_rate_cum
add_rb_player_priors <- function(df) {
  
  # Validate required columns
  required_cols <- c("player_id", "season", "week", "gameday", 
                     "carries", "targets", "receptions")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns for player priors: ", paste(missing_cols, collapse = ", "))
  }
  
  # Determine yardage column names (handle both naming conventions)
  if ("rush_yards" %in% names(df)) {
    rush_yards_col <- "rush_yards"
  } else if ("rushing_yards" %in% names(df)) {
    rush_yards_col <- "rushing_yards"
  } else {
    stop("No rushing yards column found. Expected 'rush_yards' or 'rushing_yards'.")
  }
  
  if ("rec_yards" %in% names(df)) {
    rec_yards_col <- "rec_yards"
  } else if ("receiving_yards" %in% names(df)) {
    rec_yards_col <- "receiving_yards"
  } else {
    stop("No receiving yards column found. Expected 'rec_yards' or 'receiving_yards'.")
  }
  
  # Sort by player, then chronologically
  df <- df[order(df$player_id, df$season, df$week, df$gameday), ]
  
  # Initialize prior columns
  df$carries_cum_mean <- NA_real_
  df$targets_cum_mean <- NA_real_
  df$receptions_cum_mean <- NA_real_
  df$ypc_cum <- NA_real_
  df$ypt_cum <- NA_real_
  df$catch_rate_cum <- NA_real_
  
  # Compute lagged cumulative stats per player
  players <- unique(df$player_id)
  
  for (pid in players) {
    player_rows <- which(df$player_id == pid)
    n_games <- length(player_rows)
    
    if (n_games == 0) next
    
    # Initialize accumulators
    cum_carries <- 0
    cum_targets <- 0
    cum_receptions <- 0
    cum_rush_yards <- 0
    cum_rec_yards <- 0
    
    for (i in seq_along(player_rows)) {
      row_idx <- player_rows[i]
      
      # Compute lagged means (using stats up to but NOT including current game)
      if (cum_carries > 0) {
        df$carries_cum_mean[row_idx] <- cum_carries / (i - 1)
      }
      
      if (cum_targets > 0) {
        df$targets_cum_mean[row_idx] <- cum_targets / (i - 1)
      }
      
      if (cum_targets > 0) {
        df$receptions_cum_mean[row_idx] <- cum_receptions / (i - 1)
      }
      
      # Compute lagged efficiency ratios (with denom guardrails)
      if (cum_carries > 0) {
        df$ypc_cum[row_idx] <- cum_rush_yards / cum_carries
      }
      
      if (cum_targets > 0) {
        df$ypt_cum[row_idx] <- cum_rec_yards / cum_targets
        df$catch_rate_cum[row_idx] <- cum_receptions / cum_targets
      }
      
      # Update accumulators for next iteration (LAG: add current game AFTER computing priors)
      curr_carries <- df$carries[row_idx]
      curr_targets <- df$targets[row_idx]
      curr_receptions <- df$receptions[row_idx]
      curr_rush_yards <- df[[rush_yards_col]][row_idx]
      curr_rec_yards <- df[[rec_yards_col]][row_idx]
      
      if (!is.na(curr_carries)) cum_carries <- cum_carries + curr_carries
      if (!is.na(curr_targets)) cum_targets <- cum_targets + curr_targets
      if (!is.na(curr_receptions)) cum_receptions <- cum_receptions + curr_receptions
      if (!is.na(curr_rush_yards)) cum_rush_yards <- cum_rush_yards + curr_rush_yards
      if (!is.na(curr_rec_yards)) cum_rec_yards <- cum_rec_yards + curr_rec_yards
    }
  }
  
  return(df)
}

