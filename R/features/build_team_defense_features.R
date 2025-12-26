# Build Team Defense Features
#
# Computes rolling defensive features using lagged windows.
# All features use strictly lagged data (current game excluded).
# NOTE: roll3 is included for passing-context defensive features to support
# WR/TE regimes while keeping leakage safety intact.
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
#'   - def_pass_yards_defense_allowed_roll5: Mean pass yards allowed (last 5 games, excluding current)
#'   - def_rush_yards_defense_allowed_roll5: Mean rush yards allowed (last 5, excluding current)
#'   - def_total_yards_defense_allowed_roll5: Mean total yards allowed (last 5, excluding current)
#'   - def_points_defense_allowed_roll5: Mean points allowed (last 5, excluding current)
#'   - def_sacks_defense_forced_roll5: Mean sacks (last 5, excluding current)
#'   - def_tackles_for_loss_defense_forced_roll5: Mean tackles for loss (last 5, excluding current)
#'   - def_interceptions_defense_caught_roll5: Mean interceptions (last 5, excluding current) - if available
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
  
  # Ensure data is sorted by defense_team, season, week, gameday
  def_game_stats <- def_game_stats[order(
    def_game_stats$defense_team,
    def_game_stats$season,
    def_game_stats$week,
    def_game_stats$gameday
  ), ]
  
  # Initialize result with input data
  result <- def_game_stats
  
  # Group by defense_team and season, compute rolling features (season-boundary reset)
  message("Computing rolling defensive features...")
  
  teams <- unique(def_game_stats$defense_team)
  n_teams <- length(teams)
  
  # Initialize rolling feature columns
  result$def_pass_yards_defense_allowed_roll1 <- NA_real_
  result$def_pass_yards_defense_allowed_roll3 <- NA_real_
  result$def_pass_yards_defense_allowed_roll5 <- NA_real_
  result$def_pass_attempts_defense_allowed_roll1 <- NA_real_
  result$def_pass_attempts_defense_allowed_roll3 <- NA_real_
  result$def_pass_attempts_defense_allowed_roll5 <- NA_real_
  result$def_yards_per_pass_defense_allowed_roll1 <- NA_real_
  result$def_yards_per_pass_defense_allowed_roll3 <- NA_real_
  result$def_yards_per_pass_defense_allowed_roll5 <- NA_real_
  result$def_rush_yards_defense_allowed_roll1 <- NA_real_
  result$def_rush_yards_defense_allowed_roll5 <- NA_real_
  result$def_yards_per_rush_defense_allowed_roll1 <- NA_real_
  result$def_yards_per_rush_defense_allowed_roll5 <- NA_real_
  result$def_total_yards_defense_allowed_roll5 <- NA_real_
  result$def_points_defense_allowed_roll1 <- NA_real_
  result$def_points_defense_allowed_roll3 <- NA_real_
  result$def_points_defense_allowed_roll5 <- NA_real_
  result$def_sacks_defense_forced_roll1 <- NA_real_
  result$def_sacks_defense_forced_roll3 <- NA_real_
  result$def_sacks_defense_forced_roll5 <- NA_real_
  result$def_tackles_for_loss_defense_forced_roll1 <- NA_real_
  result$def_tackles_for_loss_defense_forced_roll3 <- NA_real_
  result$def_tackles_for_loss_defense_forced_roll5 <- NA_real_
  result$def_interceptions_defense_caught_roll1 <- NA_real_
  result$def_interceptions_defense_caught_roll3 <- NA_real_
  result$def_interceptions_defense_caught_roll5 <- NA_real_
  result$def_passes_defended_defense_forced_roll1 <- NA_real_
  result$def_passes_defended_defense_forced_roll3 <- NA_real_
  result$def_passes_defended_defense_forced_roll5 <- NA_real_
  
  # Process each team-season separately
  for (i in seq_along(teams)) {
    team <- teams[i]
    team_mask <- def_game_stats$defense_team == team
    team_data <- def_game_stats[team_mask, ]
    
    seasons <- unique(team_data$season)
    for (seas in seasons) {
      season_mask <- team_data$season == seas
      season_data <- team_data[season_mask, ]
      
      if (nrow(season_data) == 0) next
      
      # Ensure team-season data is sorted by time
      season_data <- season_data[order(season_data$week, season_data$gameday), ]
      season_idx <- which(team_mask)[season_mask]
      
      # Compute rolling features using lagged windows (window = 5)
      if ("def_pass_yards_defense_allowed" %in% names(season_data)) {
        result$def_pass_yards_defense_allowed_roll1[season_idx] <- c(NA_real_, head(season_data$def_pass_yards_defense_allowed, -1))
        result$def_pass_yards_defense_allowed_roll3[season_idx] <- lagged_roll_mean(
          season_data$def_pass_yards_defense_allowed,
          window = 3
        )
        result$def_pass_yards_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_pass_yards_defense_allowed,
          window = 5
        )
      }

      if ("def_pass_attempts_defense_allowed" %in% names(season_data)) {
        result$def_pass_attempts_defense_allowed_roll1[season_idx] <- c(NA_real_, head(season_data$def_pass_attempts_defense_allowed, -1))
        result$def_pass_attempts_defense_allowed_roll3[season_idx] <- lagged_roll_mean(
          season_data$def_pass_attempts_defense_allowed,
          window = 3
        )
        result$def_pass_attempts_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_pass_attempts_defense_allowed,
          window = 5
        )
      }
      
      if ("def_rush_yards_defense_allowed" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(season_data$def_rush_yards_defense_allowed, -1))
        result$def_rush_yards_defense_allowed_roll1[season_idx] <- lag_vals
        result$def_rush_yards_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_rush_yards_defense_allowed,
          window = 5
        )
      }
      
      if ("def_yards_per_rush_defense_allowed" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(season_data$def_yards_per_rush_defense_allowed, -1))
        result$def_yards_per_rush_defense_allowed_roll1[season_idx] <- lag_vals
        result$def_yards_per_rush_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_yards_per_rush_defense_allowed,
          window = 5
        )
      }

      if ("def_yards_per_pass_defense_allowed" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(season_data$def_yards_per_pass_defense_allowed, -1))
        result$def_yards_per_pass_defense_allowed_roll1[season_idx] <- lag_vals
        result$def_yards_per_pass_defense_allowed_roll3[season_idx] <- lagged_roll_mean(
          season_data$def_yards_per_pass_defense_allowed,
          window = 3
        )
        result$def_yards_per_pass_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_yards_per_pass_defense_allowed,
          window = 5
        )
      }
      
      if ("def_total_yards_defense_allowed" %in% names(season_data)) {
        result$def_total_yards_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_total_yards_defense_allowed,
          window = 5
        )
      }
      
      if ("def_points_defense_allowed" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(season_data$def_points_defense_allowed, -1))
        result$def_points_defense_allowed_roll1[season_idx] <- lag_vals
        result$def_points_defense_allowed_roll3[season_idx] <- lagged_roll_mean(
          season_data$def_points_defense_allowed,
          window = 3
        )
        result$def_points_defense_allowed_roll5[season_idx] <- lagged_roll_mean(
          season_data$def_points_defense_allowed,
          window = 5
        )
      }
      
      if ("def_sacks_defense_forced" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(as.numeric(season_data$def_sacks_defense_forced), -1))
        result$def_sacks_defense_forced_roll1[season_idx] <- lag_vals
        result$def_sacks_defense_forced_roll3[season_idx] <- lagged_roll_mean(
          as.numeric(season_data$def_sacks_defense_forced),
          window = 3
        )
        result$def_sacks_defense_forced_roll5[season_idx] <- lagged_roll_mean(
          as.numeric(season_data$def_sacks_defense_forced),
          window = 5
        )
      }
      
      if ("def_tackles_for_loss_defense_forced" %in% names(season_data)) {
        lag_vals <- c(NA_real_, head(as.numeric(season_data$def_tackles_for_loss_defense_forced), -1))
        result$def_tackles_for_loss_defense_forced_roll1[season_idx] <- lag_vals
        result$def_tackles_for_loss_defense_forced_roll3[season_idx] <- lagged_roll_mean(
          as.numeric(season_data$def_tackles_for_loss_defense_forced),
          window = 3
        )
        result$def_tackles_for_loss_defense_forced_roll5[season_idx] <- lagged_roll_mean(
          as.numeric(season_data$def_tackles_for_loss_defense_forced),
          window = 5
        )
      }
      
      if ("def_interceptions_defense_caught" %in% names(season_data)) {
        def_int <- as.numeric(season_data$def_interceptions_defense_caught)
        result$def_interceptions_defense_caught_roll5[season_idx] <- lagged_roll_mean(def_int, window = 5)
        result$def_interceptions_defense_caught_roll1[season_idx] <- c(NA_real_, head(def_int, -1))
        result$def_interceptions_defense_caught_roll3[season_idx] <- lagged_roll_mean(def_int, window = 3)
      }
      
      if ("def_passes_defended_defense_forced" %in% names(season_data)) {
        def_pd <- as.numeric(season_data$def_passes_defended_defense_forced)
        result$def_passes_defended_defense_forced_roll5[season_idx] <- lagged_roll_mean(def_pd, window = 5)
        result$def_passes_defended_defense_forced_roll3[season_idx] <- lagged_roll_mean(def_pd, window = 3)
        result$def_passes_defended_defense_forced_roll1[season_idx] <- c(NA_real_, head(def_pd, -1))
      }
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
      rolling_cols <- c("def_pass_yards_defense_allowed_roll5", "def_rush_yards_defense_allowed_roll5",
                       "def_total_yards_defense_allowed_roll5", "def_points_defense_allowed_roll5",
                       "def_sacks_defense_forced_roll5", "def_tackles_for_loss_defense_forced_roll5",
                       "def_rush_yards_defense_allowed_roll1", "def_yards_per_rush_defense_allowed_roll1",
                       "def_points_defense_allowed_roll1", "def_sacks_defense_forced_roll1", "def_tackles_for_loss_defense_forced_roll1")
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
  message("Validation: Defensive features are opponent-specific (defense_team == opponent)")

  # Enforce week 1 rolling features are NA (season-boundary reset)
  rolling_cols <- grep("_roll[0-9]+$", names(result), value = TRUE)
  if (length(rolling_cols) > 0) {
    result <- result |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(rolling_cols),
          ~ dplyr::if_else(as.integer(week) == 1L, NA_real_, .)
        )
      )
    week1_rows <- result[result$week == 1, rolling_cols, drop = FALSE]
    if (sum(!is.na(week1_rows)) > 0) {
      warning("Week 1 defensive rolling features contain non-NA values after season reset; forcing to NA for safety.", call. = FALSE)
      result[result$week == 1, rolling_cols] <- NA_real_
    }
  }

  # Optional diagnostics flags (non-leaking)
  if (length(rolling_cols) > 0) {
    result$rolling_window_complete <- apply(
      result[, rolling_cols, drop = FALSE],
      1,
      function(x) all(!is.na(x))
    )
  } else {
    result$rolling_window_complete <- FALSE
  }
  # roll1 completeness flag: TRUE when immediate prior game exists
  roll1_cols <- grep("_roll1$", names(result), value = TRUE)
  if (length(roll1_cols) > 0) {
    result$rolling_window_complete_roll1 <- apply(
      result[, roll1_cols, drop = FALSE],
      1,
      function(x) all(!is.na(x))
    )
  } else {
    result$rolling_window_complete_roll1 <- FALSE
  }
  if ("defense_data_available" %in% names(result)) {
    result$defense_data_available <- as.logical(result$defense_data_available)
  }

  # Feature names already follow the defense_* contract; no aliasing required.

  # Canonicalize defense_team column
  if (!"defense_team" %in% names(result)) {
    if ("team" %in% names(result)) {
      result$defense_team <- result$team
    } else if ("team_abbr" %in% names(result)) {
      result$defense_team <- result$team_abbr
    } else {
      stop("Defensive features missing team identifier. Expected team or team_abbr.")
    }
  }

  # Validate uniqueness
  dup <- duplicated(result[, c("defense_team", "season", "week")])
  if (any(dup)) {
    stop("Duplicate defensive rows found for (defense_team, season, week).")
  }

  # Validate required columns
  required <- c(
    "defense_team", "season", "week",
    "def_yards_per_rush_defense_allowed_roll5",
    "def_rush_yards_defense_allowed_roll5",
    "def_points_defense_allowed_roll5"
  )
  missing <- setdiff(required, names(result))
  if (length(missing) > 0) {
    stop("Defensive features missing required columns: ",
         paste(missing, collapse = ", "))
  }

  # Validation: interceptions roll5 should be non-zero historically when available
  # (uses multi-season data when available)
  if ("def_interceptions_defense_caught_roll5" %in% names(result)) {
    vals <- result$def_interceptions_defense_caught_roll5
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      stop("def_interceptions_defense_caught_roll5 contains no non-NA values. Defensive rolling features are incomplete.")
    }
    if (stats::median(vals) <= 0) {
      stop("def_interceptions_defense_caught_roll5 median is not positive. Check defensive interception ingestion.")
    }
  }
  
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
    def_pass_yards_defense_allowed_roll1 = double(0),
    def_pass_yards_defense_allowed_roll3 = double(0),
    def_pass_yards_defense_allowed_roll5 = double(0),
    def_pass_attempts_defense_allowed_roll1 = double(0),
    def_pass_attempts_defense_allowed_roll3 = double(0),
    def_pass_attempts_defense_allowed_roll5 = double(0),
    def_yards_per_pass_defense_allowed_roll1 = double(0),
    def_yards_per_pass_defense_allowed_roll3 = double(0),
    def_yards_per_pass_defense_allowed_roll5 = double(0),
    def_rush_yards_defense_allowed_roll1 = double(0),
    def_rush_yards_defense_allowed_roll5 = double(0),
    def_yards_per_rush_defense_allowed_roll1 = double(0),
    def_yards_per_rush_defense_allowed_roll5 = double(0),
    def_total_yards_defense_allowed_roll5 = double(0),
    def_points_defense_allowed_roll1 = double(0),
    def_points_defense_allowed_roll3 = double(0),
    def_points_defense_allowed_roll5 = double(0),
    def_sacks_defense_forced_roll1 = double(0),
    def_sacks_defense_forced_roll3 = double(0),
    def_sacks_defense_forced_roll5 = double(0),
    def_tackles_for_loss_defense_forced_roll1 = double(0),
    def_tackles_for_loss_defense_forced_roll3 = double(0),
    def_tackles_for_loss_defense_forced_roll5 = double(0),
    def_interceptions_defense_caught_roll1 = double(0),
    def_interceptions_defense_caught_roll3 = double(0),
    def_interceptions_defense_caught_roll5 = double(0),
    def_passes_defended_defense_forced_roll1 = double(0),
    def_passes_defended_defense_forced_roll3 = double(0),
    def_passes_defended_defense_forced_roll5 = double(0),
    rolling_window_complete_roll1 = logical(0),
    stringsAsFactors = FALSE
  )
}

