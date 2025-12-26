# Build QB Rolling Features
#
# Computes leakage-safe rolling QB context features from team-game QB stats.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Build rolling QB context features
#'
#' @param qb_game_stats data.frame from build_qb_game_stats()
#' @return data.frame with rolling QB features
build_qb_features <- function(qb_game_stats) {
  if (is.null(qb_game_stats) || nrow(qb_game_stats) == 0) {
    stop("qb_game_stats must be provided with at least one row")
  }

  required_cols <- c("team", "season", "week",
                     "qb_pass_attempts", "qb_pass_completions",
                     "qb_completion_pct", "qb_interceptions_thrown",
                     "qb_yards_per_attempt", "qb_sacks_taken")
  missing <- setdiff(required_cols, names(qb_game_stats))
  if (length(missing) > 0) {
    stop("QB game stats missing required columns: ", paste(missing, collapse = ", "))
  }

  if (!exists("lagged_roll_mean")) {
    if (file.exists("R/utils/rolling_helpers.R")) {
      source("R/utils/rolling_helpers.R", local = TRUE)
    } else {
      stop("Missing R/utils/rolling_helpers.R for rolling QB features")
    }
  }

  qb_game_stats <- qb_game_stats[order(qb_game_stats$team, qb_game_stats$season, qb_game_stats$week), ]
  result <- qb_game_stats

  # QB-side (offense-caused) rolling context
  result$target_pass_attempts_qb_roll1 <- NA_real_
  result$target_pass_attempts_qb_roll3 <- NA_real_
  result$target_pass_attempts_qb_roll5 <- NA_real_
  result$target_pass_completions_qb_roll1 <- NA_real_
  result$target_pass_completions_qb_roll3 <- NA_real_
  result$target_pass_completions_qb_roll5 <- NA_real_
  result$target_completion_pct_qb_roll1 <- NA_real_
  result$target_completion_pct_qb_roll3 <- NA_real_
  result$target_completion_pct_qb_roll5 <- NA_real_
  result$target_interceptions_qb_thrown_roll1 <- NA_real_
  result$target_interceptions_qb_thrown_roll3 <- NA_real_
  result$target_interceptions_qb_thrown_roll5 <- NA_real_
  result$target_yards_per_pass_qb_roll1 <- NA_real_
  result$target_yards_per_pass_qb_roll3 <- NA_real_
  result$target_yards_per_pass_qb_roll5 <- NA_real_
  result$target_sacks_qb_taken_roll1 <- NA_real_
  result$target_sacks_qb_taken_roll3 <- NA_real_
  result$target_sacks_qb_taken_roll5 <- NA_real_

  teams <- unique(qb_game_stats$team)
  for (team in teams) {
    team_mask <- qb_game_stats$team == team
    team_data <- qb_game_stats[team_mask, ]
    seasons <- unique(team_data$season)
    for (seas in seasons) {
      season_mask <- team_data$season == seas
      season_data <- team_data[season_mask, ]
      if (nrow(season_data) == 0) next
      season_data <- season_data[order(season_data$week), ]
      season_idx <- which(team_mask)[season_mask]

      result$target_pass_attempts_qb_roll1[season_idx] <- c(NA_real_, head(season_data$qb_pass_attempts, -1))
      result$target_pass_attempts_qb_roll3[season_idx] <- lagged_roll_mean(season_data$qb_pass_attempts, window = 3)
      result$target_pass_attempts_qb_roll5[season_idx] <- lagged_roll_mean(season_data$qb_pass_attempts, window = 5)

      result$target_pass_completions_qb_roll1[season_idx] <- c(NA_real_, head(season_data$qb_pass_completions, -1))
      result$target_pass_completions_qb_roll3[season_idx] <- lagged_roll_mean(season_data$qb_pass_completions, window = 3)
      result$target_pass_completions_qb_roll5[season_idx] <- lagged_roll_mean(season_data$qb_pass_completions, window = 5)

      result$target_completion_pct_qb_roll1[season_idx] <- c(NA_real_, head(season_data$qb_completion_pct, -1))
      result$target_completion_pct_qb_roll3[season_idx] <- lagged_roll_mean(season_data$qb_completion_pct, window = 3)
      result$target_completion_pct_qb_roll5[season_idx] <- lagged_roll_mean(season_data$qb_completion_pct, window = 5)

      result$target_interceptions_qb_thrown_roll1[season_idx] <- c(NA_real_, head(season_data$qb_interceptions_thrown, -1))
      result$target_interceptions_qb_thrown_roll3[season_idx] <- lagged_roll_mean(season_data$qb_interceptions_thrown, window = 3)
      result$target_interceptions_qb_thrown_roll5[season_idx] <- lagged_roll_mean(season_data$qb_interceptions_thrown, window = 5)

      result$target_yards_per_pass_qb_roll1[season_idx] <- c(NA_real_, head(season_data$qb_yards_per_attempt, -1))
      result$target_yards_per_pass_qb_roll3[season_idx] <- lagged_roll_mean(season_data$qb_yards_per_attempt, window = 3)
      result$target_yards_per_pass_qb_roll5[season_idx] <- lagged_roll_mean(season_data$qb_yards_per_attempt, window = 5)

      result$target_sacks_qb_taken_roll1[season_idx] <- c(NA_real_, head(season_data$qb_sacks_taken, -1))
      result$target_sacks_qb_taken_roll3[season_idx] <- lagged_roll_mean(season_data$qb_sacks_taken, window = 3)
      result$target_sacks_qb_taken_roll5[season_idx] <- lagged_roll_mean(season_data$qb_sacks_taken, window = 5)
    }
  }

  # Enforce week 1 NA for roll features
  roll_cols <- grep("_roll[0-9]+$", names(result), value = TRUE)
  if (length(roll_cols) > 0) {
    result <- result %>%
      mutate(across(all_of(roll_cols), ~ if_else(as.integer(week) == 1L, NA_real_, .)))
  }

  dupes <- duplicated(result[, c("team", "season", "week")])
  if (any(dupes)) {
    stop("Duplicate rows detected in QB rolling features for (team, season, week)")
  }

  result
}
