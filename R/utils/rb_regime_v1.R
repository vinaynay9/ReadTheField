# RB v1 Regime System
#
# Defines week-based regimes and fixed feature contracts per regime.
# This ensures models train only on rows where all required features exist,
# preventing NA-driven training collapse.

library(dplyr)

#' Determine RB v1 regime based on week number
#'
#' @param week Integer week number (1-18)
#' @return Character regime: "early", "mid", "late", or "standard"
determine_rb_regime <- function(week) {
  case_when(
    week <= 3 ~ "early",
    week <= 5 ~ "mid",
    week <= 7 ~ "late",
    TRUE ~ "standard"
  )
}

#' Get canonical RB v1 feature contract by week (time-aware)
#'
#' Returns the authoritative list of available features based on week-of-game.
#' This ensures causal consistency: only features that would have existed at that time are used.
#' This is the single source of truth for time-aware feature availability.
#'
#' @param week Integer week number (1-18)
#' @return Character vector of feature names available at that week
get_rb_features_by_week <- function(week) {
  if (is.na(week) || week < 1 || week > 18) {
    stop("Invalid week: ", week, ". Week must be between 1 and 18.")
  }
  
  # Time-aware feature availability based on week-of-game
  # Weeks 1-3: Player priors + decayed prev-season priors (insufficient history for rolling windows)
  # Decayed priors blend previous-season stats with current-season-to-date for early-season signal
  prior_season_features <- c(
    "prev_season_carries_total",
    "prev_season_targets_total",
    "prev_season_rush_yards_total",
    "prev_season_rec_yards_total",
    "prev_season_games_played"
  )
  rookie_features <- c(
    "is_rookie",
    "draft_round",
    "draft_pick_overall"
  )
  team_context_features <- c(
    "target_pass_attempts_qb_roll1", "target_pass_yards_qb_roll1", "target_pass_tds_qb_roll1",
    "team_rb_carries_total_roll1", "team_rb_targets_total_roll1",
    "team_rb_carry_share_top1_roll1", "team_rb_carry_share_top2_roll1",
    "team_wr_targets_total_roll1", "team_wr_receiving_yards_roll1", "team_wr_air_yards_roll1"
  )
  roll1_features <- c(
    "carries_roll1", "targets_roll1",
    "rush_yards_roll1", "rec_yards_roll1",
    "rush_tds_roll1", "rec_tds_roll1",
    "def_rush_yards_defense_allowed_roll1", "def_yards_per_rush_defense_allowed_roll1",
    "def_points_defense_allowed_roll1", "def_sacks_defense_forced_roll1", "def_tackles_for_loss_defense_forced_roll1"
  )
  if (week <= 3) {
    return(c("is_home", "carries_cum_mean", "targets_cum_mean", 
             "ypc_cum", "ypt_cum", "catch_rate_cum",
             "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
             prior_season_features,
             rookie_features,
             team_context_features,
             roll1_features))
  }
  
  # Weeks 4-5: Player priors + decayed priors + roll3 features (need 3 prior games for roll3)
  if (week <= 5) {
    return(c("is_home", "carries_cum_mean", "targets_cum_mean", 
             "ypc_cum", "ypt_cum", "catch_rate_cum",
             "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
             prior_season_features,
             rookie_features,
             team_context_features,
             roll1_features,
             "carries_roll3", "targets_roll3"))
  }
  
  # Weeks 6-7: Player priors + decayed priors + roll3 + roll5 features (need 5 prior games for roll5)
  # Defensive roll5 features also available (opponent's prior 5 games)
  if (week <= 7) {
    return(c("is_home", "carries_cum_mean", "targets_cum_mean", 
             "ypc_cum", "ypt_cum", "catch_rate_cum",
             "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
             prior_season_features,
             rookie_features,
             team_context_features,
             roll1_features,
             "carries_roll3", "targets_roll3", "carries_roll5", "targets_roll5",
             "def_rush_yards_defense_allowed_roll5", "def_yards_per_rush_defense_allowed_roll5",
             "def_points_defense_allowed_roll5", "def_sacks_defense_forced_roll5", "def_tackles_for_loss_defense_forced_roll5"))
  }
  
  # Weeks 8+: Player priors + decayed priors + full rolling feature set (roll3, roll5, roll7)
  # Defensive roll5 features continue to be available
  return(c("is_home", "carries_cum_mean", "targets_cum_mean", 
           "ypc_cum", "ypt_cum", "catch_rate_cum",
            "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
            prior_season_features,
            rookie_features,
            team_context_features,
            roll1_features,
            "carries_roll3", "targets_roll3", "carries_roll5", "targets_roll5", 
            "carries_roll7", "targets_roll7",
            "def_rush_yards_defense_allowed_roll5", "def_yards_per_rush_defense_allowed_roll5",
            "def_points_defense_allowed_roll5", "def_sacks_defense_forced_roll5", "def_tackles_for_loss_defense_forced_roll5"))
}

#' Get canonical RB v1 feature contract by regime (for coefficient selection)
#'
#' Returns the authoritative list of features that a regime model CAN use.
#' Note: Actual features used depend on week-of-game via get_rb_features_by_week().
#' Regimes control which coefficients to use, not which features are available.
#'
#' @return Named list with regime names as keys and feature vectors as values
get_rb_features_by_regime <- function() {
  roll1_features <- c(
    "carries_roll1", "targets_roll1",
    "rush_yards_roll1", "rec_yards_roll1",
    "rush_tds_roll1", "rec_tds_roll1",
    "def_rush_yards_defense_allowed_roll1", "def_yards_per_rush_defense_allowed_roll1",
    "def_points_defense_allowed_roll1", "def_sacks_defense_forced_roll1", "def_tackles_for_loss_defense_forced_roll1"
  )
  defense_roll1_features <- c(
    "def_rush_yards_defense_allowed_roll1",
    "def_yards_per_rush_defense_allowed_roll1",
    "def_points_defense_allowed_roll1",
    "def_sacks_defense_forced_roll1",
    "def_tackles_for_loss_defense_forced_roll1"
  )
  prior_season_features <- c(
    "prev_season_carries_total",
    "prev_season_targets_total",
    "prev_season_rush_yards_total",
    "prev_season_rec_yards_total",
    "prev_season_games_played"
  )
  rookie_features <- c(
    "is_rookie",
    "draft_round",
    "draft_pick_overall"
  )
  team_context_features <- c(
    "target_pass_attempts_qb_roll1", "target_pass_yards_qb_roll1", "target_pass_tds_qb_roll1",
    "team_rb_carries_total_roll1", "team_rb_targets_total_roll1",
    "team_rb_carry_share_top1_roll1", "team_rb_carry_share_top2_roll1",
    "team_wr_targets_total_roll1", "team_wr_receiving_yards_roll1", "team_wr_air_yards_roll1"
  )
  counterfactual_features <- c(
    "is_home",
    "position",
    prior_season_features,
    rookie_features,
    team_context_features,
    defense_roll1_features
  )
  list(
    early = c(
      "is_home",
      "carries_cum_mean",
      "targets_cum_mean",
      "ypc_cum",
      "ypt_cum",
      "catch_rate_cum",
      "carries_prior",
      "targets_prior",
      "ypc_prior",
      "ypt_prior",
      prior_season_features,
      rookie_features,
      team_context_features,
      roll1_features
    ),
    mid = c(
      "is_home",
      "carries_cum_mean",
      "targets_cum_mean",
      "ypc_cum",
      "ypt_cum",
      "catch_rate_cum",
      "carries_prior",
      "targets_prior",
      "ypc_prior",
      "ypt_prior",
      prior_season_features,
      rookie_features,
      team_context_features,
      roll1_features,
      "carries_roll3",
      "targets_roll3"
    ),
    late = c(
      "is_home",
      "carries_cum_mean",
      "targets_cum_mean",
      "ypc_cum",
      "ypt_cum",
      "catch_rate_cum",
      "carries_prior",
      "targets_prior",
      "ypc_prior",
      "ypt_prior",
      prior_season_features,
      rookie_features,
      team_context_features,
      roll1_features,
      "carries_roll3",
      "targets_roll3",
      "carries_roll5",
      "targets_roll5",
      "def_rush_yards_defense_allowed_roll5",
      "def_yards_per_rush_defense_allowed_roll5",
      "def_points_defense_allowed_roll5",
      "def_sacks_defense_forced_roll5",
      "def_tackles_for_loss_defense_forced_roll5"
    ),
    standard = c(
      "is_home",
      "carries_cum_mean",
      "targets_cum_mean",
      "ypc_cum",
      "ypt_cum",
      "catch_rate_cum",
      "carries_prior",
      "targets_prior",
      "ypc_prior",
      "ypt_prior",
      prior_season_features,
      rookie_features,
      team_context_features,
      roll1_features,
      "carries_roll3",
      "targets_roll3",
      "carries_roll5",
      "targets_roll5",
      "carries_roll7",
      "targets_roll7",
      "def_rush_yards_defense_allowed_roll5",
      "def_yards_per_rush_defense_allowed_roll5",
      "def_points_defense_allowed_roll5",
      "def_sacks_defense_forced_roll5",
      "def_tackles_for_loss_defense_forced_roll5"
    ),
    # Availability-only regime: used when exposure-dependent features are unavailable
    counterfactual_prior = counterfactual_features
  )
}

#' Validate that a data row has all required features for its regime
#'
#' @param data_row data.frame with one row
#' @param regime Character regime name
#' @return Logical, TRUE if all required features are present and non-NA
validate_regime_features <- function(data_row, regime) {
  if (is.null(data_row) || nrow(data_row) == 0) {
    return(FALSE)
  }
  
  feature_contract <- get_rb_features_by_regime()
  required_features <- feature_contract[[regime]]
  
  if (is.null(required_features)) {
    stop("Unknown regime: ", regime, ". Valid regimes: ", 
         paste(names(feature_contract), collapse = ", "))
  }
  
  # Check all required features exist and are non-NA
  all(sapply(required_features, function(feat) {
    feat %in% names(data_row) && !is.na(data_row[[feat]])
  }))
}

#' Get model key for a (target, regime) combination
#'
#' @param target Character target name (e.g., "target_carries")
#' @param regime Character regime name (e.g., "early")
#' @return Character model key (e.g., "target_carries__early")
get_model_key <- function(target, regime) {
  paste0(target, "__", regime)
}

#' Parse model key to extract target and regime
#'
#' @param model_key Character model key (e.g., "target_carries__early")
#' @return List with target and regime
parse_model_key <- function(model_key) {
  parts <- strsplit(model_key, "__", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    stop("Invalid model key format: ", model_key, ". Expected format: target__regime")
  }
  list(target = parts[1], regime = parts[2])
}

#' Get all valid RB v1 regimes
#'
#' @return Character vector of regime names
get_rb_regimes <- function() {
  names(get_rb_features_by_regime())
}

