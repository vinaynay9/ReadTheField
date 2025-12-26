# QB v1 Regime System
#
# Defines week-based regimes and fixed feature contracts per regime.

library(dplyr)

if (!exists("get_passing_defense_roll1_features")) {
  if (file.exists("R/positions/passing_defense_features.R")) {
    source("R/positions/passing_defense_features.R", local = TRUE)
  } else {
    stop("Missing R/positions/passing_defense_features.R")
  }
}

#' Determine QB v1 regime based on week number
determine_qb_regime <- function(week) {
  case_when(
    week <= 3 ~ "early",
    week <= 5 ~ "mid",
    week <= 7 ~ "late",
    TRUE ~ "standard"
  )
}

#' Get canonical QB v1 feature contract by week (time-aware)
get_qb_features_by_week <- function(week) {
  if (is.na(week) || week < 1 || week > 18) {
    stop("Invalid week: ", week, ". Week must be between 1 and 18.")
  }

  prior_season_features <- c(
    "prev_season_pass_attempts_total",
    "prev_season_pass_yards_total",
    "prev_season_pass_tds_total",
    "prev_season_interceptions_thrown_total",
    "prev_season_sacks_taken_total",
    "prev_season_rush_attempts_total",
    "prev_season_rush_yards_total",
    "prev_season_games_played"
  )
  rookie_features <- c(
    "is_rookie",
    "draft_round",
    "draft_pick_overall"
  )
  player_static_features <- c(
    "position",
    "height",
    "weight",
    "age"
  )
  team_context_features <- c(
    "team_wr_target_share_top1_roll1",
    "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1",
    "team_rb_targets_total_roll1"
  )
  qb_roll1_features <- c(
    "target_pass_attempts_qb_roll1",
    "target_completions_qb_roll1",
    "target_completion_pct_qb_roll1",
    "target_pass_yards_qb_roll1",
    "target_pass_tds_qb_roll1",
    "target_interceptions_qb_thrown_roll1",
    "target_sacks_qb_taken_roll1",
    "target_qb_rush_attempts_roll1",
    "target_qb_rush_yards_roll1",
    "target_air_yards_qb_roll1",
    get_passing_defense_roll1_features()
  )
  defense_roll3_features <- get_passing_defense_roll3_features()
  defense_roll5_features <- get_passing_defense_roll5_features()

  if (week <= 3) {
    return(c("is_home",
             prior_season_features,
             rookie_features,
             player_static_features,
             team_context_features,
             qb_roll1_features))
  }

  if (week <= 5) {
    return(c("is_home",
             prior_season_features,
             rookie_features,
             player_static_features,
             team_context_features,
             qb_roll1_features,
             "target_pass_attempts_qb_roll3",
             "target_completions_qb_roll3",
             "target_completion_pct_qb_roll3",
             "target_pass_yards_qb_roll3",
             "target_pass_tds_qb_roll3",
             "target_interceptions_qb_thrown_roll3",
             "target_sacks_qb_taken_roll3",
             "target_qb_rush_attempts_roll3",
             "target_qb_rush_yards_roll3",
             "target_air_yards_qb_roll3",
             defense_roll3_features))
  }

  if (week <= 7) {
    return(c("is_home",
             prior_season_features,
             rookie_features,
             player_static_features,
             team_context_features,
             qb_roll1_features,
             "target_pass_attempts_qb_roll3",
             "target_completions_qb_roll3",
             "target_completion_pct_qb_roll3",
             "target_pass_yards_qb_roll3",
             "target_pass_tds_qb_roll3",
             "target_interceptions_qb_thrown_roll3",
             "target_sacks_qb_taken_roll3",
             "target_qb_rush_attempts_roll3",
             "target_qb_rush_yards_roll3",
             "target_air_yards_qb_roll3",
             "target_pass_attempts_qb_roll5",
             "target_completions_qb_roll5",
             "target_completion_pct_qb_roll5",
             "target_pass_yards_qb_roll5",
             "target_pass_tds_qb_roll5",
             "target_interceptions_qb_thrown_roll5",
             "target_sacks_qb_taken_roll5",
             "target_qb_rush_attempts_roll5",
             "target_qb_rush_yards_roll5",
             "target_air_yards_qb_roll5",
             defense_roll3_features,
             defense_roll5_features))
  }

  return(c("is_home",
           prior_season_features,
           rookie_features,
           player_static_features,
           team_context_features,
           qb_roll1_features,
           "target_pass_attempts_qb_roll3",
           "target_completions_qb_roll3",
           "target_completion_pct_qb_roll3",
           "target_pass_yards_qb_roll3",
           "target_pass_tds_qb_roll3",
           "target_interceptions_qb_thrown_roll3",
           "target_sacks_qb_taken_roll3",
           "target_qb_rush_attempts_roll3",
           "target_qb_rush_yards_roll3",
           "target_air_yards_qb_roll3",
           "target_pass_attempts_qb_roll5",
           "target_completions_qb_roll5",
           "target_completion_pct_qb_roll5",
           "target_pass_yards_qb_roll5",
           "target_pass_tds_qb_roll5",
           "target_interceptions_qb_thrown_roll5",
           "target_sacks_qb_taken_roll5",
           "target_qb_rush_attempts_roll5",
           "target_qb_rush_yards_roll5",
           "target_air_yards_qb_roll5",
           defense_roll3_features,
           defense_roll5_features))
}

#' Get canonical QB v1 feature contract by regime
get_qb_features_by_regime <- function() {
  qb_roll1_features <- c(
    "target_pass_attempts_qb_roll1",
    "target_completions_qb_roll1",
    "target_completion_pct_qb_roll1",
    "target_pass_yards_qb_roll1",
    "target_pass_tds_qb_roll1",
    "target_interceptions_qb_thrown_roll1",
    "target_sacks_qb_taken_roll1",
    "target_qb_rush_attempts_roll1",
    "target_qb_rush_yards_roll1",
    "target_air_yards_qb_roll1",
    get_passing_defense_roll1_features()
  )
  defense_roll1_features <- get_passing_defense_roll1_features()
  defense_roll3_features <- get_passing_defense_roll3_features()
  defense_roll5_features <- get_passing_defense_roll5_features()
  prior_season_features <- c(
    "prev_season_pass_attempts_total",
    "prev_season_pass_yards_total",
    "prev_season_pass_tds_total",
    "prev_season_interceptions_thrown_total",
    "prev_season_sacks_taken_total",
    "prev_season_rush_attempts_total",
    "prev_season_rush_yards_total",
    "prev_season_games_played"
  )
  rookie_features <- c(
    "is_rookie",
    "draft_round",
    "draft_pick_overall"
  )
  player_static_features <- c(
    "position",
    "height",
    "weight",
    "age"
  )
  team_context_features <- c(
    "team_wr_target_share_top1_roll1",
    "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1",
    "team_rb_targets_total_roll1"
  )
  counterfactual_features <- c(
    "is_home",
    player_static_features,
    prior_season_features,
    rookie_features,
    team_context_features,
    defense_roll1_features
  )

  list(
    early = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      qb_roll1_features
    ),
    mid = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      qb_roll1_features,
      "target_pass_attempts_qb_roll3",
      "target_completions_qb_roll3",
      "target_completion_pct_qb_roll3",
      "target_pass_yards_qb_roll3",
      "target_pass_tds_qb_roll3",
      "target_interceptions_qb_thrown_roll3",
      "target_sacks_qb_taken_roll3",
      "target_qb_rush_attempts_roll3",
      "target_qb_rush_yards_roll3",
      "target_air_yards_qb_roll3",
      defense_roll3_features
    ),
    late = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      qb_roll1_features,
      "target_pass_attempts_qb_roll3",
      "target_completions_qb_roll3",
      "target_completion_pct_qb_roll3",
      "target_pass_yards_qb_roll3",
      "target_pass_tds_qb_roll3",
      "target_interceptions_qb_thrown_roll3",
      "target_sacks_qb_taken_roll3",
      "target_qb_rush_attempts_roll3",
      "target_qb_rush_yards_roll3",
      "target_air_yards_qb_roll3",
      "target_pass_attempts_qb_roll5",
      "target_completions_qb_roll5",
      "target_completion_pct_qb_roll5",
      "target_pass_yards_qb_roll5",
      "target_pass_tds_qb_roll5",
      "target_interceptions_qb_thrown_roll5",
      "target_sacks_qb_taken_roll5",
      "target_qb_rush_attempts_roll5",
      "target_qb_rush_yards_roll5",
      "target_air_yards_qb_roll5",
      defense_roll3_features,
      defense_roll5_features
    ),
    standard = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      qb_roll1_features,
      "target_pass_attempts_qb_roll3",
      "target_completions_qb_roll3",
      "target_completion_pct_qb_roll3",
      "target_pass_yards_qb_roll3",
      "target_pass_tds_qb_roll3",
      "target_interceptions_qb_thrown_roll3",
      "target_sacks_qb_taken_roll3",
      "target_qb_rush_attempts_roll3",
      "target_qb_rush_yards_roll3",
      "target_air_yards_qb_roll3",
      "target_pass_attempts_qb_roll5",
      "target_completions_qb_roll5",
      "target_completion_pct_qb_roll5",
      "target_pass_yards_qb_roll5",
      "target_pass_tds_qb_roll5",
      "target_interceptions_qb_thrown_roll5",
      "target_sacks_qb_taken_roll5",
      "target_qb_rush_attempts_roll5",
      "target_qb_rush_yards_roll5",
      "target_air_yards_qb_roll5",
      defense_roll3_features,
      defense_roll5_features
    ),
    counterfactual_prior = counterfactual_features
  )
}

#' Validate that a data row has all required features for its regime
validate_qb_regime_features <- function(data_row, regime) {
  if (is.null(data_row) || nrow(data_row) == 0) {
    return(FALSE)
  }

  feature_contract <- get_qb_features_by_regime()
  required_features <- feature_contract[[regime]]

  if (is.null(required_features)) {
    stop("Unknown regime: ", regime, ". Valid regimes: ",
         paste(names(feature_contract), collapse = ", "))
  }

  all(sapply(required_features, function(feat) {
    feat %in% names(data_row) && !is.na(data_row[[feat]])
  }))
}

get_qb_model_key <- function(target, regime) {
  paste0(target, "__", regime)
}

get_qb_regimes <- function() {
  names(get_qb_features_by_regime())
}
