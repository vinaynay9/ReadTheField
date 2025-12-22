# WR v1 Regime System
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

#' Determine WR v1 regime based on week number
determine_wr_regime <- function(week) {
  case_when(
    week <= 3 ~ "early",
    week <= 5 ~ "mid",
    week <= 7 ~ "late",
    TRUE ~ "standard"
  )
}

#' Get canonical WR v1 feature contract by week (time-aware)
get_wr_features_by_week <- function(week) {
  if (is.na(week) || week < 1 || week > 18) {
    stop("Invalid week: ", week, ". Week must be between 1 and 18.")
  }

  prior_season_features <- c(
    "prev_season_targets_total",
    "prev_season_receptions_total",
    "prev_season_rec_yards_total",
    "prev_season_rec_tds_total",
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
    "team_qb_pass_attempts_roll1", "team_qb_pass_yards_roll1", "team_qb_pass_tds_roll1",
    "team_wr_targets_total_roll1", "team_wr_air_yards_roll1",
    "team_wr_target_share_top1_roll1", "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1", "team_rb_targets_total_roll1"
  )
  roll1_features <- c(
    "targets_roll1", "receptions_roll1", "rec_yards_roll1", "air_yards_roll1",
    "target_share_roll1", "air_yards_share_roll1",
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
             roll1_features))
  }

  if (week <= 5) {
    return(c("is_home",
             prior_season_features,
             rookie_features,
             player_static_features,
             team_context_features,
             roll1_features,
             "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
             defense_roll3_features))
  }

  if (week <= 7) {
    return(c("is_home",
             prior_season_features,
             rookie_features,
             player_static_features,
             team_context_features,
             roll1_features,
             "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
             "targets_roll5", "receptions_roll5", "rec_yards_roll5", "air_yards_roll5",
             defense_roll3_features,
             defense_roll5_features))
  }

  return(c("is_home",
           prior_season_features,
           rookie_features,
           player_static_features,
           team_context_features,
           roll1_features,
           "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
           "targets_roll5", "receptions_roll5", "rec_yards_roll5", "air_yards_roll5",
           defense_roll3_features,
           defense_roll5_features))
}

#' Get canonical WR v1 feature contract by regime
get_wr_features_by_regime <- function() {
  roll1_features <- c(
    "targets_roll1", "receptions_roll1", "rec_yards_roll1", "air_yards_roll1",
    "target_share_roll1", "air_yards_share_roll1",
    get_passing_defense_roll1_features()
  )
  defense_roll1_features <- get_passing_defense_roll1_features()
  defense_roll3_features <- get_passing_defense_roll3_features()
  defense_roll5_features <- get_passing_defense_roll5_features()
  prior_season_features <- c(
    "prev_season_targets_total",
    "prev_season_receptions_total",
    "prev_season_rec_yards_total",
    "prev_season_rec_tds_total",
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
    "team_qb_pass_attempts_roll1", "team_qb_pass_yards_roll1", "team_qb_pass_tds_roll1",
    "team_wr_targets_total_roll1", "team_wr_air_yards_roll1",
    "team_wr_target_share_top1_roll1", "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1", "team_rb_targets_total_roll1"
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
      roll1_features
    ),
    mid = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      roll1_features,
      "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
      defense_roll3_features
    ),
    late = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      roll1_features,
      "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
      "targets_roll5", "receptions_roll5", "rec_yards_roll5", "air_yards_roll5",
      defense_roll3_features,
      defense_roll5_features
    ),
    standard = c(
      "is_home",
      prior_season_features,
      rookie_features,
      player_static_features,
      team_context_features,
      roll1_features,
      "targets_roll3", "receptions_roll3", "rec_yards_roll3", "air_yards_roll3",
      "targets_roll5", "receptions_roll5", "rec_yards_roll5", "air_yards_roll5",
      defense_roll3_features,
      defense_roll5_features
    ),
    counterfactual_prior = counterfactual_features
  )
}

#' Validate that a data row has all required features for its regime
validate_wr_regime_features <- function(data_row, regime) {
  if (is.null(data_row) || nrow(data_row) == 0) {
    return(FALSE)
  }

  feature_contract <- get_wr_features_by_regime()
  required_features <- feature_contract[[regime]]

  if (is.null(required_features)) {
    stop("Unknown regime: ", regime, ". Valid regimes: ",
         paste(names(feature_contract), collapse = ", "))
  }

  all(sapply(required_features, function(feat) {
    feat %in% names(data_row) && !is.na(data_row[[feat]])
  }))
}

#' Get model key for a (target, regime) combination
get_wr_model_key <- function(target, regime) {
  paste0(target, "__", regime)
}

#' Get all valid WR v1 regimes
get_wr_regimes <- function() {
  names(get_wr_features_by_regime())
}
