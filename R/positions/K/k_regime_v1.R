# K v1 Regime System
#
# Defines week-based regimes and fixed feature contracts per regime.

library(dplyr)

#' Determine K v1 regime based on week number
determine_k_regime <- function(week) {
  case_when(
    week <= 3 ~ "early",
    week <= 5 ~ "mid",
    week <= 7 ~ "late",
    TRUE ~ "standard"
  )
}

#' Get canonical K v1 feature contract by week (time-aware)
get_k_features_by_week <- function(week) {
  if (is.na(week) || week < 1 || week > 18) {
    stop("Invalid week: ", week, ". Week must be between 1 and 18.")
  }

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
  k_roll1_features <- c(
    "target_fg_attempts_k_roll1",
    "target_fg_made_k_roll1",
    "target_fg_pct_k_roll1",
    "target_pat_attempts_k_roll1",
    "target_pat_made_k_roll1",
    "target_pat_pct_k_roll1",
    "target_fg_long_k_roll1",
    "def_sacks_defense_forced_roll1",
    "def_points_defense_allowed_roll1"
  )

  if (week <= 3) {
    return(c("is_home",
             rookie_features,
             player_static_features,
             k_roll1_features))
  }

  if (week <= 5) {
    return(c("is_home",
             rookie_features,
             player_static_features,
             k_roll1_features,
             "target_fg_attempts_k_roll3",
             "target_fg_made_k_roll3",
             "target_fg_pct_k_roll3",
             "target_pat_attempts_k_roll3",
             "target_pat_made_k_roll3",
             "target_pat_pct_k_roll3",
             "target_fg_long_k_roll3",
             "def_sacks_defense_forced_roll3",
             "def_points_defense_allowed_roll3"))
  }

  if (week <= 7) {
    return(c("is_home",
             rookie_features,
             player_static_features,
             k_roll1_features,
             "target_fg_attempts_k_roll3",
             "target_fg_made_k_roll3",
             "target_fg_pct_k_roll3",
             "target_pat_attempts_k_roll3",
             "target_pat_made_k_roll3",
             "target_pat_pct_k_roll3",
             "target_fg_long_k_roll3",
             "target_fg_attempts_k_roll5",
             "target_fg_made_k_roll5",
             "target_fg_pct_k_roll5",
             "target_pat_attempts_k_roll5",
             "target_pat_made_k_roll5",
             "target_pat_pct_k_roll5",
             "target_fg_long_k_roll5",
             "def_sacks_defense_forced_roll3",
             "def_points_defense_allowed_roll3",
             "def_sacks_defense_forced_roll5",
             "def_points_defense_allowed_roll5"))
  }

  return(c("is_home",
           rookie_features,
           player_static_features,
           k_roll1_features,
           "target_fg_attempts_k_roll3",
           "target_fg_made_k_roll3",
           "target_fg_pct_k_roll3",
           "target_pat_attempts_k_roll3",
           "target_pat_made_k_roll3",
           "target_pat_pct_k_roll3",
           "target_fg_long_k_roll3",
           "target_fg_attempts_k_roll5",
           "target_fg_made_k_roll5",
           "target_fg_pct_k_roll5",
           "target_pat_attempts_k_roll5",
           "target_pat_made_k_roll5",
           "target_pat_pct_k_roll5",
           "target_fg_long_k_roll5",
           "def_sacks_defense_forced_roll3",
           "def_points_defense_allowed_roll3",
           "def_sacks_defense_forced_roll5",
           "def_points_defense_allowed_roll5"))
}

get_k_features_by_regime <- function() {
  k_roll1_features <- c(
    "target_fg_attempts_k_roll1",
    "target_fg_made_k_roll1",
    "target_fg_pct_k_roll1",
    "target_pat_attempts_k_roll1",
    "target_pat_made_k_roll1",
    "target_pat_pct_k_roll1",
    "target_fg_long_k_roll1",
    "def_sacks_defense_forced_roll1",
    "def_points_defense_allowed_roll1"
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
  counterfactual_features <- c(
    "is_home",
    player_static_features,
    rookie_features,
    "def_sacks_defense_forced_roll1",
    "def_points_defense_allowed_roll1"
  )
  list(
    early = c("is_home", rookie_features, player_static_features, k_roll1_features),
    mid = c("is_home", rookie_features, player_static_features, k_roll1_features,
            "target_fg_attempts_k_roll3", "target_fg_made_k_roll3", "target_fg_pct_k_roll3",
            "target_pat_attempts_k_roll3", "target_pat_made_k_roll3", "target_pat_pct_k_roll3",
            "target_fg_long_k_roll3", "def_sacks_defense_forced_roll3", "def_points_defense_allowed_roll3"),
    late = c("is_home", rookie_features, player_static_features, k_roll1_features,
             "target_fg_attempts_k_roll3", "target_fg_made_k_roll3", "target_fg_pct_k_roll3",
             "target_pat_attempts_k_roll3", "target_pat_made_k_roll3", "target_pat_pct_k_roll3",
             "target_fg_long_k_roll3", "target_fg_attempts_k_roll5", "target_fg_made_k_roll5",
             "target_fg_pct_k_roll5", "target_pat_attempts_k_roll5", "target_pat_made_k_roll5",
             "target_pat_pct_k_roll5", "target_fg_long_k_roll5",
             "def_sacks_defense_forced_roll3", "def_points_defense_allowed_roll3",
             "def_sacks_defense_forced_roll5", "def_points_defense_allowed_roll5"),
    standard = c("is_home", rookie_features, player_static_features, k_roll1_features,
                "target_fg_attempts_k_roll3", "target_fg_made_k_roll3", "target_fg_pct_k_roll3",
                "target_pat_attempts_k_roll3", "target_pat_made_k_roll3", "target_pat_pct_k_roll3",
                "target_fg_long_k_roll3", "target_fg_attempts_k_roll5", "target_fg_made_k_roll5",
                "target_fg_pct_k_roll5", "target_pat_attempts_k_roll5", "target_pat_made_k_roll5",
                "target_pat_pct_k_roll5", "target_fg_long_k_roll5",
                "def_sacks_defense_forced_roll3", "def_points_defense_allowed_roll3",
                "def_sacks_defense_forced_roll5", "def_points_defense_allowed_roll5"),
    counterfactual_prior = counterfactual_features
  )
}

validate_k_regime_features <- function(data_row, regime) {
  if (is.null(data_row) || nrow(data_row) == 0) {
    return(FALSE)
  }
  feature_contract <- get_k_features_by_regime()
  required_features <- feature_contract[[regime]]
  if (is.null(required_features)) {
    stop("Unknown regime: ", regime, ". Valid regimes: ", paste(names(feature_contract), collapse = ", "))
  }
  all(sapply(required_features, function(feat) {
    feat %in% names(data_row) && !is.na(data_row[[feat]])
  }))
}

get_k_model_key <- function(target, regime) {
  paste0(target, "__", regime)
}

get_k_regimes <- function() {
  names(get_k_features_by_regime())
}
