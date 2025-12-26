# Build K Feature Row for Simulation
#
# Handles availability policy and constructs a single-row feature frame.

build_k_feature_row_for_simulation <- function(k_weekly_features,
                                               k_weekly_stats,
                                               player_dim,
                                               prior_season_player_stats,
                                               team_offense_context,
                                               defense_weekly_features,
                                               schedule,
                                               gsis_id,
                                               season,
                                               week,
                                               availability_policy = "played_only",
                                               drop_feature_groups = character(0)) {
  if (is.null(k_weekly_features) || nrow(k_weekly_features) == 0) {
    stop("k_weekly_features cache is empty. Cannot build counterfactual row.", call. = FALSE)
  }
  if (missing(gsis_id) || is.null(gsis_id) || !nzchar(as.character(gsis_id))) {
    stop("gsis_id is required.")
  }
  player_id <- as.character(gsis_id)
  season <- as.integer(season)
  week <- as.integer(week)

  policy <- validate_availability_policy(availability_policy)

  player_game_row <- k_weekly_features[
    k_weekly_features$player_id == player_id &
      k_weekly_features$season == season &
      k_weekly_features$week == week,
    , drop = FALSE
  ]
  if (nrow(player_game_row) == 0) {
    player_game_row <- NULL
  }

  availability_state <- "missing_row"
  if (!is.null(player_game_row)) {
    attempts <- c(
      player_game_row$target_fg_attempts_k,
      player_game_row$target_pat_attempts_k
    )
    attempts <- suppressWarnings(as.numeric(attempts))
    if (any(!is.na(attempts) & attempts > 0)) {
      availability_state <- "observed_played"
    } else {
      availability_state <- "observed_inactive_or_zero"
    }
  }

  if (policy == "played_only") {
    if (is.null(player_game_row) || availability_state != "observed_played") {
      reason <- if (is.null(player_game_row)) {
        "K weekly features missing for player-week (bye/inactive/no stats row)."
      } else {
        "Player-week exists but exposure is zero/NA (inactive or no snaps)."
      }
      stop(reason, " Use availability_policy=expected_active to allow counterfactual simulation.",
           call. = FALSE)
    }
  }

  dropped_feature_groups <- character(0)
  dropped_features <- character(0)
  construction_warnings <- character(0)
  used_sources <- character(0)

  if (!is.null(player_game_row) && availability_state == "observed_played" && policy != "force_counterfactual") {
    feature_row <- player_game_row
  } else {
    if (!exists("build_future_k_feature_row")) {
      if (file.exists("R/simulation/build_future_k_feature_row.R")) {
        source("R/simulation/build_future_k_feature_row.R", local = TRUE)
      } else {
        stop("Missing R/simulation/build_future_k_feature_row.R")
      }
    }
    sched_match <- schedule[
      schedule$season == season & schedule$week == week,
      , drop = FALSE
    ]
    if (nrow(sched_match) == 0) {
      stop("No schedule row found for season ", season, " week ", week, ". Cannot build counterfactual row.", call. = FALSE)
    }
    team_val <- if (!is.null(player_game_row) && "team" %in% names(player_game_row)) {
      as.character(player_game_row$team[1])
    } else {
      history <- k_weekly_stats[k_weekly_stats$player_id == player_id, , drop = FALSE]
      if (nrow(history) > 0) as.character(tail(history$team, 1)) else NA_character_
    }
    if (is.na(team_val)) {
      stop("Unable to resolve team for K counterfactual row.", call. = FALSE)
    }
    sched_row <- sched_match[1, ]
    if (sched_row$home_team == team_val) {
      opponent <- sched_row$away_team
      home_away <- "HOME"
    } else {
      opponent <- sched_row$home_team
      home_away <- "AWAY"
    }
    feature_row <- build_future_k_feature_row(
      player_id = player_id,
      season = season,
      week = week,
      team = team_val,
      opponent = opponent,
      home_away = home_away
    )
    availability_state <- if (policy == "force_counterfactual") "counterfactual_forced" else availability_state
    dropped_feature_groups <- c("player_rolling_features", "recent_performance_features")
  }

  list(
    feature_row = feature_row,
    availability_state = availability_state,
    availability_policy = policy,
    dropped_feature_groups = dropped_feature_groups,
    dropped_features = dropped_features,
    construction_warnings = construction_warnings,
    schedule_row = NULL,
    used_sources = used_sources
  )
}
