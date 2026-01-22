# Build QB Feature Row for Simulation
#
# Handles availability policy and constructs a single-row feature frame.

build_qb_feature_row_for_simulation <- function(qb_weekly_features,
                                                qb_weekly_stats,
                                                player_dim,
                                                prior_season_player_stats,
                                                team_offense_context,
                                                defense_weekly_features,
                                                schedule,
                                                gsis_id,
                                                season,
                                                week,
                                                availability_policy = "played_only",
                                                drop_feature_groups = character(0),
                                                schedule_game_id = NULL,
                                                schedule_game_date = NULL,
                                                schedule_home_away = NULL,
                                                schedule_opponent = NULL) {
  if (is.null(qb_weekly_features) || nrow(qb_weekly_features) == 0) {
    stop("qb_weekly_features cache is empty. Cannot build counterfactual row.", call. = FALSE)
  }
  if (missing(gsis_id) || is.null(gsis_id) || !nzchar(as.character(gsis_id))) {
    stop("gsis_id is required.")
  }
  player_id <- as.character(gsis_id)
  season <- as.integer(season)
  week <- as.integer(week)

  policy <- validate_availability_policy(availability_policy)

  player_game_row <- qb_weekly_features[
    qb_weekly_features$player_id == player_id &
      qb_weekly_features$season == season &
      qb_weekly_features$week == week,
    , drop = FALSE
  ]
  if (nrow(player_game_row) == 0) {
    player_game_row <- NULL
  }

  availability_state <- "missing_row"
  if (!is.null(player_game_row)) {
    touches <- c(
      player_game_row$target_pass_attempts_qb,
      player_game_row$target_pass_yards_qb,
      player_game_row$target_qb_rush_attempts
    )
    touches <- suppressWarnings(as.numeric(touches))
    if (any(!is.na(touches) & touches > 0)) {
      availability_state <- "observed_played"
    } else {
      availability_state <- "observed_inactive_or_zero"
    }
  }

  if (policy == "played_only") {
    if (is.null(player_game_row) || availability_state != "observed_played") {
      reason <- if (is.null(player_game_row)) {
        "QB weekly features missing for player-week (bye/inactive/no stats row)."
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
    if (!exists("build_future_qb_feature_row")) {
      if (file.exists("R/simulation/build_future_qb_feature_row.R")) {
        source("R/simulation/build_future_qb_feature_row.R", local = TRUE)
      } else {
        stop("Missing R/simulation/build_future_qb_feature_row.R")
      }
    }
    has_season_history <- any(qb_weekly_features$player_id == player_id &
                                qb_weekly_features$season == season &
                                qb_weekly_features$week < week)
    sched_match <- schedule[
      schedule$season == season & schedule$week == week,
      , drop = FALSE
    ]
    if (nrow(sched_match) == 0) {
      if (policy == "force_counterfactual") {
        construction_warnings <- c(construction_warnings, "Schedule row missing; using prior-only counterfactual row.")
      } else {
        stop("No schedule row found for season ", season, " week ", week, ". Cannot build counterfactual row.", call. = FALSE)
      }
    }
    # Use player's team from history if available
    team_val <- if (!is.null(player_game_row) && "team" %in% names(player_game_row)) {
      as.character(player_game_row$team[1])
    } else {
      history <- qb_weekly_stats[qb_weekly_stats$player_id == player_id, , drop = FALSE]
      if (nrow(history) > 0) as.character(tail(history$team, 1)) else NA_character_
    }
    if ((is.na(team_val) || team_val == "") && !is.null(player_dim) && nrow(player_dim) > 0) {
      last_dim <- player_dim[player_dim$player_id == player_id, , drop = FALSE]
      if (nrow(last_dim) > 0) {
        last_dim <- last_dim[order(last_dim$season, decreasing = TRUE), , drop = FALSE]
        team_val <- as.character(last_dim$team[1])
        used_sources <- c(used_sources, "player_dim_last_active")
      }
    }
    if (is.na(team_val)) {
      stop("Unable to resolve team for QB counterfactual row.", call. = FALSE)
    }
    if (policy == "force_counterfactual" && !has_season_history) {
      history_any <- qb_weekly_features[qb_weekly_features$player_id == player_id, , drop = FALSE]
      if (nrow(history_any) > 0) {
        history_any <- history_any[order(history_any$season, history_any$week, decreasing = TRUE), , drop = FALSE]
        feature_row <- history_any[1, , drop = FALSE]
      } else {
        template <- qb_weekly_features[0, , drop = FALSE]
        feature_row <- template[1, , drop = FALSE]
        feature_row[1, ] <- NA
      }
      feature_row$player_id <- player_id
      feature_row$season <- season
      feature_row$week <- week
      feature_row$team <- team_val
      if (nrow(sched_match) > 0) {
        sched_row <- sched_match[1, ]
        if (sched_row$home_team == team_val) {
          opponent <- sched_row$away_team
          home_away <- "HOME"
        } else {
          opponent <- sched_row$home_team
          home_away <- "AWAY"
        }
        if ("opponent" %in% names(feature_row)) feature_row$opponent <- opponent
        if ("home_away" %in% names(feature_row)) feature_row$home_away <- home_away
        if ("is_home" %in% names(feature_row)) {
          feature_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)
        }
      }
    } else if (nrow(sched_match) > 0) {
      sched_row <- sched_match[1, ]
      if (sched_row$home_team == team_val) {
        opponent <- sched_row$away_team
        home_away <- "HOME"
      } else {
        opponent <- sched_row$home_team
        home_away <- "AWAY"
      }
      feature_row <- build_future_qb_feature_row(
        player_id = player_id,
        season = season,
        week = week,
        team = team_val,
        opponent = opponent,
        home_away = home_away
      )
    } else {
      # Prior-only counterfactual when schedule is unavailable.
      template <- qb_weekly_features[0, , drop = FALSE]
      feature_row <- template[1, , drop = FALSE]
      feature_row[1, ] <- NA
      feature_row$player_id <- player_id
      feature_row$season <- season
      feature_row$week <- week
      feature_row$team <- team_val
    }
    availability_state <- if (policy == "force_counterfactual") "counterfactual_forced" else availability_state
    dropped_feature_groups <- c("player_rolling_features", "recent_performance_features")
  }

  # Forced counterfactual: drop QB recency roll features (player-level only).
  if (policy == "force_counterfactual" && availability_state != "observed_played") {
    roll_cols <- grep("_roll(1|3|5)$", names(feature_row), value = TRUE)
    roll_cols <- roll_cols[grepl("^target_", roll_cols)]
    if (length(roll_cols) > 0) {
      feature_row[1, roll_cols] <- NA
      dropped_features <- unique(c(dropped_features, roll_cols))
    }
  }

  # Apply schedule-derived overrides (CLI-provided)
  if (!is.null(schedule_game_id) && !is.na(schedule_game_id) && "game_id" %in% names(feature_row)) {
    feature_row$game_id <- as.character(schedule_game_id)
  }
  if (!is.null(schedule_game_date) && !is.na(schedule_game_date)) {
    if ("gameday" %in% names(feature_row)) feature_row$gameday <- as.Date(schedule_game_date)
    if ("game_date" %in% names(feature_row)) feature_row$game_date <- as.Date(schedule_game_date)
  }
  if (!is.null(schedule_home_away) && !is.na(schedule_home_away) && "home_away" %in% names(feature_row)) {
    feature_row$home_away <- toupper(as.character(schedule_home_away))
    if ("is_home" %in% names(feature_row)) {
      feature_row$is_home <- ifelse(toupper(as.character(schedule_home_away)) == "HOME", 1L, 0L)
    }
  }
  if (!is.null(schedule_opponent) && !is.na(schedule_opponent) && "opponent" %in% names(feature_row)) {
    feature_row$opponent <- as.character(schedule_opponent)
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
