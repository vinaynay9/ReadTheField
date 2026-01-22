# Build WR Feature Row for Simulation (availability-aware)
#
# Constructs a single-row feature frame for simulation, respecting availability policy.

get_wr_feature_groups <- function() {
  if (!exists("get_passing_defense_all_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/passing_defense_features.R")
    }
  }
  list(
    player_rolling_features = c(
      "targets_roll1", "targets_roll3", "targets_roll5",
      "receptions_roll1", "receptions_roll3", "receptions_roll5",
      "rec_yards_roll1", "rec_yards_roll3", "rec_yards_roll5",
      "air_yards_roll1", "air_yards_roll3", "air_yards_roll5",
      "target_share_roll1", "air_yards_share_roll1"
    ),
    recent_performance_features = character(0),
    prior_season_features = c(
      "prev_season_targets_total", "prev_season_receptions_total",
      "prev_season_rec_yards_total", "prev_season_rec_tds_total",
      "prev_season_games_played"
    ),
    draft_rookie_features = c(
      "is_rookie", "draft_round", "draft_pick_overall"
    ),
    player_static_features = c(
      "position", "height", "weight", "age"
    ),
    team_context_features = c(
      "target_pass_attempts_qb_roll1", "target_pass_yards_qb_roll1", "target_pass_tds_qb_roll1",
      "team_wr_targets_total_roll1", "team_wr_air_yards_roll1",
      "team_wr_target_share_top1_roll1", "team_wr_target_share_top2_roll1",
      "team_te_targets_total_roll1", "team_rb_targets_total_roll1"
    ),
    defense_context_features = c(
      get_passing_defense_all_features(),
      "defense_data_available", "rolling_window_complete"
    ),
    home_away_features = c(
      "is_home"
    )
  )
}

has_meaningful_wr_exposure <- function(row) {
  if (is.null(row) || nrow(row) == 0) return(FALSE)
  cols <- intersect(c("targets", "receptions", "target_targets", "target_receptions", "snap_share", "snap_pct"),
                    names(row))
  if (length(cols) == 0) return(FALSE)
  vals <- unlist(row[1, cols, drop = TRUE], use.names = FALSE)
  vals <- suppressWarnings(as.numeric(vals))
  if (all(is.na(vals))) return(FALSE)
  any(vals > 0, na.rm = TRUE)
}

resolve_schedule_row_wr <- function(schedule, season, week, team) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(NULL)
  }
  sched_match <- schedule[
    schedule$season == season &
      schedule$week == week &
      (schedule$home_team == team | schedule$away_team == team),
    , drop = FALSE
  ]
  if (nrow(sched_match) == 1) {
    return(sched_match[1, ])
  }
  if (nrow(sched_match) > 1) {
    stop("Multiple schedule entries found for team ", team, " in season ", season,
         " week ", week, ". Cannot disambiguate opponent.", call. = FALSE)
  }
  NULL
}

build_wr_feature_row_for_simulation <- function(wr_weekly_features,
                                                wr_weekly_stats,
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
  policy <- validate_availability_policy(availability_policy)
  feature_groups <- get_wr_feature_groups()
  drop_feature_groups <- unique(drop_feature_groups)
  dropped_features <- character(0)
  construction_warnings <- character(0)
  used_sources <- character(0)

  player_id <- as.character(gsis_id)
  season <- as.integer(season)
  week <- as.integer(week)

  player_game_row <- wr_weekly_features[
    wr_weekly_features$player_id == player_id &
      wr_weekly_features$season == season &
      wr_weekly_features$week == week,
    , drop = FALSE
  ]
  if (nrow(player_game_row) > 1) {
    player_game_row <- player_game_row[order(player_game_row$gameday, player_game_row$game_id), , drop = FALSE]
    player_game_row <- player_game_row[1, , drop = FALSE]
  }
  if (nrow(player_game_row) > 0) {
    used_sources <- c(used_sources, "wr_weekly_features")
  }

  exposure_row <- wr_weekly_stats[
    wr_weekly_stats$player_id == player_id &
      wr_weekly_stats$season == season &
      wr_weekly_stats$week == week,
    , drop = FALSE
  ]
  if (nrow(exposure_row) > 0) {
    exposure_row <- exposure_row[order(exposure_row$game_date, exposure_row$game_id), , drop = FALSE]
    exposure_row <- exposure_row[1, , drop = FALSE]
    used_sources <- c(used_sources, "wr_weekly_stats")
  } else if (nrow(player_game_row) > 0) {
    exposure_row <- player_game_row
  }

  availability_state <- if (nrow(player_game_row) == 0) {
    "missing_row"
  } else if (has_meaningful_wr_exposure(exposure_row)) {
    "observed_played"
  } else {
    "observed_inactive_or_zero"
  }

  counterfactual_needed <- (policy == "force_counterfactual") ||
    (policy == "expected_active" && availability_state != "observed_played")
  base_drop_groups <- if (counterfactual_needed) {
    c("player_rolling_features", "recent_performance_features")
  } else {
    character(0)
  }
  drop_feature_groups <- unique(c(base_drop_groups, drop_feature_groups))

  if (policy == "played_only" &&
      (availability_state != "observed_played")) {
    reason <- if (availability_state == "missing_row") {
      "WR weekly features missing for player-week (bye/inactive/no stats row)."
    } else {
      "Player-week exists but exposure is zero/NA (inactive or no snaps)."
    }
    stop(reason, " Use availability_policy=expected_active to allow counterfactual simulation.",
         call. = FALSE)
  }

  if (policy == "expected_active" && availability_state == "observed_played") {
    return(list(
      feature_row = player_game_row,
      availability_state = availability_state,
      availability_policy = policy,
      dropped_feature_groups = character(0),
      dropped_features = character(0),
      construction_warnings = character(0),
      schedule_row = NULL,
      used_sources = used_sources
    ))
  }

  if (policy == "played_only" && availability_state == "observed_played") {
    return(list(
      feature_row = player_game_row,
      availability_state = availability_state,
      availability_policy = policy,
      dropped_feature_groups = character(0),
      dropped_features = character(0),
      construction_warnings = character(0),
      schedule_row = NULL,
      used_sources = used_sources
    ))
  }

  if (nrow(wr_weekly_features) == 0) {
    stop("wr_weekly_features cache is empty. Cannot build counterfactual row.", call. = FALSE)
  }

  template <- wr_weekly_features[0, , drop = FALSE]
  feature_row <- template[1, , drop = FALSE]
  feature_row[1, ] <- NA

  team <- NA_character_
  player_name <- NA_character_
  position <- "WR"
  height <- NA_real_
  weight <- NA_real_
  age <- NA_real_
  if (!is.null(player_dim) && nrow(player_dim) > 0) {
    dim_row <- player_dim[player_dim$player_id == player_id & player_dim$season == season, , drop = FALSE]
    if (nrow(dim_row) > 0) {
      team <- dim_row$team[1]
      position <- if (!is.na(dim_row$position[1])) dim_row$position[1] else position
      if ("full_name" %in% names(dim_row)) player_name <- dim_row$full_name[1]
      if ("height" %in% names(dim_row)) height <- dim_row$height[1]
      if ("weight" %in% names(dim_row)) weight <- dim_row$weight[1]
      if ("age" %in% names(dim_row)) age <- dim_row$age[1]
      used_sources <- c(used_sources, "player_dim")
    }
  }
  if (is.na(team) || team == "") {
    if (!is.null(player_dim) && nrow(player_dim) > 0) {
      last_dim <- player_dim[player_dim$player_id == player_id, , drop = FALSE]
      if (nrow(last_dim) > 0) {
        last_dim <- last_dim[order(last_dim$season, decreasing = TRUE), , drop = FALSE]
        team <- last_dim$team[1]
        position <- if (!is.na(last_dim$position[1])) last_dim$position[1] else position
        if ("full_name" %in% names(last_dim)) player_name <- last_dim$full_name[1]
        if ("height" %in% names(last_dim)) height <- last_dim$height[1]
        if ("weight" %in% names(last_dim)) weight <- last_dim$weight[1]
        if ("age" %in% names(last_dim)) age <- last_dim$age[1]
        used_sources <- c(used_sources, "player_dim_last_active")
      }
    }
  }
  if (is.na(team) || team == "") {
    history <- wr_weekly_stats[
      wr_weekly_stats$player_id == player_id &
        wr_weekly_stats$season == season &
        wr_weekly_stats$week <= week,
      , drop = FALSE
    ]
    if (nrow(history) > 0) {
      history <- history[order(history$week, history$game_date), , drop = FALSE]
      team <- tail(history$team, 1)
    }
  }
  if (is.na(team) || team == "") {
    stop("Unable to resolve team for gsis_id ", player_id, " season ", season,
         ". Counterfactual simulation requires team context.", call. = FALSE)
  }

  schedule_row <- resolve_schedule_row_wr(schedule, season, week, team)
  if (is.null(schedule_row)) {
    if (policy == "force_counterfactual") {
      construction_warnings <- c(construction_warnings, "Schedule row missing; using prior-only counterfactual row.")
      opponent <- NA_character_
      home_away <- NA_character_
      is_home <- NA_integer_
      gameday <- as.Date(NA)
      game_id <- NA_character_
    } else {
      stop("No schedule entry found for team ", team, " in season ", season,
           " week ", week, ". Cannot build counterfactual row.", call. = FALSE)
    }
  } else {
    used_sources <- c(used_sources, "schedule")
    opponent <- if (schedule_row$home_team == team) schedule_row$away_team else schedule_row$home_team
    home_away <- if (schedule_row$home_team == team) "HOME" else "AWAY"
    is_home <- ifelse(home_away == "HOME", 1L, 0L)
    gameday <- as.Date(schedule_row$gameday)
    game_id <- as.character(schedule_row$game_id)
  }

  feature_row$player_id <- player_id
  feature_row$season <- season
  feature_row$week <- week
  feature_row$team <- team
  feature_row$opponent <- opponent
  feature_row$home_away <- home_away
  if ("is_home" %in% names(feature_row)) feature_row$is_home <- as.integer(is_home)
  if ("game_id" %in% names(feature_row)) feature_row$game_id <- game_id
  if ("gameday" %in% names(feature_row)) feature_row$gameday <- gameday
  if ("game_date" %in% names(feature_row)) feature_row$game_date <- gameday
  if ("player_name" %in% names(feature_row)) feature_row$player_name <- player_name
  if ("position" %in% names(feature_row)) feature_row$position <- position
  if ("height" %in% names(feature_row)) feature_row$height <- height
  if ("weight" %in% names(feature_row)) feature_row$weight <- weight
  if ("age" %in% names(feature_row)) feature_row$age <- age
  if ("game_key" %in% names(feature_row) && exists("build_game_key")) {
    feature_row$game_key <- build_game_key(season, week, gameday, team, opponent, game_id)
  }

  if (!is.null(defense_weekly_features) && nrow(defense_weekly_features) > 0) {
    def_match <- defense_weekly_features[
      defense_weekly_features$season == season &
        defense_weekly_features$week == week &
        defense_weekly_features$defense_team == opponent,
      , drop = FALSE
    ]
    if (nrow(def_match) > 0) {
      def_cols <- intersect(names(def_match), names(feature_row))
      def_cols <- setdiff(def_cols, c("season", "week", "defense_team"))
      for (col in def_cols) {
        feature_row[[col]] <- def_match[[col]][1]
      }
      used_sources <- c(used_sources, "defense_weekly_features")
    } else {
      construction_warnings <- c(construction_warnings, "Missing defense weekly features for opponent-week.")
    }
  }

  if (!is.null(team_offense_context) && nrow(team_offense_context) > 0) {
    toc_match <- team_offense_context[
      team_offense_context$team == team &
        team_offense_context$season == season &
        team_offense_context$week == week,
      , drop = FALSE
    ]
    if (nrow(toc_match) > 0) {
      toc_cols <- intersect(names(toc_match), names(feature_row))
      toc_cols <- setdiff(toc_cols, c("team", "season", "week"))
      for (col in toc_cols) {
        feature_row[[col]] <- toc_match[[col]][1]
      }
      used_sources <- c(used_sources, "team_offense_context")
    } else {
      construction_warnings <- c(construction_warnings, "Missing team offense context for team-week.")
    }
  }

  if (!is.null(prior_season_player_stats) && nrow(prior_season_player_stats) > 0) {
    prior_match <- prior_season_player_stats[
      prior_season_player_stats$player_id == player_id &
        prior_season_player_stats$season == season,
      , drop = FALSE
    ]
    if (nrow(prior_match) > 0) {
      prior_cols <- intersect(names(prior_match), names(feature_row))
      prior_cols <- setdiff(prior_cols, c("player_id", "season"))
      for (col in prior_cols) {
        feature_row[[col]] <- prior_match[[col]][1]
      }
      used_sources <- c(used_sources, "prior_season_player_stats")
    }
  }

  draft_round <- NA_integer_
  draft_pick <- NA_integer_
  if (exists("read_player_directory_cache")) {
    dir <- tryCatch(read_player_directory_cache(), error = function(e) NULL)
    if (!is.null(dir) && nrow(dir) > 0) {
      dir_row <- dir[dir$player_id == player_id, , drop = FALSE]
      if (nrow(dir_row) > 0) {
        if ("draft_round" %in% names(dir_row)) draft_round <- dir_row$draft_round[1]
        if ("draft_pick_overall" %in% names(dir_row)) draft_pick <- dir_row$draft_pick_overall[1]
        used_sources <- c(used_sources, "player_directory")
      }
    }
  }
  if ("draft_round" %in% names(feature_row)) feature_row$draft_round <- as.integer(draft_round)
  if ("draft_pick_overall" %in% names(feature_row)) feature_row$draft_pick_overall <- as.integer(draft_pick)

  first_season <- NA_integer_
  if (!is.null(wr_weekly_stats) && nrow(wr_weekly_stats) > 0) {
    seasons_played <- wr_weekly_stats$season[wr_weekly_stats$player_id == player_id]
    if (length(seasons_played) > 0) {
      first_season <- suppressWarnings(min(as.integer(seasons_played), na.rm = TRUE))
      used_sources <- c(used_sources, "wr_weekly_stats")
    }
  }
  is_rookie <- if (is.na(first_season)) TRUE else (season == first_season)
  if ("is_rookie" %in% names(feature_row)) feature_row$is_rookie <- as.logical(is_rookie)

  if (isTRUE(is_rookie)) {
    prior_cols <- intersect(feature_groups$prior_season_features, names(feature_row))
    if (length(prior_cols) > 0) {
      feature_row[1, prior_cols] <- NA
    }
  }

  if (length(drop_feature_groups) > 0) {
    drop_cols <- unlist(feature_groups[drop_feature_groups], use.names = FALSE)
    drop_cols <- intersect(drop_cols, names(feature_row))
    if (length(drop_cols) > 0) {
      feature_row[1, drop_cols] <- NA
      dropped_features <- unique(c(dropped_features, drop_cols))
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
    dropped_feature_groups = drop_feature_groups,
    dropped_features = dropped_features,
    construction_warnings = unique(construction_warnings),
    schedule_row = schedule_row,
    used_sources = unique(used_sources)
  )
}
