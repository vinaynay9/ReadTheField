# Simulate Player Game - High-Level Automated Function
#
# Entry point for player game simulations (gsis_id only).
# Ambiguous name handling must be resolved before calling this function.

#' Simulate a player's game performance (gsis_id only)
#'
#' @param gsis_id Character player gsis_id (required)
#' @param season Integer season
#' @param week Integer week
#' @param n_sims Integer number of simulations
#' @param mode Character simulation mode ("historical_replay" or "upcoming_game"/"hypothetical_matchup")
#' @param seasons Optional training seasons
#' @param max_train_seasons Optional cap on training seasons
#' @param cache_only Logical, must remain TRUE (cached data only)
#' @return List with simulation results
simulate_player_game <- function(gsis_id,
                                 season,
                                 week,
                                 n_sims = 5000,
                                 mode = c("historical_replay", "upcoming_game", "hypothetical_matchup"),
                                 seasons = NULL,
                                 max_train_seasons = NULL,
                                 cache_only = TRUE,
                                 availability_policy = "played_only",
                                 counterfactual_mode = FALSE,
                                 counterfactual_team = NULL,
                                 schedule_game_id = NULL,
                                 schedule_game_date = NULL,
                                 schedule_home_away = NULL,
                                 schedule_opponent = NULL) {
  
  mode <- match.arg(mode)
  
  make_error <- function(type, reason, player_name = NA_character_, allow_counterfactual = FALSE) {
    list(
      status = "error",
      error_type = type,
      player_name = player_name,
      season = season,
      week = week,
      reason = reason,
      counterfactual_allowed = allow_counterfactual
    )
  }
  if (missing(gsis_id) || is.null(gsis_id) || !nzchar(trimws(as.character(gsis_id)))) {
    return(make_error("PLAYER_NOT_FOUND", "gsis_id is required for simulate_player_game"))
  }
  if (missing(season) || is.null(season) || is.na(season)) {
    return(make_error("INSUFFICIENT_HISTORY", "season is required for simulate_player_game"))
  }
  if (missing(week) || is.null(week) || is.na(week)) {
    return(make_error("INSUFFICIENT_HISTORY", "week is required for simulate_player_game"))
  }
  
  season <- as.integer(season)
  week <- as.integer(week)
  
  if (!isTRUE(cache_only)) {
    warning("cache_only was FALSE, forcing to TRUE. Simulation must use cached data only.")
    cache_only <- TRUE
  }
  
  # Ensure bootstrap dependencies
  required_funcs <- c(
    "simulation_mode_policy",
    "detect_in_progress_season",
    "validate_availability_policy",
    "is_counterfactual_policy",
    "read_player_dim_cache",
    "run_rb_simulation",
    "run_wr_simulation",
    "run_te_simulation",
    "run_qb_simulation",
    "run_k_simulation",
    "build_game_key",
    "load_schedules",
    "build_future_rb_feature_row",
    "build_future_wr_feature_row",
    "build_future_te_feature_row",
    "build_future_qb_feature_row",
    "build_future_k_feature_row"
  )
  missing_funcs <- required_funcs[!sapply(required_funcs, exists)]
  if (length(missing_funcs) > 0) {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      paste0("Simulation bootstrap incomplete: missing functions: ", paste(missing_funcs, collapse = ", "))
    ))
  }
  
  available_seasons <- if (exists("get_available_seasons_from_cache")) {
    get_available_seasons_from_cache()
  } else {
    integer(0)
  }
  if (length(available_seasons) == 0 && exists("get_available_seasons_from_cache")) {
    available_seasons <- get_available_seasons_from_cache("schedules")
  }
  if (length(available_seasons) == 0) {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      "No seasons available in cached data. Run scripts/refresh_weekly_cache.R to populate caches."
    ))
  }
  
  availability_policy <- validate_availability_policy(availability_policy)
  counterfactual_mode <- isTRUE(counterfactual_mode)
  counterfactual_team <- if (!is.null(counterfactual_team)) toupper(as.character(counterfactual_team)) else NA_character_
  if (!is.null(schedule_home_away) && !is.na(schedule_home_away)) {
    schedule_home_away <- toupper(as.character(schedule_home_away))
  }
  if (counterfactual_mode) {
    availability_policy <- "force_counterfactual"
  }

  # Determine in-progress season from schedules (cache-only)
  in_progress_season <- NA_integer_
  if (exists("detect_in_progress_season")) {
    in_progress_season <- detect_in_progress_season(available_seasons, cache_only = cache_only)
  }

  # Validate requested week exists in schedule (prevents unscheduled games)
  schedules <- load_schedules(seasons = season, cache_only = cache_only)
  if (nrow(schedules) == 0) {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      paste0("Schedule data is empty for season ", season, ". Cannot validate requested week.")
    ))
  }
  if (!any(schedules$season == season & schedules$week == week)) {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      paste0("No schedule entry found for season ", season, " week ", week,
             ". Simulation is only allowed for scheduled weeks.")
    ))
  }

  player_dim <- read_player_dim_cache()
  if (is.null(player_dim) || nrow(player_dim) == 0) {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      "Player dimension cache is empty. Run scripts/refresh_weekly_cache.R to populate it."
    ))
  }
  dim_row <- player_dim[player_dim$gsis_id == gsis_id & player_dim$season == season, , drop = FALSE]
  forced_counterfactual_notice <- NULL
  if (nrow(dim_row) == 0) {
    has_any_season <- any(player_dim$gsis_id == gsis_id)
    if (isTRUE(has_any_season)) {
      last_dim_row <- player_dim[player_dim$gsis_id == gsis_id, , drop = FALSE]
      last_dim_row <- last_dim_row[order(last_dim_row$season, decreasing = TRUE), , drop = FALSE]
      dim_row <- last_dim_row[1, , drop = FALSE]
      forced_counterfactual_notice <- paste0(
        "Player not active in season ", season,
        ". Running forced counterfactual simulation using historical priors."
      )
      warning(forced_counterfactual_notice, call. = FALSE)
      availability_policy <- "force_counterfactual"
    }
  }
  if (nrow(dim_row) == 0) {
    # Fallback: infer team/position from weekly stats caches when player_dim is incomplete.
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    }
    stats_sources <- list(
      QB = if (exists("read_qb_weekly_stats_cache")) read_qb_weekly_stats_cache else NULL,
      RB = if (exists("read_rb_weekly_stats_cache")) read_rb_weekly_stats_cache else NULL,
      WR = if (exists("read_wr_weekly_stats_cache")) read_wr_weekly_stats_cache else NULL,
      TE = if (exists("read_te_weekly_stats_cache")) read_te_weekly_stats_cache else NULL,
      K  = if (exists("read_k_weekly_stats_cache")) read_k_weekly_stats_cache else NULL
    )
    inferred <- NULL
    for (pos in names(stats_sources)) {
      fn <- stats_sources[[pos]]
      if (is.null(fn)) next
      stats_df <- tryCatch(fn(), error = function(e) NULL)
      if (is.null(stats_df) || nrow(stats_df) == 0) next
      match_row <- stats_df[stats_df$player_id == gsis_id & stats_df$season == season, , drop = FALSE]
      if (nrow(match_row) > 0) {
        inferred <- list(
          team = match_row$team[1],
          position = pos,
          player_name = if ("player_name" %in% names(match_row)) match_row$player_name[1] else NA_character_
        )
        break
      }
    }
    if (!is.null(inferred)) {
      player_team <- inferred$team
      player_position <- inferred$position
      if (is.na(player_team) || player_team == "") {
        return(make_error(
          "INSUFFICIENT_HISTORY",
          paste0("Player team missing for gsis_id ", gsis_id, " in season ", season, "."),
          player_name = inferred$player_name
        ))
      }
      dim_row <- data.frame(
        gsis_id = gsis_id,
        season = season,
        team = player_team,
        position = player_position,
        full_name = inferred$player_name,
        stringsAsFactors = FALSE
      )
    } else {
      has_any_season <- any(player_dim$gsis_id == gsis_id)
      err_type <- if (has_any_season) "PLAYER_RETIRED_OR_NO_RECENT_DATA" else "PLAYER_NOT_FOUND"
      return(make_error(
        err_type,
        paste0("No player_dim row found for gsis_id ", gsis_id, " in season ", season,
               ". Run scripts/refresh_weekly_cache.R to refresh caches."),
        player_name = if (has_any_season) player_dim$full_name[player_dim$gsis_id == gsis_id][1] else NA_character_,
        allow_counterfactual = err_type %in% c("PLAYER_RETIRED_OR_NO_RECENT_DATA")
      ))
    }
  }
  player_team <- dim_row$team[1]
  player_position <- toupper(as.character(dim_row$position[1]))
  if (is.na(player_position) || player_position == "") {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      paste0("Player position missing for gsis_id ", gsis_id, " in season ", season, "."),
      player_name = dim_row$full_name[1]
    ))
  }
  if (is.na(player_team) || player_team == "") {
    return(make_error(
      "INSUFFICIENT_HISTORY",
      paste0("Player team missing for gsis_id ", gsis_id, " in season ", season, "."),
      player_name = dim_row$full_name[1]
    ))
  }
  
  policy <- simulation_mode_policy(
    mode = mode,
    target_season = season,
    target_week = week,
    target_game_date = NULL,
    available_seasons = available_seasons,
    in_progress_season = in_progress_season
  )
  seasons_train <- policy$seasons_allowed
  if (!is.null(max_train_seasons) && length(seasons_train) > max_train_seasons) {
    seasons_train <- tail(seasons_train, max_train_seasons)
  }
  
  is_future <- mode %in% c("upcoming_game", "hypothetical_matchup")
  synthetic_feature_row <- NULL
  resolved_game_date <- NULL
  
  if (counterfactual_mode) {
    if (is.na(counterfactual_team) || !nzchar(counterfactual_team)) {
      return(make_error(
        "INSUFFICIENT_HISTORY",
        "counterfactual_team is required when counterfactual_mode is TRUE.",
        player_name = dim_row$full_name[1],
        allow_counterfactual = FALSE
      ))
    }
    sched_match <- schedules[
      schedules$season == season &
        schedules$week == week &
        (schedules$home_team == counterfactual_team | schedules$away_team == counterfactual_team),
      , drop = FALSE
    ]
    if (nrow(sched_match) == 0) {
      return(make_error(
        "INSUFFICIENT_HISTORY",
        paste0("No schedule entry found for counterfactual team ", counterfactual_team,
               " in season ", season, " week ", week, "."),
        player_name = dim_row$full_name[1],
        allow_counterfactual = FALSE
      ))
    }
    if (nrow(sched_match) > 1) {
      return(make_error(
        "INSUFFICIENT_HISTORY",
        paste0("Multiple schedule entries found for counterfactual team ", counterfactual_team,
               " in season ", season, " week ", week, ". Cannot disambiguate opponent."),
        player_name = dim_row$full_name[1],
        allow_counterfactual = FALSE
      ))
    }
    sched_row <- sched_match[1, ]
    resolved_game_id <- if (!is.null(schedule_game_id) && !is.na(schedule_game_id)) {
      as.character(schedule_game_id)
    } else {
      as.character(sched_row$game_id)
    }
    resolved_game_date <- if (!is.null(schedule_game_date) && !is.na(schedule_game_date)) {
      as.Date(schedule_game_date)
    } else {
      sched_row$gameday
    }
    if (sched_row$home_team == counterfactual_team) {
      resolved_team <- sched_row$home_team
      resolved_opponent <- sched_row$away_team
      resolved_home_away <- "HOME"
    } else {
      resolved_team <- sched_row$away_team
      resolved_opponent <- sched_row$home_team
      resolved_home_away <- "AWAY"
    }
    synthetic_feature_row <- switch(
      player_position,
      RB = build_future_rb_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      WR = build_future_wr_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      TE = build_future_te_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      QB = build_future_qb_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      K = build_future_k_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      return(make_error(
        "INSUFFICIENT_HISTORY",
        paste0("simulate_player_game does not support position ", player_position, " for counterfactual simulation."),
        player_name = dim_row$full_name[1],
        allow_counterfactual = FALSE
      ))
    )
    synthetic_feature_row$player_name <- dim_row$full_name[1]
    resolved_game_date <- resolved_game_date
    is_future <- TRUE
  } else if (is_future) {
    sched_match <- schedules[
      schedules$season == season &
        schedules$week == week &
        (schedules$home_team == player_team | schedules$away_team == player_team),
      , drop = FALSE
    ]
    if (nrow(sched_match) == 0) {
      return(make_error(
        "INSUFFICIENT_HISTORY",
        paste0("No schedule entry found for gsis_id ", gsis_id, " team ", player_team,
               " in season ", season, " week ", week, "."),
        player_name = dim_row$full_name[1]
      ))
    }
    if (nrow(sched_match) > 1) {
      return(make_error(
        "INSUFFICIENT_HISTORY",
        paste0("Multiple schedule entries found for team ", player_team, " in season ", season,
               " week ", week, ". Cannot disambiguate opponent."),
        player_name = dim_row$full_name[1]
      ))
    }
    sched_row <- sched_match[1, ]
    resolved_game_id <- if (!is.null(schedule_game_id) && !is.na(schedule_game_id)) {
      as.character(schedule_game_id)
    } else {
      as.character(sched_row$game_id)
    }
    resolved_game_date <- if (!is.null(schedule_game_date) && !is.na(schedule_game_date)) {
      as.Date(schedule_game_date)
    } else {
      sched_row$gameday
    }
    if (sched_row$home_team == player_team) {
      resolved_team <- sched_row$home_team
      resolved_opponent <- sched_row$away_team
      resolved_home_away <- "HOME"
    } else {
      resolved_team <- sched_row$away_team
      resolved_opponent <- sched_row$home_team
      resolved_home_away <- "AWAY"
    }
    
    synthetic_feature_row <- switch(
      player_position,
      RB = build_future_rb_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      WR = build_future_wr_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      TE = build_future_te_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      QB = build_future_qb_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      K = build_future_k_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = resolved_game_date,
        game_id = resolved_game_id
      ),
      stop("simulate_player_game does not support position ", player_position, " for future simulation.")
    )
    synthetic_feature_row$player_name <- dim_row$full_name[1]
    resolved_game_date <- NULL
  }
  
  classify_error <- function(msg) {
    msg_lower <- tolower(msg)
    if (grepl("bye|inactive|no stats row|exposure is zero|no snaps", msg_lower)) {
      return("PLAYER_INACTIVE")
    }
    if (grepl("no player_dim row found", msg_lower)) {
      return("PLAYER_RETIRED_OR_NO_RECENT_DATA")
    }
    if (grepl("no 3-game rolling|minimum history|insufficient history|training rows", msg_lower)) {
      return("INSUFFICIENT_HISTORY")
    }
    if (grepl("gsis_id is required|player not found|no matching players", msg_lower)) {
      return("PLAYER_NOT_FOUND")
    }
    "INSUFFICIENT_HISTORY"
  }

  result <- tryCatch(
    switch(
      player_position,
      RB = run_rb_simulation(
        gsis_id = gsis_id,
        season = season,
        week = week,
        n_sims = n_sims,
        game_date = resolved_game_date,
        schedule_game_id = schedule_game_id,
        schedule_game_date = schedule_game_date,
        schedule_home_away = schedule_home_away,
        schedule_opponent = schedule_opponent,
        seasons_train = seasons_train,
        mode_policy = policy,
        synthetic_feature_row = synthetic_feature_row,
        is_future = is_future,
        availability_policy = availability_policy
      ),
      WR = run_wr_simulation(
        gsis_id = gsis_id,
        season = season,
        week = week,
        n_sims = n_sims,
        game_date = resolved_game_date,
        schedule_game_id = schedule_game_id,
        schedule_game_date = schedule_game_date,
        schedule_home_away = schedule_home_away,
        schedule_opponent = schedule_opponent,
        seasons_train = seasons_train,
        mode_policy = policy,
        synthetic_feature_row = synthetic_feature_row,
        is_future = is_future,
        availability_policy = availability_policy
      ),
      TE = run_te_simulation(
        gsis_id = gsis_id,
        season = season,
        week = week,
        n_sims = n_sims,
        game_date = resolved_game_date,
        schedule_game_id = schedule_game_id,
        schedule_game_date = schedule_game_date,
        schedule_home_away = schedule_home_away,
        schedule_opponent = schedule_opponent,
        seasons_train = seasons_train,
        mode_policy = policy,
        synthetic_feature_row = synthetic_feature_row,
        is_future = is_future,
        availability_policy = availability_policy
      ),
      QB = run_qb_simulation(
        gsis_id = gsis_id,
        season = season,
        week = week,
        n_sims = n_sims,
        game_date = resolved_game_date,
        schedule_game_id = schedule_game_id,
        schedule_game_date = schedule_game_date,
        schedule_home_away = schedule_home_away,
        schedule_opponent = schedule_opponent,
        seasons_train = seasons_train,
        mode_policy = policy,
        synthetic_feature_row = synthetic_feature_row,
        is_future = is_future,
        availability_policy = availability_policy
      ),
      K = run_k_simulation(
        gsis_id = gsis_id,
        season = season,
        week = week,
        n_sims = n_sims,
        game_date = resolved_game_date,
        schedule_game_id = schedule_game_id,
        schedule_game_date = schedule_game_date,
        schedule_home_away = schedule_home_away,
        schedule_opponent = schedule_opponent,
        seasons_train = seasons_train,
        mode_policy = policy,
        synthetic_feature_row = synthetic_feature_row,
        is_future = is_future,
        availability_policy = availability_policy
      ),
      {
        stop("simulate_player_game does not support position ", player_position, ".")
      }
    ),
    error = function(e) {
      err_type <- classify_error(conditionMessage(e))
      allow_cf <- err_type %in% c("PLAYER_INACTIVE", "PLAYER_RETIRED_OR_NO_RECENT_DATA")
      make_error(err_type, conditionMessage(e), player_name = dim_row$full_name[1], allow_counterfactual = allow_cf)
    }
  )

  if (!is.null(result$status) && identical(result$status, "error")) {
    return(result)
  }

  if (!is.na(in_progress_season) && season == in_progress_season) {
    result$metadata$current_season_in_progress <- TRUE
  }
  
  if (is_future) {
    result$metadata$synthetic <- TRUE
    result$metadata$simulation_mode <- mode
  } else {
    result$metadata$synthetic <- FALSE
    result$metadata$simulation_mode <- mode
  }
  result$metadata$counterfactual_mode <- counterfactual_mode
  result$metadata$counterfactual_team <- if (counterfactual_mode) counterfactual_team else NA_character_
  if (counterfactual_mode) {
    result$metadata$counterfactual_reason <- "User-selected counterfactual roster"
  }
  if (!is.null(forced_counterfactual_notice)) {
    result$metadata$forced_counterfactual_notice <- forced_counterfactual_notice
  }
  
  return(result)
}

