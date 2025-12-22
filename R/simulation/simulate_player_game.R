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
                                 availability_policy = "played_only") {
  
  mode <- match.arg(mode)
  
  if (missing(gsis_id) || is.null(gsis_id) || !nzchar(trimws(as.character(gsis_id)))) {
    stop("gsis_id is required for simulate_player_game")
  }
  if (missing(season) || is.null(season) || is.na(season)) {
    stop("season is required for simulate_player_game")
  }
  if (missing(week) || is.null(week) || is.na(week)) {
    stop("week is required for simulate_player_game")
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
    "build_game_key",
    "load_schedules",
    "build_future_rb_feature_row",
    "build_future_wr_feature_row",
    "build_future_te_feature_row"
  )
  missing_funcs <- required_funcs[!sapply(required_funcs, exists)]
  if (length(missing_funcs) > 0) {
    stop("Simulation bootstrap incomplete: missing functions: ", paste(missing_funcs, collapse = ", "))
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
    stop("No seasons available in cached data. Run scripts/refresh_weekly_cache.R to populate caches.")
  }
  
  availability_policy <- validate_availability_policy(availability_policy)

  # Determine in-progress season from schedules (cache-only)
  in_progress_season <- NA_integer_
  if (exists("detect_in_progress_season")) {
    in_progress_season <- detect_in_progress_season(available_seasons, cache_only = cache_only)
  }

  # Validate requested week exists in schedule (prevents unscheduled games)
  schedules <- load_schedules(seasons = season, cache_only = cache_only)
  if (nrow(schedules) == 0) {
    stop("Schedule data is empty for season ", season, ". Cannot validate requested week.")
  }
  if (!any(schedules$season == season & schedules$week == week)) {
    stop("No schedule entry found for season ", season, " week ", week,
         ". Simulation is only allowed for scheduled weeks.")
  }

  player_dim <- read_player_dim_cache()
  if (is.null(player_dim) || nrow(player_dim) == 0) {
    stop("Player dimension cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }
  dim_row <- player_dim[player_dim$gsis_id == gsis_id & player_dim$season == season, , drop = FALSE]
  if (nrow(dim_row) == 0) {
    stop("No player_dim row found for gsis_id ", gsis_id, " in season ", season,
         ". Run scripts/refresh_weekly_cache.R to refresh caches.")
  }
  player_team <- dim_row$team[1]
  player_position <- toupper(as.character(dim_row$position[1]))
  if (is.na(player_position) || player_position == "") {
    stop("Player position missing for gsis_id ", gsis_id, " in season ", season, ".")
  }
  if (is.na(player_team) || player_team == "") {
    stop("Player team missing for gsis_id ", gsis_id, " in season ", season, ".")
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
  
  if (is_future) {
    sched_match <- schedules[
      schedules$season == season &
        schedules$week == week &
        (schedules$home_team == player_team | schedules$away_team == player_team),
      , drop = FALSE
    ]
    if (nrow(sched_match) == 0) {
      stop("No schedule entry found for gsis_id ", gsis_id, " team ", player_team,
           " in season ", season, " week ", week, ".")
    }
    if (nrow(sched_match) > 1) {
      stop("Multiple schedule entries found for team ", player_team, " in season ", season,
           " week ", week, ". Cannot disambiguate opponent.")
    }
    sched_row <- sched_match[1, ]
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
        game_date = NULL
      ),
      WR = build_future_wr_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = NULL
      ),
      TE = build_future_te_feature_row(
        player_id = gsis_id,
        season = season,
        week = week,
        team = resolved_team,
        opponent = resolved_opponent,
        home_away = resolved_home_away,
        game_date = NULL
      ),
      stop("simulate_player_game does not support position ", player_position, " for future simulation.")
    )
    synthetic_feature_row$player_name <- dim_row$full_name[1]
    resolved_game_date <- NULL
  }
  
  result <- switch(
    player_position,
    RB = run_rb_simulation(
      gsis_id = gsis_id,
      season = season,
      week = week,
      n_sims = n_sims,
      game_date = resolved_game_date,
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
      seasons_train = seasons_train,
      mode_policy = policy,
      synthetic_feature_row = synthetic_feature_row,
      is_future = is_future,
      availability_policy = availability_policy
    ),
    stop("simulate_player_game does not support position ", player_position, ".")
  )

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
  
  return(result)
}

