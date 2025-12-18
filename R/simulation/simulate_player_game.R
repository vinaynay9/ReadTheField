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
                                 cache_only = TRUE) {
  
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
    "read_rb_weekly_features_cache",
    "run_rb_simulation",
    "build_game_key",
    "load_schedules",
    "build_future_rb_feature_row"
  )
  missing_funcs <- required_funcs[!sapply(required_funcs, exists)]
  if (length(missing_funcs) > 0) {
    stop("Simulation bootstrap incomplete: missing functions: ", paste(missing_funcs, collapse = ", "))
  }
  
  rb_features_for_seasons <- read_rb_weekly_features_cache()
  if (nrow(rb_features_for_seasons) == 0) {
    stop("RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }
  
  available_seasons <- sort(unique(rb_features_for_seasons$season[!is.na(rb_features_for_seasons$season)]))
  if (length(available_seasons) == 0) {
    stop("No seasons available in RB weekly features cache.")
  }
  
  policy <- simulation_mode_policy(
    mode = mode,
    target_season = season,
    target_week = week,
    target_game_date = NULL,
    available_seasons = available_seasons
  )
  seasons_train <- policy$seasons_allowed
  if (!is.null(max_train_seasons) && length(seasons_train) > max_train_seasons) {
    seasons_train <- tail(seasons_train, max_train_seasons)
  }
  
  is_future <- mode %in% c("upcoming_game", "hypothetical_matchup")
  synthetic_feature_row <- NULL
  resolved_game_date <- NULL
  player_position <- NULL
  
  if (is_future) {
    # Use player_dim for team/position/headshot metadata
    if (!exists("read_player_dim_cache")) {
      if (file.exists("R/data/build_player_dim.R")) {
        source("R/data/build_player_dim.R", local = TRUE)
      } else {
        stop("Missing R/data/build_player_dim.R. Cannot load player_dim cache.")
      }
    }
    player_dim <- read_player_dim_cache()
    dim_row <- player_dim[player_dim$gsis_id == gsis_id & player_dim$season == season, , drop = FALSE]
    if (nrow(dim_row) == 0) {
      stop("No player_dim row found for gsis_id ", gsis_id, " in season ", season,
           ". Run scripts/refresh_weekly_cache.R to refresh caches.")
    }
    player_team <- dim_row$team[1]
    player_position <- dim_row$position[1]
    
    schedules <- load_schedules(seasons = season, cache_only = cache_only)
    if (nrow(schedules) == 0) {
      stop("Schedule data is empty for season ", season, ". Cannot resolve opponent/home_away.")
    }
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
    
    synthetic_feature_row <- build_future_rb_feature_row(
      player_id = gsis_id,
      season = season,
      week = week,
      team = resolved_team,
      opponent = resolved_opponent,
      home_away = resolved_home_away,
      game_date = NULL
    )
    synthetic_feature_row$player_name <- dim_row$full_name[1]
    resolved_game_date <- NULL
  } else {
    target_row <- rb_features_for_seasons[
      rb_features_for_seasons$player_id == gsis_id &
        rb_features_for_seasons$season == season &
        rb_features_for_seasons$week == week,
      , drop = FALSE
    ]
    if (nrow(target_row) == 0) {
      stop("No cached game found for gsis_id ", gsis_id, " season ", season, " week ", week, ".")
    }
    resolved_team <- target_row$team[1]
    resolved_opponent <- target_row$opponent[1]
    resolved_home_away <- target_row$home_away[1]
    resolved_game_date <- if ("gameday" %in% names(target_row)) target_row$gameday[1] else target_row$game_date[1]
    player_position <- target_row$position[1]
  }
  
  if (toupper(player_position) != "RB") {
    stop("simulate_player_game currently supports RB only. gsis_id ", gsis_id,
         " has position ", player_position, ".")
  }
  
  result <- run_rb_simulation(
    gsis_id = gsis_id,
    season = season,
    week = week,
    n_sims = n_sims,
    game_date = resolved_game_date,
    seasons_train = seasons_train,
    mode_policy = policy,
    synthetic_feature_row = synthetic_feature_row,
    is_future = is_future
  )
  
  if (is_future) {
    result$metadata$synthetic <- TRUE
    result$metadata$simulation_mode <- mode
  } else {
    result$metadata$synthetic <- FALSE
    result$metadata$simulation_mode <- mode
  }
  
  return(result)
}

