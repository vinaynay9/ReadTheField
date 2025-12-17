# Simulate Player Game - High-Level Automated Function
#
# This is the main entry point for player game simulations.
# Takes only player_name and game_date, auto-detects everything else.
#
# Dependencies:
#   - R/utils/find_player_info.R
#   - R/simulation/run_rb_simulation.R
#   - (Future: R/simulation/run_wr_simulation.R, etc.)

#' Simulate a player's game performance - fully automated
#'
#' Main entry point for player game simulations. Supports three modes:
#'   - "historical_replay": Replay of completed games (default, requires game in cache)
#'   - "upcoming_game": Forward projection for upcoming games (requires season+week, no game_date)
#'   - "hypothetical_matchup": Hypothetical scenario with swapped opponent (requires season+week)
#'
#' Routes to appropriate position-specific simulation function.
#'
#' @param player_name Character, player name (case-insensitive, e.g., "Bijan Robinson")
#' @param game_date Date, game date (optional if season and week provided, not allowed in upcoming_game mode)
#' @param season Optional integer season (required for upcoming_game mode, optional for historical_replay)
#' @param week Optional integer week (required for upcoming_game mode, optional for historical_replay)
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @param mode Character, simulation mode (default "historical_replay"):
#'   - "historical_replay": Replay of completed games (requires game in identity cache)
#'   - "upcoming_game": Forward projection for upcoming games (requires season and week, disallows game_date)
#'   - "hypothetical_matchup": Hypothetical scenario with swapped opponent (requires season and week)
#' @param position Optional character position filter
#' @param seasons Optional integer vector of seasons to search (default: all available in cache)
#' @param max_train_seasons Optional integer, cap on number of training seasons (default: all available)
#' @param cache_only Logical, if TRUE avoid download attempts (default TRUE)
#' @return List with simulation results (structure depends on position)
#' @examples
#' # Historical replay mode (default)
#' result <- simulate_player_game("Bijan Robinson", season = 2024, week = 15, mode = "historical_replay")
#' result <- simulate_player_game("Bijan Robinson", game_date = as.Date("2024-12-11"))
#' 
#' # Upcoming game mode
#' result <- simulate_player_game("Bijan Robinson", season = 2025, week = 15, mode = "upcoming_game")
simulate_player_game <- function(player_name,
                                 game_date = NULL,
                                 season = NULL,
                                 week = NULL,
                                 n_sims = 5000,
                                 mode = c("historical_replay", "upcoming_game", "hypothetical_matchup"),
                                 position = NULL,
                                 seasons = NULL,
                                 max_train_seasons = NULL,
                                 cache_only = TRUE) {
  
  # Validate inputs
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) {
    stop("player_name is required")
  }
  player_name <- trimws(as.character(player_name))
  if (nchar(player_name) == 0) {
    stop("player_name cannot be empty")
  }
  
  # Validate and normalize mode parameter using match.arg()
  mode <- match.arg(mode)
  
  # Determine resolution mode
  has_season_week <- !is.null(season) && !is.null(week)
  has_game_date <- !is.null(game_date)
  
  # UPCOMING_GAME MODE: Require season and week, disallow game_date
  if (mode == "upcoming_game") {
    if (!has_season_week) {
      stop("upcoming_game mode requires both season and week to be specified. ",
           "Cannot use date-based resolution in upcoming_game mode.")
    }
    if (has_game_date) {
      stop("upcoming_game mode does not accept game_date parameter. ",
           "Use season and week instead.")
    }
  }
  
  # HYPOTHETICAL_MATCHUP MODE: Require season and week, disallow game_date
  if (mode == "hypothetical_matchup") {
    if (!has_season_week) {
      stop("hypothetical_matchup mode requires both season and week to be specified. ",
           "Cannot use date-based resolution in hypothetical_matchup mode.")
    }
    if (has_game_date) {
      stop("hypothetical_matchup mode does not accept game_date parameter. ",
           "Use season and week instead.")
    }
  }
  
  # HISTORICAL_REPLAY MODE: Require either season/week or game_date
  if (mode == "historical_replay") {
    if (!has_season_week && !has_game_date) {
      stop("historical_replay mode requires either (season, week) or game_date. ",
           "Resolution mode A (historical replay): provide season and week. ",
           "Resolution mode B (date-based): provide game_date.")
    }
  }
  
  if (!is.null(game_date)) {
    game_date <- as.Date(game_date)
    if (is.na(game_date)) {
      stop("game_date must be a valid date")
    }
  }
  
  # Ensure bootstrap has been loaded
  if (!exists("resolve_player_game") || !exists("get_available_seasons_from_cache") || 
      !exists("simulation_mode_policy")) {
    stop("Simulation bootstrap incomplete: Required functions not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
  }
  
  # Ensure cache_only is TRUE (no nflreadr calls during simulation)
  if (!isTRUE(cache_only)) {
    warning("cache_only was FALSE, forcing to TRUE. Simulation must use cached data only.")
    cache_only <- TRUE
  }
  
  # Resolve player/game context based on mode
  resolved <- NULL
  synthetic_feature_row <- NULL
  
  # Determine if this is an upcoming game (not in identity cache)
  is_upcoming_game <- (mode == "upcoming_game" || mode == "hypothetical_matchup")
  
  if (is_upcoming_game) {
    # UPCOMING_GAME / HYPOTHETICAL_MATCHUP MODE: Resolve player_id from directory, opponent/home_away from schedules
    # Do NOT require player_week_identity row for target week
    
    # Ensure required functions are loaded
    if (!exists("read_player_directory_cache") || !exists("canonicalize_name")) {
      stop("Simulation bootstrap incomplete: read_player_directory_cache or canonicalize_name not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    
    player_dir <- read_player_directory_cache()
    if (nrow(player_dir) == 0) {
      stop("Player directory cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
    }
    
    player_name_canonical <- canonicalize_name(player_name)
    name_matches <- player_dir[
      !is.na(player_dir$canonical_name) & player_dir$canonical_name == player_name_canonical,
      , drop = FALSE
    ]
    
    if (nrow(name_matches) == 0) {
      stop("No player found with name '", player_name, 
           "' in player directory. Cannot resolve player_id for ", mode, " mode.")
    }
    if (nrow(name_matches) > 1) {
      stop("Multiple players found with name '", player_name, 
           "'. Cannot uniquely resolve player_id for ", mode, " mode.")
    }
    
    resolved_player_id <- name_matches$player_id[1]
    resolved_player_name <- name_matches$full_name[1]
    
    # Apply position filter if provided
    if (!is.null(position)) {
      # Get position from recent games
      if (!exists("read_rb_weekly_features_cache")) {
        stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded. ",
             "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
      }
      rb_features_check <- read_rb_weekly_features_cache()
      player_games <- rb_features_check[rb_features_check$player_id == resolved_player_id, , drop = FALSE]
      if (nrow(player_games) > 0) {
        player_position <- unique(player_games$position[!is.na(player_games$position)])
        if (length(player_position) > 0 && !toupper(trimws(position)) %in% toupper(player_position)) {
          stop("Player '", resolved_player_name, "' does not play position '", position, 
               "'. Player's position: ", paste(player_position, collapse = ", "))
        }
      }
    }
    
    # Load schedules to get opponent/home_away
    if (!exists("load_schedules")) {
      stop("Simulation bootstrap incomplete: load_schedules not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    
    # Get player's team from recent games (same season, week < target_week)
    if (!exists("read_rb_weekly_features_cache")) {
      stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    rb_features <- read_rb_weekly_features_cache()
    player_recent <- rb_features[
      rb_features$player_id == resolved_player_id &
      rb_features$season == season &
      !is.na(rb_features$week) &
      rb_features$week < week,
      , drop = FALSE
    ]
    
    if (nrow(player_recent) == 0) {
      stop("No games found for player_id '", resolved_player_id, 
           "' in season ", season, " before week ", week, 
           ". Cannot determine team for future game lookup. ",
           "Player must have played at least one game earlier in the season.")
    }
    
    player_recent <- player_recent[order(
      player_recent$week,
      decreasing = TRUE
    ), , drop = FALSE]
    player_team <- player_recent$team[1]
    
    # Load schedules and find game by team
    if (!exists("load_schedules")) {
      stop("Simulation bootstrap incomplete: load_schedules not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    
    schedules <- load_schedules(seasons = season, cache_only = cache_only)
    if (nrow(schedules) == 0) {
      stop("Schedule data is empty for season ", season, 
           ". Cannot resolve opponent and home_away for future game.")
    }
    
    game_schedule <- schedules[
      schedules$season == season &
      schedules$week == week &
      (schedules$home_team == player_team | schedules$away_team == player_team),
      , drop = FALSE
    ]
    
    if (nrow(game_schedule) == 0) {
      stop("Cannot find schedule information for season ", season, " week ", week, 
           " with team '", player_team, "'. ",
           "Schedule data may be missing or team abbreviation may be incorrect.")
    }
    if (nrow(game_schedule) > 1) {
      stop("Multiple schedule entries found for season ", season, " week ", week, 
           " with team '", player_team, "'. Cannot determine opponent/home_away.")
    }
    
    # Determine team, opponent, home_away from schedule
    schedule_row <- game_schedule[1, ]
    if (schedule_row$home_team == player_team) {
      resolved_team <- schedule_row$home_team
      resolved_opponent <- schedule_row$away_team
      resolved_home_away <- "HOME"
    } else if (schedule_row$away_team == player_team) {
      resolved_team <- schedule_row$away_team
      resolved_opponent <- schedule_row$home_team
      resolved_home_away <- "AWAY"
    } else {
      stop("Team mismatch: player's team '", player_team, 
           "' not found in schedule for season ", season, " week ", week)
    }
    
    # Build resolved object for upcoming_game/hypothetical_matchup mode
    resolved <- list(
      player_id = resolved_player_id,
      player_name_canonical = resolved_player_name,
      position = if (!is.null(position)) toupper(trimws(position)) else "RB",
      team = resolved_team,
      opponent = resolved_opponent,
      home_away = resolved_home_away,
      season = season,
      week = week,
      game_date = NULL,
      game_id = NA_character_,
      game_key = NA_character_,
      resolution_mode = mode
    )
    
    # Build synthetic feature row
    if (!exists("build_future_rb_feature_row")) {
      stop("Simulation bootstrap incomplete: build_future_rb_feature_row not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    
    synthetic_feature_row <- build_future_rb_feature_row(
      player_id = resolved_player_id,
      season = season,
      week = week,
      team = resolved_team,
      opponent = resolved_opponent,
      home_away = resolved_home_away,
      game_date = NULL
    )
    
  } else {
    # REPLAY MODE: Standard resolution (unchanged behavior)
    resolved <- resolve_player_game(
      player_name = player_name,
      game_date = game_date,
      season = season,
      week = week,
      position = position,
      seasons = seasons,
      cache_only = cache_only
    )
  }
  
  # CRITICAL: Guardrail - ensure canonical names match (using same canonicalization function)
  if (!exists("canonicalize_name")) {
    stop("Simulation bootstrap incomplete: canonicalize_name not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
  }
  
  if (exists("canonicalize_name")) {
    input_canonical <- canonicalize_name(player_name)
    resolved_canonical <- canonicalize_name(resolved$player_name_canonical)
    
    if (is.na(input_canonical) || is.na(resolved_canonical) || input_canonical != resolved_canonical) {
      stop("Player name canonical mismatch: requested canonical '", input_canonical, 
           "' (input: '", player_name, "') but resolved canonical '", resolved_canonical, 
           "' (resolved: '", resolved$player_name_canonical, "'). ",
           "Resolution mode was: ", resolved$resolution_mode, ". ",
           "This should never happen with strict canonical matching - please report this bug.")
    }
  }
  
  position <- resolved$position
  
  # ENFORCE POSITION DETERMINISM: Validate position is uppercase and valid
  valid_positions <- c("QB", "RB", "WR", "TE", "K")
  if (is.null(position) || is.na(position) || !nzchar(position) || !position %in% valid_positions) {
    position_str <- if (is.null(position)) "NULL" else if (is.na(position)) "NA" else as.character(position)
    stop("Invalid position '", position_str, "' for player '", player_name, 
         "'. Position must be one of: ", paste(valid_positions, collapse = ", "),
         ". Position cannot be NULL, NA, or empty.")
  }
  
  if (!exists("read_rb_weekly_features_cache")) {
    stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
  }

  rb_features_for_seasons <- read_rb_weekly_features_cache()
  available_seasons <- sort(unique(rb_features_for_seasons$season[!is.na(rb_features_for_seasons$season)]))
  if (length(available_seasons) == 0) {
    stop("RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }
  
  # Apply simulation mode policy to determine training seasons
  policy <- simulation_mode_policy(
    mode = mode,
    target_season = resolved$season,
    target_week = resolved$week,
    target_game_date = resolved$game_date,
    available_seasons = available_seasons
  )
  
  seasons_train <- policy$seasons_allowed
  
  # Apply optional cap on training seasons
  if (!is.null(max_train_seasons) && length(seasons_train) > max_train_seasons) {
    seasons_train <- tail(seasons_train, max_train_seasons)
  }
  
  # Route to position-specific simulation
  if (position == "RB") {
    if (!exists("run_rb_simulation")) {
      stop("Simulation bootstrap incomplete: run_rb_simulation not loaded. ",
           "Source R/simulation/bootstrap_simulation.R before calling simulate_player_game().")
    }
    
    result <- run_rb_simulation(
      player_name = resolved$player_name_canonical,
      team = resolved$team,
      opponent = resolved$opponent,
      season = resolved$season,
      week = resolved$week,
      n_sims = n_sims,
      game_date = resolved$game_date,
      player_id = resolved$player_id,
      game_id = resolved$game_id,
      game_key = resolved$game_key,
      seasons_train = seasons_train,
      home_away = resolved$home_away,
      mode_policy = policy,
      synthetic_feature_row = synthetic_feature_row,
      is_future = is_upcoming_game
    )
    
    # Add synthetic flag to metadata
    if (is_upcoming_game) {
      result$metadata$synthetic <- TRUE
      result$metadata$simulation_mode <- mode
    } else {
      result$metadata$synthetic <- FALSE
      result$metadata$simulation_mode <- mode
    }
    
    return(result)
    
  } else if (position == "WR") {
    stop("WR simulation not yet implemented. Currently only RB simulations are supported.")
    
  } else if (position == "TE") {
    stop("TE simulation not yet implemented. Currently only RB simulations are supported.")
    
  } else if (position == "QB") {
    stop("QB simulation not yet implemented. Currently only RB simulations are supported.")
    
  } else if (position == "K") {
    stop("K simulation not yet implemented. Currently only RB simulations are supported.")
    
  } else {
    stop("Unsupported position: ", position, ". Supported positions: RB (WR, TE, QB, K coming soon)")
  }
}

