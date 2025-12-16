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
#' Main entry point for player game simulations. Takes only player name and game date,
#' automatically detects position, team, opponent, season, and week.
#' Routes to appropriate position-specific simulation function.
#'
#' @param player_name Character, player name (e.g., "Bijan Robinson", "B.Robinson")
#' @param game_date Date, game date (e.g., as.Date("2025-12-11"))
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @param mode Character, simulation mode: "upcoming_game", "hypothetical_matchup", or "historical_replay" (default "historical_replay")
#' @param position Optional character position filter
#' @param seasons Optional integer vector of seasons to search (default: all available in cache)
#' @param max_train_seasons Optional integer, cap on number of training seasons (default: all available)
#' @param cache_only Logical, if TRUE avoid download attempts (default TRUE)
#' @return List with simulation results (structure depends on position)
#' @examples
#' result <- simulate_player_game("Bijan Robinson", as.Date("2025-12-11"), mode = "historical_replay")
simulate_player_game <- function(player_name,
                                 game_date,
                                 n_sims = 5000,
                                 mode = "historical_replay",
                                 position = NULL,
                                 seasons = NULL,
                                 max_train_seasons = NULL,
                                 cache_only = TRUE) {
  
  # Validate inputs
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) {
    stop("player_name is required")
  }
  
  if (missing(game_date) || is.null(game_date)) {
    stop("game_date is required")
  }
  
  game_date <- as.Date(game_date)
  if (is.na(game_date)) {
    stop("game_date must be a valid date")
  }
  
  # Load helpers and resolver
  if (!exists("resolve_player_game")) {
    if (file.exists("R/simulation/resolve_player_game.R")) {
      source("R/simulation/resolve_player_game.R", local = TRUE)
    } else {
      stop("resolve_player_game function not found")
    }
  }
  if (!exists("get_available_seasons_from_cache")) {
    if (file.exists("R/utils/cache_helpers.R")) {
      source("R/utils/cache_helpers.R", local = TRUE)
    }
  }
  if (!exists("simulation_mode_policy")) {
    if (file.exists("R/simulation/simulation_mode_policy.R")) {
      source("R/simulation/simulation_mode_policy.R", local = TRUE)
    } else {
      stop("simulation_mode_policy function not found")
    }
  }
  
  # Ensure cache_only is TRUE (no nflreadr calls during simulation)
  if (!isTRUE(cache_only)) {
    warning("cache_only was FALSE, forcing to TRUE. Simulation must use cached data only.")
    cache_only <- TRUE
  }
  
  # Resolve player/game context from cached data
  resolved <- resolve_player_game(
    player_name = player_name,
    game_date = game_date,
    position = position,
    seasons = seasons,
    cache_only = cache_only
  )
  
  position <- resolved$position
  
  # ENFORCE POSITION DETERMINISM: Validate position is uppercase and valid
  valid_positions <- c("QB", "RB", "WR", "TE", "K")
  if (is.na(position) || position == "" || !position %in% valid_positions) {
    stop("Invalid position '", position, "' for player '", player_name, 
         "'. Position must be one of: ", paste(valid_positions, collapse = ", "),
         ". Position cannot be NA or empty.")
  }
  
  if (!exists("read_rb_weekly_features_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required for simulation")
    }
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
      if (file.exists("R/simulation/run_rb_simulation.R")) {
        source("R/simulation/run_rb_simulation.R", local = TRUE)
      } else {
        stop("run_rb_simulation function not found")
      }
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
      mode_policy = policy
    )
    
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

