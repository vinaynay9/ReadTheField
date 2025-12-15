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
#' @return List with simulation results (structure depends on position)
#' @examples
#' result <- simulate_player_game("Bijan Robinson", as.Date("2025-12-11"))
simulate_player_game <- function(player_name,
                                 game_date,
                                 n_sims = 5000,
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
  
  # Resolve player/game context from cached data
  resolved <- resolve_player_game(
    player_name = player_name,
    game_date = game_date,
    position = position,
    seasons = seasons,
    cache_only = cache_only
  )
  
  position <- resolved$position
  
  # Determine training seasons (max historical by default)
  available_seasons <- if (exists("get_available_seasons_from_cache")) {
    unique(c(get_available_seasons_from_cache("rb_stats"), get_available_seasons_from_cache("player_stats")))
  } else integer(0)
  available_seasons <- sort(unique(available_seasons))
  if (length(available_seasons) == 0) {
    available_seasons <- sort(unique(c(resolved$season - 1, resolved$season)))
  }
  
  seasons_train <- available_seasons[available_seasons < resolved$season]
  if (!is.null(resolved$week) && !is.na(resolved$week) && resolved$week > 1 && resolved$season %in% available_seasons) {
    seasons_train <- unique(c(seasons_train, resolved$season))
  }
  seasons_train <- sort(unique(seasons_train))
  
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
      home_away = resolved$home_away
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

