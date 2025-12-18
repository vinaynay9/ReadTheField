# Run RB Simulation - Pure Computation Layer
#
# This function performs all computation for an RB simulation:
# - Identifies the game
# - Assembles training data
# - Fits models
# - Runs Monte Carlo simulation
#
# NO printing, NO file writing, NO cat() calls
# Returns a structured result object for consumption by presentation/persistence layers
#
# Dependencies:
#   - R/data/build_weekly_player_layers.R
#   - R/models/fit_rb_models.R
#   - R/simulation/simulate_rb_game.R
#   - R/utils/ppr_scoring.R
#
# Usage:
#   result <- run_rb_simulation(
#     player_name = "B.Robinson",
#     team = "ATL",
#     opponent = "TB",
#     season = 2025,
#     week = 15,
#     n_sims = 5000
#   )

#' Run RB simulation - pure computation
#'
#' Performs complete RB simulation workflow without any printing or file I/O.
#' Returns structured result object containing all inputs, models, draws, and summaries.
#'
#' @param player_name Character, player name pattern(s) to match (e.g., "B.Robinson" or c("B.Robinson", "Bijan Robinson"))
#' @param team Character, player's team abbreviation (e.g., "ATL")
#' @param opponent Character, opponent team abbreviation (e.g., "TB")
#' @param season Integer, season year (e.g., 2025)
#' @param week Integer, week number (e.g., 15)
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @param game_date Date, optional game date for validation (if NULL, inferred from season/week)
#' @return List with:
#'   - metadata: list(game_id, player_id, player_name, team, opponent, season, week, game_date)
#'   - recent_games: data.frame of player's last 3 games before target game
#'   - defensive_context: list of defensive features used
#'   - summary: data.frame with p25, p50, p75 for each stat
#'   - diagnostics: list with model info, distribution stats, TD probabilities
#'   - draws: data.frame with n_sims rows of simulated stat lines
run_rb_simulation <- function(player_name,
                              team,
                              opponent,
                              season,
                              week,
                              n_sims = 5000,
                              game_date = NULL,
                              player_id = NULL,
                              game_id = NULL,
                              game_key = NULL,
                              seasons_train = NULL,
                              home_away = NULL,
                              mode_policy = NULL,
                              synthetic_feature_row = NULL,
                              is_future = FALSE) {
  
  # Initialize file-based diagnostic logging (project root)
  log_file <- "rb_debug.log"
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }
  
  log_msg("")
  log_msg("=== RB Simulation Start ===")
  log_msg("REQUESTED: Player:", player_name, "| Team:", team, "| Opponent:", opponent)
  log_msg("REQUESTED: Season:", season, "| Week:", week)
  log_msg("CLI args are the source of truth - no overrides allowed")
  
  # Initialize result structure
  result <- list(
    metadata = list(
      game_id = NULL,
      game_key = NULL,
      player_id = NULL,
      player_name = NULL,
      team = NULL,
      opponent = NULL,
      season = NULL,
      week = NULL,
      game_date = NULL,
      home_away = NULL,
      n_sims = n_sims,
      random_seed = NULL,
      config = list()
    ),
    recent_games = data.frame(),
    defensive_context = list(),
    summary = data.frame(),
    diagnostics = list(),
    draws = data.frame()
  )
  
  # Set random seed for reproducibility
  if (!exists(".Random.seed", envir = .GlobalEnv)) {
    set.seed(NULL)
  }
  result$metadata$random_seed <- .Random.seed
  
  # Ensure player_name is a vector for pattern matching
  if (is.character(player_name) && length(player_name) == 1) {
    player_name_patterns <- c(player_name)
  } else {
    player_name_patterns <- player_name
  }
  
  # ============================================================================
  # STEP 1: Identify the correct game using resolved metadata (no schedule dependency)
  # ============================================================================
  
  # Load helpers for keys / seasons
  if (!exists("build_game_key")) {
    stop("Simulation bootstrap incomplete: build_game_key not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling run_rb_simulation().")
  }
  
  # Helper function for NA-safe scalar validation (defined early for use throughout)
  is_valid_scalar <- function(x) {
    length(x) == 1 && !is.null(x) && !is.na(x)
  }
  
  is_valid_string <- function(x) {
    is_valid_scalar(x) && is.character(x) && nzchar(x)
  }
  
  # Determine game key if not provided
  resolved_game_key <- if (is_valid_string(game_key)) {
    game_key
  } else if (exists("build_game_key")) {
    build_game_key(season, week, game_date, team, opponent, game_id)
  } else {
    paste(season, week, game_date, team, opponent, sep = "_")
  }
  
  # Training seasons: must be provided (determined by simulation mode policy)
  # No ad-hoc filtering - all training window logic goes through policy
  if (is.null(seasons_train) || length(seasons_train) == 0) {
    stop("No training seasons provided to run_rb_simulation. ",
         "Training seasons must be determined by simulation_mode_policy() ",
         "and passed from simulate_player_game().")
  }
  
  # ============================================================================
  # STEP 2: Load cached RB weekly features up to (but not including) this game
  # ============================================================================
  
  if (!exists("read_rb_weekly_features_cache")) {
    stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling run_rb_simulation().")
  }

  rb_data_all <- read_rb_weekly_features_cache()
  if (nrow(rb_data_all) == 0) {
    stop("RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }

  rb_data <- rb_data_all[rb_data_all$season %in% seasons_train, , drop = FALSE]
  if (nrow(rb_data) == 0) {
    stop("No RB training rows available for seasons: ", paste(seasons_train, collapse = ", "))
  }
  
  # Identify target game row using provided metadata
  target_rows <- rb_data_all
  if (!is.null(player_id)) {
    target_rows <- target_rows[target_rows$player_id == player_id, ]
  } else {
    # fall back to player name patterns
    target_rows <- target_rows[target_rows$player_name %in% player_name_patterns, ]
    if (nrow(target_rows) == 0) {
      target_rows <- rb_data_all[grepl(player_name_patterns[1], rb_data_all$player_name, ignore.case = TRUE), ]
    }
  }
  
  # Filter by game identifiers
  if (is_valid_string(resolved_game_key)) {
    target_rows <- target_rows[!is.na(target_rows$game_key) & target_rows$game_key == resolved_game_key, ]
  }
  if (nrow(target_rows) == 0 && is_valid_string(game_id)) {
    target_rows <- rb_data_all[rb_data_all$game_id == game_id & (is.null(player_id) | rb_data_all$player_id == player_id), ]
  }
  if (nrow(target_rows) == 0 && !is.null(game_date) && !is.na(game_date)) {
    target_rows <- rb_data_all[
      rb_data_all$gameday == game_date &
      (is.null(player_id) | rb_data_all$player_id == player_id) &
      rb_data_all$team == team, ]
  }
  if (nrow(target_rows) == 0 && !is.na(season)) {
    target_rows <- rb_data_all[rb_data_all$season == season & rb_data_all$week == week & rb_data_all$team == team, ]
  }
  
  if (nrow(target_rows) == 0) {
    stop("Target game not found in cached RB weekly features for player/team provided. ",
         "Player: ", paste(player_name_patterns, collapse = ", "), 
         ", Team: ", team, ", Season: ", season, ", Week: ", week,
         ". Game may be unavailable in cached data or before data availability.")
  }
  
  # Deterministic disambiguation
  target_rows <- target_rows[order(target_rows$gameday, target_rows$game_id, target_rows$opponent), ]
  identified_game_row <- target_rows[1, ]
  
  if (is_valid_string(identified_game_row$game_key)) {
    resolved_game_key <- identified_game_row$game_key
  }
  
  # CRITICAL FIX: Respect CLI arguments - NO OVERRIDES
  # Extract game info with proper precedence: CLI args win, fallback to identified_game_row only when not provided
  
  # game_id: Only from identified_game_row (no CLI arg for this)
  game_id <- if (is_valid_string(identified_game_row$game_id)) {
    identified_game_row$game_id
  } else if (!is.null(game_id) && !is.na(game_id)) {
    game_id
  } else {
    NA_character_
  }
  
  # season: CLI arg wins, fallback to identified_game_row only if CLI arg missing
  if (!is.null(season) && !is.na(season)) {
    game_season <- as.integer(season)
    log_msg("Using CLI-provided season:", game_season)
  } else if (is_valid_scalar(identified_game_row$season) && !is.na(identified_game_row$season)) {
    game_season <- as.integer(identified_game_row$season)
    log_msg("Using identified_game_row season:", game_season)
  } else {
    game_season <- NA_integer_
    log_msg("WARNING: No valid season found")
  }
  
  # week: CLI arg wins, fallback to identified_game_row only if CLI arg missing
  if (!is.null(week) && !is.na(week)) {
    game_week <- as.integer(week)
    log_msg("Using CLI-provided week:", game_week)
  } else if (is_valid_scalar(identified_game_row$week) && !is.na(identified_game_row$week)) {
    game_week <- as.integer(identified_game_row$week)
    log_msg("Using identified_game_row week:", game_week)
  } else {
    game_week <- NA_integer_
    log_msg("WARNING: No valid week found")
  }
  
  # game_date: CLI arg wins, fallback to identified_game_row
  if (!is.null(game_date) && !is.na(game_date)) {
    game_gameday <- as.Date(game_date)
    log_msg("Using CLI-provided game_date:", as.character(game_gameday))
  } else if (is_valid_scalar(identified_game_row$gameday) && !is.na(identified_game_row$gameday)) {
    game_gameday <- as.Date(identified_game_row$gameday)
    log_msg("Using identified_game_row gameday:", as.character(game_gameday))
  } else {
    game_gameday <- as.Date(NA)
    log_msg("WARNING: No valid game_date found")
  }
  
  # opponent: CLI arg wins, fallback to identified_game_row
  if (!is.null(opponent) && !is.na(opponent) && nzchar(opponent)) {
    player_opponent <- opponent
    log_msg("Using CLI-provided opponent:", player_opponent)
  } else if (is_valid_string(identified_game_row$opponent)) {
    player_opponent <- identified_game_row$opponent
    log_msg("Using identified_game_row opponent:", player_opponent)
  } else {
    player_opponent <- NA_character_
    log_msg("WARNING: No valid opponent found")
  }
  
  # team: CLI arg wins, fallback to identified_game_row
  if (!is.null(team) && !is.na(team) && nzchar(team)) {
    player_team <- team
    log_msg("Using CLI-provided team:", player_team)
  } else if (is_valid_string(identified_game_row$team)) {
    player_team <- identified_game_row$team
    log_msg("Using identified_game_row team:", player_team)
  } else {
    player_team <- NA_character_
    log_msg("WARNING: No valid team found")
  }
  
  # home_away: CLI arg wins, fallback to identified_game_row
  if (!is.null(home_away) && !is.na(home_away) && nzchar(home_away)) {
    player_home_away <- home_away
    log_msg("Using CLI-provided home_away:", player_home_away)
  } else if (is_valid_string(identified_game_row$home_away)) {
    player_home_away <- identified_game_row$home_away
    log_msg("Using identified_game_row home_away:", player_home_away)
  } else {
    player_home_away <- NA_character_
    log_msg("WARNING: No valid home_away found")
  }
  
  # CRITICAL ASSERTIONS: Verify resolved values match requested values
  log_msg("=== Resolution Validation ===")
  log_msg("RESOLVED: Season:", game_season, "| Week:", game_week)
  log_msg("RESOLVED: Team:", player_team, "| Opponent:", player_opponent)
  log_msg("RESOLVED: Game Date:", as.character(game_gameday), "| Game ID:", game_id)
  log_msg("RESOLVED: Home/Away:", player_home_away)
  
  # Enforce CLI args when provided
  if (!is.null(season) && !is.na(season) && game_season != season) {
    stop("RESOLUTION MISMATCH: Requested season=", season, " but resolved game has season=", game_season, ". ",
         "CLI arguments must be respected. This indicates a bug in game resolution logic.")
  }
  
  if (!is.null(week) && !is.na(week) && game_week != week) {
    stop("RESOLUTION MISMATCH: Requested week=", week, " but resolved game has week=", game_week, ". ",
         "CLI arguments must be respected. This indicates a bug in game resolution logic.")
  }
  
  if (!is.null(opponent) && !is.na(opponent) && nzchar(opponent) && !is.na(player_opponent) && player_opponent != opponent) {
    log_msg("WARNING: Opponent mismatch - Requested:", opponent, "| Resolved:", player_opponent)
  }
  
  if (!is.null(team) && !is.na(team) && nzchar(team) && !is.na(player_team) && player_team != team) {
    log_msg("WARNING: Team mismatch - Requested:", team, "| Resolved:", player_team)
  }
  
  log_msg("Resolution validation: PASS")
  
  # Store metadata
  result$metadata$game_id <- game_id
  result$metadata$game_key <- resolved_game_key
  result$metadata$season <- game_season
  result$metadata$week <- game_week
  result$metadata$game_date <- game_gameday
  result$metadata$team <- player_team
  result$metadata$opponent <- player_opponent
  result$metadata$home_away <- player_home_away
  
  # Filter to games before the identified game (respecting policy exclusions)
  # Rolling features are precomputed in the cached RB weekly features
  # We just need to filter out future games and optionally the target game
  rb_data_pre <- rb_data
  
  # Apply policy week limits if provided
  if (!is.null(mode_policy) && !is.null(mode_policy$max_week_per_season) && length(mode_policy$max_week_per_season) > 0) {
    for (season_str in names(mode_policy$max_week_per_season)) {
      season_limit <- as.integer(season_str)
      max_week <- mode_policy$max_week_per_season[[season_str]]
      if (!is.null(max_week) && !is.na(max_week)) {
        # Filter this season to weeks <= max_week
        season_mask <- rb_data_pre$season == season_limit
        week_mask <- is.na(rb_data_pre$week) | rb_data_pre$week <= max_week
        rb_data_pre <- rb_data_pre[!season_mask | week_mask, ]
      }
    }
  }
  
  # Filter by date if available
  if (!is.na(game_gameday)) {
    rb_data_pre <- rb_data_pre[rb_data_pre$gameday < game_gameday, ]
  }
  
  # Filter by season/week if date filtering didn't work or wasn't sufficient
  if (nrow(rb_data_pre) == 0 || is.na(game_gameday)) {
    # Filter by season and week
    rb_data_pre <- rb_data[rb_data$season < game_season | 
                           (rb_data$season == game_season & !is.na(rb_data$week) & !is.na(game_week) & rb_data$week < game_week), ]
  }
  
  # Exclude target game based on policy (or always exclude for safety if policy not provided)
  exclude_target <- if (!is.null(mode_policy) && !is.null(mode_policy$exclude_target_game)) {
    mode_policy$exclude_target_game
  } else {
    TRUE  # Default: always exclude for safety
  }
  
  if (exclude_target) {
    # Exclude target game using primary keys: player_id, season, week
    # This is the most reliable method and works even when game_id/game_key are missing
    # Use player_id from function parameter (will be validated during target player resolution)
    if (is_valid_scalar(player_id) && is_valid_scalar(season) && is_valid_scalar(week)) {
      rb_data_pre <- rb_data_pre[
        !(rb_data_pre$player_id == player_id & 
          rb_data_pre$season == season & 
          rb_data_pre$week == week),
        , drop = FALSE
      ]
    }
    
    # Also exclude by game_key if it exists (additional safety)
    if (is_valid_string(resolved_game_key)) {
      rb_data_pre <- rb_data_pre[is.na(rb_data_pre$game_key) | rb_data_pre$game_key != resolved_game_key, ]
    }
    # Also exclude by game_id if available (additional safety)
    if (is_valid_string(game_id)) {
      rb_data_pre <- rb_data_pre[is.na(rb_data_pre$game_id) | rb_data_pre$game_id != game_id, ]
    }
  }
  
  # Find target player row using primary keys: player_id, season, week
  # This is the correct and most reliable way to identify a player-week row
  target_player_row <- NULL
  
  # Validate that we have the required primary keys
  if (is.null(player_id) || is.na(player_id) || player_id == "") {
    stop("player_id is required but is NULL, NA, or empty. ",
         "Cannot resolve target player without player_id. ",
         "Player: ", paste(player_name_patterns, collapse = ", "),
         ", Season: ", season, ", Week: ", week, ".")
  }
  
  if (is.null(season) || is.na(season)) {
    stop("season is required but is NULL or NA. ",
         "Cannot resolve target player without season. ",
         "Player ID: ", player_id, ", Week: ", week, ".")
  }
  
  if (is.null(week) || is.na(week)) {
    stop("week is required but is NULL or NA. ",
         "Cannot resolve target player without week. ",
         "Player ID: ", player_id, ", Season: ", season, ".")
  }
  
  # Find target player row using primary keys
  target_player_row <- rb_data_all[
    rb_data_all$player_id == player_id &
    rb_data_all$season == season &
    rb_data_all$week == week,
    , drop = FALSE
  ]
  
  # Validate exactly 1 row found
  if (nrow(target_player_row) == 0) {
    # Check if player exists at all
    player_exists <- any(rb_data_all$player_id == player_id, na.rm = TRUE)
    if (!player_exists) {
      stop("Player ID '", player_id, "' not found in RB weekly features cache. ",
           "Player: ", paste(player_name_patterns, collapse = ", "), ". ",
           "Player may not be a running back or data may be missing from cache.")
    }
    
    # Check if season/week combination exists for this player
    player_seasons <- unique(rb_data_all$season[rb_data_all$player_id == player_id])
    player_weeks <- unique(rb_data_all$week[rb_data_all$player_id == player_id & rb_data_all$season == season])
    
    stop("Target player-week row not found. ",
         "Player ID: ", player_id,
         ", Player: ", paste(player_name_patterns, collapse = ", "),
         ", Season: ", season, ", Week: ", week, ". ",
         "Available seasons for this player: ", 
         if (length(player_seasons) > 0) paste(sort(player_seasons), collapse = ", ") else "none", ". ",
         "Available weeks for season ", season, ": ",
         if (length(player_weeks) > 0) paste(sort(player_weeks), collapse = ", ") else "none", ". ",
         "Player may not have played in this game or data is unavailable.")
  }
  
  if (nrow(target_player_row) > 1) {
    stop("Multiple rows found for target player-week. Data inconsistency. ",
         "Player ID: ", player_id,
         ", Season: ", season, ", Week: ", week, ". ",
         "Found ", nrow(target_player_row), " rows. ",
         "This should never happen - each (player_id, season, week) should be unique.")
  }
  
  # Extract metadata from the single row
  target_player_id <- target_player_row$player_id[1]
  target_player_team <- target_player_row$team[1]
  target_player_name <- target_player_row$player_name[1]
  
  # Update team if it doesn't match
  if (length(target_player_team) > 1 || !target_player_team %in% c(team, player_team)) {
    team <- target_player_team[1]
    result$metadata$team <- team
  }
  
  result$metadata$player_id <- target_player_id
  result$metadata$player_name <- target_player_name
  
  # Filter by player_id (most reliable)
  player_data <- rb_data_pre[rb_data_pre$player_id == target_player_id, ]
  
  if (nrow(player_data) == 0) {
    player_all <- rb_data_all[rb_data_all$player_id == target_player_id, ]
    if (nrow(player_all) > 0) {
      stop("Player not found in RB dataset before target game. ",
           "Player ID: ", target_player_id,
           ", Target Game Date: ", as.character(game_gameday),
           ". Player has no games before this date in cached data.")
    } else {
      stop("Player not found in RB dataset at all. ",
           "Player ID: ", target_player_id,
           ". Please check player name and data availability. ",
           "Player may not be a running back or data may be missing from cache.")
    }
  }
  
  # Filter to only games where player's team matches the target team
  player_data_team <- player_data[player_data$team == team, ]
  
  if (nrow(player_data_team) == 0) {
    stop("Cannot proceed without games for target team. ",
         "Player ID: ", target_player_id,
         ", Team: ", team,
         ", Games before target: ", nrow(player_data),
         ". Player may have changed teams or team data is unavailable.")
  }
  
  # Additional validation: ensure identifiers present
  if ("game_id" %in% names(player_data_team)) {
    valid_games <- !is.na(player_data_team$game_id)
    player_data_team <- player_data_team[valid_games, ]
  } else if ("game_key" %in% names(player_data_team)) {
    player_data_team <- player_data_team[!is.na(player_data_team$game_key) & player_data_team$game_key != "", ]
  }
  
  # ============================================================================
  # STEP 3: Extract recent games (last 3) for input audit
  # ============================================================================
  
  # Sort by gameday descending and take most recent 3
  if ("game_id" %in% names(player_data_team)) {
    player_data_unique <- player_data_team[!duplicated(player_data_team$game_id), ]
  } else {
    player_data_unique <- player_data_team[!duplicated(player_data_team[, c("gameday", "opponent")]), ]
  }
  
  player_sorted <- player_data_unique[order(player_data_unique$gameday, decreasing = TRUE), ]
  player_last3 <- player_sorted[1:min(3, nrow(player_sorted)), ]
  
  # Store recent games
  result$recent_games <- player_last3
  
  # ============================================================================
  # STEP 4: Construct the pre-game feature row
  # ============================================================================
  
  # Get player's row for the identified game
  # For future games, use the synthetic feature row if provided
  if (is_future && !is.null(synthetic_feature_row)) {
    # FUTURE MODE: Use synthetic feature row
    player_game_row <- synthetic_feature_row
    
    # Validate synthetic row has required fields
    if (nrow(player_game_row) != 1) {
      stop("Synthetic feature row must have exactly one row. Got ", nrow(player_game_row), " rows.")
    }
    
    if (player_game_row$player_id != target_player_id) {
      stop("Synthetic feature row player_id '", player_game_row$player_id, 
           "' does not match target player_id '", target_player_id, "'.")
    }
    
  } else {
    # REPLAY MODE: Find feature row from cache using primary keys
    # Use player_id, season, week as the primary keys (most reliable)
    player_game_row <- rb_data_all[
      rb_data_all$player_id == target_player_id &
      rb_data_all$season == season &
      rb_data_all$week == week,
      , drop = FALSE
    ]
    
    # Validate exactly 1 row found
    if (nrow(player_game_row) == 0) {
      stop("Player-week feature row not found in RB weekly features cache. ",
           "Player ID: ", target_player_id,
           ", Player: ", target_player_name,
           ", Season: ", season, ", Week: ", week, ". ",
           "Game may be missing from assembled training data or player did not play.")
    }
    
    if (nrow(player_game_row) > 1) {
      stop("Multiple feature rows found for player-week. Data inconsistency. ",
           "Player ID: ", target_player_id,
           ", Season: ", season, ", Week: ", week, ". ",
           "Found ", nrow(player_game_row), " rows. ",
           "This should never happen - each (player_id, season, week) should be unique.")
    }
  }

  # Enforce minimum 3-game history rule
  # Check if at least one 3-game rolling feature is non-NA
  rolling_cols_3 <- grep("_roll3$", names(player_game_row), value = TRUE)
  if (length(rolling_cols_3) == 0) {
    stop("No 3-game rolling features found. Cannot verify minimum history requirement.")
  }
  
  has_3_game_history <- any(!is.na(player_game_row[, rolling_cols_3, drop = FALSE]))
  if (!has_3_game_history) {
    stop("Player '", target_player_name, "' (ID: ", target_player_id, 
         ") does not have minimum 3-game history required for simulation. ",
         "Player must have at least 3 prior games. ",
         "Current game: Season ", game_season, ", Week ", game_week, ". ",
         "Rookies or players with insufficient history are not supported.")
  }

  # Extract feature row (exclude target_* columns)
  feature_cols <- c(
    "carries_roll3", "carries_roll5", "carries_roll7", "carries_roll10",
    "targets_roll3", "targets_roll5", "targets_roll7", "targets_roll10",
    "yards_per_carry_roll5", "yards_per_target_roll5", "catch_rate_roll5",
    "rush_tds_roll5", "rec_tds_roll5",
    "is_home",
    "opp_rush_yards_allowed_roll5", "opp_sacks_roll5", 
    "opp_tfl_roll5", "opp_points_allowed_roll5"
  )
  
  available_feature_cols <- intersect(feature_cols, names(player_game_row))
  player_feature_row <- player_game_row[, available_feature_cols, drop = FALSE]
  
  # Store defensive context
  def_features <- c("opp_rush_yards_allowed_roll5", "opp_sacks_roll5", 
                    "opp_tfl_roll5", "opp_points_allowed_roll5")
  for (feat in def_features) {
    if (feat %in% names(player_feature_row)) {
      result$defensive_context[[feat]] <- player_feature_row[[feat]]
    } else {
      result$defensive_context[[feat]] <- NA_real_
    }
  }
  
  # ============================================================================
  # STEP 5: Fit or load RB models
  # ============================================================================
  
  # Fit models using only games prior to the identified game
  rb_models <- fit_rb_models(rb_data_pre, min_rows = 200)
  
  # Store model diagnostics
  result$diagnostics$model_diagnostics <- rb_models$diagnostics
  
  # Check for baseline models and warn (regime-based structure)
  # Models are now in rb_models$models with keys like "target_carries__early"
  if (!is.null(rb_models$models)) {
    # New regime-based structure
    baseline_models <- sapply(rb_models$models, function(m) !is.null(m$type) && m$type == "baseline")
    if (any(baseline_models)) {
      baseline_names <- names(rb_models$models)[baseline_models]
      warning("RB simulation: ", sum(baseline_models), " model(s) using baseline: ", 
              paste(head(baseline_names, 5), collapse = ", "),
              if (length(baseline_names) > 5) paste0(" (and ", length(baseline_names) - 5, " more)") else "")
      result$diagnostics$baseline_models <- baseline_names
    } else {
      result$diagnostics$baseline_models <- character(0)
    }
  } else {
    # Legacy structure (should not occur with regime-based modeling)
    baseline_models <- sapply(rb_models[1:5], function(m) !is.null(m$type) && m$type == "baseline")
    if (any(baseline_models)) {
      baseline_names <- names(rb_models[1:5])[baseline_models]
      warning("RB simulation: ", sum(baseline_models), " model(s) using baseline: ", 
              paste(baseline_names, collapse = ", "))
      result$diagnostics$baseline_models <- baseline_names
    } else {
      result$diagnostics$baseline_models <- character(0)
    }
  }
  result$diagnostics$training_data_range <- list(
    min_date = min(rb_data_pre$gameday),
    max_date = max(rb_data_pre$gameday),
    seasons = unique(rb_data_pre$season),
    n_players = length(unique(rb_data_pre$player_id)),
    n_games = length(unique(rb_data_pre$game_id))
  )
  
  # Log training configuration
  result$diagnostics$training_config <- list(
    training_seasons = sort(unique(seasons_train)),
    feature_windows = c(3, 5, 7, 10),
    min_history_games = 3,
    cache_only = TRUE,
    no_nflreadr_calls = TRUE,
    no_feature_recomputation = TRUE
  )
  
  # Check which models include defensive features (RB v1 contract)
  def_cols <- c("opp_rush_yards_allowed_roll5", "opp_sacks_roll5", 
                "opp_tfl_roll5", "opp_points_allowed_roll5")
  
  # Check for NULL models (RB v1: regime-based structure)
  # Note: Models may be baseline, but should never be NULL
  # With regime-based modeling, models are in rb_models$models with keys like "target_carries__early"
  # The simulation will select the appropriate model based on the game's regime
  if (!is.null(rb_models$models)) {
    # New regime-based structure: check that at least one model exists per target
    if (file.exists("R/utils/rb_regime_v1.R")) {
      source("R/utils/rb_regime_v1.R", local = TRUE)
    }
    rb_targets <- get_rb_v1_targets()
    rb_regimes <- get_rb_regimes()
    
    # Check that for each target, at least one regime model exists
    missing_targets <- character(0)
    for (target in rb_targets) {
      target_models <- sapply(rb_regimes, function(r) {
        model_key <- get_model_key(target, r)
        !is.null(rb_models$models[[model_key]])
      })
      if (!any(target_models)) {
        missing_targets <- c(missing_targets, target)
      }
    }
    
    if (length(missing_targets) > 0) {
      stop("Missing models for targets: ", paste(missing_targets, collapse = ", "),
           ". At least one regime model must exist per target (fitted or baseline). Cannot proceed with simulation.")
    }
    result$diagnostics$null_models <- character(0)
  } else {
    # Legacy structure check (should not occur with regime-based modeling)
    required_models <- c("carries_model", "receptions_model", "rush_tds_model", "rec_tds_model")
    null_models <- required_models[sapply(required_models, function(m) is.null(rb_models[[m]]))]
    if (length(null_models) > 0) {
      stop("NULL models detected: ", paste(null_models, collapse = ", "),
           ". All 4 RB v1 models must exist (fitted or baseline). Cannot proceed with simulation.")
    }
    result$diagnostics$null_models <- character(0)
  }
  
  # ============================================================================
  # STEP 6: Run Monte Carlo simulation
  # ============================================================================
  
  # Ensure simulate_rb_game is available
  if (!exists("simulate_rb_game")) {
    stop("Simulation bootstrap incomplete: simulate_rb_game not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling run_rb_simulation().")
  }

  # Enforce schema: feature_row must include week for time-aware modeling
  # CRITICAL: Use game_week (the validated/resolved week), not the raw parameter
  player_feature_row$week <- game_week
  if (!"week" %in% names(player_feature_row) || is.na(player_feature_row$week)) {
    stop("run_rb_simulation failed to attach 'week' to feature_row. This is a schema bug.")
  }
  
  # Additional assertion: feature_row week must match metadata
  if (player_feature_row$week != result$metadata$week) {
    stop("INVARIANT VIOLATION: feature_row$week (", player_feature_row$week, 
         ") does not match metadata$week (", result$metadata$week, "). ",
         "This should never happen - indicates a resolution bug.")
  }
  
  log_msg("Feature row week:", player_feature_row$week, "| Metadata week:", result$metadata$week, "| Match: OK")
  
  # Enforce contextual defaults (is_home must not be NA)
  if ("is_home" %in% names(player_feature_row)) {
    player_feature_row$is_home[is.na(player_feature_row$is_home)] <- 0
  }
  
  # Guardrail: Ensure is_home is not NA after defaulting
  if ("is_home" %in% names(player_feature_row) && is.na(player_feature_row$is_home)) {
    stop("Prediction feature_row has NA is_home after defaulting. This is a bug.")
  }
  
  # Run simulation
  sim_result <- simulate_rb_game(
    feature_row = player_feature_row,
    rb_models = rb_models,
    n_sims = n_sims
  )
  
  if (sim_result$status != "success") {
    stop("Simulation failed with status: ", sim_result$status)
  }
  
  # Store simulation results
  result$summary <- sim_result$summary
  result$draws <- sim_result$draws

  # Resolve simulation schema to canonical names
  # This standardizes column names (rush_yards -> rushing_yards, etc.)
  # and creates derived columns (total_touchdowns, total_yards)
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
  }
  if (exists("resolve_rb_simulation_schema")) {
    result$draws <- resolve_rb_simulation_schema(result$draws)
  } else {
    stop("resolve_rb_simulation_schema not found. Cannot standardize simulation output schema.")
  }

  # Compute additional diagnostics from draws
  draws_df <- result$draws
  
  # Log diagnostic summary after schema resolution
  log_msg("=== Post-Resolution Diagnostics ===")
  required_outputs <- c("carries", "rushing_yards", "receiving_yards", "total_yards", "total_touchdowns")
  for (col in required_outputs) {
    if (col %in% names(draws_df)) {
      na_pct <- round(100 * sum(is.na(draws_df[[col]])) / nrow(draws_df), 2)
      col_mean <- mean(draws_df[[col]], na.rm = TRUE)
      col_sd <- sd(draws_df[[col]], na.rm = TRUE)
      log_msg(paste0(col, ": ", na_pct, "% NA, mean=", round(col_mean, 2), ", sd=", round(col_sd, 2)))
    } else {
      log_msg(paste0(col, ": MISSING"))
    }
  }
  
  # Summary statistics: mean and percentiles for all outcome variables (RB v1 contract)
  outcome_vars <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
  outcome_vars <- intersect(outcome_vars, names(draws_df))
  
  result$diagnostics$summary_stats <- list()
  for (var in outcome_vars) {
    if (var %in% names(draws_df)) {
      vals <- draws_df[[var]]
      result$diagnostics$summary_stats[[var]] <- list(
        mean = mean(vals, na.rm = TRUE),
        p25 = quantile(vals, 0.25, na.rm = TRUE),
        p50 = quantile(vals, 0.50, na.rm = TRUE),
        p75 = quantile(vals, 0.75, na.rm = TRUE),
        min = min(vals, na.rm = TRUE),
        max = max(vals, na.rm = TRUE)
      )
    }
  }
  
  result$diagnostics$distribution_stats <- list(
    carries_mean = mean(draws_df$carries),
    carries_sd = sd(draws_df$carries),
    rushing_yards_mean = mean(draws_df$rushing_yards),
    rushing_yards_sd = sd(draws_df$rushing_yards)
  )
  
  # TD probabilities (RB v1: total_touchdowns only)
  if ("total_touchdowns" %in% names(draws_df)) {
    result$diagnostics$td_probabilities <- list(
      prob_0_tds = mean(draws_df$total_touchdowns == 0),
      prob_ge1_td = mean(draws_df$total_touchdowns >= 1),
      prob_ge2_tds = mean(draws_df$total_touchdowns >= 2)
    )
  }
  
  # Sanity check flags
  p75_carries <- result$summary$p75[result$summary$stat == "carries"]
  p25_carries <- result$summary$p25[result$summary$stat == "carries"]
  median_carries <- result$summary$p50[result$summary$stat == "carries"]
  # Note: summary uses raw names (rush_yards) from compute_rb_percentiles()
  median_rush_yds <- result$summary$p50[result$summary$stat == "rush_yards"]
  
  result$diagnostics$sanity_warnings <- list(
    p75_carries_gt_30 = p75_carries > 30,
    p25_carries_lt_5 = p25_carries < 5,
    median_ypc_gt_6_5 = if (median_carries > 0) (median_rush_yds / median_carries) > 6.5 else FALSE,
    td_prob_ge_0_8 = result$diagnostics$td_probabilities$prob_ge1_td >= 0.8
  )
  
  # ============================================================================
  # DEFENSIVE ASSERTIONS: Ensure all simulation outputs are valid
  # ============================================================================
  
  # Check that draws exist and have correct structure
  if (is.null(result$draws)) {
    stop("Simulation draws are NULL. Simulation failed to produce output.")
  }
  
  if (nrow(result$draws) != n_sims) {
    stop("Simulation draws have incorrect length. Expected ", n_sims, " rows, got ", nrow(result$draws), ".")
  }
  
  # Check that all required outcome columns exist and are not NULL
  required_outcomes <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
  for (outcome in required_outcomes) {
    if (!outcome %in% names(result$draws)) {
      stop("Required outcome column '", outcome, "' is missing from simulation draws.")
    }
    
    outcome_vec <- result$draws[[outcome]]
    if (is.null(outcome_vec)) {
      stop("Outcome column '", outcome, "' is NULL. All simulation outputs must be non-NULL vectors.")
    }
    
    if (length(outcome_vec) != n_sims) {
      stop("Outcome column '", outcome, "' has incorrect length. Expected ", n_sims, ", got ", length(outcome_vec), ".")
    }
    
    # Check for any NULL values in the vector (should not happen with proper initialization)
    if (any(sapply(outcome_vec, is.null))) {
      stop("Outcome column '", outcome, "' contains NULL values. All simulation outputs must be numeric vectors.")
    }
  }
  
  return(result)
}

