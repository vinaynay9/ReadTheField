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
                              mode_policy = NULL) {
  
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
    if (file.exists("R/utils/cache_helpers.R")) {
      source("R/utils/cache_helpers.R", local = TRUE)
    }
  }
  
  # Determine game key if not provided
  resolved_game_key <- if (!is.null(game_key) && !is.na(game_key) && game_key != "") {
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
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required to load RB weekly features")
    }
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
  if (!is.null(resolved_game_key)) {
    target_rows <- target_rows[!is.na(target_rows$game_key) & target_rows$game_key == resolved_game_key, ]
  }
  if (nrow(target_rows) == 0 && !is.null(game_id)) {
    target_rows <- rb_data_all[rb_data_all$game_id == game_id & (is.null(player_id) | rb_data_all$player_id == player_id), ]
  }
  if (nrow(target_rows) == 0 && !is.null(game_date)) {
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
  if (!is.null(identified_game_row$game_key) && !is.na(identified_game_row$game_key) && identified_game_row$game_key != "") {
    resolved_game_key <- identified_game_row$game_key
  }
  
  # Extract game info
  game_id <- ifelse(!is.null(identified_game_row$game_id) && identified_game_row$game_id != "",
                    identified_game_row$game_id, game_id)
  game_season <- ifelse(!is.na(identified_game_row$season), identified_game_row$season, season)
  game_week <- ifelse(!is.na(identified_game_row$week), identified_game_row$week, week)
  game_gameday <- ifelse(!is.na(identified_game_row$gameday), identified_game_row$gameday, game_date)
  
  # Determine opponent / home-away
  player_opponent <- ifelse(!is.na(identified_game_row$opponent) && identified_game_row$opponent != "",
                            identified_game_row$opponent, opponent)
  player_team <- ifelse(!is.na(identified_game_row$team) && identified_game_row$team != "", identified_game_row$team, team)
  player_home_away <- ifelse(!is.na(identified_game_row$home_away) && identified_game_row$home_away != "",
                             identified_game_row$home_away, home_away)
  
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
    # Exclude target game by game_key if it exists
    if (!is.na(resolved_game_key) && resolved_game_key != "") {
      rb_data_pre <- rb_data_pre[is.na(rb_data_pre$game_key) | rb_data_pre$game_key != resolved_game_key, ]
    }
    # Exclude by game_id if available
    if (!is.null(game_id) && !is.na(game_id) && game_id != "") {
      rb_data_pre <- rb_data_pre[is.na(rb_data_pre$game_id) | rb_data_pre$game_id != game_id, ]
    }
  }
  
  # Find target player in the target game to get their player_id
  target_player_game <- NULL
  for (pattern in player_name_patterns) {
    target_player_game <- rb_data_all[rb_data_all$game_key == resolved_game_key & 
                                       rb_data_all$player_name == pattern, ]
    if (nrow(target_player_game) > 0) break
  }
  
  # If exact match failed, try case-insensitive partial match
  if (nrow(target_player_game) == 0 && length(player_name_patterns) > 0) {
    pattern_parts <- strsplit(player_name_patterns[1], "[. ]+")[[1]]
    if (length(pattern_parts) >= 2) {
    target_player_game <- rb_data_all[rb_data_all$game_key == resolved_game_key & 
                                      grepl(pattern_parts[1], rb_data_all$player_name, ignore.case = TRUE) &
                                      grepl(pattern_parts[2], rb_data_all$player_name, ignore.case = TRUE), ]
    }
  }
  if (nrow(target_player_game) == 0 && !is.null(game_id)) {
    target_player_game <- rb_data_all[rb_data_all$game_id == game_id & 
                                      (rb_data_all$player_id == player_id | rb_data_all$player_name %in% player_name_patterns), ]
  }
  
  if (nrow(target_player_game) == 0) {
    stop("Target player not found in target game. Cannot determine player_id. ",
         "Player: ", paste(player_name_patterns, collapse = ", "),
         ", Game ID: ", ifelse(is.null(game_id), "NULL", game_id),
         ", Game Key: ", ifelse(is.null(resolved_game_key) || is.na(resolved_game_key), "NULL", resolved_game_key),
         ". Player may not have played in this game or data is unavailable.")
  }
  
  # Get the player_id and verify team matches
  target_player_id <- unique(target_player_game$player_id)
  if (length(target_player_id) > 1) {
    stop("Multiple player_ids found for target player in target game. Data inconsistency.")
  }
  
  target_player_team <- unique(target_player_game$team)
  target_player_name <- unique(target_player_game$player_name)[1]
  
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
  player_game_row <- rb_data_all[rb_data_all$game_key == resolved_game_key & 
                                 rb_data_all$player_id == target_player_id, ]
  if (nrow(player_game_row) == 0 && !is.null(game_id)) {
    player_game_row <- rb_data_all[rb_data_all$game_id == game_id & rb_data_all$player_id == target_player_id, ]
  }
  if (nrow(player_game_row) == 0 && !is.null(game_gameday)) {
    player_game_row <- rb_data_all[rb_data_all$player_id == target_player_id & rb_data_all$gameday == game_gameday & rb_data_all$team == team, ]
  }
  
  if (nrow(player_game_row) == 0) {
    stop("Player not found in dataset for target game. ",
         "Player ID: ", target_player_id,
         ", Game Key: ", ifelse(is.null(resolved_game_key) || is.na(resolved_game_key), "NULL", resolved_game_key),
         ", Game ID: ", ifelse(is.null(game_id) || is.na(game_id), "NULL", game_id),
         ". Game may be missing from assembled training data or player did not play.")
  }
  
  if (nrow(player_game_row) > 1) {
    player_game_row <- player_game_row[1, ]
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
  rb_models <- fit_rb_models(rb_data_pre)
  
  # Store model diagnostics
  result$diagnostics$model_info <- rb_models$model_info
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
  
  def_in_rush_yds <- FALSE
  def_features_rush_yds <- character(0)
  if (!is.null(rb_models$rushing_yards_model)) {
    model_vars <- all.vars(formula(rb_models$rushing_yards_model))
    def_features_rush_yds <- intersect(def_cols, model_vars)
    def_in_rush_yds <- length(def_features_rush_yds) > 0
  }
  
  result$diagnostics$model_features <- list(
    rushing_yards_defensive = def_features_rush_yds
  )
  
  # Check for NULL models (RB v1: exactly 5 models)
  null_models <- names(rb_models)[sapply(rb_models[1:5], is.null)]
  if (length(null_models) > 0) {
    stop("NULL models detected: ", paste(null_models, collapse = ", "),
         ". All 5 RB v1 models must be fitted. Cannot proceed with simulation.")
  }
  result$diagnostics$null_models <- character(0)
  
  # ============================================================================
  # STEP 6: Run Monte Carlo simulation
  # ============================================================================
  
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

  # Compute additional diagnostics from draws
  draws_df <- sim_result$draws
  
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
  median_rush_yds <- result$summary$p50[result$summary$stat == "rushing_yards"]
  
  result$diagnostics$sanity_warnings <- list(
    p75_carries_gt_30 = p75_carries > 30,
    p25_carries_lt_5 = p25_carries < 5,
    median_ypc_gt_6_5 = if (median_carries > 0) (median_rush_yds / median_carries) > 6.5 else FALSE,
    td_prob_ge_0_8 = result$diagnostics$td_probabilities$prob_ge1_td >= 0.8
  )
  
  return(result)
}

