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
#   - R/data/load_schedules.R
#   - R/data/load_player_stats.R
#   - R/features/build_rb_features.R
#   - R/utils/rolling_helpers.R
#   - R/assemble/assemble_rb_training_data.R
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
run_rb_simulation <- function(player_name, team, opponent, season, week, n_sims = 5000, game_date = NULL) {
  
  # Initialize result structure
  result <- list(
    metadata = list(
      game_id = NULL,
      player_id = NULL,
      player_name = NULL,
      team = NULL,
      opponent = NULL,
      season = NULL,
      week = NULL,
      game_date = NULL
    ),
    recent_games = data.frame(),
    defensive_context = list(),
    summary = data.frame(),
    diagnostics = list(),
    draws = data.frame()
  )
  
  # Ensure player_name is a vector for pattern matching
  if (is.character(player_name) && length(player_name) == 1) {
    player_name_patterns <- c(player_name)
  } else {
    player_name_patterns <- player_name
  }
  
  # ============================================================================
  # STEP 1: Identify the correct game
  # ============================================================================
  
  # Load schedules for adjacent seasons (include previous season for context)
  seasons_to_load <- c(season - 1, season)
  schedules <- load_schedules(seasons = seasons_to_load)
  
  if (nrow(schedules) == 0) {
    stop("Failed to load schedules. Cannot proceed.")
  }
  
  # Filter to target game
  if (is.null(game_date)) {
    # Find game by season, week, team, opponent
    target_games <- schedules[
      schedules$season == season & 
      schedules$week == week &
      ((schedules$away_team == team & schedules$home_team == opponent) |
       (schedules$home_team == team & schedules$away_team == opponent)),
    ]
  } else {
    # Find game by date and matchup
    target_games <- schedules[
      schedules$gameday == game_date &
      ((schedules$away_team == team & schedules$home_team == opponent) |
       (schedules$home_team == team & schedules$away_team == opponent)),
    ]
  }
  
  # Validation: exactly one game
  if (nrow(target_games) == 0) {
    stop("No game found matching: ", team, " vs ", opponent, " (season ", season, ", week ", week, ")")
  }
  
  if (nrow(target_games) > 1) {
    stop("Multiple games found matching: ", team, " vs ", opponent, " (season ", season, ", week ", week, ")")
  }
  
  # Extract game info
  identified_game <- target_games[1, ]
  game_id <- identified_game$game_id
  game_season <- identified_game$season
  game_week <- identified_game$week
  game_gameday <- identified_game$gameday
  
  # Determine if player's team is home or away
  is_player_home <- (identified_game$home_team == team)
  player_opponent <- if (is_player_home) identified_game$away_team else identified_game$home_team
  
  # Store metadata
  result$metadata$game_id <- game_id
  result$metadata$season <- game_season
  result$metadata$week <- game_week
  result$metadata$game_date <- game_gameday
  result$metadata$team <- team
  result$metadata$opponent <- player_opponent
  
  # ============================================================================
  # STEP 2: Assemble RB data up to (but not including) this game
  # ============================================================================
  
  # Assemble RB training data
  rb_data <- assemble_rb_training_data(seasons = seasons_to_load)
  
  if (nrow(rb_data) == 0) {
    stop("Failed to assemble RB training data. Cannot proceed.")
  }
  
  # Filter to games before the identified game
  rb_data_pre <- rb_data[rb_data$gameday < game_gameday, ]
  
  # Find target player in the target game to get their player_id
  target_player_game <- NULL
  for (pattern in player_name_patterns) {
    target_player_game <- rb_data[rb_data$game_id == game_id & 
                                   rb_data$player_name == pattern, ]
    if (nrow(target_player_game) > 0) break
  }
  
  # If exact match failed, try case-insensitive partial match
  if (nrow(target_player_game) == 0 && length(player_name_patterns) > 0) {
    pattern_parts <- strsplit(player_name_patterns[1], "[. ]+")[[1]]
    if (length(pattern_parts) >= 2) {
      target_player_game <- rb_data[rb_data$game_id == game_id & 
                                    grepl(pattern_parts[1], rb_data$player_name, ignore.case = TRUE) &
                                    grepl(pattern_parts[2], rb_data$player_name, ignore.case = TRUE), ]
    }
  }
  
  if (nrow(target_player_game) == 0) {
    stop("Target player not found in target game (", game_id, "). Cannot determine player_id.")
  }
  
  # Get the player_id and verify team matches
  target_player_id <- unique(target_player_game$player_id)
  if (length(target_player_id) > 1) {
    stop("Multiple player_ids found for target player in target game. Data inconsistency.")
  }
  
  target_player_team <- unique(target_player_game$team)
  target_player_name <- unique(target_player_game$player_name)[1]
  
  # Update team if it doesn't match
  if (length(target_player_team) > 1 || !target_player_team %in% c(team, identified_game$home_team, identified_game$away_team)) {
    team <- target_player_team[1]
    result$metadata$team <- team
  }
  
  result$metadata$player_id <- target_player_id
  result$metadata$player_name <- target_player_name
  
  # Filter by player_id (most reliable)
  player_data <- rb_data_pre[rb_data_pre$player_id == target_player_id, ]
  
  if (nrow(player_data) == 0) {
    player_all <- rb_data[rb_data$player_id == target_player_id, ]
    if (nrow(player_all) > 0) {
      stop("Player not found in RB dataset before ", as.character(game_gameday), ".")
    } else {
      stop("Player not found in RB dataset at all. Please check player name and data availability.")
    }
  }
  
  # Filter to only games where player's team matches the target team
  player_data_team <- player_data[player_data$team == team, ]
  
  if (nrow(player_data_team) == 0) {
    stop("Cannot proceed without games for target team (", team, ")")
  }
  
  # Additional validation: ensure game_id contains the team (sanity check)
  if ("game_id" %in% names(player_data_team)) {
    valid_games <- grepl(team, player_data_team$game_id)
    player_data_team <- player_data_team[valid_games, ]
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
  player_game_row <- rb_data[rb_data$game_id == game_id & 
                             rb_data$player_id == target_player_id, ]
  
  if (nrow(player_game_row) == 0) {
    stop("Player (player_id: ", target_player_id, ") not found in dataset for game_id: ", game_id)
  }
  
  if (nrow(player_game_row) > 1) {
    player_game_row <- player_game_row[1, ]
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
  
  # Check which models include defensive features
  def_cols <- c("opp_rush_yards_allowed_roll5", "opp_sacks_roll5", 
                "opp_tfl_roll5", "opp_points_allowed_roll5")
  
  def_in_rush_yds <- FALSE
  def_features_rush_yds <- character(0)
  if (!is.null(rb_models$rush_yards_model)) {
    model_vars <- all.vars(formula(rb_models$rush_yards_model))
    def_features_rush_yds <- intersect(def_cols, model_vars)
    def_in_rush_yds <- length(def_features_rush_yds) > 0
  }
  
  def_in_rush_tds <- FALSE
  def_features_rush_tds <- character(0)
  if (!is.null(rb_models$rush_tds_model)) {
    model_vars <- all.vars(formula(rb_models$rush_tds_model))
    def_features_rush_tds <- intersect(def_cols, model_vars)
    def_in_rush_tds <- length(def_features_rush_tds) > 0
  }
  
  result$diagnostics$model_features <- list(
    rush_yards_defensive = def_features_rush_yds,
    rush_tds_defensive = def_features_rush_tds
  )
  
  # Check for NULL models
  null_models <- names(rb_models)[sapply(rb_models[1:7], is.null)]
  result$diagnostics$null_models <- null_models
  
  # ============================================================================
  # STEP 6: Run Monte Carlo simulation
  # ============================================================================
  
  # Calculate player's historical fumble rate for fumble modeling
  player_historical <- player_data_team
  if (nrow(player_historical) > 0 && "fumbles_lost" %in% names(player_historical)) {
    total_carries_hist <- sum(player_historical$carries, na.rm = TRUE)
    total_fumbles_hist <- sum(player_historical$fumbles_lost, na.rm = TRUE)
    if (total_carries_hist > 0) {
      fumble_rate_per_carry <- total_fumbles_hist / total_carries_hist
    } else {
      fumble_rate_per_carry <- 0.001  # Default: 0.1% per carry
    }
  } else {
    fumble_rate_per_carry <- 0.001  # Default: 0.1% per carry
  }
  
  result$diagnostics$fumble_rate_per_carry <- fumble_rate_per_carry
  result$diagnostics$fumble_rate_source <- if (nrow(player_historical) > 0 && 
                                                "fumbles_lost" %in% names(player_historical) &&
                                                sum(player_historical$carries, na.rm = TRUE) > 0) {
    "player_historical"
  } else {
    "default"
  }
  
  # Run simulation
  sim_result <- simulate_rb_game(
    feature_row = player_feature_row,
    rb_models = rb_models,
    n_sims = n_sims,
    fumble_rate_per_carry = fumble_rate_per_carry
  )
  
  if (sim_result$status != "success") {
    stop("Simulation failed with status: ", sim_result$status)
  }
  
  # Store simulation results
  result$summary <- sim_result$summary
  result$draws <- sim_result$draws
  
  # Compute additional diagnostics from draws
  draws_df <- sim_result$draws
  result$diagnostics$distribution_stats <- list(
    carries_mean = mean(draws_df$carries),
    carries_sd = sd(draws_df$carries),
    rush_yards_mean = mean(draws_df$rush_yards),
    rush_yards_sd = sd(draws_df$rush_yards)
  )
  
  # TD probabilities
  total_tds <- draws_df$rush_tds + draws_df$rec_tds
  result$diagnostics$td_probabilities <- list(
    prob_0_tds = mean(total_tds == 0),
    prob_ge1_td = mean(total_tds >= 1),
    prob_ge2_tds = mean(total_tds >= 2)
  )
  
  # Sanity check flags
  p75_carries <- result$summary$p75[result$summary$stat == "carries"]
  p25_carries <- result$summary$p25[result$summary$stat == "carries"]
  median_carries <- result$summary$p50[result$summary$stat == "carries"]
  median_rush_yds <- result$summary$p50[result$summary$stat == "rush_yards"]
  
  result$diagnostics$sanity_warnings <- list(
    p75_carries_gt_30 = p75_carries > 30,
    p25_carries_lt_5 = p25_carries < 5,
    median_ypc_gt_6_5 = if (median_carries > 0) (median_rush_yds / median_carries) > 6.5 else FALSE,
    td_prob_ge_0_8 = result$diagnostics$td_probabilities$prob_ge1_td >= 0.8
  )
  
  return(result)
}

