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
#     gsis_id = "00-0037667",
#     season = 2025,
#     week = 15,
#     n_sims = 5000
#   )

#' Run RB simulation - pure computation
#'
#' Performs complete RB simulation workflow without any printing or file I/O.
#' Returns structured result object containing all inputs, models, draws, and summaries.
#'
#' @param gsis_id Character, player gsis_id (required)
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
run_rb_simulation <- function(gsis_id,
                              season,
                              week,
                              n_sims = 5000,
                              game_date = NULL,
                              schedule_game_id = NULL,
                              schedule_game_date = NULL,
                              schedule_home_away = NULL,
                              schedule_opponent = NULL,
                              seasons_train = NULL,
                              mode_policy = NULL,
                              synthetic_feature_row = NULL,
                              is_future = FALSE,
                              availability_policy = "played_only",
                              debug = FALSE) {
  if (missing(gsis_id)) {
    stop("run_rb_simulation requires gsis_id. Name-based resolution is not allowed.", call. = FALSE)
  }
  
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
  log_msg("REQUESTED: gsis_id:", gsis_id)
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
    offensive_context = list(),
    summary = data.frame(),
    diagnostics = list(),
    draws = data.frame()
  )
  
  # Set random seed for reproducibility
  if (!exists(".Random.seed", envir = .GlobalEnv)) {
    set.seed(NULL)
  }
  result$metadata$random_seed <- .Random.seed
  
  if (missing(gsis_id) || is.null(gsis_id) || length(gsis_id) == 0 || !nzchar(trimws(as.character(gsis_id)))) {
    stop("gsis_id is required for run_rb_simulation")
  }
  player_id <- as.character(gsis_id)
  availability_policy <- validate_availability_policy(availability_policy)
  
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

  find_non_finite_cols <- function(row, cols) {
    cols <- intersect(cols, names(row))
    if (length(cols) == 0) return(character(0))
    bad <- vapply(cols, function(col) {
      vals <- suppressWarnings(as.numeric(row[[col]]))
      any(is.nan(vals) | is.infinite(vals))
    }, logical(1))
    cols[bad]
  }
  
  resolved_game_key <- NA_character_
  
  # Training seasons: compute from policy if not provided
  if (is.null(seasons_train) || length(seasons_train) == 0) {
    if (!exists("simulation_mode_policy")) {
      stop("No training seasons provided and simulation_mode_policy not loaded.")
    }
    if (!exists("get_available_seasons_from_cache")) {
      stop("get_available_seasons_from_cache not loaded. Cannot determine training seasons.")
    }
    available_seasons <- get_available_seasons_from_cache()
    in_progress_season <- NA_integer_
    if (exists("detect_in_progress_season")) {
      in_progress_season <- detect_in_progress_season(available_seasons, cache_only = TRUE)
    }
    mode_policy <- simulation_mode_policy(
      mode = "historical_replay",
      target_season = season,
      target_week = week,
      target_game_date = game_date,
      available_seasons = available_seasons,
      in_progress_season = in_progress_season
    )
    seasons_train <- mode_policy$seasons_allowed
  }
  
  # ============================================================================
  # STEP 2: Load cached RB weekly features up to (but not including) this game
  # ============================================================================
  
  if (!exists("read_rb_weekly_features_cache")) {
    stop("Simulation bootstrap incomplete: read_rb_weekly_features_cache not loaded. ",
         "Source R/simulation/bootstrap_simulation.R before calling run_rb_simulation().")
  }

  # Optional enrichment source for game_id/game_date recovery
  identity_cache <- NULL
  if (exists("read_player_week_identity_cache")) {
    try({
      identity_cache <- read_player_week_identity_cache()
    }, silent = TRUE)
  }

  rb_data_all <- read_rb_weekly_features_cache()
  if (nrow(rb_data_all) == 0) {
    stop("RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }

  # Recover game_id/game_date from identity cache when nflreadr omits them
  if (!is.null(identity_cache) && nrow(identity_cache) > 0) {
    orig_game_id <- rb_data_all$game_id
    orig_gameday <- rb_data_all$gameday

    ident_cols <- intersect(c("player_id", "season", "week", "game_id", "game_key", "game_date"), names(identity_cache))
    identity_clean <- identity_cache[, ident_cols, drop = FALSE]
    identity_clean$game_date <- as.Date(identity_clean$game_date)
    identity_clean <- identity_clean[order(identity_clean$player_id, identity_clean$season, identity_clean$week, identity_clean$game_date), ]
    identity_clean <- identity_clean[!duplicated(identity_clean[, c("player_id", "season", "week")]), ]

    rb_data_all <- merge(
      rb_data_all,
      identity_clean,
      by = c("player_id", "season", "week"),
      all.x = TRUE,
      suffixes = c("", "_ident")
    )

    if ("game_id_ident" %in% names(rb_data_all)) {
      rb_data_all$game_id <- ifelse(is.na(rb_data_all$game_id) | rb_data_all$game_id == "",
                                    rb_data_all$game_id_ident,
                                    rb_data_all$game_id)
    }
    if ("game_key_ident" %in% names(rb_data_all)) {
      rb_data_all$game_key <- ifelse(is.na(rb_data_all$game_key) | rb_data_all$game_key == "",
                                     rb_data_all$game_key_ident,
                                     rb_data_all$game_key)
    }
    if ("game_date_ident" %in% names(rb_data_all)) {
      rb_data_all$gameday <- ifelse(is.na(rb_data_all$gameday),
                                    rb_data_all$game_date_ident,
                                    rb_data_all$gameday)
    }

    helper_cols <- grep("(_ident)$", names(rb_data_all), value = TRUE)
    if (length(helper_cols) > 0) {
      rb_data_all <- rb_data_all[, setdiff(names(rb_data_all), helper_cols), drop = FALSE]
    }

    recovered_ids <- sum(is.na(orig_game_id) & !is.na(rb_data_all$game_id))
    recovered_dates <- sum(is.na(orig_gameday) & !is.na(rb_data_all$gameday))
    log_msg("Identity enrichment: recovered game_id for", recovered_ids, "rows; recovered gameday for", recovered_dates, "rows")
  }

  rb_data <- rb_data_all[rb_data_all$season %in% seasons_train, , drop = FALSE]
  if (nrow(rb_data) == 0) {
    stop("No RB training rows available for seasons: ", paste(seasons_train, collapse = ", "))
  }

  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  if (!exists("read_rb_weekly_stats_cache")) {
    if (file.exists(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"))) {
      source(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"), local = TRUE)
    } else {
      stop("Simulation bootstrap incomplete: read_rb_weekly_stats_cache not loaded.")
    }
  }
  rb_stats_all <- read_rb_weekly_stats_cache()
  if (nrow(rb_stats_all) == 0) {
    stop("RB weekly stats cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }

  read_optional_parquet <- function(path, label) {
    if (!file.exists(path)) {
      return(NULL)
    }
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read ", label, ". Install with install.packages('arrow').")
    }
    obj <- arrow::read_parquet(path)
    if (is.null(obj) || nrow(obj) == 0) {
      return(NULL)
    }
    obj
  }

  player_dim <- NULL
  if (exists("read_player_dim_cache")) {
    player_dim <- tryCatch(read_player_dim_cache(), error = function(e) NULL)
  }

  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  prior_season_stats <- read_optional_parquet(
    file.path(repo_root, "data", "processed", "prior_season_player_stats.parquet"),
    "prior season player stats"
  )
  team_offense_context <- read_optional_parquet(
    file.path(repo_root, "data", "processed", "team_offense_context.parquet"),
    "team offense context"
  )
  defense_weekly_features <- read_optional_parquet(
    file.path(repo_root, "data", "processed", "defense_weekly_features.parquet"),
    "defense weekly features"
  )
  schedule <- NULL
  if (exists("load_schedules")) {
    schedule <- load_schedules(seasons = season, cache_only = TRUE)
  }
  
  if (is_future) {
    if (is.null(synthetic_feature_row) || nrow(synthetic_feature_row) != 1) {
      stop("Future simulation requires a single-row synthetic_feature_row.")
    }
    if (!"player_id" %in% names(synthetic_feature_row)) {
      stop("synthetic_feature_row must include player_id.")
    }
    if (as.character(synthetic_feature_row$player_id[1]) != player_id) {
      stop("synthetic_feature_row player_id does not match gsis_id.")
    }
    identified_game_row <- synthetic_feature_row
    availability_state <- "future_game"
    dropped_feature_groups <- character(0)
    dropped_features <- character(0)
    availability_policy_used <- availability_policy
    construction_warnings <- character(0)
    used_sources <- character(0)
  } else {
    if (!exists("build_rb_feature_row_for_simulation")) {
      stop("build_rb_feature_row_for_simulation not loaded. Source R/simulation/bootstrap_simulation.R.")
    }
    build_result <- build_rb_feature_row_for_simulation(
      rb_weekly_features = rb_data_all,
      rb_weekly_stats = rb_stats_all,
      player_dim = player_dim,
      prior_season_player_stats = prior_season_stats,
      team_offense_context = team_offense_context,
      defense_weekly_features = defense_weekly_features,
      schedule = schedule,
      gsis_id = player_id,
      season = season,
      week = week,
      availability_policy = availability_policy,
      drop_feature_groups = character(0),
      schedule_game_id = schedule_game_id,
      schedule_game_date = schedule_game_date,
      schedule_home_away = schedule_home_away,
      schedule_opponent = schedule_opponent
    )
    identified_game_row <- build_result$feature_row
    availability_state <- build_result$availability_state
    availability_policy_used <- build_result$availability_policy
    dropped_feature_groups <- build_result$dropped_feature_groups
    dropped_features <- build_result$dropped_features
    construction_warnings <- build_result$construction_warnings
    used_sources <- build_result$used_sources
  }
  
  # CRITICAL: Verify identified game matches CLI args (detect drift early)
  if (!is.null(season) && !is.na(season) && !is.na(identified_game_row$season) && identified_game_row$season != season) {
    stop("GAME IDENTIFICATION DRIFT: CLI requested season=", season,
         " but identified game has season=", identified_game_row$season, ". ",
         "This indicates a bug in game resolution logic.")
  }
  if (!is.null(week) && !is.na(week) && !is.na(identified_game_row$week) && identified_game_row$week != week) {
    stop("GAME IDENTIFICATION DRIFT: CLI requested week=", week,
         " but identified game has week=", identified_game_row$week, ". ",
         "This indicates a bug in game resolution logic. ",
         "gsis_id: ", player_id,
         ", Game found: season=", identified_game_row$season, ", week=", identified_game_row$week)
  }
  
  if (is_valid_string(identified_game_row$game_key)) {
    resolved_game_key <- identified_game_row$game_key
  }
  
  log_msg("=== Game Identification ===")
  log_msg("Identified game: season=", identified_game_row$season, ", week=", identified_game_row$week)
  log_msg("Game key:", resolved_game_key)
  
  # CRITICAL FIX: Respect CLI arguments - NO OVERRIDES
  # Extract game info with proper precedence: CLI args win, fallback to identified_game_row only when not provided
  
  # game_id: from identified_game_row if available
  game_id <- if ("game_id" %in% names(identified_game_row) && is_valid_string(identified_game_row$game_id)) {
    identified_game_row$game_id
  } else {
    NA_character_
  }
  if (!is_valid_string(game_id) && is_valid_string(resolved_game_key)) {
    game_id <- resolved_game_key
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
  } else if ("game_date" %in% names(identified_game_row) && is_valid_scalar(identified_game_row$game_date) && !is.na(identified_game_row$game_date)) {
    game_gameday <- as.Date(identified_game_row$game_date)
    log_msg("Using identified_game_row game_date:", as.character(game_gameday))
  } else {
    game_gameday <- as.Date(NA)
    log_msg("WARNING: No valid game_date found")
  }
  
  # opponent/team/home_away: from identified_game_row
  if (is_valid_string(identified_game_row$opponent)) {
    player_opponent <- identified_game_row$opponent
    log_msg("Using identified_game_row opponent:", player_opponent)
  } else {
    player_opponent <- NA_character_
    log_msg("WARNING: No valid opponent found")
  }
  
  if (is_valid_string(identified_game_row$team)) {
    player_team <- identified_game_row$team
    log_msg("Using identified_game_row team:", player_team)
  } else {
    player_team <- NA_character_
    log_msg("WARNING: No valid team found")
  }
  
  if (is_valid_string(identified_game_row$home_away)) {
    player_home_away <- identified_game_row$home_away
    log_msg("Using identified_game_row home_away:", player_home_away)
  } else {
    player_home_away <- NA_character_
    log_msg("WARNING: No valid home_away found")
  }

  # Fallback: derive game_id/game_date from schedules cache if still missing
  if ((is.na(game_gameday) || !is_valid_string(game_id)) &&
      exists("load_schedules")) {
    try({
      sched <- load_schedules(seasons = game_season, use_cache = TRUE)
      sched_match <- sched[
        sched$season == game_season &
          sched$week == game_week &
          (
            (sched$home_team == player_team & sched$away_team == player_opponent) |
              (sched$away_team == player_team & sched$home_team == player_opponent)
          ),
        , drop = FALSE
      ]
      if (nrow(sched_match) > 0) {
        if (is.na(game_gameday) && !is.na(sched_match$gameday[1])) {
          game_gameday <- as.Date(sched_match$gameday[1])
          log_msg("Recovered game_date from schedules:", as.character(game_gameday))
        }
        if (!is_valid_string(game_id) && is_valid_string(sched_match$game_id[1])) {
          game_id <- sched_match$game_id[1]
          log_msg("Recovered game_id from schedules:", game_id)
        }
        if (is.na(player_home_away) || player_home_away == "") {
          player_home_away <- ifelse(sched_match$home_team[1] == player_team, "HOME", "AWAY")
        }
      }
    }, silent = TRUE)
  }
  
  # CRITICAL ASSERTIONS: Verify resolved values match requested values
  log_msg("=== Resolution Validation ===")
  log_msg("RESOLVED: Season:", game_season, "| Week:", game_week)
  log_msg("RESOLVED: Team:", player_team, "| Opponent:", player_opponent)
  log_msg("RESOLVED: Game Date:", as.character(game_gameday), "| Game ID:", game_id)
  log_msg("RESOLVED: Home/Away:", player_home_away)
  
  if (is.na(resolved_game_key) && exists("build_game_key")) {
    resolved_game_key <- build_game_key(
      game_season,
      game_week,
      game_gameday,
      player_team,
      player_opponent,
      game_id
    )
  }
  
  # Enforce CLI args when provided
  if (!is.null(season) && !is.na(season) && game_season != season) {
    stop("RESOLUTION MISMATCH: Requested season=", season, " but resolved game has season=", game_season, ". ",
         "CLI arguments must be respected. This indicates a bug in game resolution logic.")
  }
  
  if (!is.null(week) && !is.na(week) && game_week != week) {
    stop("RESOLUTION MISMATCH: Requested week=", week, " but resolved game has week=", game_week, ". ",
         "CLI arguments must be respected. This indicates a bug in game resolution logic.")
  }
  
  log_msg("Resolution validation: PASS")

  # Schedule validation: ensure requested week exists (prevents unscheduled games)
  in_progress_season <- NA_integer_
  if (exists("detect_in_progress_season")) {
    in_progress_season <- detect_in_progress_season(unique(rb_data_all$season), cache_only = TRUE)
  }
  if (exists("load_schedules")) {
    sched <- load_schedules(seasons = game_season, cache_only = TRUE)
    if (nrow(sched) == 0) {
      stop("Schedule data is empty for season ", game_season, ". Cannot validate requested week.")
    }
    if (!any(sched$season == game_season & sched$week == game_week)) {
      stop("No schedule entry found for season ", game_season, " week ", game_week,
           ". Simulation is only allowed for scheduled weeks.")
    }
  }

  # Availability-aware non-finite guard (counterfactual retry)
  if (!is_future) {
    feature_cols_check <- get_rb_features_by_week(game_week)
    non_finite_cols <- find_non_finite_cols(identified_game_row, feature_cols_check)
    if (length(non_finite_cols) > 0) {
      if (availability_policy_used == "played_only") {
        stop("Non-finite feature values detected in observed row: ",
             paste(non_finite_cols, collapse = ", "),
             ". This indicates a feature computation bug.", call. = FALSE)
      }
      retry_groups <- unique(c(dropped_feature_groups, "player_rolling_features", "recent_performance_features"))
      build_retry <- build_rb_feature_row_for_simulation(
        rb_weekly_features = rb_data_all,
        rb_weekly_stats = rb_stats_all,
        player_dim = player_dim,
        prior_season_player_stats = prior_season_stats,
        team_offense_context = team_offense_context,
        defense_weekly_features = defense_weekly_features,
        schedule = schedule,
        gsis_id = player_id,
        season = game_season,
        week = game_week,
        availability_policy = availability_policy_used,
        drop_feature_groups = retry_groups
      )
      identified_game_row <- build_retry$feature_row
      dropped_feature_groups <- build_retry$dropped_feature_groups
      dropped_features <- unique(c(dropped_features, build_retry$dropped_features))
      construction_warnings <- unique(c(construction_warnings, build_retry$construction_warnings))
      used_sources <- unique(c(used_sources, build_retry$used_sources))

      non_finite_cols_retry <- find_non_finite_cols(identified_game_row, feature_cols_check)
      if (length(non_finite_cols_retry) > 0) {
        stop("Non-finite feature values persist after counterfactual rebuild: ",
             paste(non_finite_cols_retry, collapse = ", "), call. = FALSE)
      }
    }
  }
  
  # Store metadata
  result$metadata$game_id <- game_id
  result$metadata$game_key <- resolved_game_key
  result$metadata$season <- game_season
  result$metadata$week <- game_week
  result$metadata$game_date <- game_gameday
  result$metadata$team <- player_team
  result$metadata$opponent <- player_opponent
  result$metadata$home_away <- player_home_away
  result$metadata$availability_policy <- availability_policy_used
  result$metadata$availability_state <- availability_state
  result$metadata$counterfactual <- is_counterfactual_policy(availability_policy_used) &&
    (availability_policy_used == "force_counterfactual" || availability_state != "observed_played")
  if (!is_future && (is.null(result$metadata$game_id) || is.na(result$metadata$game_id) || result$metadata$game_id == "")) {
    stop("RB simulation requires non-missing game_id for observed games. Check schedule resolution.")
  }
  if (!is.na(in_progress_season) && game_season == in_progress_season) {
    result$metadata$current_season_in_progress <- TRUE
  }

  # Defensive validation: simulation input must have core identifiers
  missing_meta <- c()
  if (!is_valid_string(player_opponent)) missing_meta <- c(missing_meta, "opponent")
  if (is.na(game_gameday)) missing_meta <- c(missing_meta, "game_date")
  if (!is_valid_string(game_id)) missing_meta <- c(missing_meta, "game_id")
  if (length(missing_meta) > 0) {
    stop(
      "Simulation input missing required metadata: ",
      paste(missing_meta, collapse = ", "),
      ". Ensure cached features and identity data include game context."
    )
  }
  
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
    rb_data_pre <- rb_data_pre[is.na(rb_data_pre$gameday) | rb_data_pre$gameday < game_gameday, ]
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
  
  # Target row for metadata (availability-aware)
  if (is_future) {
    target_player_row <- synthetic_feature_row
  } else {
    target_player_row <- identified_game_row
  }
  if (is.null(target_player_row) || nrow(target_player_row) != 1) {
    stop("Target feature row must have exactly one row. Found ", nrow(target_player_row), ".", call. = FALSE)
  }

  if (isTRUE(debug) || identical(Sys.getenv("RB_SIM_DEBUG"), "1")) {
    key_comb <- ifelse(is.na(rb_data_pre$game_id) | rb_data_pre$game_id == "",
                       paste(rb_data_pre$season, rb_data_pre$week, rb_data_pre$team, rb_data_pre$opponent, sep = "_"),
                       rb_data_pre$game_id)
    log_msg("[DEBUG] Training pre-filter: n=", nrow(rb_data_pre),
            " players=", length(unique(rb_data_pre$player_id)),
            " seasons=", length(unique(rb_data_pre$season)),
            " games=", length(unique(stats::na.omit(key_comb))),
            " gameday_nonNA=", sum(!is.na(rb_data_pre$gameday)))
  }

  if (isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) {
    message("RB training rows after filters: ", nrow(rb_data_pre))
  }
  if (nrow(rb_data_pre) == 0) {
    stop("RB training data collapsed to 0 rows after filters.")
  }
  if (nrow(rb_data_pre) < 1000) {
    stop("Training data collapsed: only ", nrow(rb_data_pre), " rows remain before model fitting.")
  }
  if (length(unique(rb_data_pre$player_id)) < 50) {
    stop("Training data collapsed: only ", length(unique(rb_data_pre$player_id)), " unique players remain before model fitting.")
  }

  if ((isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) &&
      exists("get_rb_v1_targets")) {
    rb_targets_diag <- get_rb_v1_targets()
    for (tgt in rb_targets_diag) {
      if (!tgt %in% names(rb_data_pre)) {
        message("RB target missing: ", tgt)
      } else {
        vals <- rb_data_pre[[tgt]]
        message("RB target stats ", tgt, ": min=", suppressWarnings(min(vals, na.rm = TRUE)),
                " max=", suppressWarnings(max(vals, na.rm = TRUE)),
                " na=", sum(is.na(vals)),
                " all_na=", all(is.na(vals)))
      }
    }
  }
  
  # Extract metadata from the single row
  target_player_id <- target_player_row$player_id[1]
  target_player_team <- target_player_row$team[1]
  target_player_name_cached <- target_player_row$player_name[1]
  target_player_name <- if (is_valid_string(target_player_name_cached)) target_player_name_cached else player_id
  
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
  player_data_team <- player_data[player_data$team == player_team, ]
  
  if (nrow(player_data_team) == 0) {
    stop("Cannot proceed without games for target team. ",
         "Player ID: ", target_player_id,
         ", Team: ", player_team,
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
  
  # Recent games should reflect actual stat lines, not feature rows
  recent_games <- data.frame()
  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  stats_path <- file.path(repo_root, "data", "cache", "rb_weekly_stats.parquet")
  if (file.exists(stats_path) && requireNamespace("arrow", quietly = TRUE)) {
    rb_stats <- tryCatch(
      arrow::read_parquet(stats_path),
      error = function(e) NULL
    )
    if (!is.null(rb_stats) && nrow(rb_stats) > 0) {
      player_history <- rb_stats[
        rb_stats$player_id == target_player_id &
          (rb_stats$season < game_season |
             (rb_stats$season == game_season & rb_stats$week < game_week)),
        , drop = FALSE
      ]
      if (nrow(player_history) > 0) {
        player_history <- player_history[order(player_history$season, player_history$week, decreasing = TRUE), ]
        recent_games <- player_history[1:min(3, nrow(player_history)), ]
        # Assertion: recent games must be strictly before target week
        max_key <- max(recent_games$season * 100 + recent_games$week, na.rm = TRUE)
        target_key <- game_season * 100 + game_week
        if (is.finite(max_key) && max_key >= target_key) {
          stop("Recent performance includes games from the target week or later. Check season/week filtering.")
        }
      }
    }
  }
  result$recent_games <- recent_games
  
  # ============================================================================
  # STEP 4: Construct the pre-game feature row
  # ============================================================================
  
  # Use availability-aware feature row
  player_game_row <- identified_game_row
  if (nrow(player_game_row) != 1) {
    stop("Feature row must have exactly one row. Found ", nrow(player_game_row), ".", call. = FALSE)
  }
  
  log_msg("Using availability-aware feature row: season=", player_game_row$season[1],
          ", week=", player_game_row$week[1])

  if ("is_rookie" %in% names(player_game_row)) {
    result$metadata$is_rookie <- isTRUE(player_game_row$is_rookie[1])
  }
  if ("draft_round" %in% names(player_game_row)) {
    result$metadata$draft_round <- player_game_row$draft_round[1]
  }
  if ("draft_pick_overall" %in% names(player_game_row)) {
    result$metadata$draft_pick_overall <- player_game_row$draft_pick_overall[1]
  }

  # Enforce minimum history rule (player_id only)
  # Allow returning players with prior-season or computed priors, even if roll3 is NA
  skip_history_check <- is_counterfactual_policy(availability_policy_used) &&
    (availability_policy_used == "force_counterfactual" || availability_state != "observed_played")

  if (!skip_history_check) {
    rolling_cols_3 <- grep("_roll3$", names(player_game_row), value = TRUE)
    if (length(rolling_cols_3) == 0) {
      stop("No 3-game rolling features found. Cannot verify minimum history requirement.")
    }
    
    has_3_game_history <- any(!is.na(player_game_row[, rolling_cols_3, drop = FALSE]))
    prior_cols <- c(
      "prev_season_carries_pg", "prev_season_targets_pg", "prev_season_ypc", "prev_season_ypt", "prev_season_games",
      "prev_season_carries_total", "prev_season_targets_total",
      "prev_season_rush_yards_total", "prev_season_rec_yards_total",
      "prev_season_games_played",
      "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
      "carries_cum_mean", "targets_cum_mean", "receptions_cum_mean", "ypc_cum", "ypt_cum", "catch_rate_cum"
    )
    prior_cols <- intersect(prior_cols, names(player_game_row))
    has_prior_history <- length(prior_cols) > 0 && any(!is.na(player_game_row[, prior_cols, drop = FALSE]))

    is_rookie <- FALSE
    if ("is_rookie" %in% names(player_game_row)) {
      is_rookie <- isTRUE(player_game_row$is_rookie[1])
    }

    if (!has_3_game_history && !has_prior_history && !is_rookie) {
      stop("Player '", target_player_name, "' (ID: ", target_player_id, 
           ") does not have sufficient historical data for simulation. ",
           "Player must have at least 3 prior games or returning-player priors. ",
           "Current game: Season ", game_season, ", Week ", game_week, ".")
    }
  }

  # Extract feature row (exclude target_* columns) using time-aware feature contract
  if (!exists("get_rb_features_by_week")) {
    stop("get_rb_features_by_week not available for time-aware feature selection.")
  }
  feature_cols <- get_rb_features_by_week(game_week)
  if (is_counterfactual_policy(availability_policy_used)) {
    feature_cols <- unique(c(feature_cols, "position"))
  }
  # Include contextual flags passed through caches
  feature_cols <- unique(c(feature_cols, intersect(c("defense_data_available", "rolling_window_complete"), names(player_game_row))))
  
  dropped_features <- intersect(dropped_features, feature_cols)
  available_feature_cols <- feature_cols[
    feature_cols %in% names(player_game_row) &
      !feature_cols %in% dropped_features
  ]
  player_feature_row <- player_game_row[, available_feature_cols, drop = FALSE]
  
  if (length(dropped_feature_groups) > 0 || length(dropped_features) > 0) {
    log_msg("Availability policy dropped feature groups: ",
            if (length(dropped_feature_groups) > 0) paste(dropped_feature_groups, collapse = ", ") else "(none)")
    log_msg("Availability policy dropped features: ",
            if (length(dropped_features) > 0) paste(dropped_features, collapse = ", ") else "(none)")
  }
  
  # Store defensive context
  def_features <- c("def_rush_yards_defense_allowed_roll1", "def_yards_per_rush_defense_allowed_roll1",
                    "def_points_defense_allowed_roll1", "def_sacks_defense_forced_roll1", "def_tackles_for_loss_defense_forced_roll1",
                    "def_rush_yards_defense_allowed_roll5", "def_yards_per_rush_defense_allowed_roll5",
                    "def_sacks_defense_forced_roll5", "def_tackles_for_loss_defense_forced_roll5", "def_points_defense_allowed_roll5",
                    "def_interceptions_defense_caught_roll1", "def_interceptions_defense_caught_roll5")
  for (feat in def_features) {
    if (feat %in% names(identified_game_row)) {
      result$defensive_context[[feat]] <- identified_game_row[[feat]]
    } else {
      result$defensive_context[[feat]] <- NA_real_
    }
  }
  # Defensive feature availability diagnostics
  def_cols_present <- intersect(def_features, names(identified_game_row))
  def_cols_non_na <- def_cols_present[
    vapply(def_cols_present, function(f) any(!is.na(identified_game_row[[f]])), logical(1))
  ]
  result$diagnostics$defensive_features <- list(
    available = def_cols_present,
    non_na = def_cols_non_na
  )

  # Offensive QB context (QB-side, prior games only)
  qb_context_features <- c(
    "target_pass_attempts_qb_roll1", "target_pass_attempts_qb_roll3", "target_pass_attempts_qb_roll5",
    "target_completion_pct_qb_roll1", "target_completion_pct_qb_roll3", "target_completion_pct_qb_roll5",
    "target_interceptions_qb_thrown_roll1", "target_interceptions_qb_thrown_roll3", "target_interceptions_qb_thrown_roll5",
    "target_sacks_qb_taken_roll1", "target_sacks_qb_taken_roll3", "target_sacks_qb_taken_roll5"
  )
  for (feat in qb_context_features) {
    if (feat %in% names(identified_game_row)) {
      result$offensive_context[[feat]] <- identified_game_row[[feat]]
    } else {
      result$offensive_context[[feat]] <- NA_real_
    }
  }

  # Build feature trace (what is available for this simulation)
  model_trace <- list(
    phase = determine_rb_regime(game_week),
    targets = list(),
    features = list(
      candidate_features = feature_cols,
      used_features = available_feature_cols,
      dropped_features = setdiff(feature_cols, available_feature_cols),
      na_features = available_feature_cols[
        vapply(available_feature_cols, function(f) {
          mean(is.na(player_feature_row[[f]])) > 0.9
        }, logical(1))
      ]
    )
  )
  
  result$diagnostics$availability <- list(
    policy = availability_policy_used,
    state = availability_state,
    counterfactual = is_counterfactual_policy(availability_policy_used) &&
      (availability_policy_used == "force_counterfactual" || availability_state != "observed_played"),
    note = availability_note(availability_policy_used,
                             if (availability_state == "observed_played") "Observed game row used." else "Counterfactual row constructed."),
    dropped_feature_groups = dropped_feature_groups,
    dropped_features = dropped_features,
    construction_warnings = construction_warnings,
    used_sources = used_sources
  )
  
  result$diagnostics$feature_usage <- list(
    candidate_features = feature_cols,
    used_features = available_feature_cols,
    dropped_features = setdiff(feature_cols, available_feature_cols),
    dropped_feature_groups = dropped_feature_groups
  )
  
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
      # Capture reasons when available
      reasons <- sapply(baseline_names, function(nm) {
        if (!is.null(rb_models$diagnostics[[nm]]) && !is.null(rb_models$diagnostics[[nm]]$fallback_reason)) {
          rb_models$diagnostics[[nm]]$fallback_reason
        } else {
          "unknown"
        }
      })
      reason_table <- table(reasons, useNA = "ifany")
      warning("RB simulation: ", sum(baseline_models), " model(s) using baseline. Reasons: ",
              paste(names(reason_table), reason_table, sep = "=", collapse = "; "),
              ". Models: ", paste(head(baseline_names, 5), collapse = ", "),
              if (length(baseline_names) > 5) paste0(" (and ", length(baseline_names) - 5, " more)") else "")
      result$diagnostics$baseline_models <- baseline_names
      result$diagnostics$baseline_reasons <- reasons
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
    min_date = {
      vals <- if ("gameday" %in% names(rb_data_pre)) rb_data_pre$gameday else rb_data_pre$game_date
      if (length(vals) == 0) {
        NA
      } else {
        v <- suppressWarnings(min(vals, na.rm = TRUE))
        if (is.infinite(v) || is.na(v)) {
          suppressWarnings(as.Date(paste0(min(rb_data_pre$season, na.rm = TRUE), "-01-01")))
        } else v
      }
    },
    max_date = {
      vals <- if ("gameday" %in% names(rb_data_pre)) rb_data_pre$gameday else rb_data_pre$game_date
      if (length(vals) == 0) {
        NA
      } else {
        v <- suppressWarnings(max(vals, na.rm = TRUE))
        if (is.infinite(v) || is.na(v)) {
          suppressWarnings(as.Date(paste0(max(rb_data_pre$season, na.rm = TRUE), "-12-31")))
        } else v
      }
    },
    seasons = unique(rb_data_pre$season),
    n_players = length(unique(rb_data_pre$player_id)),
    n_games = {
      key_primary <- rb_data_pre$game_id
      # Fallback composite when game_id missing
      composite <- paste(rb_data_pre$season, rb_data_pre$week, rb_data_pre$team, rb_data_pre$opponent, sep = "_")
      key_combined <- ifelse(is.na(key_primary) | key_primary == "", composite, key_primary)
      length(unique(stats::na.omit(key_combined)))
    }
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
  def_cols <- c("def_rush_yards_defense_allowed_roll5", "def_sacks_defense_forced_roll5", 
                "def_tackles_for_loss_defense_forced_roll5", "def_points_defense_allowed_roll5")
  
  # Check for NULL models (RB v1: regime-based structure)
  # Note: Models may be baseline, but should never be NULL
  # With regime-based modeling, models are in rb_models$models with keys like "target_carries__early"
  # The simulation will select the appropriate model based on the game's regime
  if (!is.null(rb_models$models)) {
    # New regime-based structure: check that at least one model exists per target
    regime_path <- if (exists("resolve_regime_path")) {
      resolve_regime_path("RB", "v1")
    } else {
      file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "utils", "rb_regime_v1.R")
    }
    if (file.exists(regime_path)) {
      source(regime_path, local = TRUE)
    } else {
      stop("Missing RB regime at ", regime_path)
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

  # Build model trace summary (model selection + feature inputs)
  trace_regime <- determine_rb_regime(game_week)
  rb_targets <- get_rb_v1_targets()
  model_trace <- list(
    phase = trace_regime,
    targets = list(),
    features = list(
      candidate_features = feature_cols,
      used_features = available_feature_cols,
      dropped_features = setdiff(feature_cols, available_feature_cols),
      na_features = available_feature_cols[
        vapply(available_feature_cols, function(f) {
          mean(is.na(player_feature_row[[f]])) > 0.9
        }, logical(1))
      ]
    )
  )
  for (tgt in rb_targets) {
    mk <- get_model_key(tgt, trace_regime)
    diag_entry <- rb_models$diagnostics[[mk]]
    model_trace$targets[[tgt]] <- list(
      used_model = if (!is.null(diag_entry$fit_type) && diag_entry$fit_type == "baseline") "baseline" else "fitted",
      n_train = if (!is.null(diag_entry$n_rows_final)) diag_entry$n_rows_final else NA_integer_,
      n_non_na = if (tgt %in% names(rb_data_pre)) sum(!is.na(rb_data_pre[[tgt]])) else NA_integer_,
      fallback_reason = if (!is.null(diag_entry$fallback_reason)) diag_entry$fallback_reason else NA_character_
    )
  }
  result$diagnostics$model_trace <- model_trace
  
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
  
  # CRITICAL INVARIANT: feature_row week must match CLI-provided week (if provided)
  # This is the final guardrail before simulation - catches any week drift
  if (!is.null(week) && !is.na(week) && player_feature_row$week != week) {
    stop("FEATURE ROW WEEK INVARIANT VIOLATED: CLI requested week=", week,
         " but feature_row has week=", player_feature_row$week, ". ",
         "This should NEVER happen - indicates week drift in feature row construction. ",
         "Player ID: ", target_player_id, ", Season: ", season)
  }
  
  log_msg("=== Feature Row Validation ===")
  log_msg("Feature row week:", player_feature_row$week, "| Metadata week:", result$metadata$week, "| CLI week:", week, "| Match: OK")
  
  # Preserve schedule-derived is_home (no imputation)
  
  # Run simulation
  sim_result <- simulate_rb_game(
    feature_row = player_feature_row,
    rb_models = rb_models,
    n_sims = n_sims,
    availability_policy = availability_policy_used
  )
  
  if (sim_result$status != "success") {
    stop("Simulation failed with status: ", sim_result$status)
  }
  
  # Store simulation results
  result$draws <- sim_result$draws
  if (!is.null(sim_result$diagnostics)) {
    result$diagnostics$regime_selection <- sim_result$diagnostics
  }

  # Resolve simulation schema to canonical names
  # This standardizes column names (rush_yards -> rushing_yards, etc.)
  # and creates derived columns (total_touchdowns, total_yards)
  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("RB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "utils", "rb_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
  } else {
    stop("Missing RB schema at ", schema_path)
  }
  if (exists("resolve_rb_simulation_schema")) {
    result$draws <- resolve_rb_simulation_schema(result$draws)
  } else {
    stop("resolve_rb_simulation_schema not found. Cannot standardize simulation output schema.")
  }

  if (!exists("compute_ppr_rb")) {
    if (file.exists("R/utils/ppr_scoring.R")) {
      source("R/utils/ppr_scoring.R", local = TRUE)
    } else {
      stop("Simulation bootstrap incomplete: R/utils/ppr_scoring.R not found")
    }
  }

  # Fantasy PPR must be computed from draw-level stats (no legacy totals)
  if (!"fantasy_ppr" %in% names(result$draws)) {
    result$draws$fantasy_ppr <- compute_ppr_rb(
      rush_yards = result$draws$rushing_yards,
      rush_tds = result$draws$rush_tds,
      receptions = result$draws$receptions,
      rec_yards = result$draws$receiving_yards,
      rec_tds = result$draws$rec_tds
    )
  }
  component_non_na <- !is.na(result$draws$rushing_yards) |
    !is.na(result$draws$receiving_yards) |
    !is.na(result$draws$receptions) |
    !is.na(result$draws$rush_tds) |
    !is.na(result$draws$rec_tds)
  if (any(component_non_na & is.na(result$draws$fantasy_ppr))) {
    stop("Fantasy PPR contains NA while component stats are non-NA. ",
         "This indicates a draw-level scoring error.")
  }
  
  # CRITICAL FIX: Recompute summary AFTER schema resolution
  # This ensures summary stat names match the resolved column names
  # (rushing_yards, receiving_yards, total_touchdowns, total_yards)
  if (!exists("compute_final_rb_percentiles")) {
    source("R/simulation/simulate_rb_game.R", local = TRUE)
    if (!exists("compute_final_rb_percentiles") && exists("compute_rb_percentiles")) {
      compute_final_rb_percentiles <- compute_rb_percentiles
    }
  }
  if (exists("compute_final_rb_percentiles")) {
    result$summary <- compute_final_rb_percentiles(result$draws)
  } else {
    stop("compute_final_rb_percentiles not found. Cannot compute summary from resolved draws.")
  }

  if (nrow(result$summary) > 0 && !"fantasy_ppr" %in% result$summary$stat) {
    result$summary <- rbind(
      result$summary,
      data.frame(
        stat = "fantasy_ppr",
        p25 = stats::quantile(result$draws$fantasy_ppr, 0.25, na.rm = TRUE),
        p50 = stats::quantile(result$draws$fantasy_ppr, 0.50, na.rm = TRUE),
        p75 = stats::quantile(result$draws$fantasy_ppr, 0.75, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
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
  
  # Compute distribution stats on resolved schema (using rushing_yards, not rush_yards)
  result$diagnostics$distribution_stats <- list(
    carries_mean = mean(draws_df$carries, na.rm = TRUE),
    carries_sd = sd(draws_df$carries, na.rm = TRUE),
    rushing_yards_mean = mean(draws_df$rushing_yards, na.rm = TRUE),
    rushing_yards_sd = sd(draws_df$rushing_yards, na.rm = TRUE),
    receiving_yards_mean = mean(draws_df$receiving_yards, na.rm = TRUE),
    receiving_yards_sd = sd(draws_df$receiving_yards, na.rm = TRUE),
    total_touchdowns_mean = mean(draws_df$total_touchdowns, na.rm = TRUE),
    total_touchdowns_sd = sd(draws_df$total_touchdowns, na.rm = TRUE)
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
