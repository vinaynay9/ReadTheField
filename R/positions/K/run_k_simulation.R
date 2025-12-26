# Run K Simulation - Pure Computation Layer
#
# Performs complete K simulation workflow without printing or file I/O.

run_k_simulation <- function(gsis_id,
                             season,
                             week,
                             n_sims = 5000,
                             game_date = NULL,
                             seasons_train = NULL,
                             mode_policy = NULL,
                             synthetic_feature_row = NULL,
                             is_future = FALSE,
                             availability_policy = "played_only",
                             debug = FALSE) {
  if (missing(gsis_id)) {
    stop("run_k_simulation requires gsis_id.", call. = FALSE)
  }

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

  if (!exists(".Random.seed", envir = .GlobalEnv)) {
    set.seed(NULL)
  }
  result$metadata$random_seed <- .Random.seed

  player_id <- as.character(gsis_id)
  availability_policy <- validate_availability_policy(availability_policy)

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

  if (!exists("read_k_weekly_features_cache")) {
    stop("Simulation bootstrap incomplete: read_k_weekly_features_cache not loaded.")
  }
  k_data_all <- read_k_weekly_features_cache()
  if (nrow(k_data_all) == 0) {
    stop("K weekly features cache is empty.")
  }

  if (!exists("read_k_weekly_stats_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Simulation bootstrap incomplete: read_k_weekly_stats_cache not loaded.")
    }
  }
  k_stats_all <- read_k_weekly_stats_cache()
  if (nrow(k_stats_all) == 0) {
    stop("K weekly stats cache is empty.")
  }

  schedule <- NULL
  if (exists("load_schedules")) {
    schedule <- load_schedules(seasons = season, cache_only = TRUE)
  }

  if (is_future) {
    if (is.null(synthetic_feature_row) || nrow(synthetic_feature_row) != 1) {
      stop("Future simulation requires a single-row synthetic_feature_row.")
    }
    identified_game_row <- synthetic_feature_row
    availability_state <- "future_game"
    dropped_feature_groups <- character(0)
    dropped_features <- character(0)
    availability_policy_used <- availability_policy
    construction_warnings <- character(0)
    used_sources <- character(0)
  } else {
    if (!exists("build_k_feature_row_for_simulation")) {
      if (file.exists("R/positions/K/build_k_feature_row_for_simulation.R")) {
        source("R/positions/K/build_k_feature_row_for_simulation.R", local = TRUE)
      } else {
        stop("build_k_feature_row_for_simulation not loaded.")
      }
    }
    build_result <- build_k_feature_row_for_simulation(
      k_weekly_features = k_data_all,
      k_weekly_stats = k_stats_all,
      player_dim = NULL,
      prior_season_player_stats = NULL,
      team_offense_context = NULL,
      defense_weekly_features = NULL,
      schedule = schedule,
      gsis_id = player_id,
      season = season,
      week = week,
      availability_policy = availability_policy,
      drop_feature_groups = character(0)
    )
    identified_game_row <- build_result$feature_row
    availability_state <- build_result$availability_state
    availability_policy_used <- build_result$availability_policy
    dropped_feature_groups <- build_result$dropped_feature_groups
    dropped_features <- build_result$dropped_features
    construction_warnings <- build_result$construction_warnings
    used_sources <- build_result$used_sources
  }

  result$metadata$player_id <- player_id
  result$metadata$player_name <- if ("player_name" %in% names(identified_game_row)) identified_game_row$player_name else NA_character_
  result$metadata$team <- if ("team" %in% names(identified_game_row)) identified_game_row$team else NA_character_
  result$metadata$opponent <- if ("opponent" %in% names(identified_game_row)) identified_game_row$opponent else NA_character_
  result$metadata$season <- if ("season" %in% names(identified_game_row)) identified_game_row$season else season
  result$metadata$week <- if ("week" %in% names(identified_game_row)) identified_game_row$week else week
  result$metadata$game_date <- if ("gameday" %in% names(identified_game_row)) identified_game_row$gameday else NA
  result$metadata$home_away <- if ("home_away" %in% names(identified_game_row)) identified_game_row$home_away else NA_character_
  result$metadata$position <- "K"

  k_data <- k_data_all[k_data_all$season %in% seasons_train, , drop = FALSE]
  if (nrow(k_data) == 0) {
    stop("No K training rows available for seasons: ", paste(seasons_train, collapse = ", "))
  }
  k_data_pre <- k_data
  k_data_pre <- k_data_pre[!(k_data_pre$player_id == player_id &
                               k_data_pre$season == season &
                               k_data_pre$week == week), , drop = FALSE]
  if (nrow(k_data_pre) < 500) {
    stop("Training data collapsed: only ", nrow(k_data_pre), " rows remain before model fitting.")
  }

  if (!exists("fit_k_models")) {
    if (file.exists("R/positions/K/fit_k_models.R")) {
      source("R/positions/K/fit_k_models.R", local = TRUE)
    } else {
      stop("Missing R/positions/K/fit_k_models.R")
    }
  }
  k_models <- fit_k_models(k_data_pre, min_rows = 200)
  result$diagnostics$model_diagnostics <- k_models$diagnostics

  if (!exists("simulate_k_game")) {
    if (file.exists("R/positions/K/simulate_k_game.R")) {
      source("R/positions/K/simulate_k_game.R", local = TRUE)
    } else {
      stop("Missing R/positions/K/simulate_k_game.R")
    }
  }
  if (!exists("get_k_features_by_week")) {
    if (file.exists("R/positions/K/k_regime_v1.R")) {
      source("R/positions/K/k_regime_v1.R", local = TRUE)
    } else {
      stop("Missing R/positions/K/k_regime_v1.R")
    }
  }

  feature_cols <- get_k_features_by_week(as.integer(identified_game_row$week))
  feature_cols <- unique(c(feature_cols, intersect(c("defense_data_available", "rolling_window_complete"), names(identified_game_row))))
  dropped_features <- intersect(dropped_features, feature_cols)
  available_feature_cols <- feature_cols[
    feature_cols %in% names(identified_game_row) &
      !feature_cols %in% dropped_features
  ]
  player_feature_row <- identified_game_row[, available_feature_cols, drop = FALSE]

  # Defensive context (K uses sacks/points allowed as opponent pressure context)
  def_features <- c(
    "def_sacks_defense_forced_roll1", "def_sacks_defense_forced_roll3", "def_sacks_defense_forced_roll5",
    "def_points_defense_allowed_roll1", "def_points_defense_allowed_roll3", "def_points_defense_allowed_roll5"
  )
  for (feat in def_features) {
    if (feat %in% names(identified_game_row)) {
      result$defensive_context[[feat]] <- identified_game_row[[feat]]
    } else {
      result$defensive_context[[feat]] <- NA_real_
    }
  }
  def_cols_present <- intersect(def_features, names(identified_game_row))
  def_cols_non_na <- def_cols_present[
    vapply(def_cols_present, function(f) any(!is.na(identified_game_row[[f]])), logical(1))
  ]
  result$diagnostics$defensive_features <- list(
    available = def_cols_present,
    non_na = def_cols_non_na
  )

  # Offensive context table expects QB rolling fields; leave explicit NAs if missing.
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

  result$diagnostics$feature_usage <- list(
    candidate_features = feature_cols,
    used_features = available_feature_cols,
    dropped_features = setdiff(feature_cols, available_feature_cols),
    dropped_feature_groups = dropped_feature_groups
  )

  sim_result <- simulate_k_game(
    feature_row = player_feature_row,
    k_models = k_models,
    n_sims = n_sims,
    availability_policy = availability_policy_used
  )
  if (sim_result$status != "success") {
    stop("Simulation failed with status: ", sim_result$status)
  }

  result$draws <- sim_result$draws
  if (!is.null(sim_result$diagnostics)) {
    result$diagnostics$regime_selection <- sim_result$diagnostics
  }

  if (file.exists("R/positions/K/k_schema_v1.R")) {
    source("R/positions/K/k_schema_v1.R", local = TRUE)
  }
  if (exists("resolve_k_simulation_schema")) {
    result$draws <- resolve_k_simulation_schema(result$draws)
  }

  # Derive fantasy points from draw-level FG/PAT outputs (no new modeling).
  if (exists("compute_ppr_k") && !"fantasy_ppr" %in% names(result$draws)) {
    result$draws$fantasy_ppr <- compute_ppr_k(
      fg_made = result$draws$fg_made,
      xp_made = result$draws$pat_made
    )
  }

  if (exists("compute_k_percentiles")) {
    result$summary <- compute_k_percentiles(result$draws)
  }
  if (nrow(result$summary) > 0 &&
      "fantasy_ppr" %in% names(result$draws) &&
      !"fantasy_ppr" %in% result$summary$stat) {
    result$summary <- rbind(
      result$summary,
      data.frame(
        stat = "fantasy_ppr",
        p25 = stats::quantile(result$draws$fantasy_ppr, 0.25, na.rm = TRUE),
        p50 = stats::quantile(result$draws$fantasy_ppr, 0.50, na.rm = TRUE),
        p75 = stats::quantile(result$draws$fantasy_ppr, 0.75, na.rm = TRUE)
      )
    )
  }

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

  result
}
