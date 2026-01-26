# Run K Simulation - Pure Computation Layer
#
# Performs complete K simulation workflow without printing or file I/O.

run_k_simulation <- function(gsis_id,
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

  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  if (!exists("read_k_weekly_stats_cache")) {
    if (file.exists(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"))) {
      source(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"), local = TRUE)
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

  result$metadata$player_id <- player_id
  result$metadata$player_name <- if ("player_name" %in% names(identified_game_row)) identified_game_row$player_name else NA_character_
  result$metadata$team <- if ("team" %in% names(identified_game_row)) identified_game_row$team else NA_character_
  result$metadata$opponent <- if ("opponent" %in% names(identified_game_row)) identified_game_row$opponent else NA_character_
  result$metadata$season <- if ("season" %in% names(identified_game_row)) identified_game_row$season else season
  result$metadata$week <- if ("week" %in% names(identified_game_row)) identified_game_row$week else week
  result$metadata$game_id <- if ("game_id" %in% names(identified_game_row)) identified_game_row$game_id else NA_character_
  result$metadata$game_key <- if ("game_key" %in% names(identified_game_row)) identified_game_row$game_key else NA_character_
  result$metadata$game_date <- if ("gameday" %in% names(identified_game_row)) identified_game_row$gameday else NA
  result$metadata$home_away <- if ("home_away" %in% names(identified_game_row)) identified_game_row$home_away else NA_character_
  result$metadata$position <- "K"

  if (!is_future && (is.null(result$metadata$game_id) || is.na(result$metadata$game_id) || result$metadata$game_id == "") &&
      !is.null(schedule) && nrow(schedule) > 0) {
    sched_cols <- intersect(c("season", "week", "home_team", "away_team", "game_id", "gameday"), names(schedule))
    if (length(sched_cols) >= 5) {
      sched_match <- schedule[
        schedule$season == result$metadata$season &
          schedule$week == result$metadata$week &
          ((schedule$home_team == result$metadata$team & schedule$away_team == result$metadata$opponent) |
             (schedule$away_team == result$metadata$team & schedule$home_team == result$metadata$opponent)),
        , drop = FALSE
      ]
      if (nrow(sched_match) > 0) {
        result$metadata$game_id <- sched_match$game_id[1]
        if (is.na(result$metadata$game_date) && "gameday" %in% names(sched_match)) {
          result$metadata$game_date <- sched_match$gameday[1]
        }
      }
    }
  }

  if (!is_future && (is.null(result$metadata$game_id) || is.na(result$metadata$game_id))) {
    stop("K simulation requires non-missing game_id for observed games. Check feature row joins.")
  }

  k_data <- k_data_all[k_data_all$season %in% seasons_train, , drop = FALSE]
  if (nrow(k_data) == 0) {
    stop("No K training rows available for seasons: ", paste(seasons_train, collapse = ", "))
  }
  k_data_pre <- k_data
  k_data_pre <- k_data_pre[!(k_data_pre$player_id == player_id &
                               k_data_pre$season == season &
                               k_data_pre$week == week), , drop = FALSE]
  if (isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) {
    message("K training rows after filters: ", nrow(k_data_pre))
  }
  if (nrow(k_data_pre) == 0) {
    stop("K training data collapsed to 0 rows after filters.")
  }
  if (nrow(k_data_pre) < 500) {
    stop("Training data collapsed: only ", nrow(k_data_pre), " rows remain before model fitting.")
  }

  if ((isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) &&
      exists("get_k_v1_targets")) {
    k_targets_diag <- get_k_v1_targets()
    for (tgt in k_targets_diag) {
      if (!tgt %in% names(k_data_pre)) {
        message("K target missing: ", tgt)
      } else {
        vals <- k_data_pre[[tgt]]
        message("K target stats ", tgt, ": min=", suppressWarnings(min(vals, na.rm = TRUE)),
                " max=", suppressWarnings(max(vals, na.rm = TRUE)),
                " na=", sum(is.na(vals)),
                " all_na=", all(is.na(vals)))
      }
    }
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
    regime_path <- if (exists("resolve_regime_path")) {
      resolve_regime_path("K", "v1")
    } else {
      file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "K", "k_regime_v1.R")
    }
    if (file.exists(regime_path)) {
      source(regime_path, local = TRUE)
    } else {
      stop("Missing K regime at ", regime_path)
    }
  }

  feature_cols <- get_k_features_by_week(as.integer(identified_game_row$week))
  # Preserve week for regime selection in simulate_k_game.
  feature_cols <- unique(c(feature_cols, "week"))
  feature_cols <- unique(c(feature_cols, intersect(c("defense_data_available", "rolling_window_complete"), names(identified_game_row))))
  dropped_features <- intersect(dropped_features, feature_cols)
  available_feature_cols <- feature_cols[
    feature_cols %in% names(identified_game_row) &
      !feature_cols %in% dropped_features
  ]
  player_feature_row <- identified_game_row[, available_feature_cols, drop = FALSE]

  # Guardrail: K simulations must not include QB offensive context columns.
  qb_like_cols <- grep("pass_|completion|interception|sack", names(player_feature_row), value = TRUE, ignore.case = TRUE)
  qb_like_cols <- qb_like_cols[!grepl("^def_", qb_like_cols)]
  if (length(qb_like_cols) > 0) {
    stop("K simulation input contains QB/offense context columns: ",
         paste(qb_like_cols, collapse = ", "),
         ". Remove QB context from K feature assembly.")
  }

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

  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("K", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "K", "k_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
  } else {
    stop("Missing K schema at ", schema_path)
  }
  if (exists("resolve_k_simulation_schema")) {
    result$draws <- resolve_k_simulation_schema(result$draws)
  }

  if ("fg_attempts" %in% names(result$draws)) {
    if (any(is.na(result$draws$fg_attempts))) {
      stop("FG attempts are NA in K simulation draws. Check target_fg_attempts_k modeling.")
    }
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
    q <- stats::quantile(result$draws$fantasy_ppr,
                         c(0.10, 0.25, 0.40, 0.50, 0.60, 0.75, 0.90),
                         na.rm = TRUE)
    result$summary <- rbind(
      result$summary,
      data.frame(
        stat = "fantasy_ppr",
        p10 = as.numeric(q[1]),
        p25 = as.numeric(q[2]),
        p40 = as.numeric(q[3]),
        p50 = as.numeric(q[4]),
        p60 = as.numeric(q[5]),
        p75 = as.numeric(q[6]),
        p90 = as.numeric(q[7])
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
