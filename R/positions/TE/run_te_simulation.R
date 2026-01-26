# Run TE Simulation - Pure Computation Layer
#
# Performs complete TE simulation workflow without any printing or file I/O.

run_te_simulation <- function(gsis_id,
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
    stop("run_te_simulation requires gsis_id.", call. = FALSE)
  }

  log_file <- "te_debug.log"
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
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

  is_valid_scalar <- function(x) {
    length(x) == 1 && !is.null(x) && !is.na(x)
  }
  is_valid_string <- function(x) {
    is_valid_scalar(x) && is.character(x) && nzchar(x)
  }

  if (is.null(seasons_train) || length(seasons_train) == 0) {
    if (!exists("simulation_mode_policy")) {
      stop("No training seasons provided and simulation_mode_policy not loaded.")
    }
    if (!exists("get_available_seasons_from_cache")) {
      stop("get_available_seasons_from_cache not loaded.")
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

  if (!exists("read_te_weekly_features_cache")) {
    stop("Simulation bootstrap incomplete: read_te_weekly_features_cache not loaded.")
  }

  identity_cache <- NULL
  if (exists("read_player_week_identity_cache")) {
    try({
      identity_cache <- read_player_week_identity_cache()
    }, silent = TRUE)
  }

  te_data_all <- read_te_weekly_features_cache()
  if (nrow(te_data_all) == 0) {
    stop("TE weekly features cache is empty.")
  }

  if (!is.null(identity_cache) && nrow(identity_cache) > 0) {
    orig_game_id <- te_data_all$game_id
    orig_gameday <- te_data_all$gameday

    ident_cols <- intersect(c("player_id", "season", "week", "game_id", "game_key", "game_date"), names(identity_cache))
    identity_clean <- identity_cache[, ident_cols, drop = FALSE]
    identity_clean$game_date <- as.Date(identity_clean$game_date)
    identity_clean <- identity_clean[order(identity_clean$player_id, identity_clean$season, identity_clean$week, identity_clean$game_date), ]
    identity_clean <- identity_clean[!duplicated(identity_clean[, c("player_id", "season", "week")]), ]

    te_data_all <- merge(
      te_data_all,
      identity_clean,
      by = c("player_id", "season", "week"),
      all.x = TRUE,
      suffixes = c("", "_ident")
    )

    if ("game_id_ident" %in% names(te_data_all)) {
      te_data_all$game_id <- ifelse(is.na(te_data_all$game_id) | te_data_all$game_id == "",
                                    te_data_all$game_id_ident,
                                    te_data_all$game_id)
    }
    if ("game_key_ident" %in% names(te_data_all)) {
      te_data_all$game_key <- ifelse(is.na(te_data_all$game_key) | te_data_all$game_key == "",
                                     te_data_all$game_key_ident,
                                     te_data_all$game_key)
    }
    if ("game_date_ident" %in% names(te_data_all)) {
      te_data_all$gameday <- ifelse(is.na(te_data_all$gameday),
                                    te_data_all$game_date_ident,
                                    te_data_all$gameday)
    }

    helper_cols <- grep("(_ident)$", names(te_data_all), value = TRUE)
    if (length(helper_cols) > 0) {
      te_data_all <- te_data_all[, setdiff(names(te_data_all), helper_cols), drop = FALSE]
    }

    recovered_ids <- sum(is.na(orig_game_id) & !is.na(te_data_all$game_id))
    recovered_dates <- sum(is.na(orig_gameday) & !is.na(te_data_all$gameday))
    log_msg("Identity enrichment: recovered game_id for", recovered_ids, "rows; recovered gameday for", recovered_dates, "rows")
  }

  te_data <- te_data_all[te_data_all$season %in% seasons_train, , drop = FALSE]
  if (nrow(te_data) == 0) {
    stop("No TE training rows available for seasons: ", paste(seasons_train, collapse = ", "))
  }

  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  if (!exists("read_te_weekly_stats_cache")) {
    if (file.exists(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"))) {
      source(file.path(repo_root, "R", "data", "build_weekly_player_layers.R"), local = TRUE)
    } else {
      stop("Simulation bootstrap incomplete: read_te_weekly_stats_cache not loaded.")
    }
  }
  te_stats_all <- read_te_weekly_stats_cache()
  if (nrow(te_stats_all) == 0) {
    stop("TE weekly stats cache is empty.")
  }

  read_optional_parquet <- function(path, label) {
    if (!file.exists(path)) {
      return(NULL)
    }
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read ", label, ".")
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
    if (!exists("build_te_feature_row_for_simulation")) {
      stop("build_te_feature_row_for_simulation not loaded.")
    }
    build_result <- build_te_feature_row_for_simulation(
      te_weekly_features = te_data_all,
      te_weekly_stats = te_stats_all,
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

  if (!is.null(season) && !is.na(season) && !is.na(identified_game_row$season) && identified_game_row$season != season) {
    stop("GAME IDENTIFICATION DRIFT: CLI requested season=", season,
         " but identified game has season=", identified_game_row$season, ".")
  }
  if (!is.null(week) && !is.na(week) && !is.na(identified_game_row$week) && identified_game_row$week != week) {
    stop("GAME IDENTIFICATION DRIFT: CLI requested week=", week,
         " but identified game has week=", identified_game_row$week, ".")
  }

  resolved_game_key <- if (is_valid_string(identified_game_row$game_key)) identified_game_row$game_key else NA_character_

  game_id <- if ("game_id" %in% names(identified_game_row) && is_valid_string(identified_game_row$game_id)) {
    identified_game_row$game_id
  } else {
    NA_character_
  }
  if (!is_valid_string(game_id) && is_valid_string(resolved_game_key)) {
    game_id <- resolved_game_key
  }

  if (!is.null(season) && !is.na(season)) {
    game_season <- as.integer(season)
  } else if (is_valid_scalar(identified_game_row$season) && !is.na(identified_game_row$season)) {
    game_season <- as.integer(identified_game_row$season)
  } else {
    game_season <- NA_integer_
  }

  if (!is.null(week) && !is.na(week)) {
    game_week <- as.integer(week)
  } else if (is_valid_scalar(identified_game_row$week) && !is.na(identified_game_row$week)) {
    game_week <- as.integer(identified_game_row$week)
  } else {
    game_week <- NA_integer_
  }

  if (!is.null(game_date) && !is.na(game_date)) {
    game_gameday <- as.Date(game_date)
  } else if (is_valid_scalar(identified_game_row$gameday) && !is.na(identified_game_row$gameday)) {
    game_gameday <- as.Date(identified_game_row$gameday)
  } else if ("game_date" %in% names(identified_game_row) && is_valid_scalar(identified_game_row$game_date) && !is.na(identified_game_row$game_date)) {
    game_gameday <- as.Date(identified_game_row$game_date)
  } else {
    game_gameday <- as.Date(NA)
  }

  player_opponent <- if (is_valid_string(identified_game_row$opponent)) identified_game_row$opponent else NA_character_
  player_team <- if (is_valid_string(identified_game_row$team)) identified_game_row$team else NA_character_
  player_home_away <- if (is_valid_string(identified_game_row$home_away)) identified_game_row$home_away else NA_character_

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

  result$metadata$game_id <- game_id
  result$metadata$game_key <- resolved_game_key
  result$metadata$player_id <- player_id
  result$metadata$player_name <- if ("player_name" %in% names(identified_game_row)) identified_game_row$player_name else NA_character_
  result$metadata$team <- player_team
  result$metadata$opponent <- player_opponent
  result$metadata$season <- game_season
  result$metadata$week <- game_week
  result$metadata$game_date <- game_gameday
  result$metadata$home_away <- player_home_away
  result$metadata$position <- if ("position" %in% names(identified_game_row)) identified_game_row$position else "TE"
  result$metadata$is_rookie <- if ("is_rookie" %in% names(identified_game_row)) identified_game_row$is_rookie else FALSE

  if (!is_future && (is.null(result$metadata$game_id) || is.na(result$metadata$game_id) || result$metadata$game_id == "")) {
    stop("TE simulation requires non-missing game_id for observed games. Check schedule resolution.")
  }

  te_data_pre <- te_data
  if (!is.na(game_gameday)) {
    te_data_pre <- te_data_pre[is.na(te_data_pre$gameday) | te_data_pre$gameday < game_gameday, ]
  }
  if (nrow(te_data_pre) == 0 || is.na(game_gameday)) {
    te_data_pre <- te_data[te_data$season < game_season |
                             (te_data$season == game_season & te_data$week < game_week), ]
  }
  if (!is.na(game_season) && !is.na(game_week)) {
    te_data_pre <- te_data_pre[
      !(te_data_pre$player_id == player_id &
          te_data_pre$season == season &
          te_data_pre$week == week),
      , drop = FALSE
    ]
  }
  if (!is.na(resolved_game_key)) {
    te_data_pre <- te_data_pre[is.na(te_data_pre$game_key) | te_data_pre$game_key != resolved_game_key, ]
  }
  if (!is.na(game_id)) {
    te_data_pre <- te_data_pre[is.na(te_data_pre$game_id) | te_data_pre$game_id != game_id, ]
  }

  if (isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) {
    message("TE training rows after filters: ", nrow(te_data_pre))
  }
  if (nrow(te_data_pre) == 0) {
    stop("TE training data collapsed to 0 rows after filters.")
  }
  if (nrow(te_data_pre) < 1000) {
    stop("Training data collapsed: only ", nrow(te_data_pre), " rows remain before model fitting.")
  }

  if ((isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) &&
      exists("get_te_v1_targets")) {
    te_targets_diag <- get_te_v1_targets()
    for (tgt in te_targets_diag) {
      if (!tgt %in% names(te_data_pre)) {
        message("TE target missing: ", tgt)
      } else {
        vals <- te_data_pre[[tgt]]
        message("TE target stats ", tgt, ": min=", suppressWarnings(min(vals, na.rm = TRUE)),
                " max=", suppressWarnings(max(vals, na.rm = TRUE)),
                " na=", sum(is.na(vals)),
                " all_na=", all(is.na(vals)))
      }
    }
  }

  # Recent performance: strictly prior games ordered by (season DESC, week DESC)
  recent_games <- data.frame()
  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  stats_path <- file.path(repo_root, "data", "cache", "te_weekly_stats.parquet")
  if (file.exists(stats_path) && requireNamespace("arrow", quietly = TRUE)) {
    te_stats <- tryCatch(
      arrow::read_parquet(stats_path),
      error = function(e) NULL
    )
    if (!is.null(te_stats) && nrow(te_stats) > 0) {
      player_history <- te_stats[
        te_stats$player_id == player_id &
          (te_stats$season < game_season |
             (te_stats$season == game_season & te_stats$week < game_week)),
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

  if (!exists("get_te_features_by_week")) {
    stop("get_te_features_by_week not available for time-aware feature selection.")
  }
  feature_cols <- get_te_features_by_week(game_week)
  if (is_counterfactual_policy(availability_policy_used)) {
    feature_cols <- unique(c(feature_cols, "position"))
  }
  feature_cols <- unique(c(feature_cols, intersect(c("defense_data_available", "rolling_window_complete"), names(identified_game_row))))

  dropped_features <- intersect(dropped_features, feature_cols)
  available_feature_cols <- feature_cols[
    feature_cols %in% names(identified_game_row) &
      !feature_cols %in% dropped_features
  ]
  player_feature_row <- identified_game_row[, available_feature_cols, drop = FALSE]

  if (!exists("get_passing_defense_all_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/passing_defense_features.R")
    }
  }
  def_features <- get_passing_defense_all_features()
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

  te_models <- fit_te_models(te_data_pre, min_rows = 200)
  result$diagnostics$model_diagnostics <- te_models$diagnostics

  trace_regime <- determine_te_regime(game_week)
  te_targets <- get_te_v1_targets()
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
  for (tgt in te_targets) {
    mk <- get_te_model_key(tgt, trace_regime)
    diag_entry <- te_models$diagnostics[[mk]]
    model_trace$targets[[tgt]] <- list(
      used_model = if (!is.null(diag_entry$fit_type) && diag_entry$fit_type == "baseline") "baseline" else "fitted",
      n_train = if (!is.null(diag_entry$n_rows_final)) diag_entry$n_rows_final else NA_integer_,
      n_non_na = if (tgt %in% names(te_data_pre)) sum(!is.na(te_data_pre[[tgt]])) else NA_integer_,
      fallback_reason = if (!is.null(diag_entry$fallback_reason)) diag_entry$fallback_reason else NA_character_
    )
  }
  result$diagnostics$model_trace <- model_trace

  player_feature_row$week <- game_week
  if (!"week" %in% names(player_feature_row) || is.na(player_feature_row$week)) {
    stop("run_te_simulation failed to attach 'week' to feature_row.")
  }

  sim_result <- simulate_te_game(
    feature_row = player_feature_row,
    te_models = te_models,
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
    resolve_schema_path("TE", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "TE", "te_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
  } else {
    stop("Missing TE schema at ", schema_path)
  }
  if (exists("resolve_te_simulation_schema")) {
    result$draws <- resolve_te_simulation_schema(result$draws)
  } else {
    stop("resolve_te_simulation_schema not found.")
  }

  if (!exists("compute_ppr_wrte")) {
    if (file.exists("R/utils/ppr_scoring.R")) {
      source("R/utils/ppr_scoring.R", local = TRUE)
    } else {
      stop("Simulation bootstrap incomplete: R/utils/ppr_scoring.R not found")
    }
  }

  # Fantasy PPR must be computed from draw-level stats (no legacy totals)
  if (!"fantasy_ppr" %in% names(result$draws)) {
    result$draws$fantasy_ppr <- compute_ppr_wrte(
      receptions = result$draws$receptions,
      rec_yards = result$draws$receiving_yards,
      rec_tds = result$draws$receiving_tds
    )
  }
  component_non_na <- !is.na(result$draws$receptions) |
    !is.na(result$draws$receiving_yards) |
    !is.na(result$draws$receiving_tds)
  if (any(component_non_na & is.na(result$draws$fantasy_ppr))) {
    stop("Fantasy PPR contains NA while component stats are non-NA. ",
         "This indicates a draw-level scoring error.")
  }

  if (exists("compute_te_percentiles")) {
    result$summary <- compute_te_percentiles(result$draws)
  }

  if (nrow(result$summary) > 0 && !"fantasy_ppr" %in% result$summary$stat) {
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
        p90 = as.numeric(q[7]),
        stringsAsFactors = FALSE
      )
    )
  }

  draws_df <- result$draws
  result$diagnostics$distribution_stats <- list(
    targets_mean = mean(draws_df$targets, na.rm = TRUE),
    targets_sd = sd(draws_df$targets, na.rm = TRUE),
    receptions_mean = mean(draws_df$receptions, na.rm = TRUE),
    receptions_sd = sd(draws_df$receptions, na.rm = TRUE),
    receiving_yards_mean = mean(draws_df$receiving_yards, na.rm = TRUE),
    receiving_yards_sd = sd(draws_df$receiving_yards, na.rm = TRUE),
    total_touchdowns_mean = mean(draws_df$total_touchdowns, na.rm = TRUE),
    total_touchdowns_sd = sd(draws_df$total_touchdowns, na.rm = TRUE)
  )

  if ("total_touchdowns" %in% names(draws_df)) {
    result$diagnostics$td_probabilities <- list(
      prob_0_tds = mean(draws_df$total_touchdowns == 0),
      prob_ge1_td = mean(draws_df$total_touchdowns >= 1),
      prob_ge2_tds = mean(draws_df$total_touchdowns >= 2)
    )
  }

  if (is.null(result$draws)) {
    stop("Simulation draws are NULL.")
  }
  if (nrow(result$draws) != n_sims) {
    stop("Simulation draws have incorrect length.")
  }

  required_outcomes <- c("targets", "receptions", "receiving_yards", "total_touchdowns")
  for (outcome in required_outcomes) {
    if (!outcome %in% names(result$draws)) {
      stop("Required outcome column '", outcome, "' is missing from simulation draws.")
    }
    outcome_vec <- result$draws[[outcome]]
    if (is.null(outcome_vec)) {
      stop("Outcome column '", outcome, "' is NULL.")
    }
    if (length(outcome_vec) != n_sims) {
      stop("Outcome column '", outcome, "' has incorrect length.")
    }
    if (any(sapply(outcome_vec, is.null))) {
      stop("Outcome column '", outcome, "' contains NULL values.")
    }
  }

  result
}
