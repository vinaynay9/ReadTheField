# Simulation Entry Point v1 (API entrypoint)
#
# Canonical API-safe wrapper for simulation consumers (frontend/API).
# Models are fit per request from cached data; cache changes can affect outputs.

normalize_summary_stats_v1 <- function(summary_df, position, draws = NULL) {
  if (is.null(summary_df) || nrow(summary_df) == 0 || !"stat" %in% names(summary_df)) {
    return(summary_df)
  }
  summary_df$stat <- trimws(as.character(summary_df$stat))
  position <- toupper(as.character(position))
  rename_map <- list()
  if (position %in% c("RB", "WR", "TE")) {
    rename_map <- c(
      rush_yards = "rushing_yards",
      rec_yards = "receiving_yards",
      rush_tds = "rushing_tds",
      rec_tds = "receiving_tds"
    )
  } else if (position == "QB") {
    rename_map <- c(
      target_pass_attempts_qb = "passing_attempts",
      pass_attempts = "passing_attempts",
      target_completions_qb = "passing_completions",
      target_pass_yards_qb = "passing_yards",
      pass_yards = "passing_yards",
      target_pass_tds_qb = "passing_tds",
      pass_tds = "passing_tds",
      target_interceptions_qb_thrown = "interceptions_thrown",
      interceptions = "interceptions_thrown",
      target_sacks_qb_taken = "qb_sacks_taken",
      sacks_taken = "qb_sacks_taken",
      target_qb_rush_attempts = "qb_rush_attempts",
      rush_attempts = "qb_rush_attempts",
      target_qb_rush_yards = "qb_rush_yards",
      rush_yards = "qb_rush_yards",
      target_qb_rush_tds = "qb_rush_tds",
      rush_tds = "qb_rush_tds"
    )
  } else if (position == "K") {
    rename_map <- c(
      target_fg_attempts_k = "fg_attempts",
      target_fg_made_k = "fg_made",
      target_pat_made_k = "pat_made"
    )
  }

  for (nm in names(rename_map)) {
    summary_df$stat[summary_df$stat == nm] <- rename_map[[nm]]
  }

  if (position %in% c("WR", "TE")) {
    if (!"total_touchdowns" %in% summary_df$stat && "receiving_tds" %in% summary_df$stat) {
      td_rows <- summary_df[summary_df$stat == "receiving_tds", , drop = FALSE]
      if (nrow(td_rows) > 0) {
        summary_df <- rbind(
          summary_df,
          data.frame(
            stat = "total_touchdowns",
            p25 = td_rows$p25[1],
            p50 = td_rows$p50[1],
            p75 = td_rows$p75[1]
          )
        )
      }
    }
  }

  if (position == "RB") {
    if (!"total_touchdowns" %in% summary_df$stat &&
        all(c("rushing_tds", "receiving_tds") %in% summary_df$stat) &&
        !is.null(draws)) {
      td_vec <- draws$rush_tds + draws$rec_tds
      summary_df <- rbind(
        summary_df,
        data.frame(
          stat = "total_touchdowns",
          p25 = stats::quantile(td_vec, 0.25, na.rm = TRUE),
          p50 = stats::quantile(td_vec, 0.50, na.rm = TRUE),
          p75 = stats::quantile(td_vec, 0.75, na.rm = TRUE)
        )
      )
    }
  }

  summary_df
}

simulate_player_game_v1 <- function(player_id,
                                    season,
                                    week,
                                    n_sims = NULL,
                                    availability_policy = "played_only",
                                    seed = NULL,
                                    schema_version = "v1",
                                    mode = NULL,
                                    schedule_opponent = NULL,
                                    schedule_home_away = NULL) {
  # Wrap entire simulation in tryCatch to ensure structured errors.
  tryCatch({
  required_functions <- c(
    "simulate_player_game",
    "validate_report_schema_v1",
    "handle_warnings_v1",
    "validate_availability_policy",
    "get_available_seasons_from_cache",
    "read_player_dim_cache",
    "validate_simulation_request",
    "build_game_key"
  )
  missing <- required_functions[!sapply(required_functions, exists)]
  if (length(missing) > 0) {
    stop(
      "Simulation dependencies not loaded: ",
      paste(missing, collapse = ", "),
      ". Source R/simulation/bootstrap_simulation.R."
    )
  }

  if (exists("ensure_cache_fingerprint")) {
    root <- getOption("READTHEFIELD_REPO_ROOT")
    if (is.null(root) || !nzchar(root)) {
      root <- "."
    }
    ensure_cache_fingerprint(c(
      file.path(root, "data", "cache", "player_directory.parquet"),
      file.path(root, "data", "cache", "player_week_identity.parquet"),
      file.path(root, "data", "processed", "player_dim.parquet"),
      file.path(root, "data", "cache", "rb_weekly_stats.parquet"),
      file.path(root, "data", "cache", "wr_weekly_stats.parquet"),
      file.path(root, "data", "cache", "te_weekly_stats.parquet"),
      file.path(root, "data", "cache", "qb_weekly_stats.parquet"),
      file.path(root, "data", "cache", "k_weekly_stats.parquet")
    ))
  }

  make_error_v1 <- function(error_type, error_code, message, details = list(), meta = list()) {
    list(
      ok = FALSE,
      status = "error",
      error_type = error_type,
      error_code = error_code,
      message = message,
      details = details,
      metadata = meta
    )
  }

  validate_inputs <- function() {
    mode_val <- if (is.null(mode) || !nzchar(trimws(as.character(mode)))) {
      "historical_replay"
    } else {
      as.character(mode)[1]
    }
    allowed_modes <- c("historical_replay", "upcoming_game", "hypothetical_matchup")
    if (!mode_val %in% allowed_modes) {
      return(make_error_v1(
        "invalid_input",
        "invalid_mode",
        "mode is not supported.",
        list(mode = mode_val)
      ))
    }
    if (is.null(schema_version) || !nzchar(as.character(schema_version))) {
      return(make_error_v1(
        "invalid_input",
        "invalid_schema_version",
        "schema_version is required.",
        list(schema_version = schema_version)
      ))
    }
    if (!identical(as.character(schema_version), get_report_schema_version_v1())) {
      return(make_error_v1(
        "invalid_input",
        "unsupported_schema_version",
        "schema_version is not supported.",
        list(schema_version = schema_version)
      ))
    }
    if (is.null(player_id) || !nzchar(trimws(as.character(player_id)))) {
      return(make_error_v1("invalid_input", "player_id_missing", "player_id is required."))
    }
    if (is.null(season) || is.na(season)) {
      return(make_error_v1("invalid_input", "season_missing", "season is required."))
    }
    if (is.null(week) || is.na(week)) {
      return(make_error_v1("invalid_input", "week_missing", "week is required."))
    }
    season <- as.integer(season)
    week <- as.integer(week)
    if (!is.finite(season) || season < 1990 || season > 2100) {
      return(make_error_v1("invalid_input", "season_out_of_range", "season is out of range.",
                           list(season = season)))
    }
    if (!is.finite(week) || week < 1 || week > 18) {
      return(make_error_v1("invalid_input", "week_out_of_range", "week must be between 1 and 18.",
                           list(week = week)))
    }

    if (!exists("validate_availability_policy")) {
      return(make_error_v1("internal_error", "availability_policy_missing",
                           "Availability policy validation is unavailable."))
    }
    availability_policy <- tryCatch(
      validate_availability_policy(availability_policy),
      error = function(e) e
    )
    if (inherits(availability_policy, "error")) {
      return(make_error_v1("invalid_input", "availability_policy_invalid",
                           conditionMessage(availability_policy)))
    }

    if (!exists("get_available_seasons_from_pwi") && !exists("get_available_seasons_from_cache")) {
      return(make_error_v1("internal_error", "cache_helpers_missing",
                           "Cache helpers are not loaded."))
    }
    if (exists("get_available_seasons_from_pwi")) {
      available <- get_available_seasons_from_pwi()
    } else {
      available <- get_available_seasons_from_cache()
    }
    if (length(available) == 0) {
      return(make_error_v1("data_unavailable", "seasons_unavailable",
                           "No seasons available in cache. Run refresh_weekly_cache."))
    }
    if (!season %in% available) {
      return(make_error_v1("data_unavailable", "season_not_available",
                           "Requested season is not available in cache.",
                           list(season = season)))
    }

    if (!exists("read_player_dim_cache")) {
      return(make_error_v1("internal_error", "player_dim_missing",
                           "Player dimension cache loader not available."))
    }
    player_dim <- read_player_dim_cache()
    if (is.null(player_dim) || nrow(player_dim) == 0) {
      return(make_error_v1("data_unavailable", "player_dim_empty",
                           "Player dimension cache is empty. Run refresh_weekly_cache."))
    }
    player_id <- as.character(player_id)
    dim_row <- player_dim[player_dim$player_id == player_id & player_dim$season == season, , drop = FALSE]
    if (nrow(dim_row) == 0) {
      any_row <- player_dim[player_dim$player_id == player_id, , drop = FALSE]
      if (nrow(any_row) == 0) {
        return(make_error_v1("invalid_input", "player_id_unknown",
                             "player_id not found in cache.",
                             list(player_id = player_id)))
      }
      dim_row <- any_row[order(any_row$season, decreasing = TRUE), , drop = FALSE]
    }
    position <- toupper(as.character(dim_row$position[1]))
    if (is.na(position) || !nzchar(position)) {
      return(make_error_v1("data_unavailable", "position_missing",
                           "Player position is missing in cache.",
                           list(player_id = player_id)))
    }
    if (exists("resolve_schema_path")) {
      schema_path <- resolve_schema_path(position, "v1")
      source(schema_path, local = TRUE)
    }
    if (exists("resolve_regime_path")) {
      regime_path <- resolve_regime_path(position, "v1")
      source(regime_path, local = TRUE)
    }
    if (identical(position, "WR")) {
      if (!exists("read_wr_weekly_features_cache")) {
        return(make_error_v1("data_unavailable", "missing_wr_features",
                             "WR feature cache loader not available.",
                             list(player_id = player_id)))
      }
      wr_features <- read_wr_weekly_features_cache()
      if (is.null(wr_features) || nrow(wr_features) == 0) {
        return(make_error_v1("data_unavailable", "missing_wr_features",
                             "WR feature cache is empty.",
                             list(player_id = player_id)))
      }
    }

    get_min_sims <- function() {
      val <- getOption("READTHEFIELD_MIN_SIMS", NA_integer_)
      if (is.null(val) || is.na(val) || val < 1) {
        env_val <- Sys.getenv("READTHEFIELD_MIN_SIMS", "")
        if (nzchar(env_val)) {
          val <- suppressWarnings(as.integer(env_val))
        }
      }
      if (is.null(val) || is.na(val) || val < 1) {
        val <- 100L
      }
      as.integer(val)
    }

    limits <- list(
      RB = list(default = 1000L, max = 20000L),
      WR = list(default = 1000L, max = 20000L),
      TE = list(default = 1000L, max = 20000L),
      QB = list(default = 1000L, max = 20000L),
      K = list(default = 500L, max = 10000L)
    )
    if (!position %in% names(limits)) {
      return(make_error_v1("invalid_input", "position_unsupported",
                           "Position is not supported.",
                           list(position = position)))
    }
    n_sims_val <- if (is.null(n_sims) || is.na(n_sims)) limits[[position]]$default else as.integer(n_sims)
    min_sims <- get_min_sims()
    if (!is.finite(n_sims_val) || n_sims_val < min_sims) {
      return(make_error_v1("invalid_input", "n_sims_too_small",
                           paste0("n_sims must be at least ", min_sims, "."),
                           list(n_sims = n_sims_val, min_sims = min_sims)))
    }
    if (n_sims_val > limits[[position]]$max) {
      return(make_error_v1("invalid_input", "n_sims_too_large",
                           "n_sims exceeds hard limit.",
                           list(n_sims = n_sims_val, max = limits[[position]]$max)))
    }

    schedule_opponent_val <- if (!is.null(schedule_opponent) && nzchar(trimws(as.character(schedule_opponent)))) {
      toupper(trimws(as.character(schedule_opponent)))
    } else {
      NULL
    }
    schedule_home_away_val <- if (!is.null(schedule_home_away) && nzchar(trimws(as.character(schedule_home_away)))) {
      toupper(trimws(as.character(schedule_home_away)))
    } else {
      NULL
    }

    list(
      player_id = player_id,
      season = season,
      week = week,
      n_sims = n_sims_val,
      availability_policy = availability_policy,
      position = position,
      mode = mode_val,
      schedule_opponent = schedule_opponent_val,
      schedule_home_away = schedule_home_away_val
    )
  }

  runner <- function() {
    validated <- validate_inputs()
    if (is.list(validated) && !is.null(validated$status) && validated$status == "error") {
      validated$metadata <- list(
        player_id = player_id,
        season = as.integer(season),
        week = as.integer(week),
        n_sims = if (is.null(n_sims)) NA_integer_ else as.integer(n_sims),
        position = NA_character_,
        availability_policy = availability_policy,
        report_schema_version = get_report_schema_version_v1(),
        seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
        error_code = validated$error_code
      )
      return(validated)
    }

    if (!is.null(seed)) {
      set.seed(as.integer(seed))
    }

    preflight <- validate_simulation_request(
      player_id = validated$player_id,
      season = validated$season,
      week = validated$week,
      position = validated$position,
      availability_policy = validated$availability_policy
    )
    if (is.list(preflight) && isFALSE(preflight$ok)) {
      return(make_error_v1(
        preflight$error_type,
        preflight$error_code,
        preflight$message,
        preflight$details,
        meta = list(
          player_id = validated$player_id,
          season = validated$season,
          week = validated$week,
          n_sims = validated$n_sims,
          position = validated$position,
          availability_policy = validated$availability_policy,
          report_schema_version = get_report_schema_version_v1(),
          seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
          error_code = preflight$error_code
        )
      ))
    }

    result <- simulate_player_game(
      gsis_id = validated$player_id,
      season = validated$season,
      week = validated$week,
      n_sims = validated$n_sims,
      availability_policy = validated$availability_policy,
      mode = validated$mode,
      schedule_opponent = validated$schedule_opponent,
      schedule_home_away = validated$schedule_home_away
    )
    if (is.null(result) || !is.list(result)) {
      return(make_error_v1("internal_error", "invalid_result", "Simulation returned an invalid result."))
    }
    if (!is.null(result$status) && identical(result$status, "error")) {
      map <- list(
        PLAYER_NOT_FOUND = "invalid_input",
        PLAYER_RETIRED_OR_NO_RECENT_DATA = "data_unavailable",
        INSUFFICIENT_HISTORY = "data_unavailable",
        PLAYER_INACTIVE = "player_inactive"
      )
      err_type <- map[[as.character(result$error_type)]]
      if (is.null(err_type)) err_type <- "simulation_error"
      return(make_error_v1(
        err_type,
        tolower(as.character(result$error_type)),
        as.character(result$reason),
        list(player_name = result$player_name),
        meta = list(
          player_id = validated$player_id,
          season = validated$season,
          week = validated$week,
          n_sims = validated$n_sims,
          position = validated$position,
          availability_policy = validated$availability_policy,
          report_schema_version = get_report_schema_version_v1(),
          seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
          error_code = tolower(as.character(result$error_type))
        )
      ))
    }

    position <- toupper(as.character(result$metadata$position))
    if (is.na(position) || !nzchar(position)) {
      return(make_error_v1("internal_error", "position_missing",
                           "Simulation result missing metadata.position."))
    }
    result$metadata$position <- position
    if (!is.null(result$diagnostics$availability$policy)) {
      result$metadata$availability_policy <- as.character(result$diagnostics$availability$policy)
    } else {
      result$metadata$availability_policy <- validated$availability_policy
    }
    result$metadata$report_schema_version <- get_report_schema_version_v1()
    result$metadata$seed <- if (is.null(seed)) NA_integer_ else as.integer(seed)
    result$metadata$error_code <- NA_character_

    result$summary <- normalize_summary_stats_v1(result$summary, result$metadata$position, result$draws)

    if (is.null(result$metadata) || length(result$metadata) == 0) {
      return(make_error_v1("internal_error", "metadata_missing",
                           "Simulation result missing metadata."))
    }
    if (is.null(result$summary) || nrow(result$summary) == 0) {
      return(make_error_v1("internal_error", "summary_missing",
                           "Simulation result missing summary statistics."))
    }

    # Build summary_tables from summary stats using report schema targets.
    if (is.null(result$summary_tables) || length(result$summary_tables) == 0) {
      target_stats <- get_report_required_summary_v1(result$metadata$position)
      summary_tables <- list()
      for (stat in target_stats) {
        rows <- result$summary[result$summary$stat == stat, , drop = FALSE]
        if (nrow(rows) > 0) {
          summary_tables[[stat]] <- rows
        }
      }
      result$summary_tables <- summary_tables
    }
    if (length(result$summary_tables) == 0) {
      return(make_error_v1(
        "report_error",
        "no_reportable_stats",
        "Simulation ran but produced no reportable statistics.",
        details = list(reason = "No targets matched simulated outputs.")
      ))
    }

    validate_report_schema_v1(result)
    result$ok <- TRUE
    result$metadata$simulation_function <- "simulate_player_game_v1"
    result$metadata$mode <- validated$mode
    list(
      ok = TRUE,
      data = result,
      metadata = list(
        player_id = validated$player_id,
        position = validated$position,
        season = validated$season,
        week = validated$week,
        mode = validated$mode,
        simulation_function = "simulate_player_game_v1"
      )
    )
  }

  safe_runner <- function() {
    tryCatch(
      runner(),
      error = function(e) {
        make_error_v1("internal_error", "internal_error", conditionMessage(e))
      }
    )
  }

  if (exists("handle_warnings_v1")) {
    handle_warnings_v1(safe_runner(), context = "simulate_player_game_v1")
  } else {
    safe_runner()
  }
  }, error = function(e) {
    list(
      ok = FALSE,
      error_code = "simulation_failed",
      message = conditionMessage(e),
      metadata = list(
        player_id = player_id,
        position = NA_character_,
        season = season,
        week = week,
        mode = mode
      )
    )
  })
}

# Minimal WR smoke test helper (manual use; not executed automatically).
smoke_test_wr_simulate_player_game_v1 <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow required for WR smoke test.")
  }
  pwi_path <- file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "data", "cache", "player_week_identity.parquet")
  pwi <- as.data.frame(arrow::read_parquet(pwi_path))
  wr_rows <- pwi[pwi$position == "WR", , drop = FALSE]
  if (nrow(wr_rows) == 0) {
    stop("No WR rows found in player_week_identity.")
  }
  wr_rows <- wr_rows[order(wr_rows$season, wr_rows$week, decreasing = TRUE), , drop = FALSE]
  pid <- wr_rows$player_id[1]
  season <- wr_rows$season[1]
  week <- wr_rows$week[1]
  resp <- simulate_player_game_v1(
    player_id = pid,
    season = season,
    week = week,
    mode = "historical_replay",
    availability_policy = "played_only",
    schema_version = "v1",
    n_sims = 500,
    seed = 4242
  )
  if (is.null(resp) || isFALSE(resp$ok) || is.null(resp$data$summary) || nrow(resp$data$summary) == 0) {
    stop("WR smoke test failed.")
  }
  invisible(TRUE)
}
