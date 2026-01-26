# Simulate RB Game (v1 Contract)
#
# IMPORTANT:
# Rolling features are NEVER imputed.
# NA values indicate insufficient history and are valid inputs.
#
# Monte Carlo simulation for RB player-game outcomes following RB v1 contract.
# Simulation order:
#   1. Sample carries (Negative Binomial or Poisson)
#   2. Sample receptions (Negative Binomial or Poisson)
#   3. Sample rush_tds (Poisson or Negative Binomial)
#   4. Sample rec_tds (Poisson or Negative Binomial)
#
# NOTE: Yardage targets (rush_yards, rec_yards) are NOT in RB v1 schema.
# Yardage should be derived downstream if needed (e.g., carries * YPC).
#
# Fantasy points are NOT computed in this function - derived downstream using 50th percentile only.
#
# Dependencies:
#   - R/models/fit_rb_models.R (for validate_rb_models)
#
# Usage:
#   result <- simulate_rb_game(feature_row, rb_models, n_sims = 5000)

#' Simulate RB game outcomes (RB v1 contract)
#'
#' Runs Monte Carlo simulation to generate distribution of RB outcomes.
#' Returns raw simulation draws and percentile summary for 4 outcomes only.
#'
#' @param feature_row data.frame with one row containing pre-game features:
#'   - carries_roll3, carries_roll5
#'   - targets_roll3, targets_roll5
#'   - rush_tds_roll5, rec_tds_roll5
#'   - is_home
#' @param rb_models List of fitted models from fit_rb_models() (RB v1 contract)
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @return List with:
#'   - draws: data.frame with n_sims rows of simulated stat lines (4 outcomes)
#'   - summary: data.frame with p25, p50, p75 for each outcome
#'   - status: character indicating simulation status
simulate_rb_game <- function(feature_row, rb_models, n_sims = 5000, availability_policy = "played_only") {
  
  # Initialize file-based diagnostic logging (project root)
  log_file <- "rb_debug.log"
  # Note: Don't clear file here - it may have been initialized by fit_rb_models()
  # If file doesn't exist, create it
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }
  
  # CRITICAL: Load schema validation first
  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("RB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "utils", "rb_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
  } else {
    error_msg <- paste0("Missing RB schema at ", schema_path)
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  if (!exists("get_rb_v1_targets")) {
    error_msg <- "get_rb_v1_targets not loaded"
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Initialize result structure
  result <- list(
    draws = NULL,
    summary = NULL,
    status = "not_run"
  )
  
  # Validate inputs - fail loudly, no partial outputs
  if (is.null(feature_row) || nrow(feature_row) == 0) {
    error_msg <- "No feature row provided to simulate_rb_game. Cannot proceed with simulation."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  if (nrow(feature_row) > 1) {
    error_msg <- "Multiple feature rows provided to simulate_rb_game. Expected exactly one row."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Load regime system
  regime_path <- if (exists("resolve_regime_path")) {
    resolve_regime_path("RB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "utils", "rb_regime_v1.R")
  }
  if (file.exists(regime_path)) {
    source(regime_path, local = TRUE)
    # Ensure get_rb_features_by_week is available for time-aware feature selection
    if (!exists("get_rb_features_by_week")) {
      error_msg <- "get_rb_features_by_week function not found. Time-aware feature contracts require this function."
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  } else {
    error_msg <- paste0("Missing RB regime at ", regime_path)
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  if (exists("validate_availability_policy")) {
    availability_policy <- validate_availability_policy(availability_policy)
  }
  
  # Determine regime from week (forward-only: as-of-now predictions)
  if (!"week" %in% names(feature_row)) {
    error_msg <- "Missing 'week' column in feature_row. Cannot determine regime for simulation."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  week <- feature_row$week[1]
  log_msg("=== simulate_rb_game: Week Extraction ===")
  log_msg("Extracted week from feature_row: ", week)
  
  if (is.na(week)) {
    error_msg <- "Week is NA in feature_row. Cannot determine regime for simulation."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Forward-only guardrail: reject past dates (production simulation only)
  # NOTE: Historical replay/backtesting should use separate module
  if ("gameday" %in% names(feature_row) && !is.na(feature_row$gameday[1])) {
    game_date <- as.Date(feature_row$gameday[1])
    today <- Sys.Date()
    if (game_date < today) {
      error_msg <- paste("Production simulation does not support past dates. ",
           "Game date: ", game_date, " is before today: ", today, ". ",
           "Use separate backtesting module for historical replay.")
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  }
  
  # Validate week is reasonable (1-18)
  if (week < 1 || week > 18) {
    error_msg <- paste("Invalid week: ", week, ". Week must be between 1 and 18.")
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  regime <- determine_rb_regime(week)
  log_msg("Determined regime for week ", week, ": ", regime)
  
  # Validate regime is valid
  valid_regimes <- get_rb_regimes()
  if (!regime %in% valid_regimes) {
    error_msg <- paste("Invalid regime determined: ", regime, ". Valid regimes: ", paste(valid_regimes, collapse = ", "))
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  log_msg("Simulating for week", week, "-> regime:", regime)
  
  # Check if models are valid (RB v1 contract with regimes) - fail loudly
  if (is.null(rb_models)) {
    error_msg <- "RB models are NULL. Cannot proceed with simulation."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Get models list (new regime-based structure)
  if (!is.null(rb_models$models)) {
    models_list <- rb_models$models
  } else {
    # Legacy structure - convert to regime-based
    error_msg <- "RB models use legacy structure. Regime-based models required."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Determine prediction feature contract with graceful fallback for missing rolling history
  feature_contracts <- get_rb_features_by_regime()
  regime_order <- c("standard", "late", "mid", "early")
  start_idx <- match(regime, regime_order)
  candidate_regimes <- regime_order[seq(from = start_idx, to = length(regime_order))]
  
  prediction_regime <- NULL
  required_features <- NULL
  fallback_used <- FALSE
  fallback_reason <- NA_character_
  
  for (cand in candidate_regimes) {
    cand_features <- feature_contracts[[cand]]
    if (is.null(cand_features)) next
    missing_cols <- setdiff(cand_features, names(feature_row))
    if (length(missing_cols) > 0) {
      next
    }
    rolling_features <- grep("_roll[0-9]+$", cand_features, value = TRUE)
    stopifnot(all(rolling_features %in% names(feature_row)))
    optional_features <- c(
      "is_home",
      "is_rookie",
      "draft_round",
      "draft_pick_overall",
      grep("^prev_season", cand_features, value = TRUE),
      grep("_roll1$", cand_features, value = TRUE)
    )
    strict_features <- setdiff(cand_features, optional_features)
    na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
    if (length(na_strict) > 0) {
      next
    }
    prediction_regime <- cand
    required_features <- cand_features
    break
  }
  
  if (is.null(prediction_regime)) {
    if (availability_policy %in% c("expected_active", "force_counterfactual")) {
      cf_regime <- "counterfactual_prior"
      cand_features <- feature_contracts[[cf_regime]]
      if (is.null(cand_features)) {
        stop("Counterfactual regime contract missing. Cannot proceed with fallback.", call. = FALSE)
      }
      missing_cols <- setdiff(cand_features, names(feature_row))
      if (length(missing_cols) == 0) {
        optional_features <- c(
          "is_home",
          "is_rookie",
          "draft_round",
          "draft_pick_overall",
          grep("^prev_season", cand_features, value = TRUE),
          grep("_roll1$", cand_features, value = TRUE)
        )
        strict_features <- setdiff(cand_features, optional_features)
        na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
        if (length(na_strict) == 0) {
          prediction_regime <- cf_regime
          required_features <- cand_features
          fallback_used <- TRUE
          fallback_reason <- "No standard regime eligible (exposure-dependent features missing)."
        }
      }
    }
    if (is.null(prediction_regime)) {
      stop(
        "simulate_rb_game could not find a valid regime for prediction. ",
        "Missing columns or NA strict features across regimes."
      )
    }
  }
  
  # Validate presence of required columns for selected regime
  missing_features <- setdiff(required_features, names(feature_row))
  if (length(missing_features) > 0) {
    error_msg <- paste(
      "simulate_rb_game missing required feature columns: ",
      paste(missing_features, collapse = ", ")
    )
    log_msg("FATAL:", error_msg)
    stop(error_msg, call. = FALSE)
  }
  
  # NOTE: Rolling features may be NA in early weeks or with limited history. This mirrors training-time behavior.
  rolling_features <- grep("_roll[0-9]+$", required_features, value = TRUE)
  optional_features <- c(
    "is_home",
    "is_rookie",
    "draft_round",
    "draft_pick_overall",
    grep("^prev_season", required_features, value = TRUE),
    grep("_roll1$", required_features, value = TRUE)
  )
  strict_features <- setdiff(required_features, optional_features)
  na_features <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
  if (length(na_features) > 0) {
    error_msg <- paste(
      "simulate_rb_game found NA in required non-rolling features: ",
      paste(na_features, collapse = ", ")
    )
    log_msg("FATAL:", error_msg)
    stop(error_msg, call. = FALSE)
  }
  
  # Defensive guard against future-week leakage in feature names
  if (any(grepl("_roll", names(feature_row)))) {
    stopifnot(!any(grepl(paste0("week", week + 1), names(feature_row))))
  }

  # Validate regime-specific models exist for the selected prediction regime
  rb_targets <- get_rb_v1_targets()
  required_model_keys <- sapply(rb_targets, function(t) get_model_key(t, prediction_regime))
  missing_models <- required_model_keys[!sapply(required_model_keys, function(k) !is.null(models_list[[k]]))]
  if (length(missing_models) > 0) {
    error_msg <- paste("Missing required RB v1 models for regime '", prediction_regime, "': ", paste(missing_models, collapse = ", "),
         ". Cannot proceed with simulation.")
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # TIME-AWARE FIX: Prepare prediction data using week-based features only
  # This ensures training and prediction feature sets match exactly
  pred_data <- prepare_prediction_data(feature_row, week = week, required_features = required_features)

  get_baseline_value <- function(target_name, regime_name = NULL) {
    if (!is.null(regime_name)) {
      key <- get_model_key(target_name, regime_name)
      model <- models_list[[key]]
      if (!is.null(model) && !is.null(model$type) && model$type == "baseline") {
        return(as.numeric(model$value))
      }
    }
    baseline_keys <- names(models_list)[grepl(paste0("^", target_name, "__"), names(models_list))]
    for (key in baseline_keys) {
      model <- models_list[[key]]
      if (!is.null(model) && !is.null(model$type) && model$type == "baseline") {
        return(as.numeric(model$value))
      }
    }
    NA_real_
  }

  get_player_prior_mu <- function(target_name) {
    if (target_name == "target_carries") {
      if ("carries_prior" %in% names(feature_row) && is.finite(feature_row$carries_prior[1])) {
        return(as.numeric(feature_row$carries_prior[1]))
      }
      if ("carries_cum_mean" %in% names(feature_row) && is.finite(feature_row$carries_cum_mean[1])) {
        return(as.numeric(feature_row$carries_cum_mean[1]))
      }
    }
    if (target_name == "target_receptions") {
      if ("targets_prior" %in% names(feature_row) && is.finite(feature_row$targets_prior[1])) {
        return(as.numeric(feature_row$targets_prior[1]))
      }
      if ("targets_cum_mean" %in% names(feature_row) && is.finite(feature_row$targets_cum_mean[1])) {
        return(as.numeric(feature_row$targets_cum_mean[1]))
      }
    }
    NA_real_
  }

  build_fallback_mu <- function(target_name, regime_name) {
    player_prior <- get_player_prior_mu(target_name)
    if (is.finite(player_prior)) return(player_prior)
    regime_median <- get_baseline_value(target_name, regime_name)
    if (is.finite(regime_median)) return(regime_median)
    global_median <- get_baseline_value(target_name, NULL)
    if (is.finite(global_median)) return(global_median)
    0.5
  }
  
  # Initialize simulation storage (RB v1 outcomes only)
  sim_carries <- numeric(n_sims)
  sim_receptions <- numeric(n_sims)
  sim_rush_tds <- numeric(n_sims)
  sim_rec_tds <- numeric(n_sims)
  
  # Get models for this regime
  carries_model_key <- get_model_key("target_carries", prediction_regime)
  receptions_model_key <- get_model_key("target_receptions", prediction_regime)
  rush_tds_model_key <- get_model_key("target_rush_tds", prediction_regime)
  rec_tds_model_key <- get_model_key("target_rec_tds", prediction_regime)
  
  carries_model <- models_list[[carries_model_key]]
  receptions_model <- models_list[[receptions_model_key]]
  rush_tds_model <- models_list[[rush_tds_model_key]]
  rec_tds_model <- models_list[[rec_tds_model_key]]
  
  # Run simulations
  for (i in seq_len(n_sims)) {
    
    # 1. Sample carries
    sim_carries[i] <- sample_from_model(
      carries_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_carries", prediction_regime)
    )
    
    # 2. Sample receptions
    sim_receptions[i] <- sample_from_model(
      receptions_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_receptions", prediction_regime)
    )
    
    # 3. Sample rush_tds
    sim_rush_tds[i] <- sample_from_model(
      rush_tds_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_rush_tds", prediction_regime)
    )
    
    # 4. Sample rec_tds
    sim_rec_tds[i] <- sample_from_model(
      rec_tds_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_rec_tds", prediction_regime)
    )
  }
  
  # Derive yardage (RB v1: yardage is derived, not modeled)
  # Get YPC and YPT from features, prioritizing decayed priors for early season
  # Priority: ypc_prior (blended prev+current) > yards_per_carry_roll5 > default 4.0
  ypc <- if ("ypc_prior" %in% names(feature_row) && !is.na(feature_row$ypc_prior[1])) {
    feature_row$ypc_prior[1]
  } else if ("yards_per_carry_roll5" %in% names(feature_row) && !is.na(feature_row$yards_per_carry_roll5[1])) {
    feature_row$yards_per_carry_roll5[1]
  } else {
    # Fallback to default YPC if not available
    4.0
  }
  
  ypt <- if ("ypt_prior" %in% names(feature_row) && !is.na(feature_row$ypt_prior[1])) {
    feature_row$ypt_prior[1]
  } else if ("yards_per_target_roll5" %in% names(feature_row) && !is.na(feature_row$yards_per_target_roll5[1])) {
    feature_row$yards_per_target_roll5[1]
  } else {
    # Fallback to default YPT if not available
    7.0
  }
  
  if (availability_policy %in% c("expected_active", "force_counterfactual")) {
    if (!is.finite(ypc)) {
      # Counterfactual fallback: use safe median YPC
      ypc <- 4.0
    }
    if (!is.finite(ypt)) {
      # Counterfactual fallback: use safe median YPT
      ypt <- 7.0
    }
  }
  
  # Opponent-adjusted YPC (defensive context)
  opp_ypc_allowed <- if ("def_yards_per_rush_defense_allowed_roll5" %in% names(feature_row) &&
                         !is.na(feature_row$def_yards_per_rush_defense_allowed_roll5[1])) {
    feature_row$def_yards_per_rush_defense_allowed_roll5[1]
  } else {
    NA_real_
  }
  baseline_ypc <- 4.3
  defense_factor <- if (!is.na(opp_ypc_allowed)) opp_ypc_allowed / baseline_ypc else 1.0
  defense_factor <- min(max(defense_factor, 0.75), 1.25)
  ypc_adj <- ypc * defense_factor
  
  sim_rush_yards <- as.numeric(sim_carries) * ypc_adj
  sim_rec_yards <- as.numeric(sim_receptions) * ypt
  sim_rushing_yards <- as.numeric(sim_rush_yards)
  sim_receiving_yards <- as.numeric(sim_rec_yards)
  sim_total_yards <- sim_rushing_yards + sim_receiving_yards
  
  if (availability_policy %in% c("expected_active", "force_counterfactual")) {
    sim_carries[!is.finite(sim_carries)] <- 0
    sim_receptions[!is.finite(sim_receptions)] <- 0
    sim_rush_tds[!is.finite(sim_rush_tds)] <- 0
    sim_rec_tds[!is.finite(sim_rec_tds)] <- 0
    sim_rushing_yards[!is.finite(sim_rushing_yards)] <- 0
    sim_receiving_yards[!is.finite(sim_receiving_yards)] <- 0
    sim_total_yards[!is.finite(sim_total_yards)] <- 0
  }
  
  # Guardrails: ensure derived yardage is present and numeric
  if (any(!is.finite(sim_rushing_yards)) || any(!is.finite(sim_receiving_yards))) {
    error_msg <- "Derived rushing_yards or receiving_yards contain non-finite values."
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Compile draws (RB v1 outcomes + derived yardage)
  result$draws <- data.frame(
    carries = sim_carries,
    receptions = sim_receptions,
    rush_tds = sim_rush_tds,
    rec_tds = sim_rec_tds,
    rush_yards = sim_rush_yards,  # Derived
    rec_yards = sim_rec_yards,    # Derived
    rushing_yards = sim_rushing_yards,
    receiving_yards = sim_receiving_yards,
    total_yards = sim_total_yards,
    stringsAsFactors = FALSE
  )
  
  # Log simulation output schema for debugging
  log_msg("Simulation draws columns:")
  log_msg(paste(names(result$draws), collapse = ", "))
  
  # Defensive check: ensure all vectors are non-NULL and correct length
  required_outcomes <- c("carries", "receptions", "rush_tds", "rec_tds",
                         "rush_yards", "rec_yards", "rushing_yards",
                         "receiving_yards", "total_yards")
  for (outcome in required_outcomes) {
    if (is.null(result$draws[[outcome]])) {
      error_msg <- paste("Simulation output '", outcome, "' is NULL. All outputs must be numeric vectors.")
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
    if (!is.numeric(result$draws[[outcome]])) {
      error_msg <- paste("Simulation output '", outcome, "' is not numeric. All outputs must be numeric vectors.")
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
    if (length(result$draws[[outcome]]) != n_sims) {
      error_msg <- paste("Simulation output '", outcome, "' has incorrect length. Expected ", n_sims, ", got ", length(result$draws[[outcome]]), ".")
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  }
  
  # Compute percentiles
  result$summary <- compute_rb_percentiles(result$draws)
  result$status <- "success"
  result$diagnostics <- list(
    regime_selected = prediction_regime,
    fallback_used = fallback_used,
    fallback_reason = fallback_reason
  )
  
  return(result)
}


#' Prepare prediction data with defaults for NA values (time-aware)
#'
#' @param feature_row data.frame with one row
#' @param week Integer week number (optional, for time-aware feature selection)
#' @param required_features Character vector of required features (optional, if week not provided)
#' @param regime Character regime name (optional, legacy fallback)
#' @return data.frame suitable for predict()
prepare_prediction_data <- function(feature_row, week = NULL, required_features = NULL, regime = NULL) {
  
  df <- as.data.frame(feature_row)
  
  # TIME-AWARE FIX: Determine features based on week-of-game
  # This ensures prediction uses only features that existed at that time
  if (!is.null(week) && exists("get_rb_features_by_week")) {
    # Use week-based feature availability (time-aware)
    if (is.null(required_features)) {
      required_features <- get_rb_features_by_week(week)
    }
    # Validate required features exist
    missing_features <- setdiff(required_features, names(df))
    if (length(missing_features) > 0) {
      stop("Missing required features for week ", week, ": ", paste(missing_features, collapse = ", "))
    }
    rolling_features <- grep("_roll[0-9]+$", required_features, value = TRUE)
    optional_na_features <- c(
      "is_home",
      "is_rookie",
      "draft_round",
      "draft_pick_overall",
      grep("^prev_season", required_features, value = TRUE),
      grep("_roll1$", required_features, value = TRUE)
    )
    strict_features <- setdiff(required_features, optional_na_features)
    na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
    if (any(na_features)) {
      stop("NA values in required features for week ", week, ": ", 
           paste(strict_features[na_features], collapse = ", "))
    }
  } else if (!is.null(required_features)) {
    # Use explicitly provided required features
    missing_features <- setdiff(required_features, names(df))
    if (length(missing_features) > 0) {
      stop("Missing required features: ", paste(missing_features, collapse = ", "))
    }
    rolling_features <- grep("_roll[0-9]+$", required_features, value = TRUE)
    optional_na_features <- c(
      "is_home",
      "is_rookie",
      "draft_round",
      "draft_pick_overall",
      grep("^prev_season", required_features, value = TRUE),
      grep("_roll1$", required_features, value = TRUE)
    )
    strict_features <- setdiff(required_features, optional_na_features)
    na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
    if (any(na_features)) {
      stop("NA values in required features: ", 
           paste(strict_features[na_features], collapse = ", "))
    }
  } else if (!is.null(regime)) {
    # Legacy fallback: regime-based features
    regime_path <- if (exists("resolve_regime_path")) {
      resolve_regime_path("RB", "v1")
    } else {
      file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "utils", "rb_regime_v1.R")
    }
    if (file.exists(regime_path)) {
      source(regime_path, local = TRUE)
      feature_contracts <- get_rb_features_by_regime()
      required_features <- feature_contracts[[regime]]
      
      if (!is.null(required_features)) {
        missing_features <- setdiff(required_features, names(df))
        if (length(missing_features) > 0) {
          stop("Missing required features for regime '", regime, "': ", paste(missing_features, collapse = ", "))
        }
        rolling_features <- grep("_roll[0-9]+$", required_features, value = TRUE)
        optional_na_features <- c(
          "is_home",
          "is_rookie",
          "draft_round",
          "draft_pick_overall",
          grep("^prev_season", required_features, value = TRUE),
          grep("_roll1$", required_features, value = TRUE)
        )
        strict_features <- setdiff(required_features, optional_na_features)
        na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
        if (any(na_features)) {
          stop("NA values in required features for regime '", regime, "': ", 
               paste(strict_features[na_features], collapse = ", "))
        }
      }
    } else {
      stop("Missing RB regime at ", regime_path)
    }
  }
  
  return(df)
}


#' Safe predict wrapper with baseline model support
#'
#' @param model Fitted model object or baseline model
#' @param newdata data.frame for prediction
#' @param type Prediction type (default "response")
#' @param n_samples Integer, number of samples to generate (for baseline models)
#' @return Numeric vector of predictions
predict_safe <- function(model, newdata, type = "response", n_samples = 1,
                         availability_policy = "played_only", fallback_mu = 0.5) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot make prediction. This indicates a model/data mismatch.")
  }
  
  # Handle baseline models
  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      # For counts, use Poisson with mean = value
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          # Counterfactual fallback: ensure finite Poisson mean
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      result <- rpois(n_samples, lambda = mu)
    } else {
      # For continuous, use Gaussian with truncation at 0
      result <- pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd))
    }
    return(result)
  }
  
  # Unwrap nested model objects (some fit_* helpers return lists with $fit/$model)
  model_obj <- model
  if (is.list(model) && !inherits(model, c("glm", "lm", "negbin"))) {
    if (!is.null(model$fit)) {
      model_obj <- model$fit
    } else if (!is.null(model$model)) {
      model_obj <- model$model
    }
  }

  # Handle regular models
  tryCatch({
    pred <- predict(model_obj, newdata = newdata, type = type)
    as.numeric(pred)
  }, error = function(e) {
    stop("Prediction failed for model. Error: ", e$message, 
         ". This indicates a model/data schema mismatch. Cannot proceed with simulation.")
  })
}

#' Sample from a model prediction (handles baseline and regular models)
#'
#' @param model Fitted model object or baseline model
#' @param newdata data.frame for prediction
#' @param n_samples Integer, number of samples
#' @return Numeric vector of samples
sample_from_model <- function(model, newdata, n_samples = 1, availability_policy = "played_only", fallback_mu = 0.5) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot sample. This indicates a model/data mismatch.")
  }
  
  # Handle baseline models
  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      # For counts, use Poisson with mean = value, round to integer
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          # Counterfactual fallback for baseline Poisson
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
    } else {
      # For continuous, use Gaussian with truncation at 0
      return(pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd)))
    }
  }
  
  # Unwrap nested model objects (some fit_* helpers return lists with $fit/$model)
  model_obj <- model
  if (is.list(model) && !inherits(model, c("glm", "lm", "negbin"))) {
    if (!is.null(model$fit)) {
      model_obj <- model$fit
    } else if (!is.null(model$model)) {
      model_obj <- model$model
    }
  }

  # Handle regular models
  # Get mean prediction
  mu <- predict_safe(
    model_obj,
    newdata,
    type = "response",
    n_samples = 1,
    availability_policy = availability_policy,
    fallback_mu = fallback_mu
  )
  
  # Determine model type and sample accordingly
  if (inherits(model_obj, "glm")) {
    family_name <- model_obj$family$family
    if (family_name == "poisson" || family_name == "quasipoisson") {
      # Poisson: sample directly
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        mu[!is.finite(mu)] <- fallback_mu
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
    } else if (family_name == "negbin" || inherits(model_obj, "negbin")) {
      # Negative Binomial: need theta
      theta <- if (!is.null(model_obj$theta)) model_obj$theta else 1.0
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        mu[!is.finite(mu)] <- fallback_mu
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rnbinom(n_samples, size = theta, mu = mu))))) 
    }
  } else if (inherits(model_obj, "lm")) {
    # Linear model: check for transform
    if (!is.null(model_obj$transform) && model_obj$transform == "log1p") {
      # Back-transform log1p
      sigma_val <- get_residual_sd(model_obj)
      samples <- rnorm(n_samples, mean = mu, sd = sigma_val)
      return(pmax(0, expm1(samples)))
    } else {
      # Regular Gaussian
      sigma_val <- get_residual_sd(model_obj)
      return(pmax(0, rnorm(n_samples, mean = mu, sd = sigma_val)))
    }
  }
  
  # Fallback: return mean
  return(rep(mu, n_samples))
}


#' Get residual standard deviation from linear model or baseline
#'
#' @param model lm object or baseline model
#' @return Numeric, residual SD
get_residual_sd <- function(model) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot get residual SD. This indicates a model/data mismatch.")
  }
  
  # Handle baseline models
  if (!is.null(model$type) && model$type == "baseline") {
    return(model$sd)
  }
  
  # Handle regular models
  tryCatch({
    sigma(model)
  }, error = function(e) {
    stop("Failed to get residual SD from model. Error: ", e$message,
         ". Cannot proceed with simulation.")
  })
}


#' Compute percentiles from simulation draws (RB v1 outcomes + derived yardage)
#'
#' @param draws data.frame of simulation draws
#' @return data.frame with p25, p50, p75 for each outcome
compute_rb_percentiles <- function(draws) {
  stats <- c("carries", "receptions", "rushing_yards", "receiving_yards", "total_touchdowns")
  probs <- c(0.10, 0.25, 0.40, 0.50, 0.60, 0.75, 0.90)
  result <- data.frame(
    stat = stats,
    p10 = NA_real_,
    p25 = NA_real_,
    p40 = NA_real_,
    p50 = NA_real_,
    p60 = NA_real_,
    p75 = NA_real_,
    p90 = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(stats)) {
    stat <- stats[i]
    vals <- NULL
    if (stat %in% names(draws)) {
      vals <- draws[[stat]]
    } else if (stat == "rushing_yards" && "rush_yards" %in% names(draws)) {
      vals <- draws$rush_yards
    } else if (stat == "receiving_yards" && "rec_yards" %in% names(draws)) {
      vals <- draws$rec_yards
    } else if (stat == "total_touchdowns") {
      if ("total_touchdowns" %in% names(draws)) {
        vals <- draws$total_touchdowns
      } else if (all(c("rush_tds", "rec_tds") %in% names(draws))) {
        vals <- draws$rush_tds + draws$rec_tds
      } else if (all(c("rushing_tds", "receiving_tds") %in% names(draws))) {
        vals <- draws$rushing_tds + draws$receiving_tds
      }
    }
    if (!is.null(vals)) {
      q <- quantile(vals, probs, na.rm = TRUE)
      result[i, c("p10", "p25", "p40", "p50", "p60", "p75", "p90")] <- as.numeric(q)
    }
  }
  for (col in c("p10", "p25", "p40", "p50", "p60", "p75", "p90")) {
    result[[col]] <- round(result[[col]])
  }
  result
}


#' Validate RB models (RB v1 contract)
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models are fitted
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "receptions_model", "rush_tds_model", "rec_tds_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}
