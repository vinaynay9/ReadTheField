# Simulate RB Game (v1 Contract)
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
simulate_rb_game <- function(feature_row, rb_models, n_sims = 5000) {
  
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
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
  } else {
    error_msg <- "Missing R/utils/rb_schema_v1.R"
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
  if (file.exists("R/utils/rb_regime_v1.R")) {
    source("R/utils/rb_regime_v1.R", local = TRUE)
    # Ensure get_rb_features_by_week is available for time-aware feature selection
    if (!exists("get_rb_features_by_week")) {
      error_msg <- "get_rb_features_by_week function not found. Time-aware feature contracts require this function."
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  } else {
    error_msg <- "Missing R/utils/rb_regime_v1.R"
    log_msg("FATAL:", error_msg)
    stop(error_msg)
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
  
  # Validate regime-specific models exist
  rb_targets <- get_rb_v1_targets()
  required_model_keys <- sapply(rb_targets, function(t) get_model_key(t, regime))
  missing_models <- required_model_keys[!sapply(required_model_keys, function(k) !is.null(models_list[[k]]))]
  if (length(missing_models) > 0) {
    error_msg <- paste("Missing required RB v1 models for regime '", regime, "': ", paste(missing_models, collapse = ", "),
         ". Cannot proceed with simulation.")
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # TIME-AWARE FIX: Determine features based on week-of-game, not regime
  # This ensures prediction uses only features that existed at that time
  # Regime selects which model coefficients to use, week selects which features exist
  if (exists("get_rb_features_by_week")) {
    required_features <- get_rb_features_by_week(week)
  } else {
    # Fallback: use regime-based features (legacy behavior)
    feature_contracts <- get_rb_features_by_regime()
    required_features <- feature_contracts[[regime]]
    if (is.null(required_features)) {
      error_msg <- paste("Unknown regime: ", regime, ". Valid regimes: ", paste(names(feature_contracts), collapse = ", "))
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  }
  
  # Validate week-based features are present (guardrail: schema enforcement)
  missing_features <- setdiff(required_features, names(feature_row))
  if (length(missing_features) > 0) {
    error_msg <- paste("Missing required features for week ", week, " (regime '", regime, "'): ", 
         paste(missing_features, collapse = ", "),
         ". Cannot proceed with simulation. This indicates schema drift.")
    log_msg("FATAL:", error_msg)
    stop(error_msg)
  }
  
  # Check for NA in required features (guardrail: data quality)
  # CRITICAL: is_home is allowed to be NA (will be defaulted in prepare_prediction_data)
  # CRITICAL: priors (carries_prior, targets_prior, ypc_prior, ypt_prior) are allowed to be NA (rookies, model uses intercept)
  # CRITICAL: cumulative career priors (*_cum_mean, *_cum) are allowed to be NA (first career game)
  # Only strict rolling features (roll3, roll5, roll7) must be non-NA for their respective weeks
  optional_features <- c("is_home", "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
                         "carries_cum_mean", "targets_cum_mean", "ypc_cum", "ypt_cum", "catch_rate_cum", "receptions_cum_mean")
  strict_rolling_features <- setdiff(required_features, optional_features)
  
  if (length(strict_rolling_features) > 0) {
    na_features <- sapply(strict_rolling_features, function(f) {
      f %in% names(feature_row) && is.na(feature_row[[f]][1])
    })
    if (any(na_features)) {
      error_msg <- paste("NA values in required rolling features for week ", week, " (regime '", regime, "'): ", 
           paste(strict_rolling_features[na_features], collapse = ", "),
           ". All time-appropriate rolling features must be non-NA.")
      log_msg("FATAL:", error_msg)
      stop(error_msg)
    }
  }
  
  # TIME-AWARE FIX: Prepare prediction data using week-based features only
  # This ensures training and prediction feature sets match exactly
  pred_data <- prepare_prediction_data(feature_row, week = week, required_features = required_features)
  
  # Initialize simulation storage (RB v1 outcomes only)
  sim_carries <- numeric(n_sims)
  sim_receptions <- numeric(n_sims)
  sim_rush_tds <- numeric(n_sims)
  sim_rec_tds <- numeric(n_sims)
  
  # Get models for this regime
  carries_model_key <- get_model_key("target_carries", regime)
  receptions_model_key <- get_model_key("target_receptions", regime)
  rush_tds_model_key <- get_model_key("target_rush_tds", regime)
  rec_tds_model_key <- get_model_key("target_rec_tds", regime)
  
  carries_model <- models_list[[carries_model_key]]
  receptions_model <- models_list[[receptions_model_key]]
  rush_tds_model <- models_list[[rush_tds_model_key]]
  rec_tds_model <- models_list[[rec_tds_model_key]]
  
  # Run simulations
  for (i in seq_len(n_sims)) {
    
    # 1. Sample carries
    sim_carries[i] <- sample_from_model(carries_model, pred_data, n_samples = 1)
    
    # 2. Sample receptions
    sim_receptions[i] <- sample_from_model(receptions_model, pred_data, n_samples = 1)
    
    # 3. Sample rush_tds
    sim_rush_tds[i] <- sample_from_model(rush_tds_model, pred_data, n_samples = 1)
    
    # 4. Sample rec_tds
    sim_rec_tds[i] <- sample_from_model(rec_tds_model, pred_data, n_samples = 1)
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
  
  sim_rush_yards <- sim_carries * ypc
  sim_rec_yards <- sim_receptions * ypt
  
  # Compile draws (RB v1 outcomes + derived yardage)
  result$draws <- data.frame(
    carries = sim_carries,
    receptions = sim_receptions,
    rush_tds = sim_rush_tds,
    rec_tds = sim_rec_tds,
    rush_yards = sim_rush_yards,  # Derived
    rec_yards = sim_rec_yards,     # Derived
    stringsAsFactors = FALSE
  )
  
  # Log simulation output schema for debugging
  log_msg("Simulation draws columns:")
  log_msg(paste(names(result$draws), collapse = ", "))
  
  # Defensive check: ensure all vectors are non-NULL and correct length
  required_outcomes <- c("carries", "receptions", "rush_tds", "rec_tds", "rush_yards", "rec_yards")
  for (outcome in required_outcomes) {
    if (is.null(result$draws[[outcome]])) {
      error_msg <- paste("Simulation output '", outcome, "' is NULL. All outputs must be numeric vectors.")
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
    # Allow NA for priors and cumulative features (rookies / first career game)
    optional_na_features <- c("carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
                               "carries_cum_mean", "targets_cum_mean", "ypc_cum", "ypt_cum", "catch_rate_cum", "receptions_cum_mean")
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
    # Allow NA for priors and cumulative features (rookies / first career game)
    optional_na_features <- c("carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
                               "carries_cum_mean", "targets_cum_mean", "ypc_cum", "ypt_cum", "catch_rate_cum", "receptions_cum_mean")
    strict_features <- setdiff(required_features, optional_na_features)
    na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
    if (any(na_features)) {
      stop("NA values in required features: ", 
           paste(strict_features[na_features], collapse = ", "))
    }
  } else if (!is.null(regime)) {
    # Legacy fallback: regime-based features
    if (file.exists("R/utils/rb_regime_v1.R")) {
      source("R/utils/rb_regime_v1.R", local = TRUE)
      feature_contracts <- get_rb_features_by_regime()
      required_features <- feature_contracts[[regime]]
      
      if (!is.null(required_features)) {
        missing_features <- setdiff(required_features, names(df))
        if (length(missing_features) > 0) {
          stop("Missing required features for regime '", regime, "': ", paste(missing_features, collapse = ", "))
        }
        # Allow NA for priors and cumulative features (rookies / first career game)
        optional_na_features <- c("carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
                                   "carries_cum_mean", "targets_cum_mean", "ypc_cum", "ypt_cum", "catch_rate_cum", "receptions_cum_mean")
        strict_features <- setdiff(required_features, optional_na_features)
        na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
        if (any(na_features)) {
          stop("NA values in required features for regime '", regime, "': ", 
               paste(strict_features[na_features], collapse = ", "))
        }
      }
    }
  }
  
  # Set defaults for optional features (use reasonable fallbacks)
  # TIME-AWARE: Only set defaults for features not in required_features
  default_values <- list(
    carries_roll3 = 12,
    carries_roll5 = 12,
    carries_roll7 = 12,
    targets_roll3 = 3,
    targets_roll5 = 3,
    targets_roll7 = 3,
    yards_per_carry_roll5 = 4.0,
    yards_per_target_roll5 = 7.0,
    is_home = 0
  )
  
  # Only set defaults for features not in required_features (they should already be validated)
  optional_features <- setdiff(names(default_values), required_features)
  for (col in optional_features) {
    if (col %in% names(df)) {
      df[[col]] <- ifelse(is.na(df[[col]]), default_values[[col]], df[[col]])
    } else {
      df[[col]] <- default_values[[col]]
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
predict_safe <- function(model, newdata, type = "response", n_samples = 1) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot make prediction. This indicates a model/data mismatch.")
  }
  
  # Handle baseline models
  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      # For counts, use Poisson with mean = value
      result <- rpois(n_samples, lambda = max(0.01, model$value))
    } else {
      # For continuous, use Gaussian with truncation at 0
      result <- pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd))
    }
    return(result)
  }
  
  # Handle regular models
  tryCatch({
    pred <- predict(model, newdata = newdata, type = type)
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
sample_from_model <- function(model, newdata, n_samples = 1) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot sample. This indicates a model/data mismatch.")
  }
  
  # Handle baseline models
  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      # For counts, use Poisson with mean = value, round to integer
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = max(0.01, model$value))))))
    } else {
      # For continuous, use Gaussian with truncation at 0
      return(pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd)))
    }
  }
  
  # Handle regular models
  # Get mean prediction
  mu <- predict_safe(model, newdata, type = "response", n_samples = 1)
  
  # Determine model type and sample accordingly
  if (inherits(model, "glm")) {
    family_name <- model$family$family
    if (family_name == "poisson" || family_name == "quasipoisson") {
      # Poisson: sample directly
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = pmax(0.01, mu))))))
    } else if (family_name == "negbin" || inherits(model, "negbin")) {
      # Negative Binomial: need theta
      theta <- if (!is.null(model$theta)) model$theta else 1.0
      return(pmax(0L, as.integer(round(rnbinom(n_samples, size = theta, mu = pmax(0.01, mu))))))
    }
  } else if (inherits(model, "lm")) {
    # Linear model: check for transform
    if (!is.null(model$transform) && model$transform == "log1p") {
      # Back-transform log1p
      sigma_val <- get_residual_sd(model)
      samples <- rnorm(n_samples, mean = mu, sd = sigma_val)
      return(pmax(0, expm1(samples)))
    } else {
      # Regular Gaussian
      sigma_val <- get_residual_sd(model)
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
  
  # RB v1 outcomes + derived yardage
  stats <- c("carries", "receptions", "rush_tds", "rec_tds", "rush_yards", "rec_yards")
  
  result <- data.frame(
    stat = stats,
    p25 = NA_real_,
    p50 = NA_real_,
    p75 = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(stats)) {
    stat <- stats[i]
    if (stat %in% names(draws)) {
      vals <- draws[[stat]]
      result$p25[i] <- quantile(vals, 0.25, na.rm = TRUE)
      result$p50[i] <- quantile(vals, 0.50, na.rm = TRUE)
      result$p75[i] <- quantile(vals, 0.75, na.rm = TRUE)
    }
  }
  
  # Round appropriately - all are counts, round to integer
  for (i in seq_len(nrow(result))) {
    result$p25[i] <- round(result$p25[i])
    result$p50[i] <- round(result$p50[i])
    result$p75[i] <- round(result$p75[i])
  }
  
  return(result)
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
