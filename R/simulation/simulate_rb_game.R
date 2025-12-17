# Simulate RB Game (v1 Contract)
#
# Monte Carlo simulation for RB player-game outcomes following RB v1 contract.
# Simulation order:
#   1. Sample carries (Negative Binomial or Poisson)
#   2. Sample rushing_yards | carries (Gaussian, truncated at 0)
#   3. Sample receptions (Negative Binomial or Poisson)
#   4. Sample receiving_yards | receptions (Gaussian, truncated at 0)
#   5. Sample total_touchdowns (Poisson or Negative Binomial)
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
#' Returns raw simulation draws and percentile summary for 5 outcomes only.
#'
#' @param feature_row data.frame with one row containing pre-game features:
#'   - carries_roll3, carries_roll5
#'   - targets_roll3, targets_roll5
#'   - yards_per_carry_roll5, yards_per_target_roll5
#'   - is_home
#' @param rb_models List of fitted models from fit_rb_models() (RB v1 contract)
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @return List with:
#'   - draws: data.frame with n_sims rows of simulated stat lines (5 outcomes)
#'   - summary: data.frame with p25, p50, p75 for each outcome
#'   - status: character indicating simulation status
simulate_rb_game <- function(feature_row, rb_models, n_sims = 5000) {
  
  # Initialize result structure
  result <- list(
    draws = NULL,
    summary = NULL,
    status = "not_run"
  )
  
  # Validate inputs - fail loudly, no partial outputs
  if (is.null(feature_row) || nrow(feature_row) == 0) {
    stop("No feature row provided to simulate_rb_game. Cannot proceed with simulation.")
  }
  
  if (nrow(feature_row) > 1) {
    stop("Multiple feature rows provided to simulate_rb_game. Expected exactly one row.")
  }
  
  # Check if models are valid (RB v1 contract) - fail loudly
  if (is.null(rb_models)) {
    stop("RB models are NULL. Cannot proceed with simulation. ",
         "Required models: carries_model, rushing_yards_model, receiving_yards_model, ",
         "receptions_model, total_touchdowns_model")
  }
  
  if (!validate_rb_models(rb_models)) {
    stop("RB models are incomplete. Cannot proceed with simulation. ",
         "Required models: carries_model, rushing_yards_model, receiving_yards_model, ",
         "receptions_model, total_touchdowns_model")
  }
  
  # Verify all required models exist
  required_models <- c("carries_model", "rushing_yards_model", "receiving_yards_model",
                       "receptions_model", "total_touchdowns_model")
  missing_models <- required_models[!sapply(required_models, function(m) !is.null(rb_models[[m]]))]
  if (length(missing_models) > 0) {
    stop("Missing required RB v1 models: ", paste(missing_models, collapse = ", "),
         ". Cannot proceed with simulation.")
  }
  
  # Prepare prediction data frame
  pred_data <- prepare_prediction_data(feature_row)
  
  # Initialize simulation storage (RB v1 outcomes only)
  sim_carries <- numeric(n_sims)
  sim_rushing_yards <- numeric(n_sims)
  sim_receptions <- numeric(n_sims)
  sim_receiving_yards <- numeric(n_sims)
  sim_total_touchdowns <- numeric(n_sims)
  
  # Run simulations
  for (i in seq_len(n_sims)) {
    
    # 1. Sample carries
    sim_carries[i] <- sample_from_model(rb_models$carries_model, pred_data, n_samples = 1)
    
    # 2. Sample rushing_yards | carries
    pred_data_rush <- pred_data
    pred_data_rush$target_carries <- sim_carries[i]
    sim_rushing_yards[i] <- sample_from_model(rb_models$rushing_yards_model, pred_data_rush, n_samples = 1)
    
    # 3. Sample receptions
    sim_receptions[i] <- sample_from_model(rb_models$receptions_model, pred_data, n_samples = 1)
    
    # 4. Sample receiving_yards | receptions
    if (sim_receptions[i] > 0) {
      pred_data_rec <- pred_data
      pred_data_rec$target_receptions <- sim_receptions[i]
      sim_receiving_yards[i] <- sample_from_model(rb_models$receiving_yards_model, pred_data_rec, n_samples = 1)
    } else {
      sim_receiving_yards[i] <- 0
    }
    
    # 5. Sample total_touchdowns
    pred_data_td <- pred_data
    pred_data_td$target_carries <- sim_carries[i]
    pred_data_td$target_receptions <- sim_receptions[i]
    sim_total_touchdowns[i] <- sample_from_model(rb_models$total_touchdowns_model, pred_data_td, n_samples = 1)
  }
  
  # Compile draws (RB v1 outcomes only)
  result$draws <- data.frame(
    carries = sim_carries,
    rushing_yards = sim_rushing_yards,
    receptions = sim_receptions,
    receiving_yards = sim_receiving_yards,
    total_touchdowns = sim_total_touchdowns,
    stringsAsFactors = FALSE
  )
  
  # Defensive check: ensure all vectors are non-NULL and correct length
  required_outcomes <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
  for (outcome in required_outcomes) {
    if (is.null(result$draws[[outcome]])) {
      stop("Simulation output '", outcome, "' is NULL. All outputs must be numeric vectors.")
    }
    if (length(result$draws[[outcome]]) != n_sims) {
      stop("Simulation output '", outcome, "' has incorrect length. Expected ", n_sims, ", got ", length(result$draws[[outcome]]), ".")
    }
  }
  
  # Compute percentiles
  result$summary <- compute_rb_percentiles(result$draws)
  result$status <- "success"
  
  return(result)
}


#' Prepare prediction data with defaults for NA values
#'
#' @param feature_row data.frame with one row
#' @return data.frame suitable for predict()
prepare_prediction_data <- function(feature_row) {
  
  df <- as.data.frame(feature_row)
  
  # Set defaults for NA values (use reasonable fallbacks)
  default_values <- list(
    carries_roll3 = 12,
    carries_roll5 = 12,
    targets_roll3 = 3,
    targets_roll5 = 3,
    yards_per_carry_roll5 = 4.0,
    yards_per_target_roll5 = 7.0,
    is_home = 0
  )
  
  for (col in names(default_values)) {
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


#' Compute percentiles from simulation draws (RB v1 outcomes)
#'
#' @param draws data.frame of simulation draws
#' @return data.frame with p25, p50, p75 for each outcome
compute_rb_percentiles <- function(draws) {
  
  # RB v1 outcomes only
  stats <- c("carries", "rushing_yards", "receptions", "receiving_yards", "total_touchdowns")
  
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
  
  # Round appropriately
  # Counts: round to integer
  count_stats <- c("carries", "receptions", "total_touchdowns")
  for (stat in count_stats) {
    idx <- which(result$stat == stat)
    result$p25[idx] <- round(result$p25[idx])
    result$p50[idx] <- round(result$p50[idx])
    result$p75[idx] <- round(result$p75[idx])
  }
  
  # Yards: round to integer
  yards_stats <- c("rushing_yards", "receiving_yards")
  for (stat in yards_stats) {
    idx <- which(result$stat == stat)
    result$p25[idx] <- round(result$p25[idx])
    result$p50[idx] <- round(result$p50[idx])
    result$p75[idx] <- round(result$p75[idx])
  }
  
  return(result)
}


#' Validate RB models (RB v1 contract)
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models are fitted
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "rushing_yards_model", "receiving_yards_model",
                "receptions_model", "total_touchdowns_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}
