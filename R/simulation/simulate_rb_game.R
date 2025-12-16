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
    
    # 1. Sample carries (Negative Binomial / Poisson)
    carries_mu <- predict_safe(rb_models$carries_model, pred_data, type = "response")
    if (inherits(rb_models$carries_model, "negbin")) {
      # Negative Binomial: need theta (dispersion)
      theta <- rb_models$carries_model$theta
      sim_carries[i] <- rnbinom(1, size = theta, mu = carries_mu)
    } else {
      # Poisson fallback
      sim_carries[i] <- rpois(1, lambda = carries_mu)
    }
    sim_carries[i] <- max(0, sim_carries[i])  # Ensure non-negative
    
    # 2. Sample rushing_yards | carries (Gaussian, truncated at 0)
    pred_data_rush <- pred_data
    pred_data_rush$target_carries <- sim_carries[i]
    
    rushing_yards_mu <- predict_safe(rb_models$rushing_yards_model, pred_data_rush)
    rushing_yards_sigma <- get_residual_sd(rb_models$rushing_yards_model)
    
    sim_rushing_yards[i] <- max(0, rnorm(1, mean = rushing_yards_mu, sd = rushing_yards_sigma))
    
    # 3. Sample receptions (Negative Binomial / Poisson)
    receptions_mu <- predict_safe(rb_models$receptions_model, pred_data, type = "response")
    if (inherits(rb_models$receptions_model, "negbin")) {
      theta <- rb_models$receptions_model$theta
      sim_receptions[i] <- rnbinom(1, size = theta, mu = receptions_mu)
    } else {
      sim_receptions[i] <- rpois(1, lambda = receptions_mu)
    }
    sim_receptions[i] <- max(0, sim_receptions[i])  # Ensure non-negative
    
    # 4. Sample receiving_yards | receptions (Gaussian, truncated at 0)
    if (sim_receptions[i] > 0) {
      pred_data_rec <- pred_data
      pred_data_rec$target_receptions <- sim_receptions[i]
      
      receiving_yards_mu <- predict_safe(rb_models$receiving_yards_model, pred_data_rec)
      receiving_yards_sigma <- get_residual_sd(rb_models$receiving_yards_model)
      
      sim_receiving_yards[i] <- max(0, rnorm(1, mean = receiving_yards_mu, sd = receiving_yards_sigma))
    } else {
      sim_receiving_yards[i] <- 0
    }
    
    # 5. Sample total_touchdowns (Poisson or Negative Binomial)
    pred_data_td <- pred_data
    pred_data_td$target_carries <- sim_carries[i]
    pred_data_td$target_receptions <- sim_receptions[i]
    
    total_tds_mu <- predict_safe(rb_models$total_touchdowns_model, pred_data_td, type = "response")
    if (inherits(rb_models$total_touchdowns_model, "negbin")) {
      theta <- rb_models$total_touchdowns_model$theta
      sim_total_touchdowns[i] <- rnbinom(1, size = theta, mu = total_tds_mu)
    } else {
      sim_total_touchdowns[i] <- rpois(1, lambda = max(0.01, total_tds_mu))
    }
    sim_total_touchdowns[i] <- max(0, sim_total_touchdowns[i])  # Ensure non-negative
  }
  
  # Compile draws (RB v1 outcomes only)
  result$draws <- data.frame(
    carries = sim_carries,
    rushing_yards = sim_rushing_yards,
    receptions = sim_receptions,
    receiving_yards = sim_receiving_yards,
    total_touchdowns = sim_total_touchdowns
  )
  
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


#' Safe predict wrapper
#'
#' @param model Fitted model object
#' @param newdata data.frame for prediction
#' @param type Prediction type (default "response")
#' @return Predicted value, or stop() if prediction fails
predict_safe <- function(model, newdata, type = "response") {
  if (is.null(model)) {
    stop("Model is NULL. Cannot make prediction. This indicates a model/data mismatch.")
  }
  
  tryCatch({
    pred <- predict(model, newdata = newdata, type = type)
    as.numeric(pred)
  }, error = function(e) {
    stop("Prediction failed for model. Error: ", e$message, 
         ". This indicates a model/data schema mismatch. Cannot proceed with simulation.")
  })
}


#' Get residual standard deviation from linear model
#'
#' @param model lm object
#' @return Numeric, residual SD
get_residual_sd <- function(model) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot get residual SD. This indicates a model/data mismatch.")
  }
  
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
