# Simulate RB Game
#
# Monte Carlo simulation for RB player-game outcomes.
# Follows the dependency graph:
#   1. Sample carries (Negative Binomial)
#   2. Sample rush_yards | carries (Gaussian, truncated at 0)
#   3. Sample rush_tds | carries (Poisson)
#   4. Sample targets (Negative Binomial)
#   5. Sample receptions | targets (Binomial)
#   6. Sample rec_yards | receptions (Gaussian, truncated at 0)
#   7. Sample rec_tds | targets (Poisson)
#   8. Derive fantasy points
#
# Fantasy points are NEVER modeled directly - always derived from components.
#
# Dependencies:
#   - R/utils/ppr_scoring.R
#   - R/models/fit_rb_models.R (for validate_rb_models)
#
# Usage:
#   source("R/utils/ppr_scoring.R")
#   result <- simulate_rb_game(feature_row, rb_models, n_sims = 5000)

#' Simulate RB game outcomes
#'
#' Runs Monte Carlo simulation to generate distribution of RB outcomes.
#' Returns raw simulation draws and percentile summary.
#'
#' @param feature_row data.frame with one row containing pre-game features:
#'   - carries_roll3, carries_roll5
#'   - targets_roll3, targets_roll5
#'   - yards_per_carry_roll5, yards_per_target_roll5
#'   - catch_rate_roll5
#'   - rush_tds_roll5, rec_tds_roll5
#'   - is_home
#' @param rb_models List of fitted models from fit_rb_models()
#' @param n_sims Integer, number of Monte Carlo simulations (default 5000)
#' @param fumble_rate_per_carry Numeric, probability of fumble lost per carry (default 0.001 = 0.1%)
#' @return List with:
#'   - draws: data.frame with n_sims rows of simulated stat lines
#'   - summary: data.frame with p25, p50, p75 for each stat
#'   - status: character indicating simulation status
simulate_rb_game <- function(feature_row, rb_models, n_sims = 5000, fumble_rate_per_carry = 0.001) {
  
  # Initialize result structure
  result <- list(
    draws = NULL,
    summary = NULL,
    status = "not_run"
  )
  
  # Validate inputs
  if (is.null(feature_row) || nrow(feature_row) == 0) {
    warning("No feature row provided")
    result$status <- "no_features"
    result$summary <- na_rb_summary()
    return(result)
  }
  
  if (nrow(feature_row) > 1) {
    warning("Multiple rows provided; using first row only")
    feature_row <- feature_row[1, , drop = FALSE]
  }
  
  # Check if models are valid
  if (is.null(rb_models) || !validate_rb_models(rb_models)) {
    warning("RB models are NULL or incomplete. Returning NA percentiles.")
    result$status <- "invalid_models"
    result$summary <- na_rb_summary()
    return(result)
  }
  
  # Prepare prediction data frame
  # Need to handle potential NA values in features
  pred_data <- prepare_prediction_data(feature_row)
  
  # Check for defensive features and warn if missing
  def_features <- c("opp_rush_yards_allowed_roll5", "opp_tfl_roll5", "opp_sacks_roll5", 
                    "opp_points_allowed_roll5")
  missing_def <- setdiff(def_features, names(pred_data))
  if (length(missing_def) > 0) {
    warning("WARNING: Defensive opponent features missing; efficiency may be overstated")
  }
  
  # Initialize simulation storage
  sim_carries <- numeric(n_sims)
  sim_rush_yards <- numeric(n_sims)
  sim_rush_tds <- numeric(n_sims)
  sim_targets <- numeric(n_sims)
  sim_receptions <- numeric(n_sims)
  sim_rec_yards <- numeric(n_sims)
  sim_rec_tds <- numeric(n_sims)
  sim_fumbles_lost <- numeric(n_sims)
  sim_fantasy <- numeric(n_sims)
  
  # Run simulations
  for (i in seq_len(n_sims)) {
    
    # --- RUSHING PATH ---
    
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
    
    # 2. Sample rush_yards | carries (Gaussian, truncated at 0)
    # Update prediction data with sampled carries
    pred_data_rush <- pred_data
    pred_data_rush$target_carries <- sim_carries[i]
    
    rush_yards_mu <- predict_safe(rb_models$rush_yards_model, pred_data_rush)
    rush_yards_sigma <- get_residual_sd(rb_models$rush_yards_model)
    
    sim_rush_yards[i] <- max(0, rnorm(1, mean = rush_yards_mu, sd = rush_yards_sigma))
    
    # 3. Sample rush_tds | carries (Poisson)
    rush_tds_lambda <- predict_safe(rb_models$rush_tds_model, pred_data_rush, type = "response")
    sim_rush_tds[i] <- rpois(1, lambda = max(0.01, rush_tds_lambda))
    
    # --- RECEIVING PATH ---
    
    # 4. Sample targets (Negative Binomial / Poisson)
    targets_mu <- predict_safe(rb_models$targets_model, pred_data, type = "response")
    if (inherits(rb_models$targets_model, "negbin")) {
      theta <- rb_models$targets_model$theta
      sim_targets[i] <- rnbinom(1, size = theta, mu = targets_mu)
    } else {
      sim_targets[i] <- rpois(1, lambda = targets_mu)
    }
    
    # 5. Sample receptions | targets (Binomial)
    if (sim_targets[i] > 0) {
      if (!is.null(rb_models$catch_rate_model)) {
        catch_prob <- predict_safe(rb_models$catch_rate_model, pred_data, type = "response")
        catch_prob <- min(max(catch_prob, 0.1), 0.95)  # Bound probability
      } else {
        # Fallback: use historical catch rate
        catch_prob <- ifelse(is.na(pred_data$catch_rate_roll5), 0.75, pred_data$catch_rate_roll5)
        catch_prob <- min(max(catch_prob, 0.1), 0.95)
      }
      sim_receptions[i] <- rbinom(1, size = sim_targets[i], prob = catch_prob)
    } else {
      sim_receptions[i] <- 0
    }
    
    # 6. Sample rec_yards | receptions (Gaussian, truncated at 0)
    if (sim_receptions[i] > 0) {
      pred_data_rec <- pred_data
      pred_data_rec$target_receptions <- sim_receptions[i]
      
      rec_yards_mu <- predict_safe(rb_models$rec_yards_model, pred_data_rec)
      rec_yards_sigma <- get_residual_sd(rb_models$rec_yards_model)
      
      sim_rec_yards[i] <- max(0, rnorm(1, mean = rec_yards_mu, sd = rec_yards_sigma))
    } else {
      sim_rec_yards[i] <- 0
    }
    
    # 7. Sample rec_tds | targets (Poisson)
    if (sim_targets[i] > 0) {
      pred_data_rec_td <- pred_data
      pred_data_rec_td$target_targets <- sim_targets[i]
      
      rec_tds_lambda <- predict_safe(rb_models$rec_tds_model, pred_data_rec_td, type = "response")
      sim_rec_tds[i] <- rpois(1, lambda = max(0.01, rec_tds_lambda))
    } else {
      sim_rec_tds[i] <- 0
    }
    
    # 8. Sample fumbles lost (Binomial, based on carries and player's historical rate)
    # Use player-specific fumble rate per carry
    # Model as: each carry has a small probability of resulting in a fumble lost
    if (sim_carries[i] > 0) {
      # Use binomial: each carry is a trial with probability fumble_rate_per_carry
      # This is more accurate than Poisson for rare events with known number of trials
      sim_fumbles_lost[i] <- rbinom(1, size = sim_carries[i], prob = fumble_rate_per_carry)
      # Cap at 2 (very rare to lose more than 2 fumbles)
      sim_fumbles_lost[i] <- min(sim_fumbles_lost[i], 2)
    } else {
      sim_fumbles_lost[i] <- 0
    }
    
    # 9. Derive fantasy points (NEVER modeled directly - always calculated from components)
    # PPR scoring: rush_yds*0.1 + rush_tds*6 + rec*1 + rec_yds*0.1 + rec_tds*6 - fumbles_lost*2
    sim_fantasy[i] <- compute_ppr_rb(
      rush_yards = sim_rush_yards[i],
      rush_tds = sim_rush_tds[i],
      receptions = sim_receptions[i],
      rec_yards = sim_rec_yards[i],
      rec_tds = sim_rec_tds[i],
      fumbles_lost = sim_fumbles_lost[i]
    )
  }
  
  # Compile draws
  result$draws <- data.frame(
    carries = sim_carries,
    rush_yards = sim_rush_yards,
    rush_tds = sim_rush_tds,
    targets = sim_targets,
    receptions = sim_receptions,
    rec_yards = sim_rec_yards,
    rec_tds = sim_rec_tds,
    fumbles_lost = sim_fumbles_lost,
    fantasy_ppr = sim_fantasy
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
    catch_rate_roll5 = 0.75,
    rush_tds_roll5 = 0.3,
    rec_tds_roll5 = 0.1,
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
#' @return Predicted value, or fallback if prediction fails
predict_safe <- function(model, newdata, type = "response") {
  tryCatch({
    pred <- predict(model, newdata = newdata, type = type)
    as.numeric(pred)
  }, error = function(e) {
    warning(paste("Prediction failed:", e$message))
    # Return reasonable fallback
    if (type == "response") 1 else 0
  })
}


#' Get residual standard deviation from linear model
#'
#' @param model lm object
#' @return Numeric, residual SD
get_residual_sd <- function(model) {
  if (is.null(model)) return(20)  # Fallback
  
  tryCatch({
    sigma(model)
  }, error = function(e) {
    20  # Fallback
  })
}


#' Compute percentiles from simulation draws
#'
#' @param draws data.frame of simulation draws
#' @return data.frame with p25, p50, p75 for each stat
compute_rb_percentiles <- function(draws) {
  
  stats <- c("carries", "rush_yards", "rush_tds", "targets", 
             "receptions", "rec_yards", "rec_tds", "fumbles_lost", "fantasy_ppr")
  
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
  count_stats <- c("carries", "rush_tds", "targets", "receptions", "rec_tds")
  for (stat in count_stats) {
    idx <- which(result$stat == stat)
    result$p25[idx] <- round(result$p25[idx])
    result$p50[idx] <- round(result$p50[idx])
    result$p75[idx] <- round(result$p75[idx])
  }
  
  # Yards: round to integer
  yards_stats <- c("rush_yards", "rec_yards")
  for (stat in yards_stats) {
    idx <- which(result$stat == stat)
    result$p25[idx] <- round(result$p25[idx])
    result$p50[idx] <- round(result$p50[idx])
    result$p75[idx] <- round(result$p75[idx])
  }
  
  # Fantasy: round to one decimal
  idx <- which(result$stat == "fantasy_ppr")
  result$p25[idx] <- round(result$p25[idx], 1)
  result$p50[idx] <- round(result$p50[idx], 1)
  result$p75[idx] <- round(result$p75[idx], 1)
  
  return(result)
}


#' Create NA summary for failed simulations
#'
#' @return data.frame with NA percentiles
na_rb_summary <- function() {
  data.frame(
    stat = c("carries", "rush_yards", "rush_tds", "targets", 
             "receptions", "rec_yards", "rec_tds", "fumbles_lost", "fantasy_ppr"),
    p25 = NA_real_,
    p50 = NA_real_,
    p75 = NA_real_,
    stringsAsFactors = FALSE
  )
}


#' Validate RB models (imported from fit_rb_models.R)
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models are fitted
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "targets_model", "rush_yards_model", 
                "rec_yards_model", "rush_tds_model", "rec_tds_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}

