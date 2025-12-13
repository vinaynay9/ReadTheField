# Fit RB Models
#
# Fits statistical models for RB simulation following the dependency graph:
#   carries -> rush_yards | carries -> rush_tds | carries
#   targets -> receptions | targets -> rec_yards | receptions -> rec_tds | targets
#
# Models:
#   - carries_model: Negative Binomial for volume
#   - targets_model: Negative Binomial for volume
#   - rush_yards_model: Gaussian conditional on carries
#   - rec_yards_model: Gaussian conditional on receptions
#   - rush_tds_model: Poisson for scoring
#   - rec_tds_model: Poisson for scoring
#   - catch_rate_model: Logistic for receptions | targets (binomial probability)
#
# Dependencies: MASS (for glm.nb)
#
# Usage:
#   rb_models <- fit_rb_models(rb_training_data)

#' Fit all RB models from training data
#'
#' Fits the complete set of RB models for simulation.
#' Returns a list of fitted model objects.
#' If training data is empty or fitting fails, returns NULL models with warnings.
#'
#' @param training_data data.frame with feature and target columns
#' @param min_rows Integer, minimum rows required for fitting (default 50)
#' @return Named list of fitted models:
#'   - carries_model: glm.nb for carries
#'   - targets_model: glm.nb for targets
#'   - rush_yards_model: lm for rush_yards ~ carries + features
#'   - rec_yards_model: lm for rec_yards ~ receptions + features
#'   - rush_tds_model: glm poisson for rush_tds
#'   - rec_tds_model: glm poisson for rec_tds
#'   - catch_rate_model: glm binomial for catch probability
#'   - model_info: metadata about fitting
fit_rb_models <- function(training_data, min_rows = 50) {
  
  # Initialize result structure
  result <- list(
    carries_model = NULL,
    targets_model = NULL,
    rush_yards_model = NULL,
    rec_yards_model = NULL,
    rush_tds_model = NULL,
    rec_tds_model = NULL,
    catch_rate_model = NULL,
    model_info = list(
      n_training_rows = 0,
      fitted_at = Sys.time(),
      status = "not_fitted"
    )
  )
  
  # Validate input
  if (is.null(training_data) || nrow(training_data) == 0) {
    warning("Empty training data provided. All models set to NULL.")
    result$model_info$status <- "no_data"
    return(result)
  }
  
  # Filter to rows with valid features (non-NA rolling features)
  # Early-career rows with NA features cannot be used for training
  feature_cols <- c("carries_roll3", "carries_roll5", "targets_roll3", "targets_roll5")
  complete_mask <- complete.cases(training_data[, intersect(feature_cols, names(training_data))])
  train_df <- training_data[complete_mask, ]
  
  if (nrow(train_df) < min_rows) {
    warning(paste("Only", nrow(train_df), "complete rows available. Need at least", min_rows, "."))
    result$model_info$status <- "insufficient_data"
    result$model_info$n_training_rows <- nrow(train_df)
    return(result)
  }
  
  result$model_info$n_training_rows <- nrow(train_df)
  message(paste("Fitting RB models on", nrow(train_df), "training rows"))
  
  # Check for MASS package (needed for glm.nb)
  if (!requireNamespace("MASS", quietly = TRUE)) {
    warning("MASS package not installed. Using Poisson instead of Negative Binomial for volume models.")
    use_negbin <- FALSE
  } else {
    use_negbin <- TRUE
  }
  
  # 1. Fit carries model (Negative Binomial)
  message("Fitting carries model...")
  result$carries_model <- tryCatch({
    if (use_negbin) {
      MASS::glm.nb(
        target_carries ~ carries_roll3 + carries_roll5 + is_home,
        data = train_df
      )
    } else {
      glm(
        target_carries ~ carries_roll3 + carries_roll5 + is_home,
        data = train_df,
        family = poisson(link = "log")
      )
    }
  }, error = function(e) {
    warning(paste("Failed to fit carries model:", e$message))
    NULL
  })
  
  # 2. Fit targets model (Negative Binomial)
  message("Fitting targets model...")
  result$targets_model <- tryCatch({
    if (use_negbin) {
      MASS::glm.nb(
        target_targets ~ targets_roll3 + targets_roll5 + is_home,
        data = train_df
      )
    } else {
      glm(
        target_targets ~ targets_roll3 + targets_roll5 + is_home,
        data = train_df,
        family = poisson(link = "log")
      )
    }
  }, error = function(e) {
    warning(paste("Failed to fit targets model:", e$message))
    NULL
  })
  
  # 3. Fit rush yards model (Gaussian conditional on carries)
  message("Fitting rush yards model...")
  result$rush_yards_model <- tryCatch({
    lm(
      target_rush_yards ~ target_carries + yards_per_carry_roll5 + is_home,
      data = train_df
    )
  }, error = function(e) {
    warning(paste("Failed to fit rush yards model:", e$message))
    NULL
  })
  
  # 4. Fit rec yards model (Gaussian conditional on receptions)
  message("Fitting rec yards model...")
  result$rec_yards_model <- tryCatch({
    lm(
      target_rec_yards ~ target_receptions + yards_per_target_roll5 + is_home,
      data = train_df
    )
  }, error = function(e) {
    warning(paste("Failed to fit rec yards model:", e$message))
    NULL
  })
  
  # 5. Fit rush TDs model (Poisson)
  message("Fitting rush TDs model...")
  result$rush_tds_model <- tryCatch({
    glm(
      target_rush_tds ~ target_carries + rush_tds_roll5 + is_home,
      data = train_df,
      family = poisson(link = "log")
    )
  }, error = function(e) {
    warning(paste("Failed to fit rush TDs model:", e$message))
    NULL
  })
  
  # 6. Fit rec TDs model (Poisson)
  message("Fitting rec TDs model...")
  result$rec_tds_model <- tryCatch({
    glm(
      target_rec_tds ~ target_targets + rec_tds_roll5 + is_home,
      data = train_df,
      family = poisson(link = "log")
    )
  }, error = function(e) {
    warning(paste("Failed to fit rec TDs model:", e$message))
    NULL
  })
  
  # 7. Fit catch rate model (Binomial for receptions | targets)
  message("Fitting catch rate model...")
  result$catch_rate_model <- tryCatch({
    # Create a data frame for binomial fitting
    # We model P(reception | target) using catch_rate history
    catch_df <- train_df[train_df$target_targets > 0, ]
    if (nrow(catch_df) < min_rows) {
      warning("Insufficient data for catch rate model")
      NULL
    } else {
      glm(
        cbind(target_receptions, target_targets - target_receptions) ~ 
          catch_rate_roll5 + is_home,
        data = catch_df,
        family = binomial(link = "logit")
      )
    }
  }, error = function(e) {
    warning(paste("Failed to fit catch rate model:", e$message))
    NULL
  })
  
  # Update status
  n_fitted <- sum(!sapply(result[1:7], is.null))
  if (n_fitted == 7) {
    result$model_info$status <- "fully_fitted"
  } else if (n_fitted > 0) {
    result$model_info$status <- "partially_fitted"
  } else {
    result$model_info$status <- "all_failed"
  }
  
  message(paste("Fitting complete:", n_fitted, "of 7 models fitted"))
  
  return(result)
}


#' Check if RB models are valid for simulation
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models are fitted
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "targets_model", "rush_yards_model", 
                "rec_yards_model", "rush_tds_model", "rec_tds_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}

