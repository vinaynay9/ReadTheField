# Fit RB Models (v1 Contract)
#
# Fits statistical models for RB simulation following the RB v1 contract.
# Models exactly 5 outcomes: carries, rushing_yards, receiving_yards, receptions, total_touchdowns
#
# Models:
#   - carries_model: Negative Binomial or Poisson for volume
#   - rushing_yards_model: Gaussian conditional on carries
#   - receiving_yards_model: Gaussian conditional on receptions
#   - receptions_model: Negative Binomial or Poisson for volume
#   - total_touchdowns_model: Poisson or Negative Binomial for scoring
#
# Dependencies: MASS (for glm.nb)
#
# Usage:
#   rb_models <- fit_rb_models(rb_training_data)

#' Fit all RB models from training data (RB v1 contract)
#'
#' Fits exactly 5 models for RB simulation outcomes.
#' Returns a list of fitted model objects.
#' Fails loudly if insufficient data or model fitting fails.
#'
#' @param training_data data.frame with feature and target columns
#' @param min_rows Integer, minimum rows required for fitting (default 50)
#' @return Named list of fitted models:
#'   - carries_model: glm.nb or glm poisson for carries
#'   - rushing_yards_model: lm for rushing_yards ~ carries + features
#'   - receiving_yards_model: lm for receiving_yards ~ receptions + features
#'   - receptions_model: glm.nb or glm poisson for receptions
#'   - total_touchdowns_model: glm poisson or glm.nb for total_touchdowns
#'   - model_info: metadata about fitting
fit_rb_models <- function(training_data, min_rows = 50) {
  
  # Initialize result structure
  result <- list(
    carries_model = NULL,
    rushing_yards_model = NULL,
    receiving_yards_model = NULL,
    receptions_model = NULL,
    total_touchdowns_model = NULL,
    model_info = list(
      n_training_rows = 0,
      fitted_at = Sys.time(),
      status = "not_fitted"
    )
  )
  
  # Validate input - fail loudly
  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit RB models. ",
         "Training data must have at least one row.")
  }
  
  # Check for required outcome columns (RB v1 contract)
  required_outcomes <- c("target_carries", "target_rushing_yards", "target_receiving_yards",
                        "target_receptions", "target_total_touchdowns")
  missing_outcomes <- setdiff(required_outcomes, names(training_data))
  if (length(missing_outcomes) > 0) {
    stop("Missing required RB v1 outcome columns: ", paste(missing_outcomes, collapse = ", "),
         ". Cannot fit models. Expected columns: ", paste(required_outcomes, collapse = ", "))
  }
  
  # Filter to rows with valid features (non-NA rolling features)
  feature_cols <- c("carries_roll3", "carries_roll5", "targets_roll3", "targets_roll5")
  available_features <- intersect(feature_cols, names(training_data))
  if (length(available_features) == 0) {
    stop("No required rolling features found. Cannot fit models. ",
         "Expected at least one of: ", paste(feature_cols, collapse = ", "))
  }
  
  complete_mask <- complete.cases(training_data[, available_features, drop = FALSE])
  train_df <- training_data[complete_mask, , drop = FALSE]
  
  if (nrow(train_df) < min_rows) {
    stop("Insufficient training data. Found ", nrow(train_df), " complete rows, ",
         "but need at least ", min_rows, ". Cannot fit RB models.")
  }
  
  if (nrow(train_df) == 0) {
    stop("0 complete rows after filtering. Cannot fit RB models. ",
         "Check that rolling features are computed correctly.")
  }
  
  result$model_info$n_training_rows <- nrow(train_df)
  message(paste("Fitting RB v1 models on", nrow(train_df), "training rows"))
  
  # Check for MASS package (needed for glm.nb)
  if (!requireNamespace("MASS", quietly = TRUE)) {
    warning("MASS package not installed. Using Poisson instead of Negative Binomial for volume models.")
    use_negbin <- FALSE
  } else {
    use_negbin <- TRUE
  }
  
  # 1. Fit carries model (Negative Binomial or Poisson)
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
    stop("Failed to fit carries model: ", e$message, ". Cannot proceed with RB simulation.")
  })
  
  if (is.null(result$carries_model)) {
    stop("carries_model is NULL after fitting. Cannot proceed with RB simulation.")
  }
  
  # 2. Fit rushing_yards model (Gaussian conditional on carries)
  message("Fitting rushing_yards model...")
  result$rushing_yards_model <- tryCatch({
    # Build formula with defensive features if available
    def_features <- c("opp_rush_yards_allowed_roll5", "opp_tfl_roll5", "opp_sacks_roll5")
    available_def <- intersect(def_features, names(train_df))
    
    if (length(available_def) > 0) {
      # Filter to rows where defensive features are not all NA
      def_complete <- !apply(train_df[, available_def, drop = FALSE], 1, function(x) all(is.na(x)))
      train_df_rush <- train_df[def_complete, ]
      
      if (nrow(train_df_rush) >= min_rows) {
        # Build formula with available defensive features
        def_formula <- paste(available_def, collapse = " + ")
        formula_str <- paste("target_rushing_yards ~ target_carries + yards_per_carry_roll5 + is_home +", def_formula)
        lm(as.formula(formula_str), data = train_df_rush)
      } else {
        # Fallback: use model without defensive features
        warning("Insufficient data with defensive features for rushing_yards model. Using model without defensive features.")
        lm(target_rushing_yards ~ target_carries + yards_per_carry_roll5 + is_home, data = train_df)
      }
    } else {
      # No defensive features available
      lm(target_rushing_yards ~ target_carries + yards_per_carry_roll5 + is_home, data = train_df)
    }
  }, error = function(e) {
    stop("Failed to fit rushing_yards model: ", e$message, ". Cannot proceed with RB simulation.")
  })
  
  if (is.null(result$rushing_yards_model)) {
    stop("rushing_yards_model is NULL after fitting. Cannot proceed with RB simulation.")
  }
  
  # 3. Fit receptions model (Negative Binomial or Poisson)
  message("Fitting receptions model...")
  result$receptions_model <- tryCatch({
    if (use_negbin) {
      MASS::glm.nb(
        target_receptions ~ targets_roll3 + targets_roll5 + is_home,
        data = train_df
      )
    } else {
      glm(
        target_receptions ~ targets_roll3 + targets_roll5 + is_home,
        data = train_df,
        family = poisson(link = "log")
      )
    }
  }, error = function(e) {
    stop("Failed to fit receptions model: ", e$message, ". Cannot proceed with RB simulation.")
  })
  
  if (is.null(result$receptions_model)) {
    stop("receptions_model is NULL after fitting. Cannot proceed with RB simulation.")
  }
  
  # 4. Fit receiving_yards model (Gaussian conditional on receptions)
  message("Fitting receiving_yards model...")
  result$receiving_yards_model <- tryCatch({
    lm(
      target_receiving_yards ~ target_receptions + yards_per_target_roll5 + is_home,
      data = train_df
    )
  }, error = function(e) {
    stop("Failed to fit receiving_yards model: ", e$message, ". Cannot proceed with RB simulation.")
  })
  
  if (is.null(result$receiving_yards_model)) {
    stop("receiving_yards_model is NULL after fitting. Cannot proceed with RB simulation.")
  }
  
  # 5. Fit total_touchdowns model (Poisson or Negative Binomial)
  message("Fitting total_touchdowns model...")
  result$total_touchdowns_model <- tryCatch({
    # Use carries and receptions as predictors (total volume)
    if (use_negbin) {
      MASS::glm.nb(
        target_total_touchdowns ~ target_carries + target_receptions + is_home,
        data = train_df
      )
    } else {
      glm(
        target_total_touchdowns ~ target_carries + target_receptions + is_home,
        data = train_df,
        family = poisson(link = "log")
      )
    }
  }, error = function(e) {
    stop("Failed to fit total_touchdowns model: ", e$message, ". Cannot proceed with RB simulation.")
  })
  
  if (is.null(result$total_touchdowns_model)) {
    stop("total_touchdowns_model is NULL after fitting. Cannot proceed with RB simulation.")
  }
  
  # Update status
  n_fitted <- sum(!sapply(result[1:5], is.null))
  if (n_fitted == 5) {
    result$model_info$status <- "fully_fitted"
  } else {
    stop("Only ", n_fitted, " of 5 required models fitted. Cannot proceed with RB simulation.")
  }
  
  message(paste("Fitting complete: all 5 RB v1 models fitted successfully"))
  
  return(result)
}


#' Check if RB models are valid for simulation (RB v1 contract)
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models are fitted
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "rushing_yards_model", "receiving_yards_model",
                "receptions_model", "total_touchdowns_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}
