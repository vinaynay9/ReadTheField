# Fit RB Models (v1 Contract) - Robust with Fallbacks
#
# Fits statistical models for RB simulation following the RB v1 contract.
# Models exactly 5 outcomes: carries, rushing_yards, receiving_yards, receptions, total_touchdowns
#
# Features:
#   - Robust fitting with multiple fallback strategies
#   - Baseline models when fitting fails
#   - Comprehensive data validation
#   - Detailed diagnostics
#
# Dependencies: MASS (optional, for glm.nb)
#
# Usage:
#   rb_models <- fit_rb_models(rb_training_data, min_rows = 200)

#' Create a baseline model object
#'
#' @param target_name Character, name of target variable
#' @param y Numeric vector, observed values
#' @param is_count Logical, whether this is a count outcome
#' @return List with baseline model structure
create_baseline_model <- function(target_name, y, is_count = FALSE) {
  y_clean <- y[!is.na(y) & is.finite(y)]
  if (length(y_clean) == 0) {
    y_clean <- c(0)  # Fallback if all NA
  }
  
  median_val <- median(y_clean)
  sd_val <- sd(y_clean)
  if (is.na(sd_val) || sd_val == 0) {
    sd_val <- if (is_count) 1.0 else max(0.1, abs(median_val) * 0.1)
  }
  
  list(
    type = "baseline",
    target = target_name,
    value = median_val,
    sd = sd_val,
    is_count = is_count,
    family = if (is_count) "poisson" else "gaussian",
    n_obs = length(y_clean)
  )
}

#' Fit a count model with fallbacks
#'
#' @param y Numeric vector, count outcome
#' @param data data.frame, training data
#' @param formula_str Character, formula string
#' @param target_name Character, name of target
#' @return Fitted model or baseline model
fit_count_model_robust <- function(y, data, formula_str, target_name) {
  fit <- NULL
  fit_type <- "none"
  converged <- FALSE
  warnings_captured <- character(0)
  
  # Try Poisson
  tryCatch({
    fit <- glm(as.formula(formula_str), data = data, family = poisson(link = "log"))
    if (!any(is.na(coef(fit))) && fit$converged) {
      fit_type <- "poisson"
      converged <- TRUE
    } else {
      fit <- NULL
    }
  }, warning = function(w) {
    warnings_captured <<- c(warnings_captured, paste0("Poisson: ", conditionMessage(w)))
  }, error = function(e) {
    warnings_captured <<- c(warnings_captured, paste0("Poisson: ", conditionMessage(e)))
  })
  
  # Try Quasipoisson if Poisson failed
  if (is.null(fit) || !converged) {
    tryCatch({
      fit <- glm(as.formula(formula_str), data = data, family = quasipoisson(link = "log"))
      if (!any(is.na(coef(fit))) && fit$converged) {
        fit_type <- "quasipoisson"
        converged <- TRUE
      } else {
        fit <- NULL
      }
    }, warning = function(w) {
      warnings_captured <<- c(warnings_captured, paste0("Quasipoisson: ", conditionMessage(w)))
    }, error = function(e) {
      warnings_captured <<- c(warnings_captured, paste0("Quasipoisson: ", conditionMessage(e)))
    })
  }
  
  # Try Negative Binomial if available and previous attempts failed
  if ((is.null(fit) || !converged) && requireNamespace("MASS", quietly = TRUE)) {
    tryCatch({
      fit <- MASS::glm.nb(as.formula(formula_str), data = data)
      if (!any(is.na(coef(fit))) && !is.null(fit$theta) && is.finite(fit$theta)) {
        fit_type <- "negbin"
        converged <- TRUE
      } else {
        fit <- NULL
      }
    }, warning = function(w) {
      warnings_captured <<- c(warnings_captured, paste0("NegBin: ", conditionMessage(w)))
    }, error = function(e) {
      warnings_captured <<- c(warnings_captured, paste0("NegBin: ", conditionMessage(e)))
    })
  }
  
  # Fallback to baseline
  if (is.null(fit) || !converged) {
    return(create_baseline_model(target_name, y, is_count = TRUE))
  }
  
  # Attach metadata
  fit$fit_type <- fit_type
  fit$converged <- converged
  fit$warnings <- warnings_captured
  fit
}

#' Fit a continuous model with fallbacks
#'
#' @param y Numeric vector, continuous outcome
#' @param data data.frame, training data
#' @param formula_str Character, formula string
#' @param target_name Character, name of target
#' @return Fitted model or baseline model
fit_continuous_model_robust <- function(y, data, formula_str, target_name) {
  fit <- NULL
  fit_type <- "none"
  converged <- FALSE
  warnings_captured <- character(0)
  use_log_transform <- TRUE
  
  # Try log1p transform (preferred for yards)
  tryCatch({
    data_log <- data
    y_log <- log1p(y)
    data_log[[target_name]] <- y_log
    
    # Parse formula to extract RHS
    formula_parts <- strsplit(formula_str, " ~ ", fixed = TRUE)[[1]]
    if (length(formula_parts) == 2) {
      rhs <- formula_parts[2]
      formula_log <- as.formula(paste0(target_name, " ~ ", rhs))
    } else {
      stop("Invalid formula format")
    }
    
    fit <- lm(formula_log, data = data_log)
    if (!any(is.na(coef(fit)))) {
      fit_type <- "gaussian_log1p"
      converged <- TRUE
      fit$transform <- "log1p"
      fit$target_name <- target_name
    } else {
      fit <- NULL
    }
  }, warning = function(w) {
    warnings_captured <<- c(warnings_captured, paste0("Log1p: ", conditionMessage(w)))
  }, error = function(e) {
    warnings_captured <<- c(warnings_captured, paste0("Log1p: ", conditionMessage(e)))
  })
  
  # Fallback to untransformed Gaussian
  if (is.null(fit) || !converged) {
    tryCatch({
      fit <- lm(as.formula(formula_str), data = data)
      if (!any(is.na(coef(fit)))) {
        fit_type <- "gaussian"
        converged <- TRUE
        fit$transform <- "none"
      } else {
        fit <- NULL
      }
    }, warning = function(w) {
      warnings_captured <<- c(warnings_captured, paste0("Gaussian: ", conditionMessage(w)))
    }, error = function(e) {
      warnings_captured <<- c(warnings_captured, paste0("Gaussian: ", conditionMessage(e)))
    })
  }
  
  # Fallback to baseline
  if (is.null(fit) || !converged) {
    return(create_baseline_model(target_name, y, is_count = FALSE))
  }
  
  # Attach metadata
  fit$fit_type <- fit_type
  fit$converged <- converged
  fit$warnings <- warnings_captured
  fit
}

#' Validate and prepare data for a specific target
#'
#' @param data data.frame, training data
#' @param target_name Character, name of target column
#' @param feature_cols Character vector, required feature columns
#' @param min_rows Integer, minimum rows required
#' @param min_positive Integer, minimum positive values for counts
#' @return List with validated data, y vector, and diagnostics
validate_target_data <- function(data, target_name, feature_cols, min_rows = 200, min_positive = 20) {
  diagnostics <- list(
    n_rows_before = nrow(data),
    n_dropped_na = 0,
    n_rows_used = 0,
    var_y = NA_real_,
    n_positive = NA_integer_,
    fit_type = "none",
    converged = FALSE,
    warnings = character(0)
  )
  
  # Check target column exists
  if (!target_name %in% names(data)) {
    diagnostics$warnings <- c(diagnostics$warnings, paste0("Target column '", target_name, "' not found"))
    return(list(data = data.frame(), y = numeric(0), diagnostics = diagnostics))
  }
  
  y <- data[[target_name]]
  
  # Drop rows with NA in target or required features
  required_cols <- c(target_name, feature_cols)
  available_cols <- intersect(required_cols, names(data))
  complete_mask <- complete.cases(data[, available_cols, drop = FALSE])
  
  diagnostics$n_dropped_na <- sum(!complete_mask)
  data_clean <- data[complete_mask, , drop = FALSE]
  y_clean <- y[complete_mask]
  
  # Check minimum rows
  if (nrow(data_clean) < min_rows) {
    diagnostics$warnings <- c(diagnostics$warnings, 
                               paste0("Insufficient rows: ", nrow(data_clean), " < ", min_rows))
    diagnostics$n_rows_used <- nrow(data_clean)
    return(list(data = data_clean, y = y_clean, diagnostics = diagnostics))
  }
  
  # Check variance
  var_y <- var(y_clean, na.rm = TRUE)
  diagnostics$var_y <- var_y
  if (is.na(var_y) || var_y == 0) {
    diagnostics$warnings <- c(diagnostics$warnings, "Zero variance in target")
    diagnostics$n_rows_used <- nrow(data_clean)
    return(list(data = data_clean, y = y_clean, diagnostics = diagnostics))
  }
  
  # For count models, check minimum positive values
  if (all(y_clean >= 0) && all(y_clean == round(y_clean))) {
    n_positive <- sum(y_clean > 0, na.rm = TRUE)
    diagnostics$n_positive <- n_positive
    if (n_positive < min_positive) {
      diagnostics$warnings <- c(diagnostics$warnings, 
                                 paste0("Insufficient positive values: ", n_positive, " < ", min_positive))
      diagnostics$n_rows_used <- nrow(data_clean)
      return(list(data = data_clean, y = y_clean, diagnostics = diagnostics))
    }
  }
  
  diagnostics$n_rows_used <- nrow(data_clean)
  list(data = data_clean, y = y_clean, diagnostics = diagnostics)
}

#' Fit all RB models from training data (RB v1 contract)
#'
#' Fits exactly 5 models for RB simulation outcomes with robust fallbacks.
#' Returns a list of fitted models (or baseline models if fitting fails).
#'
#' @param training_data data.frame with feature and target columns
#' @param min_rows Integer, minimum rows required for fitting (default 200)
#' @return Named list with:
#'   - carries_model: fitted or baseline model
#'   - rushing_yards_model: fitted or baseline model
#'   - receiving_yards_model: fitted or baseline model
#'   - receptions_model: fitted or baseline model
#'   - total_touchdowns_model: fitted or baseline model
#'   - diagnostics: list with per-target diagnostics
fit_rb_models <- function(training_data, min_rows = 200) {
  
  # Initialize result structure
  result <- list(
    carries_model = NULL,
    rushing_yards_model = NULL,
    receiving_yards_model = NULL,
    receptions_model = NULL,
    total_touchdowns_model = NULL,
    diagnostics = list()
  )
  
  # Validate input
  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit RB models.")
  }
  
  # Check for required outcome columns (RB v1 contract)
  required_outcomes <- c("target_carries", "target_rushing_yards", "target_receiving_yards",
                        "target_receptions", "target_total_touchdowns")
  missing_outcomes <- setdiff(required_outcomes, names(training_data))
  if (length(missing_outcomes) > 0) {
    stop("Missing required RB v1 outcome columns: ", paste(missing_outcomes, collapse = ", "))
  }
  
  message(paste("Fitting RB v1 models on", nrow(training_data), "training rows"))
  
  # 1. Fit carries model
  message("Fitting carries model...")
  carries_valid <- validate_target_data(
    training_data, 
    "target_carries", 
    c("carries_roll3", "carries_roll5", "is_home"),
    min_rows = min_rows,
    min_positive = 20
  )
  result$diagnostics$carries <- carries_valid$diagnostics
  
  if (carries_valid$diagnostics$n_rows_used >= min_rows && 
      !is.na(carries_valid$diagnostics$var_y) && 
      carries_valid$diagnostics$var_y > 0) {
    result$carries_model <- fit_count_model_robust(
      carries_valid$y,
      carries_valid$data,
      "target_carries ~ carries_roll3 + carries_roll5 + is_home",
      "target_carries"
    )
    if (!is.null(result$carries_model$fit_type)) {
      result$diagnostics$carries$fit_type <- result$carries_model$fit_type
      result$diagnostics$carries$converged <- result$carries_model$converged
      result$diagnostics$carries$warnings <- c(result$diagnostics$carries$warnings, result$carries_model$warnings)
    }
  } else {
    result$carries_model <- create_baseline_model("target_carries", carries_valid$y, is_count = TRUE)
    result$diagnostics$carries$fit_type <- "baseline"
  }
  
  # 2. Fit rushing_yards model
  message("Fitting rushing_yards model...")
  rush_yds_valid <- validate_target_data(
    training_data,
    "target_rushing_yards",
    c("target_carries", "yards_per_carry_roll5", "is_home"),
    min_rows = min_rows,
    min_positive = 0
  )
  result$diagnostics$rushing_yards <- rush_yds_valid$diagnostics
  
  if (rush_yds_valid$diagnostics$n_rows_used >= min_rows && 
      !is.na(rush_yds_valid$diagnostics$var_y) && 
      rush_yds_valid$diagnostics$var_y > 0) {
    # Try with defensive features if available
    def_features <- c("opp_rush_yards_allowed_roll5", "opp_tfl_roll5", "opp_sacks_roll5")
    available_def <- intersect(def_features, names(rush_yds_valid$data))
    
    formula_str <- "target_rushing_yards ~ target_carries + yards_per_carry_roll5 + is_home"
    if (length(available_def) > 0) {
      def_complete <- !apply(rush_yds_valid$data[, available_def, drop = FALSE], 1, function(x) all(is.na(x)))
      rush_yds_data_def <- rush_yds_valid$data[def_complete, ]
      rush_yds_y_def <- rush_yds_valid$y[def_complete]
      
      if (nrow(rush_yds_data_def) >= min_rows) {
        formula_str <- paste0(formula_str, " + ", paste(available_def, collapse = " + "))
        result$rushing_yards_model <- fit_continuous_model_robust(
          rush_yds_y_def,
          rush_yds_data_def,
          formula_str,
          "target_rushing_yards"
      )
    } else {
        result$rushing_yards_model <- fit_continuous_model_robust(
          rush_yds_valid$y,
          rush_yds_valid$data,
          "target_rushing_yards ~ target_carries + yards_per_carry_roll5 + is_home",
          "target_rushing_yards"
        )
      }
    } else {
      result$rushing_yards_model <- fit_continuous_model_robust(
        rush_yds_valid$y,
        rush_yds_valid$data,
        formula_str,
        "target_rushing_yards"
      )
    }
    
    if (!is.null(result$rushing_yards_model$fit_type)) {
      result$diagnostics$rushing_yards$fit_type <- result$rushing_yards_model$fit_type
      result$diagnostics$rushing_yards$converged <- result$rushing_yards_model$converged
      result$diagnostics$rushing_yards$warnings <- c(result$diagnostics$rushing_yards$warnings, result$rushing_yards_model$warnings)
    }
  } else {
    result$rushing_yards_model <- create_baseline_model("target_rushing_yards", rush_yds_valid$y, is_count = FALSE)
    result$diagnostics$rushing_yards$fit_type <- "baseline"
  }
  
  # 3. Fit receptions model
  message("Fitting receptions model...")
  rec_valid <- validate_target_data(
    training_data,
    "target_receptions",
    c("targets_roll3", "targets_roll5", "is_home"),
    min_rows = min_rows,
    min_positive = 20
  )
  result$diagnostics$receptions <- rec_valid$diagnostics
  
  if (rec_valid$diagnostics$n_rows_used >= min_rows && 
      !is.na(rec_valid$diagnostics$var_y) && 
      rec_valid$diagnostics$var_y > 0) {
    result$receptions_model <- fit_count_model_robust(
      rec_valid$y,
      rec_valid$data,
      "target_receptions ~ targets_roll3 + targets_roll5 + is_home",
      "target_receptions"
    )
    if (!is.null(result$receptions_model$fit_type)) {
      result$diagnostics$receptions$fit_type <- result$receptions_model$fit_type
      result$diagnostics$receptions$converged <- result$receptions_model$converged
      result$diagnostics$receptions$warnings <- c(result$diagnostics$receptions$warnings, result$receptions_model$warnings)
      }
    } else {
    result$receptions_model <- create_baseline_model("target_receptions", rec_valid$y, is_count = TRUE)
    result$diagnostics$receptions$fit_type <- "baseline"
  }
  
  # 4. Fit receiving_yards model
  message("Fitting receiving_yards model...")
  rec_yds_valid <- validate_target_data(
    training_data,
    "target_receiving_yards",
    c("target_receptions", "yards_per_target_roll5", "is_home"),
    min_rows = min_rows,
    min_positive = 0
  )
  result$diagnostics$receiving_yards <- rec_yds_valid$diagnostics
  
  if (rec_yds_valid$diagnostics$n_rows_used >= min_rows && 
      !is.na(rec_yds_valid$diagnostics$var_y) && 
      rec_yds_valid$diagnostics$var_y > 0) {
    result$receiving_yards_model <- fit_continuous_model_robust(
      rec_yds_valid$y,
      rec_yds_valid$data,
      "target_receiving_yards ~ target_receptions + yards_per_target_roll5 + is_home",
      "target_receiving_yards"
    )
    if (!is.null(result$receiving_yards_model$fit_type)) {
      result$diagnostics$receiving_yards$fit_type <- result$receiving_yards_model$fit_type
      result$diagnostics$receiving_yards$converged <- result$receiving_yards_model$converged
      result$diagnostics$receiving_yards$warnings <- c(result$diagnostics$receiving_yards$warnings, result$receiving_yards_model$warnings)
      }
    } else {
    result$receiving_yards_model <- create_baseline_model("target_receiving_yards", rec_yds_valid$y, is_count = FALSE)
    result$diagnostics$receiving_yards$fit_type <- "baseline"
  }
  
  # 5. Fit total_touchdowns model
  message("Fitting total_touchdowns model...")
  td_valid <- validate_target_data(
    training_data,
    "target_total_touchdowns",
    c("target_carries", "target_receptions", "is_home"),
    min_rows = min_rows,
    min_positive = 20
  )
  result$diagnostics$total_touchdowns <- td_valid$diagnostics
  
  if (td_valid$diagnostics$n_rows_used >= min_rows && 
      !is.na(td_valid$diagnostics$var_y) && 
      td_valid$diagnostics$var_y > 0) {
    result$total_touchdowns_model <- fit_count_model_robust(
      td_valid$y,
      td_valid$data,
      "target_total_touchdowns ~ target_carries + target_receptions + is_home",
      "target_total_touchdowns"
    )
    if (!is.null(result$total_touchdowns_model$fit_type)) {
      result$diagnostics$total_touchdowns$fit_type <- result$total_touchdowns_model$fit_type
      result$diagnostics$total_touchdowns$converged <- result$total_touchdowns_model$converged
      result$diagnostics$total_touchdowns$warnings <- c(result$diagnostics$total_touchdowns$warnings, result$total_touchdowns_model$warnings)
    }
    } else {
    result$total_touchdowns_model <- create_baseline_model("target_total_touchdowns", td_valid$y, is_count = TRUE)
    result$diagnostics$total_touchdowns$fit_type <- "baseline"
  }
  
  # Summary
  baseline_models <- sapply(result[1:5], function(m) !is.null(m$type) && m$type == "baseline")
  n_baseline <- sum(baseline_models)
  if (n_baseline > 0) {
    baseline_names <- names(result[1:5])[baseline_models]
    warning("RB model fitting: ", n_baseline, " model(s) fell back to baseline: ", 
            paste(baseline_names, collapse = ", "))
  }
  
  message(paste("Fitting complete: ", 5 - n_baseline, " fitted, ", n_baseline, " baseline"))
  
  return(result)
}

#' Check if RB models are valid for simulation (RB v1 contract)
#'
#' @param rb_models List returned by fit_rb_models
#' @return Logical, TRUE if all required models exist (baseline or fitted)
validate_rb_models <- function(rb_models) {
  if (is.null(rb_models)) return(FALSE)
  
  required <- c("carries_model", "rushing_yards_model", "receiving_yards_model",
                "receptions_model", "total_touchdowns_model")
  
  all(sapply(required, function(m) !is.null(rb_models[[m]])))
}
