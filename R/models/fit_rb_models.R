# Fit RB Models (v1 Contract) - Robust with Fallbacks
#
# Fits statistical models for RB simulation following the RB v1 contract.
# Models exactly 4 outcomes: carries, receptions, rush_tds, rec_tds
# NOTE: Yardage targets (rush_yards, rec_yards) are NOT in RB v1 schema.
# Yardage should be derived downstream if needed (e.g., carries * YPC).
#
# Features:
#   - Per-target training (no intersection across targets)
#   - Robust fitting with multiple fallback strategies
#   - Baseline models when fitting fails
#   - Comprehensive data validation
#   - Detailed diagnostics
#
# Dependencies: dplyr, MASS (optional, for glm.nb)
#
# Usage:
#   rb_models <- fit_rb_models(rb_training_data, min_rows = 200)

library(dplyr)

#' Create a baseline model object
#'
#' @param target_name Character, name of target variable
#' @param y Numeric vector, observed values
#' @param is_count Logical, whether this is a count outcome
#' @return List with baseline model structure
create_baseline_model <- function(target_name, y, is_count = FALSE) {
  # Ensure target is a numeric vector (guard against list/data.frame columns)
  y <- as.numeric(y)
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
  # Ensure target is numeric to avoid list/data.frame columns from tibbles
  y <- as.numeric(y)
  data[[target_name]] <- y
  
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
  # Ensure target is numeric to avoid list/data.frame columns from tibbles
  y <- as.numeric(y)
  data[[target_name]] <- y
  
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

#' Fit all RB models from training data (RB v1 contract with regimes)
#'
#' Fits models per target per regime for RB simulation outcomes with robust fallbacks.
#' Each (target, regime) combination trains on its own valid subset.
#' Returns a list of fitted models keyed as "target__regime" (e.g., "target_carries__early").
#'
#' @param training_data data.frame with feature and target columns, including rb_regime
#' @param min_rows Integer, minimum rows required for fitting (default 200)
#' @return Named list with:
#'   - Models keyed as "target__regime" (e.g., "target_carries__early")
#'   - diagnostics: list with per-(target, regime) diagnostics
fit_rb_models <- function(training_data, min_rows = 200) {
  
  # Initialize file-based diagnostic logging (project root)
  log_file <- "rb_debug.log"
  cat("", file = log_file)  # Clear file on each run
  
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }
  
  # Load schema validation and regime system
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
  } else {
    stop("Missing R/utils/rb_schema_v1.R — cannot load RB v1 schema")
  }
  
  # CRITICAL: Hard dependency check for get_rb_v1_targets
  if (!exists("get_rb_v1_targets")) {
    stop("Missing get_rb_v1_targets function. Cannot load RB v1 schema.")
  }
  
  if (file.exists("R/utils/rb_regime_v1.R")) {
    source("R/utils/rb_regime_v1.R", local = TRUE)
    # Ensure get_rb_features_by_week is available for time-aware feature selection
    if (!exists("get_rb_features_by_week")) {
      stop("get_rb_features_by_week function not found. Time-aware feature contracts require this function.")
    }
    # Ensure get_rb_regimes and get_model_key are available
    if (!exists("get_rb_regimes")) {
      stop("get_rb_regimes function not found. Regime system requires this function.")
    }
    if (!exists("get_model_key")) {
      stop("get_model_key function not found. Model key generation requires this function.")
    }
  } else {
    stop("Missing R/utils/rb_regime_v1.R — cannot load RB v1 regime system")
  }
  
  # Initialize result structure
  result <- list(
    models = list(),
    diagnostics = list()
  )
  
  # Validate input
  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit RB models.")
  }
  
  # Hard check: rb_regime column must exist (fail loudly if missing)
  if (!"rb_regime" %in% names(training_data)) {
    stop("Missing rb_regime column in training data. Regime-based modeling requires rb_regime. ",
         "Ensure rb_regime is added in Layer 3 feature assembly and preserved through training data assembly.")
  }
  
  # Validate RB v1 target schema
  if (exists("validate_rb_v1_target_schema")) {
    validate_rb_v1_target_schema(training_data, strict = TRUE)
  } else {
    # Fallback validation
    required_outcomes <- get_rb_v1_targets()
    missing_outcomes <- setdiff(required_outcomes, names(training_data))
    if (length(missing_outcomes) > 0) {
      stop("Missing required RB v1 outcome columns: ", paste(missing_outcomes, collapse = ", "))
    }
  }
  
  # Validate regime column exists
  if (!"rb_regime" %in% names(training_data)) {
    stop("Missing rb_regime column in training data. Regime-based modeling requires rb_regime.")
  }

  # Drop rows with missing regimes (diagnostics only; preserves modeling integrity)
  if (any(is.na(training_data$rb_regime))) {
    training_data <- training_data[!is.na(training_data$rb_regime), , drop = FALSE]
  }
  
  # Validate regimes are valid
  valid_regimes <- get_rb_regimes()
  invalid_regimes <- setdiff(unique(training_data$rb_regime), valid_regimes)
  if (length(invalid_regimes) > 0) {
    stop("Invalid regimes found: ", paste(invalid_regimes, collapse = ", "),
         ". Valid regimes: ", paste(valid_regimes, collapse = ", "))
  }
  
  log_msg("=== RB Model Fitting Diagnostics ===")
  log_msg("Training rows:", nrow(training_data))
  
  # Get RB v1 targets and regimes
  rb_targets <- get_rb_v1_targets()
  rb_regimes <- get_rb_regimes()
  feature_contracts <- get_rb_features_by_regime()
  
  # Diagnostic: Check that all required features exist across all regimes
  all_required_features <- unique(unlist(feature_contracts))
    missing_global_features <- setdiff(all_required_features, names(training_data))
    if (length(missing_global_features) > 0) {
      log_msg("WARNING: Some required features are missing from training data: ", 
            paste(missing_global_features, collapse = ", "))
      log_msg("Available columns (sample): ", 
            paste(head(setdiff(names(training_data), c("player_id", "season", "week", "rb_regime")), 20), collapse = ", "))
    }
  
  # Diagnostic: Check NA rates for required features (log to file)
  if (length(all_required_features) > 0) {
    existing_features <- intersect(all_required_features, names(training_data))
    if (length(existing_features) > 0) {
      na_rates <- sapply(existing_features, function(f) {
        sum(is.na(training_data[[f]])) / nrow(training_data) * 100
      })
      high_na_features <- names(na_rates)[na_rates > 50]
      if (length(high_na_features) > 0) {
        log_msg("High NA rates in required features: ", 
                paste(paste0(high_na_features, " (", round(na_rates[high_na_features], 1), "%)"), collapse = ", "))
      }
    }
  }
  
  # Train each target per regime (nested loops)
  # TIME-AWARE MODELING: Features are determined by week-of-game, not regime alone
  # Regime controls which coefficients to use, week controls which features exist
  # Each regime model uses the minimum feature set that exists for ALL weeks in that regime
  # This ensures causal consistency: we never require features that didn't exist at that time
  for (target in rb_targets) {
    for (regime in rb_regimes) {
      model_key <- get_model_key(target, regime)
      log_msg("")
      log_msg("Fitting:", model_key)
      
      # TIME-AWARE FIX: Use centralized feature contract from rb_regime_v1.R
      # This ensures causal consistency and includes defensive features where appropriate
      # Early regime (weeks 1-3): player priors + is_home (no rolling features exist yet)
      # Mid regime (weeks 4-5): priors + is_home + roll3 (roll5 not available until week 6)
      # Late regime (weeks 6-7): priors + is_home + roll3 + roll5 + defensive roll5
      # Standard regime (weeks 8+): priors + is_home + all rolling features + defensive roll5
      feature_contracts <- get_rb_features_by_regime()
      required_features <- feature_contracts[[regime]]
      
      if (is.null(required_features)) {
        stop("Unknown regime: ", regime, ". Valid regimes: ", paste(rb_regimes, collapse = ", "))
      }
      
      # Enforce carries model feature requirements (player usage priors + opponent defense when available)
      if (target == "target_carries") {
        if (!any(c("carries_prior", "carries_cum_mean") %in% required_features)) {
          stop("Carries model feature contract missing player usage priors. ",
               "Expected carries_prior and/or carries_cum_mean for regime: ", regime)
        }
        if (regime %in% c("late", "standard")) {
          required_def <- c("opp_yards_per_rush_allowed_roll5",
                            "opp_rush_yards_allowed_roll5",
                            "opp_points_allowed_roll5")
          missing_def <- setdiff(required_def, required_features)
          if (length(missing_def) > 0) {
            stop("Carries model feature contract missing opponent defense features for regime ",
                 regime, ": ", paste(missing_def, collapse = ", "))
          }
        }
      }
      
      # Filter to rows matching this regime and target non-NA
      # CRITICAL: Only filter by this specific target, NOT all targets
      training_data_regime <- training_data %>%
        filter(rb_regime == regime) %>%
        filter(!is.na(.data[[target]]))
      
      # Drop tibble list-cols and enforce base data.frame for modeling stability
      training_data_regime <- as.data.frame(training_data_regime, stringsAsFactors = FALSE)
      training_data_regime[[target]] <- as.numeric(training_data_regime[[target]])
      
      n_rows_available <- nrow(training_data_regime)
      
      # Preserve is_home as provided (no imputation)
      if ("is_home" %in% names(training_data_regime)) {
        training_data_regime$is_home <- as.numeric(training_data_regime$is_home)
      }
      
      # TIME-AWARE FIX: Only require features that exist at that week
      # Check which features exist in the data
      available_features <- intersect(required_features, names(training_data_regime))
      missing_features <- setdiff(required_features, available_features)
      
      if (length(missing_features) > 0) {
        # Diagnostic: show what columns ARE available
        available_cols_sample <- head(setdiff(names(training_data_regime), c("player_id", "season", "week", "rb_regime", rb_targets)), 20)
        stop("Missing required features for ", model_key, ": ", paste(missing_features, collapse = ", "),
             ". Time-aware feature contract must be satisfied. ",
             "Available feature columns (sample): ", paste(available_cols_sample, collapse = ", "))
      }

      # Determine model type based on target (defined early for fallbacks)
      is_count <- TRUE  # All RB v1 targets are counts

      # Drop zero-variance features (e.g., is_home all NA/0) to avoid rank-deficient fits
      zero_var_features <- c()
      for (feat in required_features) {
        vals <- training_data_regime[[feat]]
        vals <- vals[!is.na(vals)]
        if (length(unique(vals)) <= 1) {
          zero_var_features <- c(zero_var_features, feat)
        }
      }
      if (length(zero_var_features) > 0) {
        log_msg("  Dropping zero-variance features: ", paste(zero_var_features, collapse = ", "))
        required_features <- setdiff(required_features, zero_var_features)
      }
      if (length(required_features) == 0) {
        log_msg("  All features zero-variance for ", model_key, " - using baseline fallback")
        y <- training_data_regime[[target]]
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "zero_variance_features"
        next
      }
      
      # CRITICAL FIX: Filter only on rolling features (not is_home or player priors)
      # is_home is optional and preserved as provided
      # Player priors (cum_mean, _cum) are also optional (available for established players)
      non_rolling_features <- c(
        "is_home",
        "is_rookie",
        "draft_round",
        "draft_pick_overall",
        grep("_cum", required_features, value = TRUE),
        grep("_roll1$", required_features, value = TRUE),
        grep("^prev_season", required_features, value = TRUE)
      )
      rolling_features <- setdiff(required_features, non_rolling_features)
      
      if (length(rolling_features) > 0) {
        # Filter to rows where rolling features are non-NA
        training_data_regime <- training_data_regime %>%
          filter(if_all(all_of(rolling_features), ~ !is.na(.)))
      }
      # Note: is_home is NOT filtered - it's optional and preserved as provided
      
      n_rows_final <- nrow(training_data_regime)

      # Diagnostic: Log features used and row counts to file
      log_msg("  Features used:", paste(required_features, collapse = ", "))
      log_msg("  Rows available:", n_rows_available)
      log_msg("  Rows used:", n_rows_final)

      if (n_rows_final == 0) {
        stop("Training data empty after feature/NA filtering for ", model_key, ". This indicates an upstream filtering issue.")
      }
      
      # Determine model type based on target
      is_count <- TRUE  # All RB v1 targets are counts
      min_positive <- if (grepl("_tds$", target)) 10 else 20
      
      # Build formula from regime-specific features
      formula_str <- paste0(target, " ~ ", paste(required_features, collapse = " + "))
      
      # Store diagnostics with time-aware feature information
      result$diagnostics[[model_key]] <- list(
        target = target,
        regime = regime,
        features_used = required_features,  # TIME-AWARE: Show which features were used
        n_rows_available = n_rows_available,
        n_rows_final = n_rows_final,
        fit_type = "none",
        converged = FALSE,
        warnings = character(0),
        fallback_used = FALSE,
        fallback_reason = NULL
      )

      # Check minimum rows
      if (n_rows_final < min_rows) {
        log_msg("  Insufficient rows:", n_rows_final, "<", min_rows, "- Using baseline")
        y <- training_data_regime[[target]]
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "insufficient_rows"
        log_msg("  Model:", model_key, "| Features:", paste(required_features, collapse = ", "),
                 "| Rows available:", n_rows_available, "| Rows used:", n_rows_final,
                 "| Fit: baseline | Fallback: TRUE")
        next
      }
      
      # Validate target variance
      y <- training_data_regime[[target]]
      var_y <- var(y, na.rm = TRUE)
      if (is.na(var_y) || var_y == 0) {
        log_msg("  Zero variance - Using baseline")
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "zero_variance_target"
        log_msg("  Model:", model_key, "| Features:", paste(required_features, collapse = ", "),
                 "| Rows available:", n_rows_available, "| Rows used:", n_rows_final,
                 "| Fit: baseline | Fallback: TRUE")
        next
      }
      
      # For count models, check minimum positive values
      if (is_count) {
        n_positive <- sum(y > 0, na.rm = TRUE)
        if (n_positive < min_positive) {
          log_msg("  Insufficient positive values:", n_positive, "<", min_positive, "- Using baseline")
          result$models[[model_key]] <- create_baseline_model(target, y, is_count = TRUE)
          result$diagnostics[[model_key]]$fit_type <- "baseline"
          result$diagnostics[[model_key]]$fallback_used <- TRUE
          result$diagnostics[[model_key]]$fallback_reason <- "insufficient_positive_outcomes"
          log_msg("  Model:", model_key, "| Features:", paste(required_features, collapse = ", "),
                   "| Rows available:", n_rows_available, "| Rows used:", n_rows_final,
                   "| Fit: baseline | Fallback: TRUE")
          next
        }
      }
      
      # Fit model
      fit_result <- fit_count_model_robust(y, training_data_regime, formula_str, target)
      if (!is.null(fit_result$type) && fit_result$type == "baseline") {
        stop("Model fitting failed for ", model_key, " despite sufficient data. ",
             "Refusing baseline fallback; investigate feature quality or model specification.")
      }
      result$models[[model_key]] <- fit_result
      
      # Update diagnostics
      if (!is.null(fit_result$fit_type)) {
        result$diagnostics[[model_key]]$fit_type <- fit_result$fit_type
        result$diagnostics[[model_key]]$converged <- fit_result$converged
        result$diagnostics[[model_key]]$warnings <- fit_result$warnings
      }
      
      if (!is.null(fit_result$type) && fit_result$type == "baseline") {
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        log_msg("  Model:", model_key, "| Features:", paste(required_features, collapse = ", "),
                 "| Rows available:", n_rows_available, "| Rows used:", n_rows_final,
                 "| Fit: baseline | Fallback: TRUE")
      } else {
        log_msg("  Model:", model_key, "| Features:", paste(required_features, collapse = ", "),
                 "| Rows available:", n_rows_available, "| Rows used:", n_rows_final,
                 "| Fit:", fit_result$fit_type, "| Fallback: FALSE")
        if (length(fit_result$warnings) > 0) {
          log_msg("  Warnings:", paste(fit_result$warnings, collapse = "; "))
        }
      }
    }
  }
  
  # Summary with time-aware feature information
  n_models <- length(result$models)
  n_baseline <- sum(sapply(result$models, function(m) !is.null(m$type) && m$type == "baseline"))
  n_fitted <- n_models - n_baseline
  
  log_msg("")
  log_msg("=== Fitting Summary ===")
  log_msg("Total models:", n_models)
  log_msg("Fitted:", n_fitted)
  log_msg("Baseline (fallback):", n_baseline)
  
  # TIME-AWARE: Log summary of features used per regime
  log_msg("")
  log_msg("Time-aware feature usage by regime:")
  for (regime in rb_regimes) {
    regime_models <- names(result$models)[grepl(paste0("__", regime, "$"), names(result$models))]
    if (length(regime_models) > 0) {
      # Get features used for first model in this regime (all should use same features)
      first_model_key <- regime_models[1]
      if (first_model_key %in% names(result$diagnostics)) {
        features_used <- result$diagnostics[[first_model_key]]$features_used
        n_rows_used <- result$diagnostics[[first_model_key]]$n_rows_final
        log_msg("  ", regime, ": ", paste(features_used, collapse = ", "), 
                 " (", n_rows_used, " rows)")
      }
    }
  }
  
  return(result)
}

#' Check if RB models are valid for simulation (RB v1 contract with regimes)
#'
#' @param rb_models List returned by fit_rb_models (with models in $models sublist)
#' @param regime Character regime name (optional, for specific regime check)
#' @return Logical, TRUE if all required models exist (baseline or fitted)
validate_rb_models <- function(rb_models, regime = NULL) {
  if (is.null(rb_models)) return(FALSE)

  # Helper to locate the actual model list even if nested under $models
  locate_models <- function(container, required_keys) {
    if (is.null(container) || !is.list(container)) return(container)
    # Direct hit
    if (!is.null(names(container)) && any(names(container) %in% required_keys)) {
      return(container)
    }
    # Nested under $models
    if ("models" %in% names(container) && !is.null(container$models)) {
      return(locate_models(container$models, required_keys))
    }
    container
  }
  
  # Load regime system if needed
  if (file.exists("R/utils/rb_regime_v1.R")) {
    source("R/utils/rb_regime_v1.R", local = TRUE)
  }
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
  }
  
  # Check if using new regime-based structure
  if (!is.null(rb_models$models)) {
    # Build required keys for this validation
    if (exists("get_rb_v1_targets") && exists("get_rb_regimes") && exists("get_model_key")) {
      rb_targets <- get_rb_v1_targets()
      rb_regimes <- if (!is.null(regime)) regime else get_rb_regimes()
      required_keys <- as.vector(outer(rb_targets, rb_regimes, function(t, r) get_model_key(t, r)))
    } else {
      required_keys <- character(0)
    }

    models_list <- locate_models(rb_models$models, required_keys)
    if (!is.null(regime)) {
      # Check specific regime
      if (exists("get_rb_v1_targets") && exists("get_model_key")) {
        rb_targets <- get_rb_v1_targets()
        required_keys <- sapply(rb_targets, function(t) get_model_key(t, regime))
        return(all(sapply(required_keys, function(k) !is.null(models_list[[k]]))))
      }
    } else {
      # Check all regimes
      if (exists("get_rb_v1_targets") && exists("get_rb_regimes") && exists("get_model_key")) {
        rb_targets <- get_rb_v1_targets()
        rb_regimes <- get_rb_regimes()
        required_keys <- as.vector(outer(rb_targets, rb_regimes, function(t, r) get_model_key(t, r)))
        return(all(sapply(required_keys, function(k) !is.null(models_list[[k]]))))
      }
    }
  } else {
    # Legacy structure (non-regime)
    required <- c("carries_model", "receptions_model", "rush_tds_model", "rec_tds_model")
    return(all(sapply(required, function(m) !is.null(rb_models[[m]]))))
  }
  
  # Fallback: if we can't validate, return FALSE
  return(FALSE)
}
