# Fit TE Models (v1 Contract) - Robust with Fallbacks
#
# Fits statistical models for TE simulation following the TE v1 contract.
# Models outcomes: targets, receptions, rec_yards, rec_tds.
#
# Dependencies: dplyr, MASS (optional, for glm.nb)

library(dplyr)

create_baseline_model <- function(target_name, y, is_count = FALSE) {
  y <- as.numeric(y)
  y_clean <- y[!is.na(y) & is.finite(y)]
  if (length(y_clean) == 0) {
    y_clean <- c(0)
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

fit_count_model_robust <- function(y, data, formula_str, target_name) {
  y <- as.numeric(y)
  data[[target_name]] <- y

  fit <- NULL
  fit_type <- "none"
  converged <- FALSE
  warnings_captured <- character(0)

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

  if (is.null(fit) || !converged) {
    return(create_baseline_model(target_name, y, is_count = TRUE))
  }

  fit$fit_type <- fit_type
  fit$converged <- converged
  fit$warnings <- warnings_captured
  fit
}

fit_continuous_model_robust <- function(y, data, formula_str, target_name) {
  y <- as.numeric(y)
  data[[target_name]] <- y

  fit <- NULL
  fit_type <- "none"
  converged <- FALSE
  warnings_captured <- character(0)

  tryCatch({
    data_log <- data
    y_log <- log1p(y)
    data_log[[target_name]] <- y_log

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

  if (is.null(fit) || !converged) {
    return(create_baseline_model(target_name, y, is_count = FALSE))
  }

  fit$fit_type <- fit_type
  fit$converged <- converged
  fit$warnings <- warnings_captured
  fit
}

fit_te_models <- function(training_data, min_rows = 200) {
  log_file <- "te_debug.log"
  cat("", file = log_file)

  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }

  if (file.exists("R/positions/TE/te_schema_v1.R")) {
    source("R/positions/TE/te_schema_v1.R", local = TRUE)
  } else {
    stop("Missing R/positions/TE/te_schema_v1.R - cannot load TE schema")
  }
  if (!exists("get_te_v1_targets")) {
    stop("Missing get_te_v1_targets function. Cannot load TE schema.")
  }

  if (file.exists("R/positions/TE/te_regime_v1.R")) {
    source("R/positions/TE/te_regime_v1.R", local = TRUE)
    if (!exists("get_te_features_by_week")) {
      stop("get_te_features_by_week function not found. Time-aware feature contracts require this function.")
    }
    if (!exists("get_te_regimes")) {
      stop("get_te_regimes function not found. Regime system requires this function.")
    }
    if (!exists("get_te_model_key")) {
      stop("get_te_model_key function not found. Model key generation requires this function.")
    }
  } else {
    stop("Missing R/positions/TE/te_regime_v1.R - cannot load TE regime system")
  }

  result <- list(
    models = list(),
    diagnostics = list()
  )

  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit TE models.")
  }

  if (!"te_regime" %in% names(training_data)) {
    stop("Missing te_regime column in training data. Regime-based modeling requires te_regime.")
  }

  if (exists("validate_te_v1_target_schema")) {
    validate_te_v1_target_schema(training_data, strict = TRUE)
  }

  if (any(is.na(training_data$te_regime))) {
    training_data <- training_data[!is.na(training_data$te_regime), , drop = FALSE]
  }

  valid_regimes <- get_te_regimes()
  invalid_regimes <- setdiff(unique(training_data$te_regime), valid_regimes)
  if (length(invalid_regimes) > 0) {
    stop("Invalid regimes found: ", paste(invalid_regimes, collapse = ", "),
         ". Valid regimes: ", paste(valid_regimes, collapse = ", "))
  }

  log_msg("=== TE Model Fitting Diagnostics ===")
  log_msg("Training rows:", nrow(training_data))

  te_targets <- get_te_v1_targets()
  te_regimes <- get_te_regimes()
  feature_contracts <- get_te_features_by_regime()

  for (target in te_targets) {
    for (regime in te_regimes) {
      model_key <- get_te_model_key(target, regime)
      log_msg("")
      log_msg("Fitting:", model_key)

      required_features <- feature_contracts[[regime]]
      if (is.null(required_features)) {
        stop("Unknown regime: ", regime, ". Valid regimes: ", paste(te_regimes, collapse = ", "))
      }

      training_data_regime <- if (regime == "counterfactual_prior") {
        training_data %>% filter(!is.na(.data[[target]]))
      } else {
        training_data %>%
          filter(te_regime == regime) %>%
          filter(!is.na(.data[[target]]))
      }

      training_data_regime <- as.data.frame(training_data_regime, stringsAsFactors = FALSE)
      training_data_regime[[target]] <- as.numeric(training_data_regime[[target]])

      n_rows_available <- nrow(training_data_regime)

      if ("is_home" %in% names(training_data_regime)) {
        training_data_regime$is_home <- as.numeric(training_data_regime$is_home)
      }

      available_features <- intersect(required_features, names(training_data_regime))
      missing_features <- setdiff(required_features, available_features)
      if (length(missing_features) > 0) {
        stop("Missing required features for ", model_key, ": ", paste(missing_features, collapse = ", "))
      }

      is_count <- target %in% c("target_targets", "target_receptions", "target_rec_tds")

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
        y <- training_data_regime[[target]]
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "zero_variance_features"
        next
      }

      non_rolling_features <- c(
        "is_home",
        "is_rookie",
        "draft_round",
        "draft_pick_overall",
        "height",
        "weight",
        "age",
        grep("^prev_season", required_features, value = TRUE),
        grep("_roll1$", required_features, value = TRUE)
      )
      rolling_features <- setdiff(required_features, non_rolling_features)
      if (length(rolling_features) > 0) {
        training_data_regime <- training_data_regime %>%
          filter(if_all(all_of(rolling_features), ~ !is.na(.)))
      }

      if (length(required_features) > 0) {
        complete_mask <- complete.cases(training_data_regime[, required_features, drop = FALSE])
        training_data_regime <- training_data_regime[complete_mask, , drop = FALSE]
      }

      zero_var_features <- c()
      for (feat in required_features) {
        vals <- training_data_regime[[feat]]
        vals <- vals[!is.na(vals)]
        if (length(unique(vals)) <= 1) {
          zero_var_features <- c(zero_var_features, feat)
        }
      }
      if (length(zero_var_features) > 0) {
        log_msg("  Dropping zero-variance features after NA filtering: ", paste(zero_var_features, collapse = ", "))
        required_features <- setdiff(required_features, zero_var_features)
      }
      if (length(required_features) == 0) {
        y <- training_data_regime[[target]]
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "zero_variance_features_post_na"
        next
      }

      n_rows_final <- nrow(training_data_regime)

      log_msg("  Features used:", paste(required_features, collapse = ", "))
      log_msg("  Rows available:", n_rows_available)
      log_msg("  Rows used:", n_rows_final)

      if (n_rows_final == 0) {
        stop("Training data empty after feature/NA filtering for ", model_key)
      }

      min_positive <- if (grepl("_tds$", target)) 10 else 20

      formula_str <- paste0(target, " ~ ", paste(required_features, collapse = " + "))

      result$diagnostics[[model_key]] <- list(
        target = target,
        regime = regime,
        features_used = required_features,
        n_rows_available = n_rows_available,
        n_rows_final = n_rows_final,
        fit_type = "none",
        converged = FALSE,
        warnings = character(0),
        fallback_used = FALSE,
        fallback_reason = NULL
      )

      if (n_rows_final < min_rows) {
        y <- training_data_regime[[target]]
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "insufficient_rows"
        next
      }

      y <- training_data_regime[[target]]
      var_y <- var(y, na.rm = TRUE)
      if (is.na(var_y) || var_y == 0) {
        result$models[[model_key]] <- create_baseline_model(target, y, is_count = is_count)
        result$diagnostics[[model_key]]$fit_type <- "baseline"
        result$diagnostics[[model_key]]$fallback_used <- TRUE
        result$diagnostics[[model_key]]$fallback_reason <- "zero_variance_target"
        next
      }

      if (is_count) {
        n_positive <- sum(y > 0, na.rm = TRUE)
        if (n_positive < min_positive) {
          result$models[[model_key]] <- create_baseline_model(target, y, is_count = TRUE)
          result$diagnostics[[model_key]]$fit_type <- "baseline"
          result$diagnostics[[model_key]]$fallback_used <- TRUE
          result$diagnostics[[model_key]]$fallback_reason <- "insufficient_positive_outcomes"
          next
        }
      }

      fit_result <- if (is_count) {
        fit_count_model_robust(y, training_data_regime, formula_str, target)
      } else {
        fit_continuous_model_robust(y, training_data_regime, formula_str, target)
      }
      result$models[[model_key]] <- fit_result

      if (!is.null(fit_result$fit_type)) {
        result$diagnostics[[model_key]]$fit_type <- fit_result$fit_type
        result$diagnostics[[model_key]]$converged <- fit_result$converged
        result$diagnostics[[model_key]]$warnings <- fit_result$warnings
      }
    }
  }

  return(result)
}
