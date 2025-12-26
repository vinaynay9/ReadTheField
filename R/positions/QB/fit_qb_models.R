# Fit QB Models (v1 Contract) - Robust with Fallbacks
#
# Models QB outcomes: pass attempts, completions, pass yards, pass TDs,
# interceptions thrown, sacks taken, rush attempts, rush yards.

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

  list(fit = fit, fit_type = fit_type, converged = converged, warnings = warnings_captured)
}

fit_continuous_model_robust <- function(y, data, formula_str, target_name) {
  y <- as.numeric(y)
  data[[target_name]] <- y

  fit <- NULL
  fit_type <- "none"
  warnings_captured <- character(0)

  tryCatch({
    fit <- lm(as.formula(formula_str), data = data)
    if (!any(is.na(coef(fit)))) {
      fit_type <- "gaussian"
    } else {
      fit <- NULL
    }
  }, warning = function(w) {
    warnings_captured <<- c(warnings_captured, paste0("LM: ", conditionMessage(w)))
  }, error = function(e) {
    warnings_captured <<- c(warnings_captured, paste0("LM: ", conditionMessage(e)))
  })

  list(fit = fit, fit_type = fit_type, warnings = warnings_captured)
}

fit_qb_models <- function(training_data, min_rows = 200) {
  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit QB models.")
  }
  if (!"qb_regime" %in% names(training_data)) {
    stop("Missing qb_regime column in training data. Regime-based modeling requires qb_regime.")
  }
  if (file.exists("R/positions/QB/qb_schema_v1.R")) {
    source("R/positions/QB/qb_schema_v1.R", local = TRUE)
    validate_qb_v1_target_schema(training_data, strict = TRUE)
  }
  if (file.exists("R/positions/QB/qb_regime_v1.R")) {
    source("R/positions/QB/qb_regime_v1.R", local = TRUE)
  }

  if (any(is.na(training_data$qb_regime))) {
    training_data <- training_data[!is.na(training_data$qb_regime), , drop = FALSE]
  }
  if (nrow(training_data) < min_rows) {
    stop("Insufficient training rows for QB models: ", nrow(training_data), " (< ", min_rows, ").")
  }

  targets <- get_qb_v1_targets()
  regimes <- get_qb_regimes()
  diagnostics <- list()
  models <- list()

  count_targets <- c(
    "target_pass_attempts_qb",
    "target_completions_qb",
    "target_pass_tds_qb",
    "target_interceptions_qb_thrown",
    "target_sacks_qb_taken",
    "target_qb_rush_attempts"
  )
  continuous_targets <- c(
    "target_pass_yards_qb",
    "target_qb_rush_yards"
  )

  for (regime in regimes) {
    required_features <- get_qb_features_by_regime()[[regime]]
    if (is.null(required_features)) next
    for (target in targets) {
      model_key <- get_qb_model_key(target, regime)
      training_data_regime <- if (regime == "counterfactual_prior") {
        training_data %>% filter(!is.na(.data[[target]]))
      } else {
        training_data %>% filter(qb_regime == regime, !is.na(.data[[target]]))
      }
      training_data_regime <- as.data.frame(training_data_regime, stringsAsFactors = FALSE)
      training_data_regime[[target]] <- as.numeric(training_data_regime[[target]])

      n_rows_available <- nrow(training_data_regime)
      if (n_rows_available == 0) {
        diagnostics[[model_key]] <- list(
          target = target,
          regime = regime,
          fit_type = "baseline",
          fallback_reason = "no_training_rows"
        )
        models[[model_key]] <- create_baseline_model(target, 0, is_count = target %in% count_targets)
        next
      }

      if ("is_home" %in% names(training_data_regime)) {
        training_data_regime$is_home <- as.numeric(training_data_regime$is_home)
      }

      available_features <- intersect(required_features, names(training_data_regime))
      if (length(available_features) == 0) {
        diagnostics[[model_key]] <- list(
          target = target,
          regime = regime,
          fit_type = "baseline",
          fallback_reason = "no_features_available"
        )
        models[[model_key]] <- create_baseline_model(target, training_data_regime[[target]], is_count = target %in% count_targets)
        next
      }

      formula_str <- paste(target, "~", paste(available_features, collapse = " + "))
      is_count <- target %in% count_targets

      if (is_count) {
        fit_result <- fit_count_model_robust(training_data_regime[[target]], training_data_regime, formula_str, target)
      } else {
        fit_result <- fit_continuous_model_robust(training_data_regime[[target]], training_data_regime, formula_str, target)
      }

      if (is.null(fit_result$fit)) {
        diagnostics[[model_key]] <- list(
          target = target,
          regime = regime,
          fit_type = "baseline",
          fallback_reason = "model_fit_failed",
          warnings = fit_result$warnings
        )
        models[[model_key]] <- create_baseline_model(target, training_data_regime[[target]], is_count = is_count)
      } else {
        diagnostics[[model_key]] <- list(
          target = target,
          regime = regime,
          fit_type = fit_result$fit_type,
          n_rows_final = n_rows_available,
          warnings = fit_result$warnings
        )
        models[[model_key]] <- list(
          type = fit_result$fit_type,
          target = target,
          model = fit_result$fit,
          is_count = is_count
        )
      }
    }
  }

  list(models = models, diagnostics = diagnostics)
}
