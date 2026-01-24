# Fit QB Models (v1 Contract) - Robust with Fallbacks
#
# Models QB outcomes: pass attempts, completions, pass yards, pass TDs,
# interceptions thrown, sacks taken, rush attempts, rush yards, rush TDs.

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
    fit <- glm(as.formula(formula_str), data = data, family = poisson(link = "log"),
               control = glm.control(maxit = 50))
    coef_vals <- coef(fit)
    if (!is.null(coef_vals) && sum(!is.na(coef_vals)) >= 1) {
      fit_type <- if (isTRUE(fit$converged)) "poisson" else "poisson_nonconverged"
      converged <- TRUE
      if (!isTRUE(fit$converged)) {
        warnings_captured <- c(warnings_captured, "Poisson: fit did not fully converge")
      }
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
      fit <- glm(as.formula(formula_str), data = data, family = quasipoisson(link = "log"),
                 control = glm.control(maxit = 50))
      coef_vals <- coef(fit)
      if (!is.null(coef_vals) && sum(!is.na(coef_vals)) >= 1) {
        fit_type <- if (isTRUE(fit$converged)) "quasipoisson" else "quasipoisson_nonconverged"
        converged <- TRUE
        if (!isTRUE(fit$converged)) {
          warnings_captured <- c(warnings_captured, "Quasipoisson: fit did not fully converge")
        }
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
      coef_vals <- coef(fit)
      if (!is.null(coef_vals) && sum(!is.na(coef_vals)) >= 1 && !is.null(fit$theta) && is.finite(fit$theta)) {
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
    coef_vals <- coef(fit)
    if (!is.null(coef_vals) && sum(!is.na(coef_vals)) >= 1) {
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

fit_binary_model_robust <- function(y, data, formula_str, target_name) {
  y <- as.numeric(y)
  data[[target_name]] <- y

  fit <- NULL
  warnings_captured <- character(0)

  tryCatch({
    fit <- glm(as.formula(formula_str), data = data, family = binomial(link = "logit"))
    coef_vals <- coef(fit)
    if (is.null(coef_vals) || sum(!is.na(coef_vals)) < 1) {
      fit <- NULL
    }
  }, warning = function(w) {
    warnings_captured <<- c(warnings_captured, paste0("Logit: ", conditionMessage(w)))
  }, error = function(e) {
    warnings_captured <<- c(warnings_captured, paste0("Logit: ", conditionMessage(e)))
  })

  list(fit = fit, warnings = warnings_captured)
}

fit_qb_models <- function(training_data, min_rows = 200) {
  if (is.null(training_data) || nrow(training_data) == 0) {
    stop("Empty training data provided. Cannot fit QB models.")
  }
  if (!"qb_regime" %in% names(training_data)) {
    stop("Missing qb_regime column in training data. Regime-based modeling requires qb_regime.")
  }
  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("QB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "QB", "qb_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
    validate_qb_v1_target_schema(training_data, strict = TRUE)
  } else {
    stop("Missing QB schema at ", schema_path)
  }
  regime_path <- if (exists("resolve_regime_path")) {
    resolve_regime_path("QB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "QB", "qb_regime_v1.R")
  }
  if (file.exists(regime_path)) {
    source(regime_path, local = TRUE)
  } else {
    stop("Missing QB regime at ", regime_path)
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
    "target_qb_rush_attempts",
    "target_qb_rush_tds"
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
      if (length(available_features) > 0) {
        is_informative <- vapply(available_features, function(feat) {
          vals <- training_data_regime[[feat]]
          vals <- vals[!is.na(vals)]
          length(unique(vals)) > 1
        }, logical(1))
      available_features <- available_features[is_informative]
      # Drop non-numeric predictors to avoid single-level factor errors.
      is_numeric_like <- vapply(available_features, function(feat) {
        vals <- training_data_regime[[feat]]
        is.numeric(vals) || is.integer(vals) || is.logical(vals)
      }, logical(1))
      available_features <- available_features[is_numeric_like]
      }
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

      is_count <- target %in% count_targets

      if (target == "target_qb_rush_tds") {
        # Goal-line signal for rushing TDs: prefer roll3 rate, fallback to roll5 rate.
        if ("target_qb_rush_td_rate_roll3" %in% names(training_data_regime) ||
            "target_qb_rush_td_rate_roll5" %in% names(training_data_regime)) {
          training_data_regime$rush_td_signal <- ifelse(
            !is.na(training_data_regime$target_qb_rush_td_rate_roll3),
            training_data_regime$target_qb_rush_td_rate_roll3,
            training_data_regime$target_qb_rush_td_rate_roll5
          )
          if (!all(is.na(training_data_regime$rush_td_signal))) {
            available_features <- unique(c(available_features, "rush_td_signal"))
          }
        }
        # Hurdle model for zero-inflated rushing TDs: P(td>0) * count(td|td>0).
        formula_str <- paste(target, "~", paste(available_features, collapse = " + "))
        y_td <- as.numeric(training_data_regime[[target]])
        y_bin <- as.integer(y_td > 0)
        formula_str_bin <- paste("qb_rush_td_any", "~", paste(available_features, collapse = " + "))
        prob_fit <- fit_binary_model_robust(y_bin, training_data_regime, formula_str_bin, "qb_rush_td_any")
        count_fit <- NULL
        count_warnings <- character(0)
        if (any(y_td > 0, na.rm = TRUE)) {
          count_data <- training_data_regime[y_td > 0, , drop = FALSE]
          if (nrow(count_data) >= 30) {
            count_fit <- fit_count_model_robust(count_data[[target]], count_data, formula_str, target)
            count_warnings <- count_fit$warnings
          }
        }

        if (is.null(prob_fit$fit)) {
          diagnostics[[model_key]] <- list(
            target = target,
            regime = regime,
            fit_type = "baseline",
            fallback_reason = "hurdle_prob_fit_failed",
            warnings = prob_fit$warnings
          )
          models[[model_key]] <- create_baseline_model(target, y_td, is_count = TRUE)
        } else {
          diagnostics[[model_key]] <- list(
            target = target,
            regime = regime,
            fit_type = "hurdle",
            n_rows_final = n_rows_available,
            warnings = c(prob_fit$warnings, count_warnings)
          )
          models[[model_key]] <- list(
            type = "hurdle",
            target = target,
            prob_model = prob_fit$fit,
            count_model = if (!is.null(count_fit) && !is.null(count_fit$fit)) count_fit$fit else NULL,
            count_is_count = TRUE,
            prob_fallback = mean(y_bin, na.rm = TRUE),
            count_fallback = if (any(y_td > 0, na.rm = TRUE)) mean(y_td[y_td > 0], na.rm = TRUE) else 1.0,
            warnings = c(prob_fit$warnings, count_warnings)
          )
        }
        next
      }

      formula_str <- paste(target, "~", paste(available_features, collapse = " + "))
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
