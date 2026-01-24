# Simulate K Game - Core Monte Carlo Engine (K v1)
#
# Uses fitted K models to generate Monte Carlo draws.

simulate_k_game <- function(feature_row,
                            k_models,
                            n_sims = 5000,
                            availability_policy = "played_only") {
  if (is.null(feature_row) || nrow(feature_row) == 0) {
    stop("No feature row provided to simulate_k_game.")
  }
  if (is.null(k_models) || is.null(k_models$models)) {
    stop("k_models missing required model objects.")
  }
  if (!exists("determine_k_regime")) {
    regime_path <- if (exists("resolve_regime_path")) {
      resolve_regime_path("K", "v1")
    } else {
      file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "K", "k_regime_v1.R")
    }
    if (file.exists(regime_path)) {
      source(regime_path, local = TRUE)
    } else {
      stop("Missing K regime at ", regime_path)
    }
  }

  week <- if ("week" %in% names(feature_row)) as.integer(feature_row$week[1]) else NA_integer_
  prediction_regime <- determine_k_regime(week)

  if (!exists("get_k_features_by_regime")) {
    stop("get_k_features_by_regime not available for regime-aware feature selection.")
  }

  feature_contracts <- get_k_features_by_regime()
  regime_order <- c("standard", "late", "mid", "early")
  start_idx <- match(prediction_regime, regime_order)
  candidate_regimes <- regime_order[seq(from = start_idx, to = length(regime_order))]

  required_features <- NULL
  fallback_used <- FALSE
  fallback_reason <- NA_character_

  for (cand in candidate_regimes) {
    cand_features <- feature_contracts[[cand]]
    if (is.null(cand_features)) next
    missing_cols <- setdiff(cand_features, names(feature_row))
    if (length(missing_cols) > 0) {
      next
    }
    optional_features <- c(
      "is_home",
      "is_rookie",
      "draft_round",
      "draft_pick_overall",
      "position",
      "height",
      "weight",
      "age",
      grep("_roll1$", cand_features, value = TRUE)
    )
    strict_features <- setdiff(cand_features, optional_features)
    if (length(strict_features) == 0) {
      na_strict <- character(0)
    } else {
      na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
    }
    if (length(na_strict) > 0) {
      next
    }
    prediction_regime <- cand
    required_features <- cand_features
    break
  }

  if (is.null(required_features)) {
    if (availability_policy %in% c("expected_active", "force_counterfactual")) {
      cf_regime <- "counterfactual_prior"
      cand_features <- feature_contracts[[cf_regime]]
      if (is.null(cand_features)) {
        stop("Counterfactual regime contract missing. Cannot proceed with fallback.", call. = FALSE)
      }
      missing_cols <- setdiff(cand_features, names(feature_row))
      if (length(missing_cols) == 0) {
        optional_features <- c(
          "is_home",
          "is_rookie",
          "draft_round",
          "draft_pick_overall",
          "position",
          "height",
          "weight",
          "age",
          grep("_roll1$", cand_features, value = TRUE)
        )
        strict_features <- setdiff(cand_features, optional_features)
        if (length(strict_features) == 0) {
          na_strict <- character(0)
        } else {
          na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
        }
        if (length(na_strict) == 0) {
          prediction_regime <- cf_regime
          required_features <- cand_features
          fallback_used <- TRUE
          fallback_reason <- "No standard regime eligible (exposure-dependent features missing)."
        }
      }
    }
    if (is.null(required_features)) {
      stop("simulate_k_game could not find a valid regime for prediction. Missing columns or NA strict features.")
    }
  }

  missing_features <- setdiff(required_features, names(feature_row))
  if (length(missing_features) > 0) {
    stop("Missing required features for regime ", prediction_regime, ": ",
         paste(missing_features, collapse = ", "))
  }

  prediction_data <- as.data.frame(feature_row[, required_features, drop = FALSE])
  if ("is_home" %in% names(prediction_data)) {
    prediction_data$is_home <- as.numeric(prediction_data$is_home)
  }

  targets <- get_k_v1_targets()
  draws <- data.frame(row = seq_len(n_sims))

  predict_safe_k <- function(model, newdata, type = "response", n_samples = 1,
                             availability_policy = "played_only", fallback_mu = 0.5) {
    if (is.null(model)) {
      stop("Model is NULL. Cannot make prediction.")
    }
    if (!is.null(model$type) && model$type == "baseline") {
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      return(rpois(n_samples, lambda = mu))
    }
    # Unwrap nested model objects (fit_k_models returns lists with $model)
    model_obj <- model
    if (is.list(model) && !inherits(model, c("glm", "lm", "negbin"))) {
      if (!is.null(model$fit)) {
        model_obj <- model$fit
      } else if (!is.null(model$model)) {
        model_obj <- model$model
      }
    }
    tryCatch({
      pred <- predict(model_obj, newdata = newdata, type = type)
      as.numeric(pred)
    }, error = function(e) {
      stop("Prediction failed for model. Error: ", e$message)
    })
  }

  sample_from_model_k <- function(model, newdata, n_samples = 1, availability_policy = "played_only", fallback_mu = 0.5) {
    if (is.null(model)) {
      stop("Model is NULL. Cannot sample.")
    }
    if (!is.null(model$type) && model$type == "baseline") {
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
    }
    # Unwrap nested model objects (fit_k_models returns lists with $model)
    model_obj <- model
    if (is.list(model) && !inherits(model, c("glm", "lm", "negbin"))) {
      if (!is.null(model$fit)) {
        model_obj <- model$fit
      } else if (!is.null(model$model)) {
        model_obj <- model$model
      }
    }
    mu <- predict_safe_k(model_obj, newdata, type = "response", n_samples = 1,
                         availability_policy = availability_policy, fallback_mu = fallback_mu)
    if (inherits(model_obj, "glm")) {
      family_name <- model_obj$family$family
      if (family_name %in% c("poisson", "quasipoisson")) {
        if (availability_policy %in% c("expected_active", "force_counterfactual")) {
          mu[!is.finite(mu)] <- fallback_mu
        }
        mu <- pmax(mu, 0.01)
        return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
      }
    }
    stop("Unsupported model type for sampling.")
  }

  for (target in targets) {
    model_key <- get_k_model_key(target, prediction_regime)
    model_obj <- k_models$models[[model_key]]
    if (is.null(model_obj)) {
      stop("Missing model for target ", target, " and regime ", prediction_regime)
    }
    draws[[target]] <- sample_from_model_k(model_obj, prediction_data, n_sims,
                                           availability_policy = availability_policy)
  }

  result <- list(
    status = "success",
    draws = draws,
    summary = compute_k_percentiles(draws),
    diagnostics = list(
      regime_selected = prediction_regime,
      fallback_used = fallback_used,
      fallback_reason = fallback_reason
    )
  )

  result
}

compute_k_percentiles <- function(draws) {
  stats <- c("target_fg_attempts_k", "target_fg_made_k", "target_pat_made_k")
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
  result
}
