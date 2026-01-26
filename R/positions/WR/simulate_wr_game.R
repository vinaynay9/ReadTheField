# Simulate WR Game (v1 Contract)
#
# Monte Carlo simulation for WR player-game outcomes following WR v1 contract.

simulate_wr_game <- function(feature_row, wr_models, n_sims = 5000, availability_policy = "played_only") {
  log_file <- "wr_debug.log"
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  log_msg <- function(...) {
    cat(paste(...), "\n", file = log_file, append = TRUE)
  }

  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("WR", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "WR", "wr_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
  } else {
    stop("Missing WR schema at ", schema_path)
  }
  if (!exists("get_wr_v1_targets")) {
    stop("get_wr_v1_targets not loaded")
  }

  result <- list(
    draws = NULL,
    summary = NULL,
    status = "not_run"
  )

  if (is.null(feature_row) || nrow(feature_row) == 0) {
    stop("No feature row provided to simulate_wr_game.")
  }
  if (nrow(feature_row) > 1) {
    stop("Multiple feature rows provided to simulate_wr_game.")
  }

  regime_path <- if (exists("resolve_regime_path")) {
    resolve_regime_path("WR", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "WR", "wr_regime_v1.R")
  }
  if (file.exists(regime_path)) {
    source(regime_path, local = TRUE)
    if (!exists("get_wr_features_by_week")) {
      stop("get_wr_features_by_week function not found.")
    }
  } else {
    stop("Missing WR regime at ", regime_path)
  }

  if (exists("validate_availability_policy")) {
    availability_policy <- validate_availability_policy(availability_policy)
  }

  if (!"week" %in% names(feature_row)) {
    stop("Missing 'week' column in feature_row.")
  }
  week <- feature_row$week[1]
  if (is.na(week)) {
    stop("Week is NA in feature_row.")
  }
  if (week < 1 || week > 18) {
    stop("Invalid week: ", week, ". Week must be between 1 and 18.")
  }

  regime <- determine_wr_regime(week)
  valid_regimes <- get_wr_regimes()
  if (!regime %in% valid_regimes) {
    stop("Invalid regime determined: ", regime)
  }

  if (is.null(wr_models)) {
    stop("WR models are NULL. Cannot proceed with simulation.")
  }
  if (is.null(wr_models$models)) {
    stop("WR models use legacy structure. Regime-based models required.")
  }
  models_list <- wr_models$models

  feature_contracts <- get_wr_features_by_regime()
  regime_order <- c("standard", "late", "mid", "early")
  start_idx <- match(regime, regime_order)
  candidate_regimes <- regime_order[seq(from = start_idx, to = length(regime_order))]

  prediction_regime <- NULL
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
      "height",
      "weight",
      "age",
      grep("^prev_season", cand_features, value = TRUE),
      grep("_roll1$", cand_features, value = TRUE)
    )
    strict_features <- setdiff(cand_features, optional_features)
    na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
    if (length(na_strict) > 0) {
      next
    }
    prediction_regime <- cand
    required_features <- cand_features
    break
  }

  if (is.null(prediction_regime)) {
    if (availability_policy %in% c("expected_active", "force_counterfactual")) {
      cf_regime <- "counterfactual_prior"
      cand_features <- feature_contracts[[cf_regime]]
      if (!is.null(cand_features)) {
        missing_cols <- setdiff(cand_features, names(feature_row))
        if (length(missing_cols) == 0) {
          optional_features <- c(
            "is_home",
            "is_rookie",
            "draft_round",
            "draft_pick_overall",
            "height",
            "weight",
            "age",
            grep("^prev_season", cand_features, value = TRUE),
            grep("_roll1$", cand_features, value = TRUE)
          )
          strict_features <- setdiff(cand_features, optional_features)
          na_strict <- strict_features[sapply(strict_features, function(f) is.na(feature_row[[f]][1]))]
          if (length(na_strict) == 0) {
            prediction_regime <- cf_regime
            required_features <- cand_features
            fallback_used <- TRUE
            fallback_reason <- "No standard regime eligible (exposure-dependent features missing)."
          }
        }
      }
    }
    if (is.null(prediction_regime)) {
      stop("simulate_wr_game could not find a valid regime for prediction.")
    }
  }

  missing_features <- setdiff(required_features, names(feature_row))
  if (length(missing_features) > 0) {
    stop("simulate_wr_game missing required feature columns: ",
         paste(missing_features, collapse = ", "))
  }

  wr_targets <- get_wr_v1_targets()
  required_model_keys <- sapply(wr_targets, function(t) get_wr_model_key(t, prediction_regime))
  missing_models <- required_model_keys[!sapply(required_model_keys, function(k) !is.null(models_list[[k]]))]
  if (length(missing_models) > 0) {
    stop("Missing required WR v1 models for regime '", prediction_regime, "': ",
         paste(missing_models, collapse = ", "))
  }

  pred_data <- prepare_wr_prediction_data(feature_row, week = week, required_features = required_features)

  get_baseline_value <- function(target_name, regime_name = NULL) {
    if (!is.null(regime_name)) {
      key <- get_wr_model_key(target_name, regime_name)
      model <- models_list[[key]]
      if (!is.null(model) && !is.null(model$type) && model$type == "baseline") {
        return(as.numeric(model$value))
      }
    }
    baseline_keys <- names(models_list)[grepl(paste0("^", target_name, "__"), names(models_list))]
    for (key in baseline_keys) {
      model <- models_list[[key]]
      if (!is.null(model) && !is.null(model$type) && model$type == "baseline") {
        return(as.numeric(model$value))
      }
    }
    NA_real_
  }

  get_player_prior_mu <- function(target_name) {
    if (target_name == "target_targets") {
      if ("prev_season_targets_total" %in% names(feature_row) &&
          "prev_season_games_played" %in% names(feature_row) &&
          is.finite(feature_row$prev_season_targets_total[1]) &&
          is.finite(feature_row$prev_season_games_played[1]) &&
          feature_row$prev_season_games_played[1] > 0) {
        return(as.numeric(feature_row$prev_season_targets_total[1] / feature_row$prev_season_games_played[1]))
      }
    }
    if (target_name == "target_receptions") {
      if ("prev_season_receptions_total" %in% names(feature_row) &&
          "prev_season_games_played" %in% names(feature_row) &&
          is.finite(feature_row$prev_season_receptions_total[1]) &&
          is.finite(feature_row$prev_season_games_played[1]) &&
          feature_row$prev_season_games_played[1] > 0) {
        return(as.numeric(feature_row$prev_season_receptions_total[1] / feature_row$prev_season_games_played[1]))
      }
    }
    if (target_name == "target_rec_tds") {
      if ("prev_season_rec_tds_total" %in% names(feature_row) &&
          "prev_season_games_played" %in% names(feature_row) &&
          is.finite(feature_row$prev_season_rec_tds_total[1]) &&
          is.finite(feature_row$prev_season_games_played[1]) &&
          feature_row$prev_season_games_played[1] > 0) {
        return(as.numeric(feature_row$prev_season_rec_tds_total[1] / feature_row$prev_season_games_played[1]))
      }
    }
    NA_real_
  }

  build_fallback_mu <- function(target_name, regime_name) {
    player_prior <- get_player_prior_mu(target_name)
    if (is.finite(player_prior)) return(player_prior)
    regime_median <- get_baseline_value(target_name, regime_name)
    if (is.finite(regime_median)) return(regime_median)
    global_median <- get_baseline_value(target_name, NULL)
    if (is.finite(global_median)) return(global_median)
    0.5
  }

  sim_targets <- numeric(n_sims)
  sim_receptions <- numeric(n_sims)
  sim_rec_yards <- numeric(n_sims)
  sim_rec_tds <- numeric(n_sims)

  targets_model_key <- get_wr_model_key("target_targets", prediction_regime)
  receptions_model_key <- get_wr_model_key("target_receptions", prediction_regime)
  rec_yards_model_key <- get_wr_model_key("target_rec_yards", prediction_regime)
  rec_tds_model_key <- get_wr_model_key("target_rec_tds", prediction_regime)

  targets_model <- models_list[[targets_model_key]]
  receptions_model <- models_list[[receptions_model_key]]
  rec_yards_model <- models_list[[rec_yards_model_key]]
  rec_tds_model <- models_list[[rec_tds_model_key]]

  for (i in seq_len(n_sims)) {
    sim_targets[i] <- sample_from_model_wr(
      targets_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_targets", prediction_regime)
    )
    sim_receptions[i] <- sample_from_model_wr(
      receptions_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_receptions", prediction_regime)
    )
    sim_rec_yards[i] <- sample_from_model_wr(
      rec_yards_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_rec_yards", prediction_regime)
    )
    sim_rec_tds[i] <- sample_from_model_wr(
      rec_tds_model,
      pred_data,
      n_samples = 1,
      availability_policy = availability_policy,
      fallback_mu = build_fallback_mu("target_rec_tds", prediction_regime)
    )
  }

  sim_receiving_yards <- as.numeric(sim_rec_yards)
  sim_total_yards <- sim_receiving_yards
  sim_total_tds <- as.numeric(sim_rec_tds)

  if (availability_policy %in% c("expected_active", "force_counterfactual")) {
    sim_targets[!is.finite(sim_targets)] <- 0
    sim_receptions[!is.finite(sim_receptions)] <- 0
    sim_rec_yards[!is.finite(sim_rec_yards)] <- 0
    sim_rec_tds[!is.finite(sim_rec_tds)] <- 0
    sim_receiving_yards[!is.finite(sim_receiving_yards)] <- 0
    sim_total_yards[!is.finite(sim_total_yards)] <- 0
    sim_total_tds[!is.finite(sim_total_tds)] <- 0
  }

  if (any(!is.finite(sim_receiving_yards))) {
    stop("Derived receiving_yards contain non-finite values.")
  }

  result$draws <- data.frame(
    targets = sim_targets,
    receptions = sim_receptions,
    rec_yards = sim_rec_yards,
    rec_tds = sim_rec_tds,
    receiving_yards = sim_receiving_yards,
    total_yards = sim_total_yards,
    total_touchdowns = sim_total_tds,
    stringsAsFactors = FALSE
  )

  required_outcomes <- c("targets", "receptions", "rec_yards", "rec_tds", "receiving_yards", "total_yards", "total_touchdowns")
  for (outcome in required_outcomes) {
    if (is.null(result$draws[[outcome]])) {
      stop("Simulation output '", outcome, "' is NULL.")
    }
    if (!is.numeric(result$draws[[outcome]])) {
      stop("Simulation output '", outcome, "' is not numeric.")
    }
    if (length(result$draws[[outcome]]) != n_sims) {
      stop("Simulation output '", outcome, "' has incorrect length.")
    }
  }

  result$summary <- compute_wr_percentiles(result$draws)
  result$status <- "success"
  result$diagnostics <- list(
    regime_selected = prediction_regime,
    fallback_used = fallback_used,
    fallback_reason = fallback_reason
  )

  return(result)
}

prepare_wr_prediction_data <- function(feature_row, week = NULL, required_features = NULL) {
  df <- as.data.frame(feature_row)

  if (!is.null(week) && exists("get_wr_features_by_week")) {
    if (is.null(required_features)) {
      required_features <- get_wr_features_by_week(week)
    }
    missing_features <- setdiff(required_features, names(df))
    if (length(missing_features) > 0) {
      stop("Missing required features for week ", week, ": ", paste(missing_features, collapse = ", "))
    }
    optional_na_features <- c(
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
    strict_features <- setdiff(required_features, optional_na_features)
    na_features <- sapply(strict_features, function(f) f %in% names(df) && is.na(df[[f]][1]))
    if (any(na_features)) {
      stop("NA values in required features for week ", week, ": ",
           paste(strict_features[na_features], collapse = ", "))
    }
  }

  return(df)
}

predict_safe_wr <- function(model, newdata, type = "response", n_samples = 1,
                            availability_policy = "played_only", fallback_mu = 0.5) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot make prediction.")
  }

  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      result <- rpois(n_samples, lambda = mu)
    } else {
      result <- pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd))
    }
    return(result)
  }

  # Unwrap nested model objects (some fit_* helpers return lists with $fit/$model)
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

sample_from_model_wr <- function(model, newdata, n_samples = 1, availability_policy = "played_only", fallback_mu = 0.5) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot sample.")
  }

  if (!is.null(model$type) && model$type == "baseline") {
    if (model$is_count) {
      mu <- as.numeric(model$value)
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        if (!is.finite(mu)) {
          mu <- fallback_mu
        }
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
    } else {
      return(pmax(0, rnorm(n_samples, mean = model$value, sd = model$sd)))
    }
  }

  # Unwrap nested model objects (some fit_* helpers return lists with $fit/$model)
  model_obj <- model
  if (is.list(model) && !inherits(model, c("glm", "lm", "negbin"))) {
    if (!is.null(model$fit)) {
      model_obj <- model$fit
    } else if (!is.null(model$model)) {
      model_obj <- model$model
    }
  }

  mu <- predict_safe_wr(
    model_obj,
    newdata,
    type = "response",
    n_samples = 1,
    availability_policy = availability_policy,
    fallback_mu = fallback_mu
  )

  if (inherits(model_obj, "glm")) {
    family_name <- model_obj$family$family
    if (family_name == "poisson" || family_name == "quasipoisson") {
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        mu[!is.finite(mu)] <- fallback_mu
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rpois(n_samples, lambda = mu)))))
    } else if (family_name == "negbin" || inherits(model_obj, "negbin")) {
      theta <- if (!is.null(model_obj$theta)) model_obj$theta else 1.0
      if (availability_policy %in% c("expected_active", "force_counterfactual")) {
        mu[!is.finite(mu)] <- fallback_mu
      }
      mu <- pmax(mu, 0.01)
      return(pmax(0L, as.integer(round(rnbinom(n_samples, size = theta, mu = mu)))))
    }
  } else if (inherits(model_obj, "lm")) {
    if (!is.null(model_obj$transform) && model_obj$transform == "log1p") {
      sigma_val <- get_residual_sd_wr(model_obj)
      samples <- rnorm(n_samples, mean = mu, sd = sigma_val)
      return(pmax(0, expm1(samples)))
    } else {
      sigma_val <- get_residual_sd_wr(model_obj)
      return(pmax(0, rnorm(n_samples, mean = mu, sd = sigma_val)))
    }
  }

  return(rep(mu, n_samples))
}

get_residual_sd_wr <- function(model) {
  if (is.null(model)) {
    stop("Model is NULL. Cannot get residual SD.")
  }
  if (!is.null(model$type) && model$type == "baseline") {
    return(model$sd)
  }
  tryCatch({
    sigma(model)
  }, error = function(e) {
    stop("Failed to get residual SD from model. Error: ", e$message)
  })
}

compute_wr_percentiles <- function(draws) {
  stats <- c("targets", "receptions", "receiving_yards", "total_touchdowns")
  probs <- c(0.10, 0.25, 0.40, 0.50, 0.60, 0.75, 0.90)
  result <- data.frame(
    stat = stats,
    p10 = NA_real_,
    p25 = NA_real_,
    p40 = NA_real_,
    p50 = NA_real_,
    p60 = NA_real_,
    p75 = NA_real_,
    p90 = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(stats)) {
    stat <- stats[i]
    vals <- NULL
    if (stat %in% names(draws)) {
      vals <- draws[[stat]]
    } else if (stat == "receiving_yards" && "rec_yards" %in% names(draws)) {
      vals <- draws$rec_yards
    } else if (stat == "total_touchdowns") {
      if ("total_touchdowns" %in% names(draws)) {
        vals <- draws$total_touchdowns
      } else if ("receiving_tds" %in% names(draws)) {
        vals <- draws$receiving_tds
      } else if ("rec_tds" %in% names(draws)) {
        vals <- draws$rec_tds
      }
    }
    if (!is.null(vals)) {
      q <- quantile(vals, probs, na.rm = TRUE)
      result[i, c("p10", "p25", "p40", "p50", "p60", "p75", "p90")] <- as.numeric(q)
    }
  }

  for (col in c("p10", "p25", "p40", "p50", "p60", "p75", "p90")) {
    result[[col]] <- round(result[[col]])
  }

  return(result)
}
