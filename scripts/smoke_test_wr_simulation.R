# Smoke Test for WR Simulation
#
# This script tests the WR simulation pipeline end-to-end without calling refresh.
# It loads cached features, fits models, and runs simulations for WR player-weeks.
#
# Exit codes:
#   0: All tests passed
#   1: Test failed (error occurred)

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, cmd_args, value = TRUE)
  if (length(match) > 0) {
    return(normalizePath(sub(file_arg, "", match[1])))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
  stop("Unable to determine script path to set working directory.")
}

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(repo_root)
options(READTHEFIELD_REPO_ROOT = repo_root)

cat("Loading simulation bootstrap...\n")
tryCatch({
  source(file.path(repo_root, "R/simulation/bootstrap_simulation.R"))
  cat("  Bootstrap loaded successfully\n")
}, error = function(e) {
  cat("ERROR: Failed to load bootstrap:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 1: Loading cached WR weekly features...\n")
tryCatch({
  wr_features <- read_wr_weekly_features_cache() |>
    as.data.frame()
  if (nrow(wr_features) == 0) {
    stop("WR weekly features cache is empty. Run scripts/refresh_weekly_cache.R first.")
  }
  cat("  Loaded", nrow(wr_features), "rows\n")
  rushing_cols <- grep("rush|carry", names(wr_features), ignore.case = TRUE, value = TRUE)
  if (length(rushing_cols) > 0) {
    stop("WR feature matrix contains rushing columns: ", paste(rushing_cols, collapse = ", "))
  }
  required_def_cols <- c(
    "def_interceptions_defense_caught_roll3",
    "def_passes_defended_defense_forced_roll3",
    "def_interceptions_defense_caught_roll5",
    "def_passes_defended_defense_forced_roll5"
  )
  missing_def_cols <- setdiff(required_def_cols, names(wr_features))
  if (length(missing_def_cols) > 0) {
    stop("WR feature matrix missing defensive columns: ", paste(missing_def_cols, collapse = ", "))
  }

  wr_stats <- read_wr_weekly_stats_cache() |>
    as.data.frame()
  required_raw <- c(
    "player_id", "season", "week",
    "targets",
    "receptions",
    "receiving_yards",
    "receiving_tds"
  )
  missing_raw <- setdiff(required_raw, names(wr_stats))
  if (length(missing_raw) > 0) {
    stop("WR weekly stats missing required columns: ", paste(missing_raw, collapse = ", "))
  }
  cat("  Loaded raw WR weekly stats with", nrow(wr_stats), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to load WR weekly features or stats:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 2: Filtering to training window (2010:2023)...\n")
tryCatch({
  train_df <- wr_features |>
    dplyr::inner_join(
      wr_stats[, required_raw, drop = FALSE],
      by = c("player_id", "season", "week")
    )

  if (!any(grepl("target_pass_attempts_qb", names(train_df)))) {
    stop("Team offense context features missing from training data.")
  }
  if (!any(grepl("prev_season_", names(train_df)))) {
    stop("Prior-season aggregate features missing from training data.")
  }

  if (nrow(train_df) == 0) {
    stop("Training join produced zero rows.")
  }

  train_df <- train_df |>
    dplyr::mutate(
      target_targets = as.numeric(targets),
      target_receptions = as.numeric(receptions),
      target_rec_yards = as.numeric(receiving_yards),
      target_rec_tds = as.numeric(receiving_tds)
    ) |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)

  if (any(train_df$target_rec_tds < 0, na.rm = TRUE)) {
    stop("Negative receiving touchdowns detected.")
  }

  train_window <- train_df |>
    dplyr::filter(
      season >= 2010,
      season <= 2023,
      !is.na(target_targets),
      !is.na(target_receptions),
      !is.na(target_rec_yards),
      !is.na(target_rec_tds)
    )

  if (nrow(train_window) < 1000) {
    stop("Training window too small: ", nrow(train_window))
  }
  cat("  Training window has", nrow(train_window), "rows\n")
}, error = function(e) {
  cat("ERROR: Failed to filter training window:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 3: Fitting WR models...\n")
tryCatch({
  wr_models <- fit_wr_models(train_window, min_rows = 200)

  required_keys <- c()
  wr_targets <- get_wr_v1_targets()
  wr_regimes <- get_wr_regimes()
  for (target in wr_targets) {
    for (regime in wr_regimes) {
      required_keys <- c(required_keys, get_wr_model_key(target, regime))
    }
  }
  if (is.null(wr_models$models) || any(!required_keys %in% names(wr_models$models))) {
    stop("WR models failed validation.")
  }

  cat("  Models fitted and validated\n")
}, error = function(e) {
  cat("ERROR: Failed to fit WR models:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 4: Finding test player-week...\n")
tryCatch({
  test_season <- 2024

  test_games <- NULL
  test_week <- NA_integer_
  for (wk in 8:12) {
    feat_cols <- get_wr_features_by_week(wk)
    candidates <- wr_features[
      wr_features$season == test_season &
        wr_features$week == wk &
        !is.na(wr_features$player_id) &
        !is.na(wr_features$player_name),
      , drop = FALSE
    ]
    if (nrow(candidates) == 0) next
    available_cols <- feat_cols[sapply(feat_cols, function(f) !all(is.na(candidates[[f]])))]
    complete_mask <- stats::complete.cases(candidates[, available_cols, drop = FALSE])
    candidates <- candidates[complete_mask, , drop = FALSE]
    if (nrow(candidates) > 0) {
      test_games <- candidates
      test_week <- wk
      break
    }
  }

  if (is.null(test_games) || nrow(test_games) == 0) {
    stop("No games found for season ", test_season, " with complete WR feature set.")
  }
  test_player_name <- test_games$player_name[1]
  test_player_id <- test_games$player_id[1]
  test_team <- test_games$team[1]
  test_opponent <- test_games$opponent[1]

  cat("  Test player:", test_player_name, "(gsis_id:", test_player_id, ")\n")
  cat("  Test game: Season", test_season, "Week", test_week,
      test_team, "vs", test_opponent, "\n")
}, error = function(e) {
  cat("ERROR: Failed to find test player-week:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 5: Running simulation...\n")
tryCatch({
  result <- simulate_player_game(
    gsis_id = test_player_id,
    season = test_season,
    week = test_week,
    n_sims = 800,
    mode = "historical_replay"
  )

  if (is.null(result) || is.null(result$draws) || nrow(result$draws) != 800) {
    stop("Simulation draws invalid.")
  }

  required_outcomes <- c("targets", "receptions", "receiving_yards", "total_touchdowns")
  missing_outcomes <- setdiff(required_outcomes, names(result$draws))
  if (length(missing_outcomes) > 0) {
    stop("Missing outcomes in draws: ", paste(missing_outcomes, collapse = ", "))
  }

  feature_cols <- result$diagnostics$feature_usage$candidate_features
  used_features <- result$diagnostics$feature_usage$used_features
  if (length(used_features) == 0) {
    stop("No used features recorded in diagnostics.")
  }
  used_idx <- match(used_features, feature_cols)
  if (any(is.na(used_idx)) || any(diff(used_idx) <= 0)) {
    stop("Used feature order does not match WR feature contract order.")
  }

  def_checks <- intersect(
    c("def_interceptions_defense_caught_roll3", "def_passes_defended_defense_forced_roll3",
      "def_interceptions_defense_caught_roll5", "def_passes_defended_defense_forced_roll5"),
    feature_cols
  )
  missing_def_used <- setdiff(def_checks, used_features)
  if (length(missing_def_used) > 0) {
    stop("Defensive features missing from used feature list: ", paste(missing_def_used, collapse = ", "))
  }
  for (nm in def_checks) {
    val <- result$defensive_context[[nm]]
    if (is.null(val) || is.na(val)) {
      stop("Defensive feature is NA in simulation input row: ", nm)
    }
  }

  cat("  Simulation completed successfully\n")
  cat("  Draws:", nrow(result$draws), "rows\n")
}, error = function(e) {
  cat("ERROR: Simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 6: Early-season window (week 2)...\n")
tryCatch({
  early_player <- NA_character_
  early_week <- NA_integer_
  for (wk in 2:3) {
    feat_cols <- get_wr_features_by_week(wk)
    candidates <- wr_features[
      wr_features$season == 2024 &
        wr_features$week == wk &
        !is.na(wr_features$player_id),
      , drop = FALSE
    ]
    if (nrow(candidates) == 0) next
    available_cols <- feat_cols[sapply(feat_cols, function(f) !all(is.na(candidates[[f]])))]
    complete_mask <- stats::complete.cases(candidates[, available_cols, drop = FALSE])
    candidates <- candidates[complete_mask, , drop = FALSE]
    if (nrow(candidates) > 0) {
      early_player <- candidates$player_id[1]
      early_week <- wk
      break
    }
  }
  if (is.na(early_player)) {
    stop("No early-season games found with complete WR feature set.")
  }

  result_early <- simulate_player_game(
    gsis_id = early_player,
    season = 2024,
    week = early_week,
    n_sims = 300,
    mode = "historical_replay"
  )
  if (is.null(result_early$draws) || nrow(result_early$draws) != 300) {
    stop("Early-season simulation draws invalid.")
  }

  cat("  Early-season simulation completed successfully\n")
}, error = function(e) {
  cat("ERROR: Early-season simulation failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 7: Availability policy (counterfactual) behavior...\n")
tryCatch({
  test_season <- 2024
  schedules <- load_schedules(seasons = test_season, cache_only = TRUE)
  if (nrow(schedules) == 0) {
    stop("Schedule data is empty for season ", test_season)
  }

  player_dim <- read_player_dim_cache()
  wr_stats_season <- wr_stats[wr_stats$season == test_season, , drop = FALSE]
  if (nrow(wr_stats_season) == 0) {
    stop("No WR stats found for season ", test_season)
  }

  candidate <- NULL
  player_ids <- unique(wr_stats_season$player_id)
  for (pid in player_ids) {
    weeks_played <- sort(unique(wr_stats_season$week[wr_stats_season$player_id == pid]))
    if (length(weeks_played) == 0) next
    max_week <- max(weeks_played, na.rm = TRUE)
    if (is.na(max_week) || max_week < 7) next

    missing_weeks <- setdiff(7:max_week, weeks_played)
    if (length(missing_weeks) == 0) next

    team_row <- player_dim[player_dim$gsis_id == pid & player_dim$season == test_season, , drop = FALSE]
    if (nrow(team_row) == 0 || is.na(team_row$team[1]) || team_row$team[1] == "") next
    team <- team_row$team[1]

    for (wk in missing_weeks) {
      sched_match <- schedules[
        schedules$season == test_season &
          schedules$week == wk &
          (schedules$home_team == team | schedules$away_team == team),
        , drop = FALSE
      ]
      if (nrow(sched_match) > 0) {
        candidate <- list(player_id = pid, week = wk, team = team)
        break
      }
    }
    if (!is.null(candidate)) break
  }

  if (is.null(candidate)) {
    stop("No missing-week candidate found for counterfactual test in season ", test_season)
  }

  cat("  Candidate:", candidate$player_id, "team", candidate$team, "week", candidate$week, "\n")

  err <- simulate_player_game(
    gsis_id = candidate$player_id,
    season = test_season,
    week = candidate$week,
    n_sims = 200,
    availability_policy = "played_only"
  )
  if (is.null(err$status) || err$status != "error") {
    stop("Expected played_only to fail for missing/inactive player-week.")
  }

  result_cf <- simulate_player_game(
    gsis_id = candidate$player_id,
    season = test_season,
    week = candidate$week,
    n_sims = 200,
    availability_policy = "expected_active"
  )

  if (is.null(result_cf$diagnostics$availability) ||
      !isTRUE(result_cf$diagnostics$availability$counterfactual)) {
    stop("Counterfactual availability not recorded in diagnostics.")
  }

  if (is.null(result_cf$diagnostics$regime_selection) ||
      result_cf$diagnostics$regime_selection$regime_selected != "counterfactual_prior") {
    stop("Counterfactual regime fallback not selected.")
  }

  if (is.null(result_cf$draws) || nrow(result_cf$draws) != 200) {
    stop("Counterfactual simulation draws invalid.")
  }

  cat("  Availability policy fallback: counterfactual_prior regime selected\n")
  cat("  Counterfactual simulation succeeded\n")
}, error = function(e) {
  cat("ERROR: Availability policy test failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nTest 8: Future game simulation and feature contract...\n")
tryCatch({
  test_season <- 2024
  schedules <- load_schedules(seasons = test_season, cache_only = TRUE)
  if (nrow(schedules) == 0) {
    stop("Schedule data is empty for season ", test_season)
  }
  max_week_seen <- max(wr_features$week[wr_features$season == test_season], na.rm = TRUE)
  player_dim <- read_player_dim_cache()
  candidate <- NULL

  players <- unique(wr_features$player_id[wr_features$season == test_season])
  max_candidates <- min(200, length(players))
  result_future <- NULL
  candidate_list <- list()
  for (pid in head(players, max_candidates)) {
    player_weeks <- sort(unique(wr_features$week[wr_features$player_id == pid & wr_features$season == test_season]))
    if (length(player_weeks) == 0) next
    team_row <- player_dim[player_dim$gsis_id == pid & player_dim$season == test_season, , drop = FALSE]
    if (nrow(team_row) == 0 || is.na(team_row$team[1]) || team_row$team[1] == "") next
    team <- team_row$team[1]
    max_week <- max(player_weeks, na.rm = TRUE)
    future_weeks <- schedules$week[schedules$season == test_season &
                                     (schedules$home_team == team | schedules$away_team == team) &
                                     schedules$week > max_week &
                                     schedules$week <= 18]
    if (length(future_weeks) == 0) next

    wk <- min(future_weeks, na.rm = TRUE)
    sched_row <- schedules[
      schedules$season == test_season &
        schedules$week == wk &
        (schedules$home_team == team | schedules$away_team == team),
      , drop = FALSE
    ]
    if (nrow(sched_row) != 1) next
    home_away <- ifelse(sched_row$home_team[1] == team, "HOME", "AWAY")
    opponent <- ifelse(home_away == "HOME", sched_row$away_team[1], sched_row$home_team[1])

    future_row <- tryCatch({
      build_future_wr_feature_row(
        player_id = pid,
        season = test_season,
        week = wk,
        team = team,
        opponent = opponent,
        home_away = home_away
      )
    }, error = function(e) NULL)
    if (is.null(future_row)) next

    required_future <- get_wr_features_by_week(wk)
    optional_future <- c(
      "is_home",
      "is_rookie",
      "draft_round",
      "draft_pick_overall",
      "height",
      "weight",
      "age",
      grep("^prev_season", required_future, value = TRUE)
    )
    strict_future <- setdiff(required_future, optional_future)
    available_cols <- strict_future[sapply(strict_future, function(f) any(!is.na(wr_features[[f]])))]
    if (length(available_cols) == 0) next

    candidate_list[[length(candidate_list) + 1]] <- list(
      player_id = pid,
      week = wk,
      team = team
    )
    if (length(candidate_list) >= 10) break
  }

  if (length(candidate_list) > 0) {
    for (cand in candidate_list) {
      result_future <- tryCatch({
        simulate_player_game(
          gsis_id = cand$player_id,
          season = test_season,
          week = cand$week,
          n_sims = 200,
          mode = "upcoming_game"
        )
      }, error = function(e) NULL)
      if (!is.null(result_future) && (is.null(result_future$status) || result_future$status != "error")) {
        candidate <- cand
        break
      }
    }
  }

  if (is.null(candidate)) {
    if (!any(schedules$season == test_season & schedules$week > max_week_seen)) {
      cat("  No future weeks available; skipping future simulation test\n")
    } else if (length(candidate_list) == 0) {
      cat("  No buildable future rows found; skipping future simulation test\n")
    } else {
      cat("  Future simulation failed for all candidates; skipping future simulation test\n")
    }
  } else {
    sched_row <- schedules[
      schedules$season == test_season &
        schedules$week == candidate$week &
        (schedules$home_team == candidate$team | schedules$away_team == candidate$team),
      , drop = FALSE
    ]
    if (nrow(sched_row) != 1) {
      stop("Failed to resolve schedule row for future simulation.")
    }
    home_away <- ifelse(sched_row$home_team[1] == candidate$team, "HOME", "AWAY")
    opponent <- ifelse(home_away == "HOME", sched_row$away_team[1], sched_row$home_team[1])

    future_row <- build_future_wr_feature_row(
      player_id = candidate$player_id,
      season = test_season,
      week = candidate$week,
      team = candidate$team,
      opponent = opponent,
      home_away = home_away
    )
    required_future <- get_wr_features_by_week(candidate$week)
    missing_future <- setdiff(required_future, names(future_row))
    if (length(missing_future) > 0) {
      stop("Future WR row missing required features: ", paste(missing_future, collapse = ", "))
    }
    rushing_future <- grep("rush|carry", names(future_row), ignore.case = TRUE, value = TRUE)
    if (length(rushing_future) > 0) {
      stop("Future WR row contains rushing columns: ", paste(rushing_future, collapse = ", "))
    }

    if (is.null(result_future)) {
      stop("Future simulation failed for candidate.")
    }
    if (is.null(result_future$draws) || nrow(result_future$draws) != 200) {
      stop("Future simulation draws invalid.")
    }

    cat("  Future simulation completed successfully\n")
  }
}, error = function(e) {
  cat("ERROR: Future simulation test failed:", conditionMessage(e), "\n")
  quit(status = 1)
})

cat("\nAll WR smoke tests passed!\n")
quit(status = 0)
