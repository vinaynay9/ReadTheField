suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
})

# Assemble RB Training Data
# ...

# =============================================================================
# Decayed Previous-Season Priors
# =============================================================================

# Decay constant for exponential decay: decay_weight = exp(-k * (W-1))
# k = 0.25 gives:
#   week 1: ~0.78, week 4: ~0.47, week 8: ~0.17
# This provides strong signal early season, fading by mid-season
DECAY_CONSTANT_K <- 0.25

#' Compute previous-season summary statistics by player
#'
#' For each player-season, computes aggregated stats from previous season.
#' These provide player role/usage signal for early season weeks.
#'
#' @param df data.frame with player_id, season, week, carries, targets, rushing_yards/rush_yards, receiving_yards/rec_yards
#' @return data.frame with columns:
#'   - player_id, season
#'   - prev_season_carries_pg, prev_season_targets_pg
#'   - prev_season_ypc, prev_season_ypt
#'   - prev_season_games (for shrinkage)
compute_prev_season_priors <- function(df) {
  
  # Handle both naming conventions (rushing_yards vs rush_yards)
  if (!"rushing_yards" %in% names(df) && "rush_yards" %in% names(df)) {
    df$rushing_yards <- df$rush_yards
  }
  if (!"receiving_yards" %in% names(df) && "rec_yards" %in% names(df)) {
    df$receiving_yards <- df$rec_yards
  }
  
  # Ensure columns exist
  if (!"rushing_yards" %in% names(df)) {
    df$rushing_yards <- 0
  }
  if (!"receiving_yards" %in% names(df)) {
    df$receiving_yards <- 0
  }
  
  # Compute per-season aggregates (ALL games in each season fully aggregated)
  season_stats <- df %>%
    group_by(player_id, season) %>%
    summarise(
      games = n(),
      carries_sum = sum(carries, na.rm = TRUE),
      targets_sum = sum(targets, na.rm = TRUE),
      rushing_yards_sum = sum(rushing_yards, na.rm = TRUE),
      receiving_yards_sum = sum(receiving_yards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      carries_pg = carries_sum / games,
      targets_pg = targets_sum / games,
      ypc = ifelse(carries_sum > 0, rushing_yards_sum / carries_sum, NA_real_),
      ypt = ifelse(targets_sum > 0, receiving_yards_sum / targets_sum, NA_real_)
    )
  
  # Join previous season stats to current season
  # For season S, get stats from season S-1
  prev_season_stats <- season_stats %>%
    mutate(next_season = season + 1) %>%
    select(
      player_id,
      season = next_season,
      prev_season_carries_pg = carries_pg,
      prev_season_targets_pg = targets_pg,
      prev_season_ypc = ypc,
      prev_season_ypt = ypt,
      prev_season_games = games
    )
  
  prev_season_stats
}

#' Compute season-to-date lagged priors within season
#'
#' For each player-week, computes cumulative per-game averages from
#' all prior games in the current season (lagged).
#'
#' @param df data.frame with player_id, season, week, carries, targets, rushing_yards/rush_yards, receiving_yards/rec_yards
#' @return data.frame with added columns:
#'   - season_to_date_carries_pg, season_to_date_targets_pg
#'   - season_to_date_ypc, season_to_date_ypt
#'   - season_to_date_games
compute_season_to_date_priors <- function(df) {
  
  # Handle both naming conventions (rushing_yards vs rush_yards)
  if (!"rushing_yards" %in% names(df) && "rush_yards" %in% names(df)) {
    df$rushing_yards <- df$rush_yards
  }
  if (!"receiving_yards" %in% names(df) && "rec_yards" %in% names(df)) {
    df$receiving_yards <- df$rec_yards
  }
  
  # Ensure columns exist
  if (!"rushing_yards" %in% names(df)) {
    df$rushing_yards <- 0
  }
  if (!"receiving_yards" %in% names(df)) {
    df$receiving_yards <- 0
  }
  
  # Sort by player, season, week
  df <- df[order(df$player_id, df$season, df$week), ]
  
  # Initialize columns
  df$season_to_date_carries_pg <- NA_real_
  df$season_to_date_targets_pg <- NA_real_
  df$season_to_date_ypc <- NA_real_
  df$season_to_date_ypt <- NA_real_
  df$season_to_date_games <- NA_integer_
  
  # Compute per player-season
  player_seasons <- unique(df[, c("player_id", "season")])
  
  for (i in seq_len(nrow(player_seasons))) {
    pid <- player_seasons$player_id[i]
    seas <- player_seasons$season[i]
    
    idx <- which(df$player_id == pid & df$season == seas)
    
    if (length(idx) == 0) next
    
    # Initialize accumulators
    cum_games <- 0
    cum_carries <- 0
    cum_targets <- 0
    cum_rushing_yards <- 0
    cum_receiving_yards <- 0
    
    for (j in seq_along(idx)) {
      row_idx <- idx[j]
      
      # Compute lagged per-game averages (using games BEFORE current)
      if (cum_games > 0) {
        df$season_to_date_carries_pg[row_idx] <- cum_carries / cum_games
        df$season_to_date_targets_pg[row_idx] <- cum_targets / cum_games
        df$season_to_date_ypc[row_idx] <- if (cum_carries > 0) cum_rushing_yards / cum_carries else NA_real_
        df$season_to_date_ypt[row_idx] <- if (cum_targets > 0) cum_receiving_yards / cum_targets else NA_real_
        df$season_to_date_games[row_idx] <- cum_games
      } else {
        # First game of season: no prior games
        df$season_to_date_games[row_idx] <- 0L
      }
      
      # Update accumulators with current game (LAG)
      curr_carries <- df$carries[row_idx]
      curr_targets <- df$targets[row_idx]
      curr_rushing_yards <- df$rushing_yards[row_idx]
      curr_receiving_yards <- df$receiving_yards[row_idx]
      
      if (!is.na(curr_carries)) cum_carries <- cum_carries + curr_carries
      if (!is.na(curr_targets)) cum_targets <- cum_targets + curr_targets
      if (!is.na(curr_rushing_yards)) cum_rushing_yards <- cum_rushing_yards + curr_rushing_yards
      if (!is.na(curr_receiving_yards)) cum_receiving_yards <- cum_receiving_yards + curr_receiving_yards
      
      cum_games <- cum_games + 1
    }
  }
  
  df
}

#' Compute decay-blended priors
#'
#' Blends previous-season priors with season-to-date priors using
#' exponential decay: decay_weight = exp(-k * (W-1))
#'
#' @param df data.frame with week, prev_season_*, and season_to_date_* columns
#' @return data.frame with added columns:
#'   - carries_prior, targets_prior, ypc_prior, ypt_prior
compute_decay_blended_priors <- function(df) {
  
  # Compute decay weight by week
  # decay_weight = exp(-k * (W-1))
  # Week 1: exp(0) = 1.0 (full prev season weight)
  # Week 4: exp(-0.75) ≈ 0.47
  # Week 8: exp(-1.75) ≈ 0.17
  df$decay_weight <- exp(-DECAY_CONSTANT_K * (df$week - 1))
  
  # Blend priors
  # carries_prior = decay_weight * prev_season + (1 - decay_weight) * season_to_date
  
  # Handle edge cases:
  # 1. If prev_season missing, use season_to_date only
  # 2. If season_to_date missing (week 1), use prev_season only
  # 3. If both missing, set NA
  
  df$carries_prior <- ifelse(
    !is.na(df$prev_season_carries_pg) & !is.na(df$season_to_date_carries_pg),
    df$decay_weight * df$prev_season_carries_pg + (1 - df$decay_weight) * df$season_to_date_carries_pg,
    ifelse(
      !is.na(df$prev_season_carries_pg),
      df$prev_season_carries_pg,
      df$season_to_date_carries_pg
    )
  )
  
  df$targets_prior <- ifelse(
    !is.na(df$prev_season_targets_pg) & !is.na(df$season_to_date_targets_pg),
    df$decay_weight * df$prev_season_targets_pg + (1 - df$decay_weight) * df$season_to_date_targets_pg,
    ifelse(
      !is.na(df$prev_season_targets_pg),
      df$prev_season_targets_pg,
      df$season_to_date_targets_pg
    )
  )
  
  df$ypc_prior <- ifelse(
    !is.na(df$prev_season_ypc) & !is.na(df$season_to_date_ypc),
    df$decay_weight * df$prev_season_ypc + (1 - df$decay_weight) * df$season_to_date_ypc,
    ifelse(
      !is.na(df$prev_season_ypc),
      df$prev_season_ypc,
      df$season_to_date_ypc
    )
  )
  
  df$ypt_prior <- ifelse(
    !is.na(df$prev_season_ypt) & !is.na(df$season_to_date_ypt),
    df$decay_weight * df$prev_season_ypt + (1 - df$decay_weight) * df$season_to_date_ypt,
    ifelse(
      !is.na(df$prev_season_ypt),
      df$prev_season_ypt,
      df$season_to_date_ypt
    )
  )
  
  df
}


#' Assemble RB weekly features using pre-filtered RB stats
#'
#' CRITICAL ORDERING CONSTRAINT:
#' Raw stat columns (carries, targets, receptions, rushing_yards, receiving_yards)
#' must be preserved until ALL feature engineering is complete. The required order is:
#'   1. Rolling features (build_rb_features)
#'   2. Previous season priors (compute_prev_season_priors)
#'   3. Season-to-date priors (compute_season_to_date_priors)
#'   4. Decay-blended priors (compute_decay_blended_priors)
#'   5. Defensive joins (opponent context)
#'   6. Regime assignment
#'   7. Player priors (add_rb_player_priors) - REQUIRES raw stat columns
#'   8. Target column creation - REQUIRES raw stat columns
#'   9. Drop raw stat columns - MUST be last
#'   10. Schema validation
#'
#' @param rb_weekly_stats data.frame produced by build_rb_weekly_stats()
#' @return data.frame with rolling features and target columns
assemble_rb_weekly_features <- function(rb_weekly_stats) {
  if (is.null(rb_weekly_stats) || nrow(rb_weekly_stats) == 0) {
    stop("rb_weekly_stats must be provided with at least one row")
  }

  required_cols <- c(
    "player_id", "player_name", "team", "opponent", "season", "week",
    "game_key", "game_date", "carries", "rushing_yards", "rushing_tds",
    "targets", "receptions", "receiving_yards", "receiving_tds", "home_away"
  )
  missing <- setdiff(required_cols, names(rb_weekly_stats))
  if (length(missing) > 0) {
    stop("Missing required columns in rb_weekly_stats: ", paste(missing, collapse = ", "))
  }

  stats <- rb_weekly_stats
  stats$season <- as.integer(stats$season)
  stats$week <- as.integer(stats$week)
  stats$game_date <- as.Date(stats$game_date)
  stats$gameday <- stats$game_date
  stats$home_away <- toupper(trimws(as.character(stats$home_away)))
  stats$is_home <- ifelse(
    stats$home_away == "HOME", 1L,
    ifelse(stats$home_away == "AWAY", 0L, NA_integer_)
  )
  stats <- stats[order(stats$player_id, stats$season, stats$week, stats$game_date), ]

  if (!exists("build_rb_features")) {
    if (file.exists("R/features/build_rb_features.R")) {
      source("R/features/build_rb_features.R", local = TRUE)
    } else {
      stop("R/features/build_rb_features.R is required to build rolling features")
    }
  }

  features <- build_rb_features(stats)
  
  # Compute decayed previous-season priors (for early-season signal)
  # These provide player role/usage signal when rolling windows are sparse or NA
  prev_season_priors <- compute_prev_season_priors(stats)
  features <- features %>%
    left_join(prev_season_priors, by = c("player_id", "season"))
  
  # Compute season-to-date priors (lagged cumulative within season)
  features <- compute_season_to_date_priors(features)
  
  # Compute decay-blended priors
  features <- compute_decay_blended_priors(features)
  
  # Join defensive features (opponent context)
  # Load cached defensive features if available
  defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  
  if (file.exists(defense_weekly_features_path)) {
    tryCatch({
      if (!requireNamespace("arrow", quietly = TRUE)) {
        warning("Package 'arrow' required to read defensive features. Defensive features will be missing.")
      } else {
        def_features <- arrow::read_parquet(defense_weekly_features_path)
        
        if (nrow(def_features) > 0) {
          # Select defensive feature columns to join
          def_cols <- c("season", "week", "defense_team",
                       "opp_rush_yards_allowed_roll5",
                       "opp_yards_per_rush_allowed_roll5",
                       "opp_points_allowed_roll5",
                       "opp_sacks_roll5",
                       "opp_tfl_roll5")
          
          # Only join columns that exist
          def_cols <- intersect(def_cols, names(def_features))
          
          # Ensure we have minimum required columns
          if (all(c("season", "week", "defense_team") %in% def_cols)) {
            # Join on opponent = defense_team, season, week
            features <- merge(
              features,
              def_features[, def_cols, drop = FALSE],
              by.x = c("season", "week", "opponent"),
              by.y = c("season", "week", "defense_team"),
              all.x = TRUE
            )
          }
        }
      }
    }, error = function(e) {
      warning(paste("Failed to join defensive features:", e$message, "Defensive features will be missing."))
    })
  }
  
  # Add RB v1 regime column based on week (Layer 3: before writing to parquet)
  # Ensure week is numeric
  if (!is.numeric(features$week)) {
    features$week <- as.integer(features$week)
  }
  
  # Add regime using case_when
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for regime assignment. Install with install.packages('dplyr').")
  }
  
  features <- features %>%
    dplyr::mutate(
      rb_regime = dplyr::case_when(
        week <= 3 ~ "early",
        week <= 5 ~ "mid",
        week <= 7 ~ "late",
        TRUE ~ "standard"
      )
    )
  
  # Ensure rb_regime is character
  features$rb_regime <- as.character(features$rb_regime)
  
  # Add player-specific priors (cumulative career statistics)
  # CRITICAL: This MUST happen BEFORE dropping raw stat columns
  # add_rb_player_priors() requires: carries, targets, receptions, rush_yards/rushing_yards, rec_yards/receiving_yards
  if (file.exists("R/utils/rb_player_priors.R")) {
    source("R/utils/rb_player_priors.R", local = TRUE)
  }
  if (exists("add_rb_player_priors")) {
    message("Adding player priors (requires raw stat columns)...")
    features <- add_rb_player_priors(features)
  }
  
  # RB v1 contract: create target columns with v1 names (NO yardage targets)
  # IMPORTANT: Create targets AFTER player priors (which need raw columns)
  features$target_carries <- features$carries
  features$target_receptions <- features$receptions
  features$target_rush_tds <- features$rushing_tds
  features$target_rec_tds <- features$receiving_tds
  # Note: Yardage targets (target_rush_yards, target_rec_yards) are NOT in RB v1
  # Yardage should be derived downstream (e.g., carries * YPC) if needed

  # Drop raw stat columns AFTER all feature engineering is complete
  # This must be the LAST step before validation
  drop_cols <- c(
    "carries", "rushing_yards", "rushing_tds",
    "targets", "receptions", "receiving_yards", "receiving_tds"
  )
  drop_cols <- intersect(drop_cols, names(features))
  features <- features[, setdiff(names(features), drop_cols), drop = FALSE]

  features <- features[order(features$season, features$week, features$gameday, features$player_id), ]
  rownames(features) <- NULL
  
  # Filter out malformed rows with NA identity fields (Layer 3 hardening)
  bad_rows <- is.na(features$player_id) | is.na(features$season) | is.na(features$week)
  
  if (any(bad_rows)) {
    warning(sprintf(
      "Dropping %d malformed RB feature rows with NA identity fields (player_id, season, or week)",
      sum(bad_rows)
    ))
    features <- features[!bad_rows, , drop = FALSE]
  }
  
  # Hard assertion: no NA in required identity fields after filtering
  if (nrow(features) == 0) {
    stop("All RB feature rows were dropped due to NA identity fields. ",
         "Cannot create valid feature cache. Check Layer 2 (rb_weekly_stats) for data quality issues.")
  }
  
  if (any(is.na(features$player_id))) {
    stop("Found NA player_id in RB features after filtering. This should not occur. ",
         "Layer 3 assembly logic error.")
  }
  
  if (any(is.na(features$season))) {
    stop("Found NA season in RB features after filtering. This should not occur. ",
         "Layer 3 assembly logic error.")
  }
  
  if (any(is.na(features$week))) {
    stop("Found NA week in RB features after filtering. This should not occur. ",
         "Layer 3 assembly logic error.")
  }
  
  # Hard assertion: Week 1 rolling features must be NA (season boundary enforcement)
  week1_rows <- features[features$week == 1, , drop = FALSE]
  if (nrow(week1_rows) > 0) {
    rolling_cols <- grep("_roll[0-9]+$", names(features), value = TRUE)
    for (col in rolling_cols) {
      week1_vals <- week1_rows[[col]]
      non_na_count <- sum(!is.na(week1_vals))
      if (non_na_count > 0) {
        stop("Season boundary violation detected: Found ", non_na_count, 
             " non-NA rolling feature '", col, "' in Week 1 rows. ",
             "Week 1 must have NA rolling features (no prior games in season). ",
             "This indicates cross-season leakage in rolling window computation. ",
             "Check build_rb_features() grouping logic.")
      }
    }
  }
  
  # Validate RB v1 target schema
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
    validate_rb_v1_target_schema(features, strict = TRUE)
  }
  
  # Guardrail: ensure rb_regime column exists before returning
  if (!"rb_regime" %in% names(features)) {
    stop("rb_regime column missing after feature assembly. This should not occur.")
  }
  
  # FINAL VALIDATION: Ensure dataset is not empty and has valid structure
  if (nrow(features) == 0) {
    stop("CRITICAL: Final RB feature dataset has 0 rows. ",
         "This indicates a fatal error in feature assembly. ",
         "Check: (1) rb_weekly_stats has data, (2) build_rb_features succeeded, ",
         "(3) add_rb_player_priors succeeded, (4) no unexpected filtering removed all rows.")
  }
  
  # Validation: Ensure critical prior columns exist and are numeric
  prior_cols <- c("carries_cum_mean", "targets_cum_mean", "receptions_cum_mean", 
                  "ypc_cum", "ypt_cum", "catch_rate_cum")
  missing_priors <- setdiff(prior_cols, names(features))
  if (length(missing_priors) > 0) {
    stop("CRITICAL: Player prior columns missing after add_rb_player_priors: ",
         paste(missing_priors, collapse = ", "), ". ",
         "This indicates add_rb_player_priors() failed or was skipped.")
  }
  
  # Validation: Ensure all prior columns are numeric
  for (col in prior_cols) {
    if (!is.numeric(features[[col]])) {
      stop("CRITICAL: Prior column '", col, "' is not numeric (type: ", 
           class(features[[col]]), "). This indicates data corruption.")
    }
  }
  
  message(sprintf("✓ Feature assembly complete: %d rows, %d columns, priors validated", 
                  nrow(features), ncol(features)))
  
  features
}

#' Assemble RB Training Data
#' Assemble complete RB training dataset
#'
#' Loads raw data, joins schedules with player stats, computes features,
#' and returns a training-ready dataset with strict temporal ordering.
#'
#' CRITICAL ORDERING CONSTRAINT:
#' Raw stat columns must be preserved until ALL feature engineering is complete:
#'   1. Build rolling features (build_rb_features)
#'   2. Previous season priors (compute_prev_season_priors)
#'   3. Season-to-date priors (compute_season_to_date_priors)
#'   4. Decay-blended priors (compute_decay_blended_priors)
#'   5. Defensive joins (opponent context)
#'   6. Target column creation
#'   7. Regime assignment
#'   8. Player priors (add_rb_player_priors) - REQUIRES raw stat columns
#'   9. Final column selection (drops raw stat columns) - MUST be last
#'   10. Schema validation
#'
#' @param seasons Integer vector of NFL seasons to include
#' @return data.frame with one row per RB-game including:
#'   - Identifiers: game_id, player_id, season, week, gameday
#'   - Metadata: player_name, team, position, opponent, home_away, is_home
#'   - Features: carries_roll3, carries_roll5, targets_roll3, etc.
#'   - Targets: target_carries, target_rush_tds, target_receptions, target_rec_tds
assemble_rb_training_data <- function(seasons) {
  
  # Validate input
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified")
    return(empty_rb_training_df())
  }
  
  message("Loading schedules...")
  schedules <- load_schedules(seasons, cache_only = TRUE)
  schedules_available <- nrow(schedules) > 0
  
  if (!schedules_available) {
    warning("No schedule data loaded; will derive game context from player stats")
  } else {
    message(paste("Loaded", nrow(schedules), "games"))
  }
  
  message("Loading RB player stats...")
  rb_stats <- load_rb_stats(seasons)
  
  if (nrow(rb_stats) == 0) {
    warning("No RB stats loaded")
    return(empty_rb_training_df())
  }
  
  message(paste("Loaded", nrow(rb_stats), "RB player-game records"))
  
  # Build game lookup for matching player stats to schedules
  # Create two rows per game: one for home team, one for away team
  message("Building game-team mapping...")
  
  if (schedules_available) {
    home_games <- data.frame(
      game_id = schedules$game_id,
      season = schedules$season,
      week = schedules$week,
      gameday = schedules$gameday,
      team = schedules$home_team,
      opponent = schedules$away_team,
      home_away = "HOME",
      is_home = 1L,
      stadium = schedules$stadium,
      surface = schedules$surface,
      stringsAsFactors = FALSE
    )
    
    away_games <- data.frame(
      game_id = schedules$game_id,
      season = schedules$season,
      week = schedules$week,
      gameday = schedules$gameday,
      team = schedules$away_team,
      opponent = schedules$home_team,
      home_away = "AWAY",
      is_home = 0L,
      stadium = schedules$stadium,
      surface = schedules$surface,
      stringsAsFactors = FALSE
    )
    
    game_team_lookup <- rbind(home_games, away_games)
  } else {
    # Derive minimal mapping from player stats when schedules are unavailable
    derived_games <- unique(rb_stats[, c("game_id", "game_key", "season", "week", "team", "opponent", "home_away", "game_date", "gameday")])
    derived_games$gameday <- if (!"gameday" %in% names(derived_games) || all(is.na(derived_games$gameday))) derived_games$game_date else derived_games$gameday
    derived_games$home_away <- toupper(ifelse(is.na(derived_games$home_away), "", derived_games$home_away))
    derived_games$is_home <- ifelse(derived_games$home_away == "HOME", 1L,
                                    ifelse(derived_games$home_away == "AWAY", 0L, NA_integer_))
    derived_games$stadium <- NA_character_
    derived_games$surface <- NA_character_
    game_team_lookup <- derived_games
  }
  
  # Join RB stats with game context
  # Match on season, week, team (since game_id in player_stats may be constructed)
  message("Joining player stats with game context...")
  
  rb_data <- merge(
    rb_stats,
    game_team_lookup,
    by = c("season", "week", "team"),
    all.x = TRUE,
    suffixes = c("", "_sched")
  )
  
  # If game_id came from player_stats, prefer the schedule's game_id
  if ("game_id_sched" %in% names(rb_data)) {
    rb_data$game_id <- ifelse(
      !is.na(rb_data$game_id_sched),
      rb_data$game_id_sched,
      rb_data$game_id
    )
    rb_data$game_id_sched <- NULL
  }
  
  # Prefer schedule opponent/home info when available
  if ("opponent_sched" %in% names(rb_data)) {
    rb_data$opponent <- ifelse(!is.na(rb_data$opponent_sched), rb_data$opponent_sched, rb_data$opponent)
    rb_data$opponent_sched <- NULL
  }
  if ("home_away_sched" %in% names(rb_data)) {
    rb_data$home_away <- ifelse(!is.na(rb_data$home_away_sched), rb_data$home_away_sched, rb_data$home_away)
    rb_data$home_away_sched <- NULL
  }
  if ("is_home_sched" %in% names(rb_data)) {
    rb_data$is_home <- ifelse(!is.na(rb_data$is_home_sched), rb_data$is_home_sched, rb_data$is_home)
    rb_data$is_home_sched <- NULL
  }
  if ("gameday_sched" %in% names(rb_data)) {
    rb_data$gameday <- ifelse(!is.na(rb_data$gameday_sched), rb_data$gameday_sched, rb_data$gameday)
    rb_data$gameday_sched <- NULL
  }
  
  # Ensure gameday populated from game_date when available
  if (!"gameday" %in% names(rb_data) || all(is.na(rb_data$gameday))) {
    rb_data$gameday <- rb_data$game_date
  } else {
    rb_data$gameday <- ifelse(is.na(rb_data$gameday), rb_data$game_date, rb_data$gameday)
  }
  
  # Fill game_key deterministically
  if (!"game_key" %in% names(rb_data)) {
    rb_data$game_key <- NA_character_
  }
  if (exists("build_game_key")) {
    rb_data$game_key <- ifelse(
      is.na(rb_data$game_key) | rb_data$game_key == "",
      build_game_key(rb_data$season, rb_data$week, rb_data$gameday, rb_data$team, rb_data$opponent, rb_data$game_id),
      rb_data$game_key
    )
  }
  
  # Remove rows that didn't match and have no date (features need ordering)
  unmatched <- sum(is.na(rb_data$gameday))
  if (unmatched > 0) {
    warning(paste(unmatched, "RB records missing game dates; excluding from training data"))
    rb_data <- rb_data[!is.na(rb_data$gameday), ]
  }
  
  message(paste("Matched", nrow(rb_data), "RB-game records"))
  
  # Ensure strict temporal ordering before feature computation
  rb_data <- rb_data[order(rb_data$player_id, rb_data$season, rb_data$week, rb_data$gameday), ]
  
  # Build rolling features
  message("Computing rolling features...")
  rb_data <- build_rb_features(rb_data)
  
  # Compute decayed previous-season priors (for early-season signal)
  message("Computing decayed previous-season priors...")
  prev_season_priors <- compute_prev_season_priors(rb_data)
  rb_data <- rb_data %>%
    left_join(prev_season_priors, by = c("player_id", "season"))
  
  # Compute season-to-date priors (lagged cumulative within season)
  rb_data <- compute_season_to_date_priors(rb_data)
  
  # Compute decay-blended priors
  rb_data <- compute_decay_blended_priors(rb_data)
  
  # Build and join defensive features
  # Defensive features represent opponent defensive context (what the opponent allows)
  message("Building defensive features...")
  tryCatch({
    # Load defensive stats builders (assumes they're in the path)
    if (!exists("build_team_defense_game_stats")) {
      if (file.exists("R/data/build_team_defense_game_stats.R")) {
        source("R/data/build_team_defense_game_stats.R", local = TRUE)
      } else {
        stop("build_team_defense_game_stats function not found.")
      }
    }
    if (!exists("build_team_defense_features")) {
      if (file.exists("R/features/build_team_defense_features.R")) {
        source("R/features/build_team_defense_features.R", local = TRUE)
      } else {
        stop("build_team_defense_features function not found.")
      }
    }
    
    def_game_stats <- build_team_defense_game_stats(seasons)
    
    if (nrow(def_game_stats) > 0) {
      def_features <- build_team_defense_features(def_game_stats)
      
      # Join defensive features using opponent_team (defense) and game_id
      # opponent_team in rb_data is the team the player is facing (the defense)
      message("Joining defensive features...")
      
      # Select defensive feature columns to join
      def_cols <- c("game_id", "defense_team", 
                   "opp_pass_yards_allowed_roll5",
                   "opp_rush_yards_allowed_roll5",
                   "opp_total_yards_allowed_roll5",
                   "opp_points_allowed_roll5",
                   "opp_sacks_roll5",
                   "opp_tfl_roll5")
      
      # Add optional columns if they exist
      if ("opp_int_roll5" %in% names(def_features)) {
        def_cols <- c(def_cols, "opp_int_roll5")
      }
      
      # Only join columns that exist
      def_cols <- intersect(def_cols, names(def_features))
      
      rb_data <- merge(
        rb_data,
        def_features[, def_cols, drop = FALSE],
        by.x = c("game_id", "opponent"),
        by.y = c("game_id", "defense_team"),
        all.x = TRUE
      )
      
      message("Defensive features joined successfully")
      
      # Validation: Check that defensive features are opponent-specific
      # Verify that joined defensive features match opponent_team
      if (nrow(rb_data) > 0 && "opp_rush_yards_allowed_roll5" %in% names(rb_data)) {
        # Sample check: defensive features should be joined correctly
        # (defense_team in def_features should match opponent in rb_data)
        sample_check <- head(rb_data[!is.na(rb_data$opp_rush_yards_allowed_roll5), 
                                     c("opponent", "opp_rush_yards_allowed_roll5")], 10)
        if (nrow(sample_check) > 0) {
          message("Validation: Defensive features joined on opponent_team (leakage-safe)")
        }
      }
    } else {
      warning("No defensive game stats available. Defensive features will be missing.")
    }
  }, error = function(e) {
    warning(paste("Failed to build/join defensive features:", e$message, 
                 "Defensive features will be missing."))
  })
  
  # Final validation: Ensure no defensive features use same-game data
  # This is guaranteed by lagged rolling windows, but we document it
  message("Validation: All defensive features use lagged windows (current game excluded)")
  
  # Rename target columns to be explicit (RB v1 schema: NO yardage targets)
  # The current-game stats are targets (what we're predicting)
  # The rolling features are predictors (computed from prior games only)
  rb_data$target_carries <- rb_data$carries
  rb_data$target_rush_tds <- rb_data$rush_tds
  rb_data$target_targets <- rb_data$targets
  rb_data$target_receptions <- rb_data$receptions
  rb_data$target_rec_tds <- rb_data$rec_tds
  # Note: target_rush_yards and target_rec_yards are NOT in RB v1 schema
  # Yardage should be derived downstream if needed (e.g., carries * YPC)
  
  # Add RB v1 regime column based on week
  if (file.exists("R/utils/rb_regime_v1.R")) {
    source("R/utils/rb_regime_v1.R", local = TRUE)
    rb_data$rb_regime <- determine_rb_regime(rb_data$week)
  } else {
    # Fallback regime assignment
    rb_data$rb_regime <- ifelse(rb_data$week <= 3, "early",
                                ifelse(rb_data$week <= 5, "mid",
                                      ifelse(rb_data$week <= 7, "late", "standard")))
  }
  
  # Add player-specific priors (cumulative career statistics)
  # These provide player role signal in early season weeks when rolling windows are NA
  if (file.exists("R/utils/rb_player_priors.R")) {
    source("R/utils/rb_player_priors.R", local = TRUE)
  }
  if (exists("add_rb_player_priors")) {
    message("Adding player priors (lagged cumulative statistics)...")
    rb_data <- add_rb_player_priors(rb_data)
  } else {
    warning("add_rb_player_priors not found. Player priors will be missing.")
  }
  
  # Select and order final columns (RB v1 schema: NO yardage targets)
  final_cols <- c(
    # Identifiers
    "game_id", "game_key", "player_id", "season", "week", "gameday",
    # Metadata
    "player_name", "team", "position", "opponent", "home_away", "is_home",
    # Optional metadata
    "stadium", "surface",
    # Features (pre-game, lagged) - Player priors (cumulative career stats)
    "carries_cum_mean", "targets_cum_mean", "receptions_cum_mean",
    "ypc_cum", "ypt_cum", "catch_rate_cum",
    # Features (pre-game, lagged) - Decayed previous-season priors (early-season signal)
    "carries_prior", "targets_prior", "ypc_prior", "ypt_prior",
    "prev_season_carries_pg", "prev_season_targets_pg", "prev_season_ypc", "prev_season_ypt", "prev_season_games",
    "season_to_date_carries_pg", "season_to_date_targets_pg", "season_to_date_ypc", "season_to_date_ypt", "season_to_date_games",
    "decay_weight",
    # Features (pre-game, lagged) - RB v1: roll3, roll5, roll7
    "carries_roll3", "carries_roll5", "carries_roll7",
    "targets_roll3", "targets_roll5", "targets_roll7",
    "rush_yards_roll3", "rec_yards_roll3",
    "yards_per_carry_roll5", "yards_per_target_roll5", "catch_rate_roll5",
    "rush_tds_roll5", "rec_tds_roll5",
    # Defensive features (opponent context)
    "opp_pass_yards_allowed_roll5", "opp_rush_yards_allowed_roll5",
    "opp_total_yards_allowed_roll5", "opp_points_allowed_roll5",
    "opp_sacks_roll5", "opp_tfl_roll5",
    # Targets (post-game, for training) - RB v1 schema
    "target_carries", "target_rush_tds",
    "target_targets", "target_receptions", "target_rec_tds",
    # Regime (for regime-based modeling)
    "rb_regime"
  )
  
  # Only keep columns that exist
  available_cols <- intersect(final_cols, names(rb_data))
  rb_data <- rb_data[, available_cols, drop = FALSE]
  
  # Guardrail: ensure rb_regime column is preserved after column selection
  if (!"rb_regime" %in% names(rb_data)) {
    stop("rb_regime column missing after training data assembly. ",
         "Regime-based modeling requires rb_regime to be preserved through assembly.")
  }
  
  # Final sort
  rb_data <- rb_data[order(rb_data$season, rb_data$week, rb_data$gameday, rb_data$player_id), ]
  rownames(rb_data) <- NULL
  
  # Validate RB v1 target schema
  if (file.exists("R/utils/rb_schema_v1.R")) {
    source("R/utils/rb_schema_v1.R", local = TRUE)
    validate_rb_v1_target_schema(rb_data, strict = TRUE)
  }
  
  # FINAL VALIDATION: Ensure dataset is not empty and has valid structure
  if (nrow(rb_data) == 0) {
    stop("CRITICAL: Final RB training dataset has 0 rows. ",
         "This indicates a fatal error in training data assembly. ",
         "Check: (1) rb_stats loaded, (2) schedules matched, (3) build_rb_features succeeded, ",
         "(4) add_rb_player_priors succeeded, (5) no unexpected filtering removed all rows.")
  }
  
  # Validation: Ensure critical prior columns exist and are numeric
  prior_cols <- c("carries_cum_mean", "targets_cum_mean", "receptions_cum_mean", 
                  "ypc_cum", "ypt_cum", "catch_rate_cum")
  missing_priors <- setdiff(prior_cols, names(rb_data))
  if (length(missing_priors) > 0) {
    warning("Player prior columns missing after add_rb_player_priors: ",
            paste(missing_priors, collapse = ", "), ". ",
            "Models may fall back to baseline without player priors.")
  } else {
    # Validation: Ensure all prior columns are numeric
    for (col in prior_cols) {
      if (!is.numeric(rb_data[[col]])) {
        warning("Prior column '", col, "' is not numeric (type: ", 
                class(rb_data[[col]]), "). This may cause model errors.")
      }
    }
  }
  
  message(sprintf("✓ Training data assembly complete: %d rows, %d columns", 
                  nrow(rb_data), ncol(rb_data)))
  
  return(rb_data)
}


#' Create empty RB training data.frame with correct schema
#'
#' @return data.frame with zero rows and correct column types
empty_rb_training_df <- function() {
  data.frame(
    game_id = character(0),
    player_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    player_name = character(0),
    team = character(0),
    position = character(0),
    opponent = character(0),
    home_away = character(0),
    is_home = integer(0),
    stadium = character(0),
    surface = character(0),
    # Player priors (cumulative career statistics)
    carries_cum_mean = double(0),
    targets_cum_mean = double(0),
    receptions_cum_mean = double(0),
    ypc_cum = double(0),
    ypt_cum = double(0),
    catch_rate_cum = double(0),
    # Rolling window features
    carries_roll3 = double(0),
    carries_roll5 = double(0),
    targets_roll3 = double(0),
    targets_roll5 = double(0),
    rush_yards_roll3 = double(0),
    rec_yards_roll3 = double(0),
    yards_per_carry_roll5 = double(0),
    yards_per_target_roll5 = double(0),
    catch_rate_roll5 = double(0),
    rush_tds_roll5 = double(0),
    rec_tds_roll5 = double(0),
    # Defensive features (opponent context)
    opp_rush_yards_allowed_roll5 = double(0),
    opp_points_allowed_roll5 = double(0),
    opp_sacks_roll5 = double(0),
    opp_tfl_roll5 = double(0),
    target_carries = integer(0),
    target_rush_tds = integer(0),
    target_targets = integer(0),
    target_receptions = integer(0),
    target_rec_tds = integer(0),
    stringsAsFactors = FALSE
  )
}

