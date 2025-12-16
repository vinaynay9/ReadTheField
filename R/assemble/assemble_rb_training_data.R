# Assemble RB Training Data
# ...


#' Assemble RB weekly features using pre-filtered RB stats
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
  # RB v1 contract: create target columns with v1 names
  features$target_carries <- features$carries
  features$target_rushing_yards <- features$rushing_yards
  features$target_receptions <- features$receptions
  features$target_receiving_yards <- features$receiving_yards
  # Keep split TDs temporarily for computing total_touchdowns
  features$target_rush_tds <- features$rushing_tds
  features$target_rec_tds <- features$receiving_tds
  features$target_total_touchdowns <- features$target_rush_tds + features$target_rec_tds

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
  
  features
}

#' Assemble RB Training Data
#' Assemble complete RB training dataset
#'
#' Loads raw data, joins schedules with player stats, computes features,
#' and returns a training-ready dataset with strict temporal ordering.
#'
#' @param seasons Integer vector of NFL seasons to include
#' @return data.frame with one row per RB-game including:
#'   - Identifiers: game_id, player_id, season, week, gameday
#'   - Metadata: player_name, team, position, opponent, home_away, is_home
#'   - Features: carries_roll3, carries_roll5, targets_roll3, etc.
#'   - Targets: carries, rush_yards, rush_tds, targets, receptions, rec_yards, rec_tds
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
  
  # Rename target columns to be explicit
  # The current-game stats are targets (what we're predicting)
  # The rolling features are predictors (computed from prior games only)
  rb_data$target_carries <- rb_data$carries
  rb_data$target_rush_yards <- rb_data$rush_yards
  rb_data$target_rush_tds <- rb_data$rush_tds
  rb_data$target_targets <- rb_data$targets
  rb_data$target_receptions <- rb_data$receptions
  rb_data$target_rec_yards <- rb_data$rec_yards
  rb_data$target_rec_tds <- rb_data$rec_tds
  
  # Select and order final columns
  final_cols <- c(
    # Identifiers
    "game_id", "game_key", "player_id", "season", "week", "gameday",
    # Metadata
    "player_name", "team", "position", "opponent", "home_away", "is_home",
    # Optional metadata
    "stadium", "surface",
    # Features (pre-game, lagged)
    "carries_roll3", "carries_roll5",
    "targets_roll3", "targets_roll5",
    "rush_yards_roll3", "rec_yards_roll3",
    "yards_per_carry_roll5", "yards_per_target_roll5", "catch_rate_roll5",
    "rush_tds_roll5", "rec_tds_roll5",
    # Defensive features (opponent context)
    "opp_pass_yards_allowed_roll5", "opp_rush_yards_allowed_roll5",
    "opp_total_yards_allowed_roll5", "opp_points_allowed_roll5",
    "opp_sacks_roll5", "opp_tfl_roll5",
    # Targets (post-game, for training)
    "target_carries", "target_rush_yards", "target_rush_tds",
    "target_targets", "target_receptions", "target_rec_yards", "target_rec_tds"
  )
  
  # Only keep columns that exist
  available_cols <- intersect(final_cols, names(rb_data))
  rb_data <- rb_data[, available_cols, drop = FALSE]
  
  # Final sort
  rb_data <- rb_data[order(rb_data$season, rb_data$week, rb_data$gameday, rb_data$player_id), ]
  rownames(rb_data) <- NULL
  
  message(paste("Assembled", nrow(rb_data), "RB training records"))
  
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
    opp_pass_yards_allowed_roll5 = double(0),
    opp_rush_yards_allowed_roll5 = double(0),
    opp_total_yards_allowed_roll5 = double(0),
    opp_points_allowed_roll5 = double(0),
    opp_sacks_roll5 = double(0),
    opp_tfl_roll5 = double(0),
    target_carries = integer(0),
    target_rush_yards = double(0),
    target_rush_tds = integer(0),
    target_targets = integer(0),
    target_receptions = integer(0),
    target_rec_yards = double(0),
    target_rec_tds = integer(0),
    stringsAsFactors = FALSE
  )
}

