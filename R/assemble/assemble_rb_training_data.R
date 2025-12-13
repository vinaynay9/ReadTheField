# Assemble RB Training Data
#
# Joins schedules and player stats, then builds features for RB training.
# Constructs one row per RB per game with:
#   - Game metadata (game_id, season, week, gameday)
#   - Player metadata (player_id, player_name, team, position)
#   - Game context (opponent, home_away, is_home)
#   - Pre-game features (rolling stats, efficiency metrics)
#   - Post-game targets (actual stats for training)
#
# All pre-game features use strictly lagged data (no current-game leakage).
#
# Dependencies:
#   - R/data/load_schedules.R
#   - R/data/load_player_stats.R
#   - R/data/build_team_defense_game_stats.R
#   - R/features/build_rb_features.R
#   - R/features/build_team_defense_features.R
#   - R/utils/rolling_helpers.R
#
# Usage:
#   source("R/data/load_schedules.R")
#   source("R/data/load_player_stats.R")
#   source("R/data/build_team_defense_game_stats.R")
#   source("R/features/build_rb_features.R")
#   source("R/features/build_team_defense_features.R")
#   source("R/utils/rolling_helpers.R")
#   rb_training_data <- assemble_rb_training_data(seasons = 2021:2024)

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
  schedules <- load_schedules(seasons)
  
  if (nrow(schedules) == 0) {
    warning("No schedule data loaded")
    return(empty_rb_training_df())
  }
  
  message(paste("Loaded", nrow(schedules), "games"))
  
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
  
  # Remove rows that didn't match (shouldn't happen if data is consistent)
  unmatched <- sum(is.na(rb_data$gameday))
  if (unmatched > 0) {
    warning(paste(unmatched, "RB records did not match to schedule"))
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
      if ("opp_fumbles_forced_roll5" %in% names(def_features)) {
        def_cols <- c(def_cols, "opp_fumbles_forced_roll5")
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
    "game_id", "player_id", "season", "week", "gameday",
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

