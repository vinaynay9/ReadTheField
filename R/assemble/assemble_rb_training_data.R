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
#   - R/features/build_rb_features.R
#   - R/utils/rolling_helpers.R
#
# Usage:
#   source("R/data/load_schedules.R")
#   source("R/data/load_player_stats.R")
#   source("R/features/build_rb_features.R")
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

