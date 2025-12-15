# Find Player Info - Auto-Detection Helper
#
# Automatically detects player position, team, player_id, and game info
# from player name and game date.
#
# Dependencies:
#   - R/data/load_schedules.R
#   - R/data/load_all_player_stats.R

#' Find player information from name and game date
#'
#' Auto-detects player position, team, player_id, opponent, season, and week
#' from player name and game date. Searches player stats and schedules to find
#' the matching game and player record.
#'
#' @param player_name Character, player name (e.g., "Bijan Robinson", "B.Robinson")
#' @param game_date Date, game date (e.g., as.Date("2025-12-11"))
#' @param seasons_to_search Integer vector, seasons to search (default: inferred from game_date)
#' @return List with:
#'   - player_id: character, unique player identifier
#'   - player_name: character, canonical player name from data
#'   - position: character, player position (QB/RB/WR/TE/K)
#'   - team: character, player's team abbreviation
#'   - opponent: character, opponent team abbreviation
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - game_id: character, unique game identifier
#'   - game_date: Date, game date
#' @examples
#' info <- find_player_info("Bijan Robinson", as.Date("2025-12-11"))
find_player_info <- function(player_name, game_date, seasons_to_search = NULL) {
  
  # Validate inputs
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) {
    stop("player_name is required")
  }
  
  if (missing(game_date) || is.null(game_date)) {
    stop("game_date is required")
  }
  
  game_date <- as.Date(game_date)
  if (is.na(game_date)) {
    stop("game_date must be a valid date")
  }
  
  # Determine seasons to search
  if (is.null(seasons_to_search)) {
    # NFL season typically runs from September to February
    # If game_date is Jan/Feb, it's part of the previous calendar year's season
    game_year <- as.integer(format(game_date, "%Y"))
    game_month <- as.integer(format(game_date, "%m"))
    
    if (game_month >= 9) {
      # Sep-Dec: current year is the season
      seasons_to_search <- c(game_year, game_year - 1)
    } else {
      # Jan-Aug: previous year is the season
      seasons_to_search <- c(game_year - 1, game_year)
    }
  }
  
  # Load schedules to find the game
  if (!exists("load_schedules")) {
    if (file.exists("R/data/load_schedules.R")) {
      source("R/data/load_schedules.R", local = TRUE)
    } else {
      stop("load_schedules function not found")
    }
  }
  
  schedules <- load_schedules(seasons = seasons_to_search)
  
  if (nrow(schedules) == 0) {
    stop("No schedule data found for seasons: ", paste(seasons_to_search, collapse = ", "))
  }
  
  # Find games on the target date
  target_games <- schedules[schedules$gameday == game_date, ]
  
  if (nrow(target_games) == 0) {
    stop("No games found on date: ", as.character(game_date))
  }
  
  # Load all player stats (no position filter) to find the player
  if (!exists("load_all_player_stats")) {
    if (file.exists("R/data/load_all_player_stats.R")) {
      source("R/data/load_all_player_stats.R", local = TRUE)
    } else {
      stop("load_all_player_stats function not found")
    }
  }
  
  all_stats <- load_all_player_stats(seasons = seasons_to_search)
  
  if (nrow(all_stats) == 0) {
    stop("No player stats found for seasons: ", paste(seasons_to_search, collapse = ", "))
  }
  
  # Join player stats with schedules to get gameday
  # Create game-team lookup similar to assemble_rb_training_data
  home_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    team = schedules$home_team,
    stringsAsFactors = FALSE
  )
  
  away_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    team = schedules$away_team,
    stringsAsFactors = FALSE
  )
  
  game_team_lookup <- rbind(home_games, away_games)
  
  # Join player stats with game context
  all_stats_with_dates <- merge(
    all_stats,
    game_team_lookup,
    by = c("season", "week", "team"),
    all.x = TRUE
  )
  
  # Filter to games on target date
  games_on_date <- all_stats_with_dates[all_stats_with_dates$gameday == game_date, ]
  
  if (nrow(games_on_date) == 0) {
    stop("No players found in any game on ", as.character(game_date))
  }
  
  # Try to find player by name patterns
  player_name_patterns <- c(player_name)
  name_parts <- NULL
  
  # If name contains space or period, try splitting
  if (grepl("[. ]", player_name)) {
    name_parts <- strsplit(player_name, "[. ]+")[[1]]
    if (length(name_parts) >= 2) {
      # Try "First.Last" format
      player_name_patterns <- c(player_name_patterns, paste(name_parts[1], name_parts[2], sep = "."))
      # Try "First Last" format
      player_name_patterns <- c(player_name_patterns, paste(name_parts[1], name_parts[2], sep = " "))
    }
  }
  
  # Search for player in games on target date
  player_found <- NULL
  
  for (pattern in player_name_patterns) {
    # Exact match first
    matches <- games_on_date[games_on_date$player_name == pattern, ]
    
    if (nrow(matches) > 0) {
      player_found <- matches
      break
    }
    
    # Case-insensitive partial match
    matches <- games_on_date[
      grepl(pattern, games_on_date$player_name, ignore.case = TRUE, fixed = TRUE),
    ]
    
    if (nrow(matches) > 0) {
      player_found <- matches
      break
    }
  }
  
  # If still not found, try fuzzy matching on name parts
  if (is.null(player_found) && !is.null(name_parts) && length(name_parts) >= 2) {
    matches <- games_on_date[
      grepl(name_parts[1], games_on_date$player_name, ignore.case = TRUE) &
      grepl(name_parts[2], games_on_date$player_name, ignore.case = TRUE),
    ]
    
    if (nrow(matches) > 0) {
      player_found <- matches
    }
  }
  
  if (is.null(player_found) || nrow(player_found) == 0) {
    stop("Player '", player_name, "' not found in any game on ", as.character(game_date))
  }
  
  # If multiple matches, prefer exact name match
  if (nrow(player_found) > 1) {
    exact_match <- player_found[player_found$player_name == player_name, ]
    if (nrow(exact_match) > 0) {
      player_found <- exact_match
    } else {
      # Take first match
      player_found <- player_found[1, ]
    }
  }
  
  # Extract player info
  player_id <- unique(player_found$player_id)
  if (length(player_id) > 1) {
    stop("Multiple player_ids found for player '", player_name, "' on ", as.character(game_date))
  }
  
  player_name_canonical <- unique(player_found$player_name)[1]
  position <- unique(player_found$position)
  if (length(position) > 1) {
    # Player changed positions? Use most common
    position <- names(sort(table(player_found$position), decreasing = TRUE))[1]
  }
  
  team <- unique(player_found$team)
  if (length(team) > 1) {
    # Player changed teams? Use most recent
    team <- team[1]
  }
  
  # Find the game this player played in
  game_ids <- unique(player_found$game_id)
  if (length(game_ids) > 1) {
    stop("Player '", player_name, "' found in multiple games on ", as.character(game_date))
  }
  
  game_id <- game_ids[1]
  
  # Find game in schedule
  game_info <- schedules[schedules$game_id == game_id, ]
  if (nrow(game_info) == 0) {
    # Try matching by date and team
    game_info <- schedules[
      schedules$gameday == game_date &
      (schedules$home_team == team | schedules$away_team == team),
    ]
  }
  
  if (nrow(game_info) == 0) {
    stop("Game not found in schedule for player '", player_name, "' on ", as.character(game_date))
  }
  
  if (nrow(game_info) > 1) {
    # Multiple games on same date with same team? Shouldn't happen, but take first
    game_info <- game_info[1, ]
  }
  
  # Determine opponent
  if (game_info$home_team == team) {
    opponent <- game_info$away_team
  } else if (game_info$away_team == team) {
    opponent <- game_info$home_team
  } else {
    stop("Team mismatch: player team '", team, "' not found in game ", game_id)
  }
  
  # Return structured result
  result <- list(
    player_id = player_id,
    player_name = player_name_canonical,
    position = position,
    team = team,
    opponent = opponent,
    season = as.integer(game_info$season),
    week = as.integer(game_info$week),
    game_id = game_id,
    game_date = game_date
  )
  
  return(result)
}

