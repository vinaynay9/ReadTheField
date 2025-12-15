# Resolve Player Game from cached player stats
#
# Cache-first resolver that identifies a specific player-game from player stats
# without requiring live schedule downloads.

#' Resolve player/game context from local data
#'
#' @param player_name Character, player name (any casing/abbrev)
#' @param game_date Date, target game date
#' @param position Optional character position filter
#' @param seasons Optional integer vector of seasons to search (default: all available in cache)
#' @param cache_only Logical, if TRUE avoid download attempts (default TRUE)
#' @return List with player/game context (player_id, team, opponent, season, week, home_away, game_key, row_refs)
resolve_player_game <- function(player_name,
                                game_date,
                                position = NULL,
                                seasons = NULL,
                                cache_only = TRUE) {
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) stop("player_name is required")
  if (missing(game_date) || is.null(game_date)) stop("game_date is required")
  
  game_date <- as.Date(game_date)
  if (is.na(game_date)) stop("game_date must be a valid date")
  
  # Load helpers
  if (!exists("build_game_key") || !exists("get_available_seasons_from_cache")) {
    if (file.exists("R/utils/cache_helpers.R")) source("R/utils/cache_helpers.R", local = TRUE)
  }
  if (!exists("load_all_player_stats")) {
    if (file.exists("R/data/load_all_player_stats.R")) source("R/data/load_all_player_stats.R", local = TRUE)
  }
  if (!exists("load_schedules")) {
    if (file.exists("R/data/load_schedules.R")) source("R/data/load_schedules.R", local = TRUE)
  }
  
  # Determine search seasons
  if (is.null(seasons) || length(seasons) == 0) {
    seasons <- if (exists("get_available_seasons_from_cache")) get_available_seasons_from_cache("player_stats") else integer(0)
    if (length(seasons) == 0) {
      year_guess <- as.integer(format(game_date, "%Y"))
      seasons <- c(year_guess - 1L, year_guess, year_guess + 1L)
    }
  }
  seasons <- sort(unique(as.integer(seasons)))
  
  # Load player stats (cache-first)
  stats_all <- load_all_player_stats(seasons = seasons, cache_only = cache_only)
  if (nrow(stats_all) == 0) stop("No player stats available for seasons: ", paste(seasons, collapse = ", "))
  
  # Normalize player names for robust matching
  normalize_name <- function(x) toupper(gsub("[^A-Z]", "", x))
  target_norm <- normalize_name(player_name)
  stats_all$norm_name <- normalize_name(stats_all$player_name)
  
  # Ensure game_date column
  if (!"game_date" %in% names(stats_all)) {
    stats_all$game_date <- stats_all$gameday
  } else if (!"gameday" %in% names(stats_all)) {
    stats_all$gameday <- stats_all$game_date
  }
  stats_all$gameday <- as.Date(stats_all$gameday)
  
  # Candidates on the exact date
  candidates <- stats_all[stats_all$gameday == game_date, ]
  if (!is.null(position)) {
    candidates <- candidates[candidates$position == position, ]
  }
  # Name matching tiers
  exact_matches <- candidates[candidates$norm_name == target_norm | candidates$player_name == player_name, ]
  partial_matches <- candidates[grepl(target_norm, candidates$norm_name, fixed = TRUE), ]
  
  chosen <- NULL
  if (nrow(exact_matches) > 0) {
    chosen_pool <- exact_matches
  } else if (nrow(partial_matches) > 0) {
    chosen_pool <- partial_matches
  } else {
    stop("Player '", player_name, "' not found on ", as.character(game_date))
  }
  
  # Deterministic ordering: prefer regular season > opponent known > latest data row
  chosen_pool$pref_preseason <- ifelse(!is.na(chosen_pool$game_type) & chosen_pool$game_type == "PRE", 1L, 0L)
  chosen_pool$has_opp <- !is.na(chosen_pool$opponent) & chosen_pool$opponent != ""
  chosen_pool <- chosen_pool[order(chosen_pool$pref_preseason, -chosen_pool$has_opp, chosen_pool$player_name), ]
  chosen <- chosen_pool[1, ]
  
  # Infer opponent/home/away/week from schedules if missing
  inferred_opponent <- chosen$opponent
  inferred_home_away <- chosen$home_away
  inferred_week <- chosen$week
  inferred_season <- chosen$season
  inferred_game_id <- chosen$game_id
  
  if ((is.na(inferred_opponent) || inferred_opponent == "" || is.na(inferred_week)) && exists("load_schedules")) {
    sched <- try(load_schedules(seasons = inferred_season, cache_only = TRUE), silent = TRUE)
    if (!inherits(sched, "try-error") && nrow(sched) > 0) {
      sched_on_date <- sched[sched$gameday == game_date, ]
      sched_on_date <- sched_on_date[
        sched_on_date$home_team == chosen$team | sched_on_date$away_team == chosen$team, ]
      if (nrow(sched_on_date) > 0) {
        sched_row <- sched_on_date[1, ]
        inferred_week <- ifelse(is.na(inferred_week), sched_row$week, inferred_week)
        inferred_season <- ifelse(is.na(inferred_season), sched_row$season, inferred_season)
        if (sched_row$home_team == chosen$team) {
          inferred_opponent <- sched_row$away_team
          inferred_home_away <- "HOME"
        } else {
          inferred_opponent <- sched_row$home_team
          inferred_home_away <- "AWAY"
        }
        inferred_game_id <- ifelse(!is.null(inferred_game_id) && inferred_game_id != "",
                                   inferred_game_id, sched_row$game_id)
      }
    }
  }
  
  # Fallback opponent inference using other player stats with same game_id/date
  if ((is.na(inferred_opponent) || inferred_opponent == "") && !is.na(chosen$game_id)) {
    same_game <- stats_all[stats_all$game_id == chosen$game_id, ]
    other_teams <- unique(same_game$team[same_game$team != chosen$team])
    if (length(other_teams) == 1) inferred_opponent <- other_teams
  }
  
  # Build game key
  resolved_game_key <- if (exists("build_game_key")) {
    build_game_key(inferred_season, inferred_week, game_date, chosen$team, inferred_opponent, inferred_game_id)
  } else {
    paste(inferred_season, inferred_week, format(game_date, "%Y%m%d"), chosen$team, inferred_opponent, sep = "_")
  }
  
  list(
    player_id = chosen$player_id,
    player_name_canonical = chosen$player_name,
    position = ifelse(is.null(position), chosen$position, position),
    team = chosen$team,
    opponent = inferred_opponent,
    home_away = inferred_home_away,
    season = inferred_season,
    week = inferred_week,
    game_id = inferred_game_id,
    game_key = resolved_game_key,
    game_date = game_date,
    row_refs = list(player_stats_rows = which(stats_all$gameday == game_date & stats_all$norm_name == chosen$norm_name))
  )
}


