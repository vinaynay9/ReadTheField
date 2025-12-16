## Resolve Player Game from cached identity layers
##
## Resolves a single player-game entry using the layer 1 identity cache.
## Only reads cached data; no downloads are performed.
#
#' Resolve player/game context from cached identity data
#'
#' @param player_name Character, player name (any casing/abbrev)
#' @param game_date Date, target game date (optional if both season and week provided)
#' @param position Optional character position filter
#' @param seasons Optional vector of seasons to constrain the search
#' @param season Optional integer season (used when game_date is unavailable)
#' @param week Optional integer week (used when game_date is unavailable)
#' @param cache_only Logical, if TRUE avoid downloads (default TRUE)
#' @return List with player/game metadata
resolve_player_game <- function(player_name,
                                game_date = NULL,
                                position = NULL,
                                seasons = NULL,
                                season = NULL,
                                week = NULL,
                                cache_only = TRUE) {
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) {
    stop("player_name is required")
  }

  if (is.null(game_date) && (is.null(season) || is.null(week))) {
    stop("Either game_date or both season and week must be provided")
  }

  if (!is.null(game_date)) {
    game_date <- as.Date(game_date)
    if (is.na(game_date)) stop("game_date must be a valid date")
  }

  if (!exists("read_player_week_identity_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required by resolve_player_game")
    }
  }

  identity <- read_player_week_identity_cache()
  if (nrow(identity) == 0) {
    stop("Player identity cache empty. Run scripts/refresh_weekly_cache.R to generate caches.")
  }

  identity$gameday <- as.Date(identity$gameday)
  identity$.row_id <- seq_len(nrow(identity))

  available_seasons <- sort(unique(identity$season[!is.na(identity$season)]))
  if (length(available_seasons) == 0) {
    stop("No seasons found in player identity cache. Run scripts/refresh_weekly_cache.R to refresh.")
  }

  seasons_requested <- seasons
  if (is.null(seasons_requested) || length(seasons_requested) == 0) {
    seasons_requested <- available_seasons
  }
  seasons_requested <- intersect(seasons_requested, available_seasons)

  if (length(seasons_requested) == 0) {
    if (isTRUE(cache_only)) {
      stop("No cached seasons available for requested range. Run scripts/refresh_weekly_cache.R to update.")
    }
    seasons_requested <- available_seasons
  }

  candidates <- identity[identity$season %in% seasons_requested, , drop = FALSE]
  if (nrow(candidates) == 0) {
    stop("No player identity rows found for the requested seasons.")
  }

  normalize_name <- function(x) {
    x <- toupper(gsub("[^A-Z]", "", x))
    x[nchar(x) == 0] <- NA_character_
    x
  }

  target_norm <- normalize_name(player_name)
  candidates$norm_name <- normalize_name(candidates$player_name)

  matches <- candidates[
    candidates$norm_name == target_norm | candidates$player_name == player_name, ,
    drop = FALSE
  ]

  if (nrow(matches) == 0) {
    matches <- candidates[grepl(target_norm, candidates$norm_name, fixed = TRUE), , drop = FALSE]
  }

  if (!is.null(position)) {
    position_filter <- toupper(trimws(position))
    valid_positions <- c("QB", "RB", "WR", "TE", "K")
    if (!position_filter %in% valid_positions) {
      stop("Invalid position filter '", position, "'. Must be one of: ", paste(valid_positions, collapse = ", "))
    }
    matches <- matches[!is.na(matches$position) & matches$position == position_filter, , drop = FALSE]
  }

  if (nrow(matches) == 0) {
    stop("Player '", player_name, "' not found in cached identity data.")
  }

  filtered_matches <- matches
  if (!is.null(game_date)) {
    date_filtered <- filtered_matches[!is.na(filtered_matches$gameday) & filtered_matches$gameday == game_date, , drop = FALSE]
    if (nrow(date_filtered) > 0) {
      filtered_matches <- date_filtered
    }
  }

  if (nrow(filtered_matches) == 0 && !is.null(season) && !is.null(week)) {
    week_filtered <- matches[matches$season == season & matches$week == week, , drop = FALSE]
    if (nrow(week_filtered) > 0) {
      filtered_matches <- week_filtered
    }
  }

  if (nrow(filtered_matches) == 0) {
    stop("No matching game found for player '", player_name, "'. Provide a valid game_date or season/week.")
  }

  filtered_matches$has_opponent <- !is.na(filtered_matches$opponent) & filtered_matches$opponent != ""
  filtered_matches <- filtered_matches[order(
    is.na(filtered_matches$game_date),
    -filtered_matches$has_opponent,
    filtered_matches$player_name
  ), , drop = FALSE]

  chosen <- filtered_matches[1, ]

  resolved_season <- ifelse(is.na(chosen$season) && !is.null(season), season, chosen$season)
  resolved_week <- ifelse(is.na(chosen$week) && !is.null(week), week, chosen$week)

  resolved_game_date <- if (!is.na(chosen$game_date)) {
    chosen$game_date
  } else {
    game_date
  }

  resolved_position <- ifelse(is.null(position), chosen$position, position)
  if (is.na(resolved_position) || resolved_position == "") {
    stop("Cannot resolve position for player '", player_name, "'.")
  }
  resolved_position <- toupper(trimws(resolved_position))

  resolved_team <- chosen$team
  resolved_opponent <- chosen$opponent
  resolved_home_away <- chosen$home_away

  resolved_game_key <- if (!is.na(chosen$game_key) && nzchar(chosen$game_key)) {
    chosen$game_key
  } else if (exists("build_game_key")) {
    build_game_key(
      resolved_season,
      resolved_week,
      resolved_game_date,
      resolved_team,
      resolved_opponent,
      chosen$game_id
    )
  } else {
    paste(resolved_season, resolved_week, format(resolved_game_date, "%Y%m%d"), resolved_team, resolved_opponent, sep = "_")
  }

  if (is.na(resolved_game_key) || resolved_game_key == "") {
    stop("Cannot build game_key for player '", player_name, "'.")
  }

  resolved_player_id <- chosen$player_id
  resolved_player_name <- chosen$player_name

  list(
    player_id = resolved_player_id,
    player_name_canonical = resolved_player_name,
    position = resolved_position,
    team = resolved_team,
    opponent = resolved_opponent,
    home_away = resolved_home_away,
    season = resolved_season,
    week = resolved_week,
    game_id = chosen$game_id,
    game_key = resolved_game_key,
    game_date = resolved_game_date,
    row_refs = list(player_identity_rows = chosen$.row_id)
  )
}


