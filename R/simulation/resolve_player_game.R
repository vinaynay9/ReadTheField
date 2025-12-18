## Resolve Player Game from cached identity layers
##
## Resolves a single player-game entry using the layer 1 identity cache.
## Only reads cached data; no downloads are performed.
##
## Resolution Modes:
##   A. Historical replay mode: season + week provided → exact match required
##   B. Date-based mode: game_date provided → exact match required, must be unique
#
#' Canonicalize player name for matching
#'
#' Normalizes player names to a canonical form for case-insensitive matching.
#' Removes periods, normalizes whitespace, converts to lowercase.
#' Does NOT perform fuzzy matching or partial matching.
#'
#' @param x Character vector of player names
#' @return Character vector of canonicalized names
canonicalize_name <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x <- as.character(x)
  x <- tolower(x)
  x <- gsub("\\.", "", x)  # Remove periods
  x <- gsub("\\s+", " ", x)  # Normalize whitespace
  x <- trimws(x)
  x[nchar(x) == 0] <- NA_character_
  x
}

#' Canonicalize full first+last name for strict matching
#'
#' @param first Character vector of first names
#' @param last Character vector of last names
#' @return Character vector of canonicalized "first last" names
canonicalize_full_name <- function(first, last) {
  canonicalize_name(paste(first, last))
}

#' Resolve player/game context from cached identity data (strict resolution with canonical names)
#'
#' Resolution modes:
#'   A. Historical replay: season + week provided → exact (player_id, season, week) match
#'   B. Date-based: game_date provided → exact player_name + game_date match (must be unique)
#'
#' Player names are matched using canonical form (case-insensitive, normalized).
#' Still requires exactly one match - no fuzzy matching or ambiguity allowed.
#'
#' @param player_name Character, player name (case-insensitive, will be canonicalized)
#' @param game_date Date, target game date (optional if season and week provided)
#' @param season Optional integer season (required for historical replay mode)
#' @param week Optional integer week (required for historical replay mode)
#' @param position Optional character position filter
#' @param seasons Optional vector of seasons to constrain the search
#' @param cache_only Logical, if TRUE avoid downloads (default TRUE)
#' @return List with player/game metadata
resolve_player_game <- function(player_name,
                                game_date = NULL,
                                season = NULL,
                                week = NULL,
                                position = NULL,
                                team = NULL,
                                seasons = NULL,
                                cache_only = TRUE) {
  
  # Validate inputs
  if (missing(player_name) || is.null(player_name) || length(player_name) == 0) {
    stop("player_name is required")
  }
  player_name_input <- trimws(as.character(player_name))
  if (nchar(player_name_input) == 0) {
    stop("player_name cannot be empty")
  }
  
  # Canonicalize input name
  player_name_canonical <- canonicalize_name(player_name_input)
  if (is.na(player_name_canonical)) {
    stop("player_name cannot be canonicalized (empty after normalization)")
  }
  
  # Enforce full first+last name only (no abbreviated aliases)
  name_parts <- strsplit(player_name_canonical, "\\s+")[[1]]
  if (length(name_parts) < 2) {
    stop("player_name must include full first and last name (e.g., 'Bijan Robinson'). ",
         "Abbreviated aliases like 'B.Robinson' are not allowed.")
  }
  if (length(name_parts) > 2) {
    stop("player_name must be full first and last name only (no middle names or initials). ",
         "Input was: '", player_name_input, "'.")
  }
  if (nchar(name_parts[1]) == 1) {
    stop("Abbreviated aliases like 'B.Robinson' are not allowed. ",
         "Provide the full first and last name.")
  }
  player_name_full_canonical <- canonicalize_name(paste(name_parts[1], name_parts[2]))

  # Determine resolution mode
  has_season_week <- !is.null(season) && !is.null(week)
  has_game_date <- !is.null(game_date)
  
  if (!has_season_week && !has_game_date) {
    stop("Either (season, week) or game_date must be provided. ",
         "Resolution mode A (historical replay): provide season and week. ",
         "Resolution mode B (date-based): provide game_date.")
  }
  
  if (has_season_week && has_game_date) {
    # Both provided - use historical replay mode (more specific)
    warning("Both (season, week) and game_date provided. Using historical replay mode (season, week).")
  }

  if (!is.null(game_date)) {
  game_date <- as.Date(game_date)
    if (is.na(game_date)) {
      stop("game_date must be a valid date")
    }
  }

  if (!is.null(season)) {
    season <- as.integer(season)
    if (is.na(season)) {
      stop("season must be a valid integer")
    }
  }

  if (!is.null(week)) {
    week <- as.integer(week)
    if (is.na(week)) {
      stop("week must be a valid integer")
    }
  }

  # Load player directory and identity cache
  if (!exists("read_player_directory_cache") || !exists("read_player_week_identity_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required by resolve_player_game")
    }
  }

  # STEP 1: Resolve player_id from player directory (source of truth for names)
  player_dir <- read_player_directory_cache()
  if (nrow(player_dir) == 0) {
    stop("Player directory cache empty. Run scripts/refresh_weekly_cache.R to generate caches.")
  }
  
  # Match on canonical full first+last name in player directory
  player_dir$canonical_full_name <- canonicalize_full_name(player_dir$first_name, player_dir$last_name)
  name_matches <- player_dir[
    !is.na(player_dir$canonical_full_name) & player_dir$canonical_full_name == player_name_full_canonical,
    , drop = FALSE
  ]
  
  if (nrow(name_matches) == 0) {
    # Find close matches for error message (same last name)
    input_parts <- strsplit(player_name_canonical, "\\s+")[[1]]
    input_last_name <- if (length(input_parts) > 0) input_parts[length(input_parts)] else ""
    
    if (nchar(input_last_name) > 0) {
      player_dir$last_name_canonical <- canonicalize_name(player_dir$last_name)
      same_last_name <- player_dir[
        !is.na(player_dir$last_name_canonical) & player_dir$last_name_canonical == input_last_name,
        , drop = FALSE
      ]
      
      if (nrow(same_last_name) > 0) {
        # Get career span from identity cache if available
        identity_check <- read_player_week_identity_cache()
        if (nrow(identity_check) > 0) {
          identity_check$gameday <- as.Date(identity_check$gameday)
          identity_check$game_date <- as.Date(identity_check$game_date)
          
          # Get seasons for each player
          player_seasons <- aggregate(
            season ~ player_id,
            identity_check[identity_check$player_id %in% same_last_name$player_id, , drop = FALSE],
            FUN = function(x) paste(range(x, na.rm = TRUE), collapse = "-")
          )
          
          # Get teams for each player
          player_teams <- aggregate(
            team ~ player_id,
            identity_check[identity_check$player_id %in% same_last_name$player_id, , drop = FALSE],
            FUN = function(x) paste(unique(x[!is.na(x)]), collapse = "/")
          )
          
          same_last_name <- merge(same_last_name, player_seasons, by = "player_id", all.x = TRUE)
          same_last_name <- merge(same_last_name, player_teams, by = "player_id", all.x = TRUE)
          
          suggestions <- paste(
            same_last_name$full_name,
            " (", same_last_name$team, ", ", same_last_name$season, ")",
            sep = "",
            collapse = "\n- "
          )
        } else {
          suggestions <- paste(same_last_name$full_name, collapse = "\n- ")
        }
        
        stop("No player found with canonical name '", player_name_canonical, 
             "' (input: '", player_name_input, "').\n",
             "Did you mean:\n- ", suggestions, "\n",
             "Specify season/week if ambiguous.")
      }
    }
    
    stop("No player found with canonical full name '", player_name_full_canonical, 
         "' (input: '", player_name_input, "'). ",
         "Player name matching is case-insensitive but requires exact full first+last name.")
  }
  
  if (nrow(name_matches) > 1) {
    # Disambiguate using season/team context from identity cache
    identity_check <- read_player_week_identity_cache()
    identity_check$gameday <- as.Date(identity_check$gameday)
    identity_check$game_date <- as.Date(identity_check$game_date)
    
    candidate_ids <- name_matches$player_id
    identity_subset <- identity_check[identity_check$player_id %in% candidate_ids, , drop = FALSE]
    
    if (!is.null(season) && nrow(identity_subset) > 0) {
      identity_subset <- identity_subset[!is.na(identity_subset$season) & identity_subset$season == season, , drop = FALSE]
    }
    if (!is.null(week) && nrow(identity_subset) > 0) {
      identity_subset <- identity_subset[!is.na(identity_subset$week) & identity_subset$week == week, , drop = FALSE]
    }
    if (!is.null(team) && nrow(identity_subset) > 0) {
      team_filter <- toupper(trimws(as.character(team)))
      identity_subset <- identity_subset[!is.na(identity_subset$team) & identity_subset$team == team_filter, , drop = FALSE]
    }
    
    if (nrow(identity_subset) > 0) {
      narrowed_ids <- unique(identity_subset$player_id)
      name_matches <- name_matches[name_matches$player_id %in% narrowed_ids, , drop = FALSE]
    }
    
    if (nrow(name_matches) > 1) {
      # Build ambiguity report
      identity_all <- read_player_week_identity_cache()
      identity_all$gameday <- as.Date(identity_all$gameday)
      identity_all$game_date <- as.Date(identity_all$game_date)
      
      seasons_active <- aggregate(
        season ~ player_id,
        identity_all[identity_all$player_id %in% name_matches$player_id, , drop = FALSE],
        FUN = function(x) {
          x <- sort(unique(x[!is.na(x)]))
          if (length(x) == 0) return(NA_character_)
          paste0(min(x), "-", max(x))
        }
      )
      
      teams_active <- aggregate(
        team ~ player_id,
        identity_all[identity_all$player_id %in% name_matches$player_id, , drop = FALSE],
        FUN = function(x) paste(unique(x[!is.na(x)]), collapse = "/")
      )
      
      candidates <- merge(name_matches, seasons_active, by = "player_id", all.x = TRUE)
      candidates <- merge(candidates, teams_active, by = "player_id", all.x = TRUE)
      
      candidate_lines <- paste(
        candidates$full_name, "|", candidates$team, "|",
        candidates$player_id, "|", candidates$season,
        sep = " "
      )
      
      stop("Multiple players found with full name '", player_name_full_canonical, 
           "' (input: '", player_name_input, "'). ",
           "Ambiguous candidates:\n- ", paste(candidate_lines, collapse = "\n- "))
    }
  }
  
  # Resolved player_id (source of truth)
  resolved_player_id <- name_matches$player_id[1]
  resolved_full_name <- name_matches$full_name[1]
  
  # STEP 2: Resolve games using player_id (never match on player_name from weekly stats)
  identity <- read_player_week_identity_cache()
  if (nrow(identity) == 0) {
    stop("Player identity cache empty. Run scripts/refresh_weekly_cache.R to generate caches.")
  }

  # Initialize .row_id if it doesn't exist (for row reference tracking)
  if (!".row_id" %in% names(identity)) {
    identity <- identity %>%
      dplyr::mutate(.row_id = dplyr::row_number())
  }

  identity$gameday <- as.Date(identity$gameday)
  identity$game_date <- as.Date(identity$game_date)
  
  # Filter by resolved player_id (source of truth)
  candidates <- identity[identity$player_id == resolved_player_id, , drop = FALSE]
  if (nrow(candidates) == 0) {
    stop("No games found for player_id '", resolved_player_id, "' (", resolved_full_name, "). ",
         "This player exists in the directory but has no games in the identity cache.")
  }

  # Filter by requested seasons if provided
  available_seasons <- sort(unique(candidates$season[!is.na(candidates$season)]))
  if (length(available_seasons) == 0) {
    stop("No seasons found for player_id '", resolved_player_id, "' (", resolved_full_name, ").")
  }

  seasons_requested <- seasons
  if (is.null(seasons_requested) || length(seasons_requested) == 0) {
    seasons_requested <- available_seasons
  }
  seasons_requested <- intersect(seasons_requested, available_seasons)

  if (length(seasons_requested) == 0) {
    if (isTRUE(cache_only)) {
      stop("No cached seasons available for requested range for player_id '", resolved_player_id, 
           "' (", resolved_full_name, "). Run scripts/refresh_weekly_cache.R to update.")
    }
    seasons_requested <- available_seasons
  }

  candidates <- candidates[candidates$season %in% seasons_requested, , drop = FALSE]
  if (nrow(candidates) == 0) {
    stop("No games found for player_id '", resolved_player_id, "' (", resolved_full_name, 
         ") in the requested seasons.")
  }

  # Apply position filter if provided
  if (!is.null(position)) {
    position_filter <- toupper(trimws(position))
    valid_positions <- c("QB", "RB", "WR", "TE", "K")
    if (!position_filter %in% valid_positions) {
      stop("Invalid position filter '", position, "'. Must be one of: ", paste(valid_positions, collapse = ", "))
    }
    candidates <- candidates[!is.na(candidates$position) & candidates$position == position_filter, , drop = FALSE]
    if (nrow(candidates) == 0) {
      stop("No games found for player_id '", resolved_player_id, "' (", resolved_full_name, 
           ") with position '", position_filter, "' in the requested seasons.")
    }
  }

  # RESOLUTION MODE A: Historical replay (season + week provided)
  if (has_season_week) {
    # Match on player_id + season + week (player_id already resolved)
    matches <- candidates[
      !is.na(candidates$season) & candidates$season == season &
      !is.na(candidates$week) & candidates$week == week,
      , drop = FALSE
    ]
    
    if (nrow(matches) == 0) {
      available_weeks <- sort(unique(candidates$week[candidates$season == season & !is.na(candidates$week)]))
      stop("No matching game found for player '", resolved_full_name, 
           "' (player_id: ", resolved_player_id, ") in season ", season, " week ", week, ". ",
           "Available weeks for this player in season ", season, ": ", 
           if (length(available_weeks) > 0) paste(head(available_weeks, 10), collapse = ", ") else "none",
           if (length(available_weeks) > 10) " ..." else ".")
    }
    
    if (nrow(matches) > 1) {
      # Multiple matches for same (player_id, season, week) - should not happen but handle it
      stop("Multiple player-game entries found for player '", resolved_full_name, 
           "' (player_id: ", resolved_player_id, ") in season ", season, " week ", week, ". ",
           "This indicates a data integrity issue. Found ", nrow(matches), " matches.")
    }
    
    chosen <- matches[1, ]
    resolution_mode <- "historical_replay"
    
        } else {
    # RESOLUTION MODE B: Date-based (game_date provided)
    # Match on player_id + game_date (player_id already resolved, must be unique)
    
    # Filter by exact game_date
    date_matches <- candidates[
      !is.na(candidates$game_date) & candidates$game_date == game_date,
      , drop = FALSE
    ]
    
    if (nrow(date_matches) == 0) {
      # Try game_date alias
      date_matches <- candidates[
        !is.na(candidates$gameday) & candidates$gameday == game_date,
        , drop = FALSE
      ]
    }
    
    if (nrow(date_matches) == 0) {
      available_dates <- sort(unique(candidates$game_date[!is.na(candidates$game_date)]))
      stop("No game found for player '", resolved_full_name, 
           "' (player_id: ", resolved_player_id, ") on date ", format(game_date, "%Y-%m-%d"), ". ",
           "Available game dates for this player: ", 
           if (length(available_dates) > 0) paste(format(head(available_dates, 5), "%Y-%m-%d"), collapse = ", ") else "none",
           if (length(available_dates) > 5) " ..." else "",
           ". Try specifying season and week instead for historical replay mode.")
    }
    
    if (nrow(date_matches) > 1) {
      stop("Multiple games found for player '", resolved_full_name, 
           "' (player_id: ", resolved_player_id, ") on date ", format(game_date, "%Y-%m-%d"), ". ",
           "Found ", nrow(date_matches), " matches. ",
           "Specify season and week to disambiguate (historical replay mode).")
    }
    
    chosen <- date_matches[1, ]
    resolution_mode <- "date_based"
  }

  # Extract resolved values
  # CRITICAL: Use CLI-provided season/week when available (source of truth)
  # Only fall back to chosen row if CLI args were not provided
  resolved_season <- if (has_season_week && !is.null(season) && !is.na(season)) {
    as.integer(season)
  } else {
    as.integer(chosen$season)
  }
  
  resolved_week <- if (has_season_week && !is.null(week) && !is.na(week)) {
    as.integer(week)
  } else {
    as.integer(chosen$week)
  }
  
  resolved_game_date <- if (!is.na(chosen$game_date)) {
    as.Date(chosen$game_date)
  } else if (!is.na(chosen$gameday)) {
    as.Date(chosen$gameday)
  } else {
    game_date
  }

  resolved_position <- toupper(trimws(chosen$position))
  if (is.na(resolved_position) || resolved_position == "") {
    stop("Cannot resolve position for player '", player_name_input, "'. Position is NA or empty in cache.")
  }
  
  valid_positions <- c("QB", "RB", "WR", "TE", "K")
  if (!resolved_position %in% valid_positions) {
    stop("Invalid position '", resolved_position, "' for player '", player_name_input, "'. ",
         "Position must be one of: ", paste(valid_positions, collapse = ", "))
  }

  resolved_team <- chosen$team
  resolved_opponent <- chosen$opponent
  resolved_home_away <- chosen$home_away

  # CRITICAL FIX: Always rebuild game_key from resolved values (CLI args), never use chosen$game_key
  # This ensures game_key matches resolved_season/resolved_week when CLI overrides are provided
  # chosen$game_key may be from a different game (e.g., week 8 when CLI requested week 2)
  resolved_game_key <- if (exists("build_game_key")) {
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
    stop("Cannot build game_key for player '", player_name_input, "'. ",
         "Required fields: season=", resolved_season, ", week=", resolved_week, 
         ", game_date=", resolved_game_date)
  }

  # CRITICAL: Guardrail - ensure resolved player_id matches directory lookup
  if (chosen$player_id != resolved_player_id) {
    stop("Player ID mismatch: resolved player_id '", resolved_player_id, 
         "' from directory but found '", chosen$player_id, "' in identity cache. ",
         "This indicates a data integrity issue. Resolution mode was: ", resolution_mode, ". ",
         "This should never happen - please report this bug.")
  }
  
  # Use full_name from directory (source of truth), not player_name from weekly stats
  resolved_player_name <- resolved_full_name

  # CRITICAL INVARIANT: Verify resolved week matches CLI-provided week (if provided)
  # This ensures no silent week drift due to cache lookups or heuristic overrides
  if (has_season_week && !is.null(week) && !is.na(week)) {
    if (resolved_week != week) {
      stop("WEEK RESOLUTION INVARIANT VIOLATED: CLI requested week=", week,
           " but resolved week=", resolved_week, ". ",
           "This indicates game resolution logic found the wrong game. ",
           "Player: ", resolved_player_name, ", Season: ", resolved_season, ". ",
           "Resolution mode: ", resolution_mode)
    }
  }
  
  # CRITICAL INVARIANT: Verify resolved season matches CLI-provided season (if provided)
  if (has_season_week && !is.null(season) && !is.na(season)) {
    if (resolved_season != season) {
      stop("SEASON RESOLUTION INVARIANT VIOLATED: CLI requested season=", season,
           " but resolved season=", resolved_season, ". ",
           "This indicates game resolution logic found the wrong game. ",
           "Player: ", resolved_player_name, ", Week: ", resolved_week, ". ",
           "Resolution mode: ", resolution_mode)
    }
  }

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
    resolution_mode = resolution_mode,
    row_refs = list(player_identity_rows = chosen$.row_id)
  )
}
