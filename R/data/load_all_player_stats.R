# Load All Player Stats (Position Detection)
#
# Loads NFL player-level weekly game data from ffverse sources without position filtering.
# Used for auto-detecting player position, team, and player_id from name.
# Uses cache-first strategy: reads from cache if available, downloads if needed.
#
# Dependencies: ffverse
#
# Usage:
#   all_stats <- load_all_player_stats(seasons = 2021:2024)

#' Load all player statistics (no position filter) for specified seasons
#'
#' Uses ffverse to load weekly player-game data with local caching.
#' Does NOT filter by position - returns all offensive positions.
#' Cache-first: reads from cache if available, downloads if needed.
#' Returns a data.frame with one row per player-game.
#'
#' @param seasons Integer vector of NFL seasons to load (e.g., 2021:2024)
#' @param use_cache Logical, if TRUE (default) reads from cache if available
#' @param write_cache Logical, if TRUE (default) writes to cache after successful download
#' @param cache_name Character, cache file name (default "all_player_stats.rds")
#' @param retries Integer, number of download retry attempts (default 3)
#' @param retry_sleep_sec Numeric, seconds to wait between retries (default 2)
#' @param cache_only Logical, if TRUE skip download attempts and rely on cached data (default TRUE)
#' @return data.frame with columns:
#'   - player_id: character, unique player identifier (gsis_id)
#'   - player_name: character, player display name
#'   - game_id: character, unique game identifier
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - team: character, player's team for that game
#'   - position: character, player position (QB/RB/WR/TE/K)
load_all_player_stats <- function(seasons = NULL,
                                  use_cache = TRUE,
                                  write_cache = TRUE,
                                  cache_name = "all_player_stats.rds",
                                  retries = 3,
                                  retry_sleep_sec = 2,
                                  cache_only = TRUE) {
  
  # Load cache helpers
  if (!exists("read_cache") || !exists("save_cache") || !exists("cache_exists") || !exists("build_game_key") || !exists("get_available_seasons_from_cache")) {
    if (file.exists("R/utils/cache_helpers.R")) {
      source("R/utils/cache_helpers.R", local = TRUE)
    }
  }

  normalize_all_stats <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (!"opponent" %in% names(df)) df$opponent <- NA_character_
    if (!"home_away" %in% names(df)) df$home_away <- NA_character_
    if (!"game_type" %in% names(df)) df$game_type <- NA_character_
    if (!"game_date" %in% names(df)) df$game_date <- if ("gameday" %in% names(df)) df$gameday else as.Date(NA)
    if (!"gameday" %in% names(df)) df$gameday <- df$game_date
    if (!"game_key" %in% names(df)) df$game_key <- NA_character_
    if (exists("build_game_key")) {
      df$game_key <- ifelse(
        is.na(df$game_key) | df$game_key == "",
        build_game_key(df$season, df$week, df$gameday, df$team, df$opponent, ifelse("game_id" %in% names(df), df$game_id, NA)),
        df$game_key
      )
    }
    
    # Validate and normalize position (required for cached data)
    if (!"position" %in% names(df)) {
      stop("Cached player stats missing required column: position")
    }
    
    # Normalize position: uppercase, trim whitespace
    df$position <- toupper(trimws(df$position))
    
    # Validate all positions are valid
    invalid <- is.na(df$position) | 
               !df$position %in% c("QB", "RB", "WR", "TE", "K")
    
    if (any(invalid)) {
      stop(
        "Invalid or missing position in cached player stats for ",
        sum(invalid),
        " rows. All positions must be uppercase and one of: QB, RB, WR, TE, K"
      )
    }
    
    df
  }
  
  # Validate input
  if (missing(seasons) || is.null(seasons) || length(seasons) == 0) {
    if (exists("get_available_seasons_from_cache")) {
      seasons <- get_available_seasons_from_cache("player_stats")
    }
  }
  
  if (is.null(seasons) || length(seasons) == 0) {
    warning("No seasons specified and none found in cache, returning empty player stats")
    return(empty_all_player_stats_df())
  }
  
  seasons <- as.integer(seasons)
  if (any(is.na(seasons))) {
    warning("Some seasons could not be converted to integer")
    seasons <- seasons[!is.na(seasons)]
  }
  
  if (length(seasons) == 0) {
    return(empty_all_player_stats_df())
  }
  
  # Try cache first if enabled
  if (use_cache && exists("read_cache")) {
    cached_data <- read_cache(cache_name)
    if (!is.null(cached_data) && nrow(cached_data) > 0) {
      # Filter cached data to requested seasons
      cached_filtered <- cached_data[cached_data$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        cached_filtered <- normalize_all_stats(cached_filtered)
        return(cached_filtered)
      }
    }
  }
  
  if (isTRUE(cache_only)) {
    warning("cache_only=TRUE but cache miss for requested seasons; skipping download")
    return(empty_all_player_stats_df())
  }
  
  # Cache miss or use_cache = FALSE: attempt download with retries
  download_success <- FALSE
  last_error <- NULL
  
  for (attempt in seq_len(retries)) {
    tryCatch({
      # Check if ffverse is available
      if (!requireNamespace("ffverse", quietly = TRUE)) {
        stop("ffverse package not installed")
      }
      
      # Load weekly opportunity data (one row per player per game)
      opp_data <- ffverse::load_ff_opportunity(seasons = seasons)
      
      if (is.null(opp_data) || nrow(opp_data) == 0) {
        stop("ffverse::load_ff_opportunity returned no data for specified seasons")
      }
      
      # Load player IDs and metadata (including position)
      player_ids <- ffverse::load_ff_playerids()
      
      if (is.null(player_ids) || nrow(player_ids) == 0) {
        stop("ffverse::load_ff_playerids returned no data")
      }
      
      # Join opportunity data with player IDs to get position and player name
      # Determine join key (typically player_id or gsis_id)
      opp_player_id_col <- if ("player_id" %in% names(opp_data)) "player_id" else 
                          if ("gsis_id" %in% names(opp_data)) "gsis_id" else
                          if ("fantasypros_id" %in% names(opp_data)) "fantasypros_id" else NULL
      
      player_id_col <- if ("player_id" %in% names(player_ids)) "player_id" else
                       if ("gsis_id" %in% names(player_ids)) "gsis_id" else
                       if ("fantasypros_id" %in% names(player_ids)) "fantasypros_id" else NULL
      
      if (is.null(opp_player_id_col) || is.null(player_id_col)) {
        stop("Cannot determine player ID column for joining opportunity data with player IDs")
      }
      
      # Determine player name and position columns in player_ids
      player_name_col <- if ("player_name" %in% names(player_ids)) "player_name" else 
                        if ("name" %in% names(player_ids)) "name" else NULL
      position_col_ids <- if ("position" %in% names(player_ids)) "position" else
                         if ("pos" %in% names(player_ids)) "pos" else NULL
      
      if (is.null(player_name_col) || is.null(position_col_ids)) {
        stop("Missing required columns in player_ids: player_name and position")
      }
      
      # Select columns to join from player_ids
      player_cols_to_join <- c(player_id_col, player_name_col, position_col_ids)
      player_cols_to_join <- player_cols_to_join[player_cols_to_join %in% names(player_ids)]
      
      # Join to get position and player name
      merged_data <- merge(
        opp_data,
        player_ids[, player_cols_to_join, drop = FALSE],
        by.x = opp_player_id_col,
        by.y = player_id_col,
        all.x = TRUE
      )
      
      if (nrow(merged_data) == 0) {
        stop("Join between opportunity data and player IDs produced no rows")
      }
      
      # Extract position column (should be from player_ids join)
      position_col <- position_col_ids
      
      if (!position_col %in% names(merged_data)) {
        stop("Position column '", position_col, "' not found after joining with player IDs")
      }
      
      position_raw <- merged_data[[position_col]]
      
      # Normalize position: uppercase, trim whitespace, map aliases
      position_normalized <- toupper(trimws(position_raw))
      
      # Map position aliases
      position_normalized <- ifelse(
        position_normalized %in% c("QB", "RB", "WR", "TE", "K"),
        position_normalized,
        ifelse(
          position_normalized %in% c("FB"),
          "RB",  # Fullbacks treated as RBs
          ifelse(
            position_normalized %in% c("P"),
            "K",  # Punters grouped with K for now
            NA_character_
          )
        )
      )
      
      # Fail fast on invalid positions (after normalization)
      valid_positions <- c("QB", "RB", "WR", "TE", "K")
      invalid_pos <- unique(position_normalized[!is.na(position_normalized) & !position_normalized %in% valid_positions])
      if (length(invalid_pos) > 0) {
        stop("Invalid positions found in player data: ", paste(invalid_pos, collapse = ", "),
             ". Valid positions are: ", paste(valid_positions, collapse = ", "))
      }
      
      # Extract columns with safe access
      safe_get <- function(df, cols, default = NA) {
        if (is.character(cols) && length(cols) > 1) {
          for (c in cols) {
            if (c %in% names(df)) return(df[[c]])
          }
          return(rep(default, nrow(df)))
        }
        if (cols %in% names(df)) df[[cols]] else rep(default, nrow(df))
      }
      
      safe_get_date <- function(df, cols, default = NA) {
        vals <- safe_get(df, cols, default = default)
        tryCatch(as.Date(vals), error = function(e) rep(as.Date(default), length(vals)))
      }
      
      # Extract player ID (use the join key)
      player_ids_final <- as.character(merged_data[[opp_player_id_col]])
      
      # Extract player name (should be from player_ids join)
      if (!player_name_col %in% names(merged_data)) {
        stop("Player name column '", player_name_col, "' not found after joining with player IDs")
      }
      player_names <- as.character(merged_data[[player_name_col]])
      
      # Extract game metadata
      game_ids <- if ("game_id" %in% names(merged_data)) {
        as.character(merged_data$game_id)
      } else {
        rep(NA_character_, nrow(merged_data))
      }
      
      game_dates <- safe_get_date(merged_data, c("gameday", "game_date", "date"), default = NA)
      opponents <- as.character(safe_get(merged_data, c("opponent_team", "opponent", "opp_team", "defteam"), NA))
      home_away <- as.character(safe_get(merged_data, c("home_away", "location"), NA))
      game_types <- as.character(safe_get(merged_data, "game_type", NA))
      team_col <- if ("team" %in% names(merged_data)) "team" else
                  if ("team_abbr" %in% names(merged_data)) "team_abbr" else NULL
      if (is.null(team_col)) {
        stop("No team column found in opportunity data")
      }
      teams <- as.character(merged_data[[team_col]])
      
      # Build game keys
      game_keys <- if (exists("build_game_key")) {
        mapply(
          build_game_key,
          season = merged_data$season,
          week = merged_data$week,
          game_date = game_dates,
          team = teams,
          opponent = opponents,
          game_id = game_ids,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
      } else {
        ifelse(!is.na(game_ids), game_ids, 
               paste(merged_data$season, merged_data$week, teams, opponents, sep = "_"))
      }
      
      # Build result dataframe
      result_all <- data.frame(
        player_id = player_ids_final,
        player_name = player_names,
        game_id = game_ids,
        game_key = as.character(game_keys),
        season = as.integer(merged_data$season),
        week = as.integer(merged_data$week),
        team = teams,
        opponent = opponents,
        home_away = home_away,
        game_type = game_types,
        game_date = game_dates,
        position = position_normalized,
        stringsAsFactors = FALSE
      )
      
      result_all$gameday <- result_all$game_date
      
      # Remove rows with NA position (non-offensive positions or invalid)
      result_all <- result_all[!is.na(result_all$position), ]
      
      # Fail loudly if no rows remain
      if (nrow(result_all) == 0) {
        stop("No player-game rows with valid positions after filtering. Cannot proceed.")
      }
      
      # Sort by player, season, week
      result_all <- result_all[order(result_all$player_id, result_all$season, result_all$week), ]
      rownames(result_all) <- NULL
      
      # Validate position before writing cache
      if (!"position" %in% names(result_all)) {
        stop("Cached player stats missing required column: position")
      }
      
      invalid <- is.na(result_all$position) | 
                 !result_all$position %in% c("QB", "RB", "WR", "TE", "K")
      
      if (any(invalid)) {
        stop(
          "Invalid or missing position in cached player stats for ",
          sum(invalid),
          " rows. All positions must be uppercase and one of: QB, RB, WR, TE, K"
        )
      }
      
      # Fail loudly if no data to cache
      if (nrow(result_all) == 0) {
        stop("No player-game rows to cache. Cannot write empty cache.")
      }
      
      # Write full dataset to cache if enabled (only if non-empty)
      if (write_cache && exists("save_cache") && nrow(result_all) > 0) {
        save_cache(result_all, cache_name)
      }
      
      # Filter to requested seasons for return
      result <- result_all[result_all$season %in% seasons, ]
      result <- normalize_all_stats(result)
      
      download_success <- TRUE
      return(result)
      
    }, error = function(e) {
      last_error <<- e
      if (attempt < retries) {
        Sys.sleep(retry_sleep_sec)
      }
    })
    
    if (download_success) {
      break
    }
  }
  
  # Download failed after retries
  # Check if cache exists
  if (exists("cache_exists") && cache_exists(cache_name)) {
    cached_data <- read_cache(cache_name)
    if (!is.null(cached_data) && nrow(cached_data) > 0) {
      cached_filtered <- cached_data[cached_data$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        warning("Download failed, using cached data. Run scripts/refresh_nflverse_cache.R when online to update cache.")
        return(normalize_all_stats(cached_filtered))
      }
    }
  }
  
  # No cache and download failed: stop with clear error
  stop("Player stats unavailable. Download failed after ", retries, " attempts: ", 
       if (!is.null(last_error)) last_error$message else "unknown error",
       "\n\nIf you're offline or GitHub is blocked, run scripts/refresh_nflverse_cache.R once when online.")
  
  # Fallback (should not reach here)
  return(empty_all_player_stats_df())
}


#' Create empty all player stats data.frame with correct schema
#'
#' @return data.frame with zero rows and correct column types
empty_all_player_stats_df <- function() {
  data.frame(
    player_id = character(0),
    player_name = character(0),
    game_id = character(0),
    game_key = character(0),
    season = integer(0),
    week = integer(0),
    team = character(0),
    opponent = character(0),
    home_away = character(0),
    game_type = character(0),
    game_date = as.Date(character(0)),
    gameday = as.Date(character(0)),
    position = character(0),
    stringsAsFactors = FALSE
  )
}

