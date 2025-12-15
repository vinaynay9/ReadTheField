# Load All Player Stats (Position Detection)
#
# Loads NFL player-level offensive statistics from nflverse sources without position filtering.
# Used for auto-detecting player position, team, and player_id from name.
# Uses cache-first strategy: reads from cache if available, downloads if needed.
#
# Dependencies: nflreadr
#
# Usage:
#   all_stats <- load_all_player_stats(seasons = 2021:2024)

#' Load all player statistics (no position filter) for specified seasons
#'
#' Uses nflreadr to load player weekly stats from nflverse with local caching.
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
      # Check if nflreadr is available
      if (!requireNamespace("nflreadr", quietly = TRUE)) {
        stop("nflreadr package not installed")
      }
      
      # Load player stats (weekly) - ALL positions
      raw_stats <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
      
      if (is.null(raw_stats) || nrow(raw_stats) == 0) {
        stop("nflreadr returned no player stats for specified seasons")
      }
      
      # Map nflverse column names to our schema
      player_id_col <- if ("player_id" %in% names(raw_stats)) "player_id" else "gsis_id"
      player_name_col <- if ("player_name" %in% names(raw_stats)) "player_name" else "player_display_name"
      team_col <- if ("team" %in% names(raw_stats)) "team" else "recent_team"
      position_col <- if ("position" %in% names(raw_stats)) "position" else "position_group"
      
      # Extract position (normalize to position_group values: QB, RB, WR, TE, K)
      position_raw <- raw_stats[[position_col]]
      if (is.null(position_raw)) {
        stop("No position column found in player stats")
      }
      
      # Normalize position to position_group values
      position_normalized <- ifelse(
        position_raw %in% c("QB", "RB", "WR", "TE", "K"),
        position_raw,
        ifelse(
          position_raw %in% c("FB"),
          "RB",  # Fullbacks treated as RBs
          ifelse(
            position_raw %in% c("P"),
            "K",  # Punters grouped with K for now
            NA_character_
          )
        )
      )
      
      # Extract statistics with safe column access
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
      
      # Build game_id if present; always produce stable game_key
      if ("game_id" %in% names(raw_stats)) {
        game_ids <- as.character(raw_stats$game_id)
      } else {
        game_ids <- rep(NA_character_, nrow(raw_stats))
      }
      
      game_dates <- safe_get_date(raw_stats, c("gameday", "game_date"), default = NA)
      opponents <- as.character(safe_get(raw_stats, c("opponent_team", "opponent", "opp_team", "defteam"), NA))
      home_away <- as.character(safe_get(raw_stats, "home_away", NA))
      game_types <- as.character(safe_get(raw_stats, "game_type", NA))
      
      game_keys <- if (exists("build_game_key")) {
        mapply(
          build_game_key,
          season = raw_stats$season,
          week = raw_stats$week,
          game_date = game_dates,
          team = raw_stats[[team_col]],
          opponent = opponents,
          game_id = game_ids,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
      } else {
        ifelse(!is.na(game_ids), game_ids, paste(raw_stats$season, raw_stats$week, raw_stats[[team_col]], opponents, sep = "_"))
      }
      
      result_all <- data.frame(
        player_id = as.character(raw_stats[[player_id_col]]),
        player_name = as.character(raw_stats[[player_name_col]]),
        game_id = game_ids,
        game_key = as.character(game_keys),
        season = as.integer(raw_stats$season),
        week = as.integer(raw_stats$week),
        team = as.character(raw_stats[[team_col]]),
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
      
      # Sort by player, season, week
      result_all <- result_all[order(result_all$player_id, result_all$season, result_all$week), ]
      rownames(result_all) <- NULL
      
      # Write full dataset to cache if enabled
      if (write_cache && exists("save_cache")) {
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

