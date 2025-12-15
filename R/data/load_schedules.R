# Load Schedules
#
# Loads NFL schedule data from nflverse sources with local caching support.
# Returns one row per game with game metadata.
# Uses cache-first strategy: reads from cache if available, downloads if needed.
#
# Dependencies: nflreadr
#
# Usage:
#   schedules <- load_schedules(seasons = 2021:2024)
#   schedules <- load_schedules(seasons = 2021:2024, use_cache = FALSE)  # Force download

#' Load NFL schedules for specified seasons
#'
#' Uses nflreadr to load schedule data from nflverse with local caching.
#' Cache-first: reads from cache if available, downloads if needed.
#' Returns a data.frame with one row per game.
#'
#' @param seasons Integer vector of NFL seasons to load (e.g., 2021:2024)
#' @param use_cache Logical, if TRUE (default) reads from cache if available
#' @param write_cache Logical, if TRUE (default) writes to cache after successful download
#' @param cache_name Character, cache file name (default "schedules.rds")
#' @param retries Integer, number of download retry attempts (default 3)
#' @param retry_sleep_sec Numeric, seconds to wait between retries (default 2)
#' @param cache_only Logical, if TRUE skip download attempts and rely on local cache
#' @return data.frame with columns:
#'   - game_id: character, unique game identifier
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - gameday: Date, game date
#'   - home_team: character, home team abbreviation
#'   - away_team: character, away team abbreviation
#'   - home_score: integer, home team final score (NA if game not played)
#'   - away_score: integer, away team final score (NA if game not played)
#'   - stadium: character, stadium name (optional)
#'   - surface: character, playing surface type (optional)
#' @examples
#' schedules <- load_schedules(2023)
#' schedules <- load_schedules(2021:2024, use_cache = FALSE)  # Force download
load_schedules <- function(seasons,
                           use_cache = TRUE,
                           write_cache = TRUE,
                           cache_name = "schedules.rds",
                           retries = 3,
                           retry_sleep_sec = 2,
                           cache_only = FALSE) {
  
  # Validate input
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified, returning empty schedule")
    return(empty_schedule_df())
  }
  
  seasons <- as.integer(seasons)
  if (any(is.na(seasons))) {
    warning("Some seasons could not be converted to integer")
    seasons <- seasons[!is.na(seasons)]
  }
  
  if (length(seasons) == 0) {
    return(empty_schedule_df())
  }
  
  # Load cache helpers
  if (!exists("read_cache") || !exists("save_cache") || !exists("cache_exists") || !exists("build_game_key")) {
    if (file.exists("R/utils/cache_helpers.R")) {
      source("R/utils/cache_helpers.R", local = TRUE)
    }
  }

  # Helper to read cached schedules flexibly (rds/parquet) without download
  read_cached_schedules <- function() {
    cache_dir <- "data/cache"
    candidates <- c(cache_path(cache_name), list.files(cache_dir, pattern = "schedule", full.names = TRUE))
    candidates <- unique(candidates[file.exists(candidates)])
    
    for (path in candidates) {
      try({
        if (grepl("\\.rds$", path, ignore.case = TRUE)) {
          obj <- readRDS(path)
        } else if (grepl("\\.parquet$", path, ignore.case = TRUE) && requireNamespace("arrow", quietly = TRUE)) {
          obj <- arrow::read_parquet(path)
        } else {
          obj <- NULL
        }
        if (!is.null(obj) && nrow(obj) > 0) {
          return(obj)
        }
      }, silent = TRUE)
    }
    return(NULL)
  }
  
  # Try cache first if enabled (ALWAYS check cache before attempting download)
  if (use_cache && exists("read_cache")) {
    cached_data <- read_cache(cache_name)
    if (!is.null(cached_data) && nrow(cached_data) > 0) {
      # Filter cached data to requested seasons
      cached_filtered <- cached_data[cached_data$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        return(cached_filtered)
      }
    }
  }

  # If cache_only, never attempt download; return cached or empty frame
  if (isTRUE(cache_only)) {
    cached_flexible <- read_cached_schedules()
    if (!is.null(cached_flexible)) {
      cached_filtered <- cached_flexible[cached_flexible$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        return(cached_filtered)
      }
    }
    warning("cache_only=TRUE but no cached schedules found for requested seasons")
    return(empty_schedule_df())
  }
  
  # Pre-check for cache availability as fallback (even if use_cache=FALSE)
  # This allows using cache if download fails
  cache_fallback_available <- FALSE
  cached_fallback_data <- NULL
  if (exists("cache_exists") && cache_exists(cache_name)) {
    cached_fallback_data <- read_cache(cache_name)
    if (!is.null(cached_fallback_data) && nrow(cached_fallback_data) > 0) {
      cached_fallback_filtered <- cached_fallback_data[cached_fallback_data$season %in% seasons, ]
      if (nrow(cached_fallback_filtered) > 0) {
        cache_fallback_available <- TRUE
      }
    }
  } else {
    cached_flexible <- read_cached_schedules()
    if (!is.null(cached_flexible) && nrow(cached_flexible) > 0) {
      cached_fallback_data <- cached_flexible
      cached_fallback_filtered <- cached_flexible[cached_flexible$season %in% seasons, ]
      cache_fallback_available <- nrow(cached_fallback_filtered) > 0
    }
  }
  
  # Cache miss or use_cache = FALSE: attempt download with retries
  download_success <- FALSE
  last_error <- NULL
  
  for (attempt in seq_len(retries)) {
    tryCatch({
      # Check if nflreadr is available
      if (!requireNamespace("nflreadr", quietly = TRUE)) {
        stop("nflreadr package not installed. Run: install.packages('nflreadr')")
      }
      
      # Load schedules - try multiple approaches
      # nflreadr::load_schedules can return empty data.frame without error
      # So we need to explicitly check for 0 rows
      raw_schedules <- NULL
      
      # Strategy 1: Try with specific seasons
      tryCatch({
        raw_schedules <- nflreadr::load_schedules(seasons = seasons)
        if (!is.null(raw_schedules) && nrow(raw_schedules) > 0) {
          # Success - check if we got data for requested seasons
          if (any(raw_schedules$season %in% seasons)) {
            # Good, we have data for at least one requested season
          } else {
            # Got data but not for requested seasons - try loading all
            raw_schedules <- NULL
          }
        }
      }, error = function(e) {
        # Strategy 1 failed, will try strategy 2
        raw_schedules <<- NULL
      })
      
      # Strategy 2: If specific seasons failed, try loading all available seasons
      if (is.null(raw_schedules) || nrow(raw_schedules) == 0) {
        tryCatch({
          raw_schedules_all <- nflreadr::load_schedules()
          if (!is.null(raw_schedules_all) && nrow(raw_schedules_all) > 0) {
            # Filter to requested seasons
            raw_schedules <- raw_schedules_all[raw_schedules_all$season %in% seasons, ]
            if (nrow(raw_schedules) == 0) {
              # No data for requested seasons
              available_seasons <- sort(unique(raw_schedules_all$season))
              stop(paste("Requested seasons", paste(seasons, collapse = ", "), 
                        "not available. Available seasons:", paste(available_seasons, collapse = ", ")))
            }
          }
        }, error = function(e) {
          # Both strategies failed
          raw_schedules <<- NULL
        })
      }
      
      # Check if download actually succeeded (has data)
      if (is.null(raw_schedules)) {
        stop("nflreadr::load_schedules returned NULL - both specific seasons and all-seasons approaches failed")
      }
      
      if (nrow(raw_schedules) == 0) {
        # Empty result - this is a failure, not success
        stop(paste("nflreadr returned empty schedule data (0 rows) for seasons:", paste(seasons, collapse = ", "),
                   "\nThis might mean:", 
                   "- Those seasons don't exist yet in nflverse",
                   "- Network/GitHub connection issue",
                   "- nflreadr version needs update",
                   "- GitHub rate limiting",
                   "\nTry: update.packages('nflreadr')", sep = "\n  "))
      }
      
      # Verify we got data for at least one requested season
      if (!any(raw_schedules$season %in% seasons)) {
        available_seasons <- if (exists("raw_schedules_all")) sort(unique(raw_schedules_all$season)) else "unknown"
        stop(paste("Downloaded schedules but none match requested seasons:", paste(seasons, collapse = ", "),
                   "\nDownloaded seasons:", available_seasons))
      }
      
      # Select and rename columns to match schema
      schedules_all <- data.frame(
        game_id = as.character(raw_schedules$game_id),
        season = as.integer(raw_schedules$season),
        week = as.integer(raw_schedules$week),
        gameday = as.Date(raw_schedules$gameday),
        home_team = as.character(raw_schedules$home_team),
        away_team = as.character(raw_schedules$away_team),
        home_score = as.integer(raw_schedules$home_score),
        away_score = as.integer(raw_schedules$away_score),
        stadium = as.character(raw_schedules$stadium),
        surface = as.character(raw_schedules$surface),
        stringsAsFactors = FALSE
      )
      
      # Sort by season, week, gameday
      schedules_all <- schedules_all[order(schedules_all$season, schedules_all$week, schedules_all$gameday), ]
      rownames(schedules_all) <- NULL
      
      # Write full dataset to cache if enabled
      if (write_cache && exists("save_cache")) {
        save_cache(schedules_all, cache_name)
      }
      
      # Filter to requested seasons for return
      schedules <- schedules_all[schedules_all$season %in% seasons, ]
      
      download_success <- TRUE
      return(schedules)
      
    }, error = function(e) {
      last_error <<- e
      if (attempt < retries) {
        cat("  Download attempt", attempt, "failed, retrying in", retry_sleep_sec, "seconds...\n")
        Sys.sleep(retry_sleep_sec)
      } else {
        cat("  All", retries, "download attempts failed.\n")
      }
    })
    
    if (download_success) {
      break
    }
  }
  
  # Download failed after retries - try to use cached data as fallback
  if (cache_fallback_available && !is.null(cached_fallback_data)) {
    cached_fallback_filtered <- cached_fallback_data[cached_fallback_data$season %in% seasons, ]
    if (nrow(cached_fallback_filtered) > 0) {
      warning("Download failed after ", retries, " attempts. Using cached data from previous successful download.",
              "\nTo update cache, run: scripts/refresh_nflverse_cache.R or scripts/fix_cache_issue.R when online.")
      return(cached_fallback_filtered)
    }
  }
  
  # Also check cache one more time (in case it was just created)
  if (exists("cache_exists") && cache_exists(cache_name)) {
    cached_data <- read_cache(cache_name)
    if (!is.null(cached_data) && nrow(cached_data) > 0) {
      cached_filtered <- cached_data[cached_data$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        warning("Download failed, using cached data. Run scripts/refresh_nflverse_cache.R when online to update cache.")
        return(cached_filtered)
      }
    }
  } else {
    cached_flexible <- read_cached_schedules()
    if (!is.null(cached_flexible) && nrow(cached_flexible) > 0) {
      cached_filtered <- cached_flexible[cached_flexible$season %in% seasons, ]
      if (nrow(cached_filtered) > 0) {
        warning("Download failed, using local cached schedule file.")
        return(cached_filtered)
      }
    }
  }
  
  # No cache and download failed: stop with clear error
  error_msg <- if (!is.null(last_error)) last_error$message else "unknown error"
  stop("Schedules unavailable. Download failed after ", retries, " attempts: ", error_msg,
       "\n\nIf you're offline or GitHub is blocked, run scripts/refresh_nflverse_cache.R once when online.",
       "\nAlternatively, run scripts/fix_cache_issue.R to populate the cache.")
  
  # Fallback (should not reach here, but ensures consistent return)
  return(empty_schedule_df())
}


#' Create empty schedule data.frame with correct schema
#'
#' Used as fallback when data loading fails.
#'
#' @return data.frame with zero rows and correct column types
empty_schedule_df <- function() {
  data.frame(
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    home_team = character(0),
    away_team = character(0),
    home_score = integer(0),
    away_score = integer(0),
    stadium = character(0),
    surface = character(0),
    stringsAsFactors = FALSE
  )
}

