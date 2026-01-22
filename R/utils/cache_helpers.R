# Cache Helpers
#
# Utilities for managing local cache of nflverse data.
# Provides cache directory management and read/write operations.
#
# Dependencies: None (base R only)
#
# Usage:
#   source("R/utils/cache_helpers.R")
#   ensure_cache_dir()
#   save_cache(schedules, "schedules.rds")
#   cached_data <- read_cache("schedules.rds")

#' Ensure cache directory exists
#'
#' Creates the cache directory if it doesn't exist.
#' No-op if directory already exists.
#'
#' @param path Character, path to cache directory (default "data/cache")
#' @return Character, normalized path to cache directory (invisible)
ensure_cache_dir <- function(path = "data/cache") {
  resolved_path <- normalizePath(path, mustWork = FALSE)
  if (grepl("/api/data(/|$)", resolved_path)) {
    stop(
      "Refusing to create cache under api/. Set working directory to repo root ",
      "so data/cache resolves correctly."
    )
  }
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  return(invisible(resolved_path))
}

get_snapshot_id <- function() {
  snap <- getOption("READTHEFIELD_SNAPSHOT_ID", NA_character_)
  snap <- if (is.null(snap) || !nzchar(as.character(snap))) NA_character_ else as.character(snap)[1]
  if (is.na(snap)) {
    env_snap <- Sys.getenv("READTHEFIELD_SNAPSHOT_ID", "")
    if (nzchar(env_snap)) {
      snap <- env_snap
    }
  }
  if (is.na(snap)) {
    snap <- format(Sys.Date(), "%Y%m%d")
  }
  snap
}

get_snapshot_dir <- function(create = TRUE) {
  dir <- file.path("data", "cache", "raw", get_snapshot_id())
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

read_raw_snapshot <- function(name) {
  path <- file.path(get_snapshot_dir(create = FALSE), name)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

write_raw_snapshot <- function(obj, name) {
  dir <- get_snapshot_dir(create = TRUE)
  path <- file.path(dir, name)
  tryCatch({
    saveRDS(obj, path)
    invisible(TRUE)
  }, error = function(e) {
    invisible(FALSE)
  })
}

write_snapshot_info <- function(entries) {
  dir <- get_snapshot_dir(create = TRUE)
  info_path <- file.path(dir, "SNAPSHOT_INFO.txt")
  lines <- c(
    paste0("snapshot_id: ", get_snapshot_id()),
    paste0("created_at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("r_version: ", R.version.string),
    paste0("nflreadr_version: ", if (requireNamespace("nflreadr", quietly = TRUE)) as.character(utils::packageVersion("nflreadr")) else "unavailable"),
    entries
  )
  writeLines(lines, info_path)
  invisible(TRUE)
}

.rtf_cache_state <- new.env(parent = emptyenv())

get_cache_fingerprint <- function(paths) {
  paths <- unique(paths)
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(list())
  hashes <- tools::md5sum(paths)
  list(
    files = as.character(paths),
    md5 = as.character(hashes),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}

ensure_cache_fingerprint <- function(paths) {
  fp <- get_cache_fingerprint(paths)
  if (!exists("fingerprint", envir = .rtf_cache_state, inherits = FALSE)) {
    assign("fingerprint", fp, envir = .rtf_cache_state)
    return(invisible(TRUE))
  }
  prior <- get("fingerprint", envir = .rtf_cache_state)
  if (!identical(prior$md5, fp$md5)) {
    stop("Cache fingerprint changed during session. Restart API or refresh caches for a consistent run.", call. = FALSE)
  }
  invisible(TRUE)
}

canonicalize_team_abbr <- function(team) {
  team <- toupper(trimws(as.character(team)))
  canonical_team_map <- c(
    "HST" = "TEN",
    "BLT" = "BAL",
    "CLV" = "CLE",
    "SL" = "LAR",
    "ARZ" = "ARI",
    "WSH" = "WAS",
    "OAK" = "LV",
    "SD" = "LAC",
    "STL" = "LAR",
    "LA" = "LAR",
    "JAC" = "JAX"
  )
  ifelse(team %in% names(canonical_team_map), canonical_team_map[team], team)
}

#' Get cache file path
#'
#' Returns the full path to a cached file.
#'
#' @param name Character, cache file name (e.g., "schedules.rds")
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Character, full path to cache file
cache_path <- function(name, cache_dir = "data/cache") {
  ensure_cache_dir(cache_dir)
  file.path(cache_dir, name)
}


#' Save object to cache
#'
#' Saves an R object to the cache directory using saveRDS.
#'
#' @param obj Any R object to cache
#' @param name Character, cache file name (e.g., "schedules.rds")
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Logical, TRUE if save succeeded, FALSE otherwise (invisible)
save_cache <- function(obj, name, cache_dir = "data/cache") {
  ensure_cache_dir(cache_dir)
  cache_file <- cache_path(name, cache_dir)
  tryCatch({
    saveRDS(obj, file = cache_file)
    return(invisible(TRUE))
  }, error = function(e) {
    return(invisible(FALSE))
  })
}


#' Read object from cache
#'
#' Reads an R object from the cache directory using readRDS.
#' Returns NULL if cache file doesn't exist or read fails.
#'
#' @param name Character, cache file name (e.g., "schedules.rds")
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Cached object, or NULL if not found or read failed
read_cache <- function(name, cache_dir = "data/cache") {
  cache_file <- cache_path(name, cache_dir)
  
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  
  tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    NULL
  })
}


#' Check if cache file exists
#'
#' @param name Character, cache file name
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Logical, TRUE if cache file exists
cache_exists <- function(name, cache_dir = "data/cache") {
  cache_file <- cache_path(name, cache_dir)
  file.exists(cache_file)
}

#' Return available seasons from a cached dataset
#'
#' Inspects cached data to find unique seasons without triggering downloads.
#'
#' @param dataset Character, one of "player_stats", "all_player_stats",
#'   "rb_stats", or "schedules"
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Integer vector of seasons (can be length 0 if cache missing)
get_available_seasons_from_cache <- function(dataset = c("player_stats", "all_player_stats", "rb_stats", "schedules"),
                                             cache_dir = "data/cache") {
  dataset <- match.arg(dataset)
  ensure_cache_dir(cache_dir)
  
  file_name <- switch(
    dataset,
    player_stats = "all_player_stats.rds",
    all_player_stats = "all_player_stats.rds",
    rb_stats = "rb_player_stats.rds",
    schedules = "schedules.rds"
  )
  
  path <- file.path(cache_dir, file_name)
  candidate_files <- c(path, list.files(cache_dir, pattern = "schedule|player_stats|rb_player_stats", full.names = TRUE))
  candidate_files <- unique(candidate_files[file.exists(candidate_files)])
  
  seasons <- integer(0)
  for (f in candidate_files) {
    try({
      if (grepl("\\.rds$", f, ignore.case = TRUE)) {
        obj <- readRDS(f)
      } else if (grepl("\\.parquet$", f, ignore.case = TRUE) && requireNamespace("arrow", quietly = TRUE)) {
        obj <- arrow::read_parquet(f)
      } else {
        obj <- NULL
      }
      if (!is.null(obj) && "season" %in% names(obj)) {
        seasons <- unique(c(seasons, as.integer(obj$season)))
      }
    }, silent = TRUE)
  }
  
  seasons <- seasons[!is.na(seasons)]
  sort(unique(seasons))
}


#' Build a deterministic game key when game_id is missing
#'
#' @param season Integer season
#' @param week Integer week (can be NA)
#' @param game_date Date or character date (can be NA)
#' @param team Character team abbreviation
#' @param opponent Character opponent abbreviation
#' @param game_id Optional existing game_id; returned as-is if provided
#' @return Character key stable across runs
build_game_key <- function(season, week = NA_integer_, game_date = NA, team = NA, opponent = NA, game_id = NULL) {
  if (!is.null(game_id) && !is.na(game_id) && nchar(game_id) > 0) {
    return(as.character(game_id))
  }
  
  season_val <- ifelse(is.na(season), "0000", sprintf("%04d", as.integer(season)))
  week_val <- ifelse(is.na(week), "wk00", sprintf("wk%02d", as.integer(week)))
  
  date_val <- tryCatch({
    date_parsed <- as.Date(game_date)
    ifelse(is.na(date_parsed), "nodate", format(date_parsed, "%Y%m%d"))
  }, error = function(e) {
    "nodate"
  })
  
  team_val <- ifelse(is.na(team) | team == "", "UNK", toupper(team))
  opp_val <- ifelse(is.na(opponent) | opponent == "", "UNK", toupper(opponent))
  
  paste(season_val, week_val, date_val, team_val, opp_val, sep = "_")
}



#' Check if cache is fresh
#'
#' Checks if cache file exists and is newer than max_age_days.
#'
#' @param name Character, cache file name
#' @param max_age_days Numeric, maximum age in days (default 7)
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Logical, TRUE if cache exists and is fresh, FALSE otherwise
is_cache_fresh <- function(name, max_age_days = 7, cache_dir = "data/cache") {
  cache_file <- cache_path(name, cache_dir)
  
  if (!file.exists(cache_file)) {
    return(FALSE)
  }
  
  file_age_days <- as.numeric(Sys.time() - file.info(cache_file)$mtime, units = "days")
  return(file_age_days <= max_age_days)
}


#' Get available training seasons from cache
#'
#' Inspects cached data to return all available historical seasons for training.
#' Defaults to maximum history available in cache.
#'
#' @param position Optional character, position filter (e.g., "RB")
#' @param max_seasons Optional integer, cap on number of seasons (default: all available)
#' @param cache_dir Character, cache directory (default "data/cache")
#' @return Integer vector of available seasons, sorted ascending
get_available_training_seasons <- function(position = NULL, max_seasons = NULL, cache_dir = "data/cache") {
  # Get available seasons from cache
  if (exists("get_available_seasons_from_cache")) {
    if (!is.null(position) && position == "RB") {
      available <- get_available_seasons_from_cache("rb_stats", cache_dir = cache_dir)
    } else {
      available <- unique(c(
        get_available_seasons_from_cache("rb_stats", cache_dir = cache_dir),
        get_available_seasons_from_cache("player_stats", cache_dir = cache_dir)
      ))
    }
  } else {
    available <- integer(0)
  }
  
  available <- sort(unique(as.integer(available)))
  available <- available[!is.na(available)]
  
  # Apply optional cap
  if (!is.null(max_seasons) && length(available) > max_seasons) {
    available <- tail(available, max_seasons)
  }
  
  return(available)
}
