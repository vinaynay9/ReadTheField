# Simulation Mode Policy
#
# Determines allowed seasons, weeks, and training window exclusions
# based on simulation mode (upcoming_game, hypothetical_matchup, historical_replay).
#
# This function centralizes all training window logic to prevent ad-hoc filtering.

#' Get simulation mode policy
#'
#' Returns a structured policy object that determines:
#' - Which seasons are allowed for training
#' - Which weeks are allowed per season
#' - Whether the target game should be excluded from rolling features
#' - Whether post-game data is permitted
#'
#' @param mode Character, one of "upcoming_game", "hypothetical_matchup", "historical_replay"
#' @param target_season Integer, season of the target game
#' @param target_week Integer or NA, week of the target game
#' @param target_game_date Date, date of the target game
#' @param available_seasons Integer vector, all seasons available in cache
#' @param in_progress_season Integer, latest season if incomplete (optional)
#' @return List with:
#'   - seasons_allowed: integer vector of seasons allowed for training
#'   - max_week_per_season: named list mapping season -> max week allowed (or NULL for all)
#'   - exclude_target_game: logical, whether to exclude target game from rolling features
#'   - allow_post_game_data: logical, whether post-game data is permitted
simulation_mode_policy <- function(mode = c("upcoming_game", "hypothetical_matchup", "historical_replay"),
                                   target_season,
                                   target_week = NA_integer_,
                                   target_game_date = NULL,
                                   available_seasons = NULL,
                                   in_progress_season = NA_integer_) {
  mode <- match.arg(mode)
  
  # Validate inputs
  if (missing(target_season) || is.na(target_season)) {
    stop("target_season is required for simulation mode policy")
  }
  
  # Get available seasons from cache if not provided
  if (is.null(available_seasons) || length(available_seasons) == 0) {
    if (exists("get_available_seasons_from_cache")) {
      available_seasons <- unique(c(
        get_available_seasons_from_cache("rb_stats"),
        get_available_seasons_from_cache("player_stats")
      ))
    } else {
      # Fallback: assume target season and previous season
      available_seasons <- sort(unique(c(target_season - 1L, target_season)))
    }
  }
  available_seasons <- sort(unique(as.integer(available_seasons)))
  
  # Default: exclude target game from rolling features (no leakage)
  exclude_target_game <- TRUE
  
  # Default: no post-game data
  allow_post_game_data <- FALSE
  
  # Default: all historical seasons before target
  seasons_allowed <- available_seasons[available_seasons < target_season]
  
  # Per-season week limits (NULL means all weeks allowed)
  max_week_per_season <- list()
  
  if (mode == "upcoming_game") {
    # upcoming_game: all historical data up to current week
    # Include target season if we have weeks before target week
    if (!is.na(target_week) && target_week > 1 && target_season %in% available_seasons) {
      seasons_allowed <- unique(c(seasons_allowed, target_season))
      # For target season, only allow weeks before target week
      max_week_per_season[[as.character(target_season)]] <- target_week - 1L
    }
    # Exclude target game (it's upcoming, not yet played)
    exclude_target_game <- TRUE
    allow_post_game_data <- FALSE
    
  } else if (mode == "hypothetical_matchup") {
    # hypothetical_matchup: same as upcoming_game, opponent swapped
    # All historical data up to current week
    if (!is.na(target_week) && target_week > 1 && target_season %in% available_seasons) {
      seasons_allowed <- unique(c(seasons_allowed, target_season))
      max_week_per_season[[as.character(target_season)]] <- target_week - 1L
    }
    exclude_target_game <- TRUE
    allow_post_game_data <- FALSE
    
  } else if (mode == "historical_replay") {
    # historical_replay: all data except future weeks relative to that game
    # Include target season, but only weeks up to and including target week
    if (target_season %in% available_seasons) {
      seasons_allowed <- unique(c(seasons_allowed, target_season))
      if (!is.na(target_week)) {
        # Allow target week and earlier for historical replay
        max_week_per_season[[as.character(target_season)]] <- target_week
      }
    }
    # For historical replay, we can include the target game in rolling features
    # if we're simulating a game that already happened (but exclude it from training targets)
    exclude_target_game <- FALSE
    allow_post_game_data <- TRUE
  }
  
  # Ensure seasons are sorted
  seasons_allowed <- sort(unique(seasons_allowed))

  # Never train on in-progress season (diagnostic-only exclusion)
  if (!is.na(in_progress_season) && in_progress_season %in% seasons_allowed) {
    seasons_allowed <- setdiff(seasons_allowed, in_progress_season)
    max_week_per_season[[as.character(in_progress_season)]] <- NULL
  }
  
  # If no seasons allowed, this is an error condition
  if (length(seasons_allowed) == 0) {
    stop("No training seasons available for mode '", mode, 
         "' with target season ", target_season,
         ". Available seasons: ", paste(available_seasons, collapse = ", "))
  }
  
  list(
    seasons_allowed = seasons_allowed,
    max_week_per_season = max_week_per_season,
    exclude_target_game = exclude_target_game,
    allow_post_game_data = allow_post_game_data
  )
}

#' Detect in-progress season using schedule completeness
#'
#' Returns the latest season if schedule weeks are incomplete; otherwise NA.
detect_in_progress_season <- function(available_seasons, cache_only = TRUE) {
  if (is.null(available_seasons) || length(available_seasons) == 0) {
    return(NA_integer_)
  }
  if (!exists("load_schedules")) {
    return(NA_integer_)
  }
  schedules <- tryCatch(
    load_schedules(seasons = available_seasons, cache_only = cache_only),
    error = function(e) NULL
  )
  if (is.null(schedules) || nrow(schedules) == 0) {
    return(NA_integer_)
  }
  schedules$season <- as.integer(schedules$season)
  schedules$week <- as.integer(schedules$week)
  max_season <- suppressWarnings(max(schedules$season, na.rm = TRUE))
  if (!is.finite(max_season)) {
    return(NA_integer_)
  }
  max_by_season <- tapply(schedules$week, schedules$season, max, na.rm = TRUE)
  if (length(max_by_season) == 0) {
    return(NA_integer_)
  }
  prior_seasons <- setdiff(names(max_by_season), as.character(max_season))
  expected_max_week <- NA_integer_
  if (length(prior_seasons) > 0) {
    prior_maxes <- as.integer(max_by_season[prior_seasons])
    prior_maxes <- prior_maxes[!is.na(prior_maxes)]
    if (length(prior_maxes) > 0) {
      mode_counts <- sort(table(prior_maxes), decreasing = TRUE)
      expected_max_week <- as.integer(names(mode_counts)[1])
    }
  }
  if (is.na(expected_max_week)) {
    expected_max_week <- as.integer(max(max_by_season, na.rm = TRUE))
  }
  current_max_week <- as.integer(max_by_season[as.character(max_season)])
  if (!is.na(current_max_week) && !is.na(expected_max_week) && current_max_week < expected_max_week) {
    return(as.integer(max_season))
  }
  NA_integer_
}

