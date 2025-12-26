# Build Team Defense Game Stats
#
# Aggregates player-level defensive statistics to team-game level.
# Computes defensive totals per game including sacks, TFL, interceptions,
# and yards/points allowed.
#
# Data Discovery Notes (nflverse):
# - load_player_stats(stat_type = "defense") provides player defensive stats
# - Available columns typically include: sacks, tackles_for_loss, interceptions,
#   passes_defended
# - QB hits/pressures: May not be reliably available in nflverse player_stats
# - Yards allowed: Computed from opponent offensive totals (from schedules + player_stats)
# - Passing yards allowed: Uses opponent offensive passing yards aggregated per game
# - Points allowed: Computed from opponent scores (from schedules)
#
# Dependencies:
#   - nflreadr
#   - R/data/load_schedules.R
#   - R/data/build_weekly_player_layers.R (for normalize_team_abbr)
#
# Usage:
#   source("R/data/load_schedules.R")
#   def_stats <- build_team_defense_game_stats(seasons = 2021:2024)

#' Build team defensive game statistics
#'
#' Aggregates player-level defensive stats to one row per (game_id, defense_team).
#' Computes defensive totals and yards/points allowed from opponent offensive performance.
#'
#' @param seasons Integer vector of NFL seasons to process
#' @return data.frame with one row per team-game containing:
#'   - game_id: character, unique game identifier
#'   - season: integer, NFL season year
#'   - week: integer, week number
#'   - gameday: Date, game date
#'   - defense_team: character, team abbreviation
#'   - def_sacks_defense_forced: integer, total sacks by defense
#'   - def_tackles_for_loss_defense_forced: integer, total tackles for loss
#'   - def_interceptions_defense_caught: integer, total interceptions (if available)
#'   - def_passes_defended_defense_forced: integer, total passes defended (if available)
#'   - def_pass_yards_defense_allowed: double, passing yards allowed (from opponent offense)
#'   - def_pass_attempts_defense_allowed: double, opponent pass attempts faced
#'   - def_rush_yards_defense_allowed: double, rushing yards allowed (from opponent offense)
#'   - def_total_yards_defense_allowed: double, total yards allowed
#'   - def_points_defense_allowed: integer, points allowed (from opponent score)
#' @examples
#' def_stats <- build_team_defense_game_stats(2023)
#' def_stats <- build_team_defense_game_stats(2021:2024)
build_team_defense_game_stats <- function(seasons) {
  
  # Validate input
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified, returning empty defense stats")
    return(empty_defense_game_stats_df())
  }
  
  seasons <- as.integer(seasons)
  if (any(is.na(seasons))) {
    warning("Some seasons could not be converted to integer")
    seasons <- seasons[!is.na(seasons)]
  }
  
  if (length(seasons) == 0) {
    return(empty_defense_game_stats_df())
  }
  
  # Load schedules first (needed for game context and opponent scores)
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("nflreadr package required but not installed. Cannot build defensive stats.")
  }
  if (!exists("normalize_team_abbr")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("normalize_team_abbr not available. Cannot normalize defense team abbreviations.")
    }
  }
  
  message("Loading schedules...")
  # Load schedules function (assumes it's in the path)
  if (!exists("load_schedules")) {
    if (file.exists("R/data/load_schedules.R")) {
      source("R/data/load_schedules.R", local = TRUE)
    } else {
      stop("load_schedules function not found. Please source R/data/load_schedules.R first.")
    }
  }
  schedules <- load_schedules(seasons)
  
  if (nrow(schedules) == 0) {
    warning("No schedule data loaded. Cannot build defensive stats.")
    return(empty_defense_game_stats_df())
  }
  
  # Build game-team mapping (one row per team per game)
  message("Building game-team mapping...")
  home_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    defense_team = normalize_team_abbr(schedules$home_team, schedules$season),
    opponent_team = normalize_team_abbr(schedules$away_team, schedules$season),
    opponent_score = schedules$away_score,
    stringsAsFactors = FALSE
  )
  
  away_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    defense_team = normalize_team_abbr(schedules$away_team, schedules$season),
    opponent_team = normalize_team_abbr(schedules$home_team, schedules$season),
    opponent_score = schedules$home_score,
    stringsAsFactors = FALSE
  )
  
  game_team_lookup <- rbind(home_games, away_games)
  
  # Load player defensive stats
  message("Loading player defensive stats...")
  def_stats_available <- FALSE
  player_def_stats <- NULL
  
  tryCatch({
    player_def_stats <- nflreadr::load_player_stats(seasons = seasons, stat_type = "defense")
    
    if (!is.null(player_def_stats) && nrow(player_def_stats) > 0) {
      def_stats_available <- TRUE
      message(paste("Loaded", nrow(player_def_stats), "player defensive stat records"))
    } else {
      warning("nflreadr returned no defensive player stats. Defensive stats (sacks, TFL, INT) will be missing.")
    }
  }, error = function(e) {
    warning(paste("Failed to load defensive player stats:", e$message, 
                 "Defensive stats (sacks, TFL, INT) will be missing."))
  })
  
  # Aggregate defensive stats by team-game if available
  if (def_stats_available) {
    # Determine column names (nflverse naming may vary)
    team_col <- if ("team" %in% names(player_def_stats)) "team" else "recent_team"
    game_id_col <- if ("game_id" %in% names(player_def_stats)) "game_id" else NULL
    
    # Safe column access helper
    safe_get <- function(df, col, default = NA_integer_) {
      if (col %in% names(df)) {
        vals <- df[[col]]
        as.integer(vals)
      } else {
        rep(default, nrow(df))
      }
    }
    
    # Extract defensive stats
    # Note: Column names in nflverse may vary - check common alternatives
    sacks_col <- NULL
    tfl_col <- NULL
    int_col <- NULL
    ff_col <- NULL
    passes_defended_col <- NULL
    
    # Try to find sacks column - check more variations
    for (col in c("sacks", "sack", "defensive_sacks", "sacks_total", "sack_total")) {
      if (col %in% names(player_def_stats)) {
        sacks_col <- col
        break
      }
    }
    
    # If still not found, try case-insensitive search
    if (is.null(sacks_col)) {
      sacks_matches <- grep("^sack", names(player_def_stats), ignore.case = TRUE, value = TRUE)
      if (length(sacks_matches) > 0) {
        sacks_col <- sacks_matches[1]
        message("Found sacks column (case-insensitive): ", sacks_col)
      }
    }
    
    # Try to find TFL column - check more variations
    for (col in c("tackles_for_loss", "tfl", "defensive_tfl", "tfl_total", "tackles_for_loss_total")) {
      if (col %in% names(player_def_stats)) {
        tfl_col <- col
        break
      }
    }
    
    # If still not found, try case-insensitive search
    if (is.null(tfl_col)) {
      tfl_matches <- grep("tfl|tackles.*loss", names(player_def_stats), ignore.case = TRUE, value = TRUE)
      if (length(tfl_matches) > 0) {
        tfl_col <- tfl_matches[1]
        message("Found TFL column (case-insensitive): ", tfl_col)
      }
    }
    
    # Diagnostic: show available columns if sacks/TFL not found
    if (is.null(sacks_col) || is.null(tfl_col)) {
      message("Available defensive stat columns: ", paste(names(player_def_stats), collapse = ", "))
      if (is.null(sacks_col)) {
        warning("Sacks column not found. Available columns: ", paste(names(player_def_stats), collapse = ", "))
      }
      if (is.null(tfl_col)) {
        warning("TFL column not found. Available columns: ", paste(names(player_def_stats), collapse = ", "))
      }
    }
    
    # Try to find interceptions column (case-insensitive fallback)
    for (col in c("interceptions", "defensive_interceptions", "def_int", "int")) {
      if (col %in% names(player_def_stats)) {
        int_col <- col
        break
      }
    }
    if (is.null(int_col)) {
      int_matches <- grep("interception|\\bint\\b", names(player_def_stats), ignore.case = TRUE, value = TRUE)
      if (length(int_matches) > 0) {
        int_col <- int_matches[1]
        message("Found interceptions column (case-insensitive): ", int_col)
      }
    }

    # Try to find passes defended column (case-insensitive fallback)
    for (col in c("passes_defended", "pass_defended", "passes_defensed", "pass_deflections", "pass_deflections_total",
                  "defended_passes", "pass_deflections_def")) {
      if (col %in% names(player_def_stats)) {
        passes_defended_col <- col
        break
      }
    }
    if (is.null(passes_defended_col)) {
      pd_matches <- grep("pass.*defend|defend.*pass|pass.*deflect", names(player_def_stats), ignore.case = TRUE, value = TRUE)
      if (length(pd_matches) > 0) {
        passes_defended_col <- pd_matches[1]
        message("Found passes defended column (case-insensitive): ", passes_defended_col)
      }
    }
    
    # Build aggregation key
    if (!is.null(game_id_col) && game_id_col %in% names(player_def_stats)) {
      # Use game_id if available
      player_def_stats$agg_key <- paste(
        as.character(player_def_stats[[game_id_col]]),
        as.character(player_def_stats[[team_col]]),
        sep = "_"
      )
    } else {
      # Fallback: use season, week, team
      player_def_stats$agg_key <- paste(
        as.character(player_def_stats$season),
        as.character(player_def_stats$week),
        as.character(player_def_stats[[team_col]]),
        sep = "_"
      )
    }
    
    # Aggregate defensive stats by team-game
    sum_or_na <- function(x) {
      if (all(is.na(x))) return(NA_real_)
      sum(x, na.rm = TRUE)
    }

    def_agg <- aggregate(
      list(
        def_sacks_defense_forced = if (!is.null(sacks_col)) safe_get(player_def_stats, sacks_col) else rep(NA_integer_, nrow(player_def_stats)),
        def_tackles_for_loss_defense_forced = if (!is.null(tfl_col)) safe_get(player_def_stats, tfl_col) else rep(NA_integer_, nrow(player_def_stats)),
        def_interceptions_defense_caught = if (!is.null(int_col)) safe_get(player_def_stats, int_col) else rep(NA_integer_, nrow(player_def_stats)),
        def_passes_defended_defense_forced = if (!is.null(passes_defended_col)) safe_get(player_def_stats, passes_defended_col) else rep(NA_integer_, nrow(player_def_stats))
      ),
      by = list(agg_key = player_def_stats$agg_key),
      FUN = sum_or_na
    )
    
    # Extract game_id and team from agg_key
    if (!is.null(game_id_col) && game_id_col %in% names(player_def_stats)) {
      # Parse game_id and team from agg_key
      def_agg$game_id <- sub("_.*$", "", def_agg$agg_key)
      def_agg$defense_team <- sub("^[^_]*_", "", def_agg$agg_key)
    } else {
      # Parse season, week, team
      parts <- strsplit(def_agg$agg_key, "_")
      def_agg$season <- as.integer(sapply(parts, function(x) x[1]))
      def_agg$week <- as.integer(sapply(parts, function(x) x[2]))
      def_agg$defense_team <- sapply(parts, function(x) x[3])
      def_agg$game_id <- NULL  # Will join on season/week/team
    }
    
    def_agg$agg_key <- NULL

    # Normalize defense_team abbreviations to schedule standard
    if ("defense_team" %in% names(def_agg)) {
      if ("season" %in% names(def_agg)) {
        def_agg$defense_team <- normalize_team_abbr(def_agg$defense_team, def_agg$season)
      } else {
        def_agg$defense_team <- normalize_team_abbr(def_agg$defense_team, NA)
      }
    }

    # Diagnostic: Confirm interceptions and passes defended are not uniformly zero when available
    if (!is.null(int_col)) {
      total_int <- sum(def_agg$def_interceptions_defense_caught, na.rm = TRUE)
      if (!is.na(total_int) && total_int == 0) {
        warning("Defensive interceptions aggregated but all values are zero. Check nflreadr column mapping.", call. = FALSE)
      }
    }
    if (!is.null(passes_defended_col)) {
      total_pd <- sum(def_agg$def_passes_defended_defense_forced, na.rm = TRUE)
      if (!is.na(total_pd) && total_pd == 0) {
        warning("Defensive passes defended aggregated but all values are zero. Check nflreadr column mapping.", call. = FALSE)
      }
    }
    
  } else {
    # No defensive stats available - create empty aggregation
    def_agg <- data.frame(
      game_id = character(0),
      defense_team = character(0),
      def_sacks_defense_forced = integer(0),
      def_tackles_for_loss_defense_forced = integer(0),
      def_interceptions_defense_caught = integer(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Load offensive stats to compute yards allowed
  message("Loading offensive stats to compute yards allowed...")
  off_agg <- NULL
  tryCatch({
    off_stats <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
    
    if (is.null(off_stats) || nrow(off_stats) == 0) {
      warning("No offensive stats available. Yards allowed will be missing.")
      off_agg <- NULL
    } else {
      # Determine team column
      team_col_off <- if ("team" %in% names(off_stats)) "team" else "recent_team"
      game_id_col_off <- if ("game_id" %in% names(off_stats)) "game_id" else NULL
      
      # Safe column access
      safe_get_off <- function(df, col, default = NA_real_) {
        if (col %in% names(df)) {
          vals <- df[[col]]
          vals[is.na(vals)] <- default
          as.double(vals)
        } else {
          rep(default, nrow(df))
        }
      }

      sum_or_na <- function(x) {
        if (all(is.na(x))) return(NA_real_)
        sum(x, na.rm = TRUE)
      }

      find_off_col <- function(candidates) {
        for (col in candidates) {
          if (col %in% names(off_stats)) return(col)
        }
        NULL
      }

      # Passing yards allowed approach:
      # Use opponent offensive passing yards (QB passing yards) aggregated per game.
      pass_yards_col <- find_off_col(c("passing_yards", "pass_yards", "pass_yds", "net_passing_yards", "net_pass_yards"))
      pass_attempts_col <- find_off_col(c("pass_attempts", "passing_attempts", "pass_att", "attempts"))
      
      # Build aggregation key
      if (!is.null(game_id_col_off) && game_id_col_off %in% names(off_stats)) {
        off_stats$agg_key <- paste(
          as.character(off_stats[[game_id_col_off]]),
          as.character(off_stats[[team_col_off]]),
          sep = "_"
        )
      } else {
        off_stats$agg_key <- paste(
          as.character(off_stats$season),
          as.character(off_stats$week),
          as.character(off_stats[[team_col_off]]),
          sep = "_"
        )
      }
      
      # Aggregate offensive stats by team-game
      off_agg <- aggregate(
        list(
          rush_yards = safe_get_off(off_stats, "rushing_yards", 0),
          pass_yards = if (!is.null(pass_yards_col)) safe_get_off(off_stats, pass_yards_col, 0) else rep(NA_real_, nrow(off_stats)),
          rush_attempts = safe_get_off(off_stats, "carries", 0),
          pass_attempts = if (!is.null(pass_attempts_col)) safe_get_off(off_stats, pass_attempts_col, 0) else rep(NA_real_, nrow(off_stats))
        ),
        by = list(agg_key = off_stats$agg_key),
        FUN = sum_or_na
      )
      
      # Extract identifiers
      if (!is.null(game_id_col_off) && game_id_col_off %in% names(off_stats)) {
        off_agg$game_id <- sub("_.*$", "", off_agg$agg_key)
        off_agg$offense_team <- sub("^[^_]*_", "", off_agg$agg_key)
      } else {
        parts <- strsplit(off_agg$agg_key, "_")
        off_agg$season <- as.integer(sapply(parts, function(x) x[1]))
        off_agg$week <- as.integer(sapply(parts, function(x) x[2]))
        off_agg$offense_team <- sapply(parts, function(x) x[3])
        off_agg$game_id <- NULL
      }
      
      off_agg$agg_key <- NULL
      if ("offense_team" %in% names(off_agg)) {
        if ("season" %in% names(off_agg)) {
          off_agg$offense_team <- normalize_team_abbr(off_agg$offense_team, off_agg$season)
        } else {
          off_agg$offense_team <- normalize_team_abbr(off_agg$offense_team, NA)
        }
      }

      if (is.null(pass_yards_col)) {
        off_agg$pass_yards <- NA_real_
      }
      if (is.null(pass_attempts_col)) {
        off_agg$pass_attempts <- NA_real_
      }
    }
  }, error = function(e) {
    warning(paste("Failed to load offensive stats:", e$message, "Yards allowed will be missing."))
    off_agg <<- NULL
  })
  
  # Join defensive stats with game-team lookup
  message("Joining defensive stats with game context...")
  
  # Start with game-team lookup
  result <- game_team_lookup
  
  # Join defensive player stats aggregation
  if (def_stats_available && nrow(def_agg) > 0) {
    if ("game_id" %in% names(def_agg) && "game_id" %in% names(result)) {
      # Join on game_id and defense_team
      merge_cols <- c("game_id", "defense_team",
                      "def_sacks_defense_forced",
                      "def_tackles_for_loss_defense_forced",
                      "def_interceptions_defense_caught")
      if ("def_passes_defended_defense_forced" %in% names(def_agg)) {
        merge_cols <- c(merge_cols, "def_passes_defended_defense_forced")
      }
      result <- merge(
        result,
        def_agg[, merge_cols],
        by = c("game_id", "defense_team"),
        all.x = TRUE,
        sort = FALSE
      )
    } else if ("season" %in% names(def_agg) && "week" %in% names(def_agg)) {
      # Join on season, week, defense_team (fallback when game_id not available)
      merge_cols <- c("season", "week", "defense_team",
                      "def_sacks_defense_forced",
                      "def_tackles_for_loss_defense_forced",
                      "def_interceptions_defense_caught")
      if ("def_passes_defended_defense_forced" %in% names(def_agg)) {
        merge_cols <- c(merge_cols, "def_passes_defended_defense_forced")
      }
      result <- merge(
        result,
        def_agg[, merge_cols],
        by = c("season", "week", "defense_team"),
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      warning("Cannot join defensive stats: missing join keys")
      result$def_sacks_defense_forced <- 0L
      result$def_tackles_for_loss_defense_forced <- 0L
      result$def_interceptions_defense_caught <- NA_integer_
      if ("def_passes_defended_defense_forced" %in% names(def_agg)) {
        result$def_passes_defended_defense_forced <- NA_integer_
      }
    }
  } else {
    # Initialize defensive stat columns as 0
    result$def_sacks_defense_forced <- 0L
    result$def_tackles_for_loss_defense_forced <- 0L
    result$def_interceptions_defense_caught <- NA_integer_
    if (!"def_passes_defended_defense_forced" %in% names(result)) {
      result$def_passes_defended_defense_forced <- NA_integer_
    }
  }
  
  # Join offensive stats to compute yards allowed
  # Yards allowed = what the opponent's offense produced
  if (!is.null(off_agg) && nrow(off_agg) > 0) {
    # Match opponent_team (offense) to compute what defense allowed
    if ("game_id" %in% names(off_agg) && "game_id" %in% names(result)) {
      yards_allowed <- merge(
        result[, c("game_id", "opponent_team"), drop = FALSE],
        off_agg[, c("game_id", "offense_team", "rush_yards", "pass_yards", "rush_attempts", "pass_attempts"), drop = FALSE],
        by.x = c("game_id", "opponent_team"),
        by.y = c("game_id", "offense_team"),
        all.x = TRUE,
        sort = FALSE
      )
    } else if ("season" %in% names(off_agg) && "week" %in% names(off_agg)) {
      yards_allowed <- merge(
        result[, c("season", "week", "opponent_team"), drop = FALSE],
        off_agg[, c("season", "week", "offense_team", "rush_yards", "pass_yards", "rush_attempts", "pass_attempts"), drop = FALSE],
        by.x = c("season", "week", "opponent_team"),
        by.y = c("season", "week", "offense_team"),
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      warning("Cannot join offensive stats for yards allowed: missing join keys")
      yards_allowed <- data.frame(
        rush_yards = NA_real_,
        pass_yards = NA_real_,
        pass_attempts = NA_real_,
        stringsAsFactors = FALSE
      )
      if (nrow(result) > 0) {
        yards_allowed <- yards_allowed[rep(1, nrow(result)), , drop = FALSE]
      }
    }
    
    result$def_rush_yards_defense_allowed <- yards_allowed$rush_yards
    result$def_pass_yards_defense_allowed <- yards_allowed$pass_yards
    result$def_rush_attempts_defense_allowed <- yards_allowed$rush_attempts
    result$def_pass_attempts_defense_allowed <- yards_allowed$pass_attempts
    result$yards_source_found <- !is.na(yards_allowed$rush_yards) | !is.na(yards_allowed$pass_yards)
    result$def_total_yards_defense_allowed <- ifelse(
      is.na(result$def_rush_yards_defense_allowed) | is.na(result$def_pass_yards_defense_allowed),
      NA_real_,
      result$def_rush_yards_defense_allowed + result$def_pass_yards_defense_allowed
    )
    # Compute yards per rush allowed
    result$def_yards_per_rush_defense_allowed <- ifelse(
      !is.na(result$def_rush_attempts_defense_allowed) & result$def_rush_attempts_defense_allowed > 0,
      result$def_rush_yards_defense_allowed / result$def_rush_attempts_defense_allowed,
      NA_real_
    )
    # Compute yards per pass allowed
    result$def_yards_per_pass_defense_allowed <- ifelse(
      !is.na(result$def_pass_attempts_defense_allowed) & result$def_pass_attempts_defense_allowed > 0,
      result$def_pass_yards_defense_allowed / result$def_pass_attempts_defense_allowed,
      NA_real_
    )
  } else {
    result$def_rush_yards_defense_allowed <- NA_real_
    result$def_pass_yards_defense_allowed <- NA_real_
    result$def_rush_attempts_defense_allowed <- NA_integer_
    result$def_pass_attempts_defense_allowed <- NA_integer_
    result$def_total_yards_defense_allowed <- NA_real_
    result$def_yards_per_rush_defense_allowed <- NA_real_
    result$def_yards_per_pass_defense_allowed <- NA_real_
    result$yards_source_found <- FALSE
  }
  
  # Points allowed = opponent score
  result$def_points_defense_allowed <- as.integer(result$opponent_score)
  result$points_source_found <- !is.na(result$opponent_score)
  
  # Replace NA defensive stats with 0 (if stat was available but missing for this game)
  if (def_stats_available) {
    result$def_sacks_defense_forced[is.na(result$def_sacks_defense_forced)] <- 0L
    result$def_tackles_for_loss_defense_forced[is.na(result$def_tackles_for_loss_defense_forced)] <- 0L
    # INT and FF may be NA if not available in source
  }
  
  # Sort by defense_team, season, week, gameday
  result <- result[order(result$defense_team, result$season, result$week, result$gameday), ]
  rownames(result) <- NULL
  
  # Validation: Check for missing critical stats
  missing_sacks <- sum(is.na(result$def_sacks_defense_forced))
  missing_tfl <- sum(is.na(result$def_tackles_for_loss_defense_forced))
  missing_yards <- sum(is.na(result$def_total_yards_defense_allowed))
  missing_points <- sum(is.na(result$def_points_defense_allowed))
  
  if (missing_sacks > 0 || missing_tfl > 0) {
    warning(paste("Some defensive stats are missing:", 
                 missing_sacks, "games missing sacks,", 
                 missing_tfl, "games missing TFL"))
  }
  
  if (missing_yards > 0) {
    warning(paste(missing_yards, "games missing yards allowed data"))
  }
  
  if (missing_points > 0) {
    warning(paste(missing_points, "games missing points allowed (opponent score)"))
  }
  
  # Categorize missingness for diagnostics (logging only; no file writes)
  if (missing_yards > 0 || missing_points > 0) {
    missing_detail <- result[, c("game_id", "season", "week", "defense_team", "opponent_team",
                                 "yards_source_found", "points_source_found"), drop = FALSE]
    missing_detail$yards_missing <- is.na(result$def_total_yards_defense_allowed)
    missing_detail$points_missing <- is.na(result$def_points_defense_allowed)
    
    if (missing_yards > 0) {
      cat("  Yards allowed missing by category:\n")
      cat("    No offensive stats match: ", sum(missing_detail$yards_missing & !missing_detail$yards_source_found), "\n", sep = "")
      cat("    Unknown cause: ", sum(missing_detail$yards_missing & missing_detail$yards_source_found), "\n", sep = "")
    }
    if (missing_points > 0) {
      cat("  Points allowed missing by category:\n")
      cat("    Schedule score missing: ", sum(missing_detail$points_missing & !missing_detail$points_source_found), "\n", sep = "")
      cat("    Unknown cause: ", sum(missing_detail$points_missing & missing_detail$points_source_found), "\n", sep = "")
    }
  }
  
  # Availability flags for downstream diagnostics
  result$defense_data_available <- result$yards_source_found | result$points_source_found |
    (!is.na(result$def_sacks_defense_forced)) | (!is.na(result$def_tackles_for_loss_defense_forced))

  # Sanity checks: ensure defensive stats are non-zero for the majority of games (historical)
  check_nonzero_majority <- function(x, label) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0) {
      stop("Defensive stat '", label, "' has no non-NA values. Cannot proceed.")
    }
    prop_nonzero <- mean(vals > 0)
    if (!is.finite(prop_nonzero) || prop_nonzero <= 0.5) {
      stop("Defensive stat '", label, "' is non-zero in only ",
           round(prop_nonzero * 100, 1), "% of games; expected majority. Check ingestion.")
    }
  }

  check_nonzero_majority(result$def_sacks_defense_forced, "def_sacks_defense_forced")
  check_nonzero_majority(result$def_tackles_for_loss_defense_forced, "def_tackles_for_loss_defense_forced")
  check_nonzero_majority(result$def_interceptions_defense_caught, "def_interceptions_defense_caught")
  check_nonzero_majority(result$def_passes_defended_defense_forced, "def_passes_defended_defense_forced")
  check_nonzero_majority(result$def_pass_yards_defense_allowed, "def_pass_yards_defense_allowed")
  check_nonzero_majority(result$def_pass_attempts_defense_allowed, "def_pass_attempts_defense_allowed")
  
  message(paste("Built defensive stats for", nrow(result), "team-game records"))
  
  # Validation: Ensure no future-looking data
  # Check that defensive stats are computed from opponent offensive totals (not future games)
  # This is guaranteed by the join logic, but we document it here
  message("Validation: Defensive stats computed from opponent offensive performance (leakage-safe)")
  
  # Additional validation: Check for duplicate team-game combinations
  duplicates <- duplicated(result[, c("game_id", "defense_team")])
  if (any(duplicates)) {
    warning(paste("Found", sum(duplicates), "duplicate team-game combinations in defensive stats"))
  }
  
  # Build and write weekly defensive roll5 features (leakage-safe)
  if (!exists("build_team_defense_features")) {
    if (file.exists("R/features/build_team_defense_features.R")) {
      source("R/features/build_team_defense_features.R", local = TRUE)
    } else {
      stop("build_team_defense_features function not found. Cannot build weekly defensive features.")
    }
  }
  
  def_weekly_features <- build_team_defense_features(result)
  if (is.null(def_weekly_features) || nrow(def_weekly_features) == 0) {
    stop("Defensive weekly features are empty before writing. Cannot create defense_weekly_features.parquet.")
  }
  
  # Keep only rolling features and keys (roll1 + roll5) plus diagnostics
  roll_cols <- grep("_roll[0-9]+$", names(def_weekly_features), value = TRUE)
  required_cols <- c("season", "week", "defense_team")
  optional_diag_cols <- intersect(c("defense_data_available", "rolling_window_complete", "rolling_window_complete_roll1"), names(def_weekly_features))
  missing_required <- setdiff(required_cols, names(def_weekly_features))
  if (length(missing_required) > 0) {
    stop("Defensive weekly features missing required keys: ", paste(missing_required, collapse = ", "))
  }
  
  # Preserve defense_team and also provide team alias for compatibility
  def_weekly_features <- def_weekly_features[, c(required_cols, optional_diag_cols, roll_cols), drop = FALSE]
  def_weekly_features$team <- def_weekly_features$defense_team
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write defensive weekly features.")
  }
  
  defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  dir.create(dirname(defense_weekly_features_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(def_weekly_features, defense_weekly_features_path)
  
  return(result)
}


#' Create empty defense game stats data.frame with correct schema
#'
#' @return data.frame with zero rows and correct column types
empty_defense_game_stats_df <- function() {
  data.frame(
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    defense_team = character(0),
    def_sacks_defense_forced = integer(0),
    def_tackles_for_loss_defense_forced = integer(0),
    def_interceptions_defense_caught = integer(0),
    def_passes_defended_defense_forced = integer(0),
    def_rush_yards_defense_allowed = double(0),
    def_pass_yards_defense_allowed = double(0),
    def_rush_attempts_defense_allowed = integer(0),
    def_pass_attempts_defense_allowed = integer(0),
    def_total_yards_defense_allowed = double(0),
    def_yards_per_rush_defense_allowed = double(0),
    def_yards_per_pass_defense_allowed = double(0),
    def_points_defense_allowed = integer(0),
    stringsAsFactors = FALSE
  )
}

