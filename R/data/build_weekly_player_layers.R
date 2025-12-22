## Build Weekly Player Layers
##
## Provides functions to ingest nflreadr weekly player stats and materialize
## Layer 1 (player-week identity) and Layer 2 (position-specific stats) caches.

# Internal env for caching nflreadr downloads within a session.
.weekly_player_stats_env <- new.env(parent = emptyenv())

player_week_identity_path <- file.path("data", "cache", "player_week_identity.parquet")
rb_weekly_stats_path <- file.path("data", "cache", "rb_weekly_stats.parquet")
rb_weekly_features_path <- file.path("data", "processed", "rb_weekly_features.parquet")
wr_weekly_stats_path <- file.path("data", "cache", "wr_weekly_stats.parquet")
wr_weekly_features_path <- file.path("data", "processed", "wr_weekly_features.parquet")
te_weekly_stats_path <- file.path("data", "cache", "te_weekly_stats.parquet")
te_weekly_features_path <- file.path("data", "processed", "te_weekly_features.parquet")
player_directory_path <- file.path("data", "cache", "player_directory.parquet")

# Ensure cache helpers are available for build_game_key
if (!exists("build_game_key")) {
  if (file.exists("R/utils/cache_helpers.R")) {
    source("R/utils/cache_helpers.R", local = TRUE)
  } else {
    stop("R/utils/cache_helpers.R is required for building game keys")
  }
}

normalize_team_abbr <- function(team, season) {
  team <- toupper(trimws(as.character(team)))
  season <- as.integer(season)
  # Align legacy team codes to schedule-era abbreviations
  team <- ifelse(!is.na(season) & season <= 2019 & team == "LV", "OAK", team)
  team <- ifelse(!is.na(season) & season >= 2020 & team == "OAK", "LV", team)
  team <- ifelse(!is.na(season) & season <= 2016 & team == "LAC", "SD", team)
  team <- ifelse(!is.na(season) & season >= 2017 & team == "SD", "LAC", team)
  team <- ifelse(!is.na(season) & season <= 2015 & team == "LAR", "STL", team)
  team <- ifelse(!is.na(season) & season >= 2016 & team == "STL", "LAR", team)
  team <- ifelse(!is.na(season) & season <= 2015 & team == "LA", "STL", team)
  team <- ifelse(!is.na(season) & season >= 2016 & team == "LA", "LAR", team)
  team <- ifelse(team == "JAC", "JAX", team)
  team <- ifelse(!is.na(season) & season <= 2019 & team == "WSH", "WAS", team)
  team <- ifelse(!is.na(season) & season >= 2020 & team == "WAS", "WSH", team)
  team
}

build_schedule_team_lookup <- function(seasons) {
  if (!exists("load_schedules")) {
    if (file.exists("R/data/load_schedules.R")) {
      source("R/data/load_schedules.R", local = TRUE)
    } else {
      stop("R/data/load_schedules.R is required for schedule joins")
    }
  }
  schedules <- load_schedules(seasons, use_cache = TRUE)
  if (is.null(schedules) || nrow(schedules) == 0) {
    warning("Schedules unavailable; schedule-derived home/away fields will be NA.")
    return(NULL)
  }
  home_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    team = normalize_team_abbr(schedules$home_team, schedules$season),
    opponent = normalize_team_abbr(schedules$away_team, schedules$season),
    home_team = normalize_team_abbr(schedules$home_team, schedules$season),
    away_team = normalize_team_abbr(schedules$away_team, schedules$season),
    home_away = "HOME",
    is_home = 1L,
    stringsAsFactors = FALSE
  )
  away_games <- data.frame(
    game_id = schedules$game_id,
    season = schedules$season,
    week = schedules$week,
    gameday = schedules$gameday,
    team = normalize_team_abbr(schedules$away_team, schedules$season),
    opponent = normalize_team_abbr(schedules$home_team, schedules$season),
    home_team = normalize_team_abbr(schedules$home_team, schedules$season),
    away_team = normalize_team_abbr(schedules$away_team, schedules$season),
    home_away = "AWAY",
    is_home = 0L,
    stringsAsFactors = FALSE
  )
  rbind(home_games, away_games)
}

load_weekly_player_stats_from_nflreadr <- function(seasons, season_type) {
  if (missing(seasons) || length(seasons) == 0) {
    stop("seasons vector is required")
  }

  seasons <- sort(unique(as.integer(seasons)))
  seasons <- seasons[!is.na(seasons)]

  if (length(seasons) == 0) {
    stop("No valid seasons provided")
  }

  season_type_input <- if (!missing(season_type) && !is.null(season_type)) {
    toupper(trimws(season_type))
  } else {
    "REG"
  }

  cache_key <- paste0(paste(seasons, collapse = ","), "|", season_type_input)
  if (exists(cache_key, envir = .weekly_player_stats_env, inherits = FALSE)) {
    return(get(cache_key, envir = .weekly_player_stats_env))
  }

  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("Package 'nflreadr' is required. Install with install.packages('nflreadr').")
  }

  raw_stats <- nflreadr::load_player_stats(
    seasons = seasons,
    summary_level = "week"
  )

  if (is.null(raw_stats) || nrow(raw_stats) == 0) {
    stop("nflreadr returned no player stats for seasons: ", paste(seasons, collapse = ", "))
  }

  stats_df <- as.data.frame(raw_stats, stringsAsFactors = FALSE)

  # If a season_type column exists (varies by nflreadr version), filter on it:
  if ("season_type" %in% names(stats_df) && !is.null(season_type_input)) {
    available_types <- unique(stats_df$season_type)
    stats_df <- stats_df[stats_df$season_type == season_type_input, , drop = FALSE]
    if (nrow(stats_df) == 0) {
      stop("No player stats found after filtering for season_type='", season_type_input, 
           "'. Available season_type values: ", paste(available_types, collapse = ", "))
    }
  }

  assign(cache_key, stats_df, envir = .weekly_player_stats_env)
  stats_df
}

select_first_available <- function(df, candidates, default = NA) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) {
      return(df[[candidate]])
    }
  }
  rep(default, nrow(df))
}

safe_parse_date <- function(values, n) {
  if (length(values) == 0) {
    return(rep(as.Date(NA), n))
  }
  parsed <- tryCatch(as.Date(values), error = function(e) rep(as.Date(NA), n))
  if (length(parsed) != n) {
    parsed <- rep(parsed, length.out = n)
  }
  parsed
}

normalize_position <- function(pos) {
  result <- toupper(trimws(as.character(pos)))
  result <- ifelse(is.na(result) | result == "", NA_character_, result)
  result
}

normalize_home_away <- function(values) {
  cand <- toupper(trimws(as.character(values)))
  cand <- ifelse(is.na(cand) | cand == "", NA_character_, cand)
  home_values <- cand %in% c("HOME", "H", "VS", "V", "HOMEFIELD")
  away_values <- cand %in% c("AWAY", "A", "@", "AWAYFIELD")
  result <- rep(NA_character_, length(cand))
  result[home_values & !away_values] <- "HOME"
  result[away_values & !home_values] <- "AWAY"
  result
}

build_player_game_key <- function(season, week, team, opponent, game_date, game_id, player_id) {
  base <- if (exists("build_game_key")) {
    build_game_key(season, week, game_date, team, opponent, game_id)
  } else {
    paste(season, week, format(as.Date(game_date), "%Y%m%d"), team, opponent, sep = "_")
  }
  if (!is.na(player_id) && player_id != "") {
    paste(base, player_id, sep = "_")
  } else {
    base
  }
}

write_parquet_cache <- function(df, path, label) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write ", label, ". Install with install.packages('arrow').")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path)
  invisible(TRUE)
}

read_parquet_cache <- function(path, label, refresh_hint) {
  if (!file.exists(path)) {
    stop(label, " cache missing at ", path, ". Run ", refresh_hint, " to rebuild.")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read ", label, ". Install with install.packages('arrow').")
  }
  arrow::read_parquet(path)
}

build_player_week_identity <- function(seasons,
                                       season_type = "REG",
                                       write_cache = TRUE) {
  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)

  # Filter out non player-week rows (nflreadr includes season-level, team-level, or partial stat rows)
  # Keep only rows with valid player_id, position, season, week, and team
  player_id_vals <- select_first_available(stats, c("player_id", "gsis_id", "gsis_id_season"), NA)
  position_vals <- select_first_available(stats, c("position", "position_group"), NA)
  team_vals <- select_first_available(stats, c("team", "team_abbr", "team_name"), NA)
  stats$position <- position_vals
  
  valid_mask <- !is.na(player_id_vals) & player_id_vals != "" &
                !is.na(stats$position) & stats$position != "" &
                !is.na(stats$season) &
                !is.na(stats$week) &
                !is.na(team_vals) & team_vals != ""

  stats <- stats[valid_mask, , drop = FALSE]

  if (nrow(stats) == 0) {
    stop("No valid player-week rows found after filtering. nflreadr may have returned only aggregated rows.")
  }

  # Restrict to offensive player-game rows and require evidence of participation
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for coalesce when filtering player-week rows.")
  }
  stats$position <- toupper(stats$position)
  stats <- stats[stats$position %in% c("QB", "RB", "WR", "TE", "K"), , drop = FALSE]

  played <- (
    dplyr::coalesce(stats$carries, 0) > 0 |
    dplyr::coalesce(stats$targets, 0) > 0 |
    dplyr::coalesce(stats$pass_attempts, 0) > 0 |
    dplyr::coalesce(stats$receptions, 0) > 0 |
    dplyr::coalesce(stats$rushing_yards, 0) > 0 |
    dplyr::coalesce(stats$receiving_yards, 0) > 0 |
    dplyr::coalesce(stats$passing_yards, 0) > 0
  )
  stats <- stats[played, , drop = FALSE]

  player_ids <- as.character(select_first_available(stats, c("player_id", "gsis_id", "gsis_id_season"), NA))
  player_names <- as.character(select_first_available(stats, c("player_name", "player_display_name", "name"), NA))
  teams <- normalize_team_abbr(select_first_available(stats, c("team", "team_abbr", "team_name"), NA), stats$season)
  opponents <- normalize_team_abbr(select_first_available(stats, c("opponent_team", "opponent", "opp_team", "defteam"), NA), stats$season)
  game_dates <- safe_parse_date(select_first_available(stats, c("gameday", "game_date", "date"), NA), nrow(stats))
  home_aways <- normalize_home_away(select_first_available(stats, c("home_away", "location", "game_location"), NA))
  season_types <- if ("season_type" %in% names(stats)) {
    as.character(stats$season_type)
  } else {
    rep(toupper(trimws(season_type)), nrow(stats))
  }

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  seasons_vec <- as.integer(stats$season)
  weeks_vec <- as.integer(stats$week)
  game_ids <- as.character(select_first_available(stats, c("game_id"), NA))
  team_ids <- as.character(select_first_available(stats, c("team_id", "team_id_alt"), NA))

  identity <- data.frame(
    player_id = player_ids,
    player_name = player_names,
    team = teams,
    opponent = opponents,
    season = seasons_vec,
    week = weeks_vec,
    position = positions,
    game_date = game_dates,
    home_away = home_aways,
    season_type = season_types,
    team_id = team_ids,
    game_id = game_ids,
    stringsAsFactors = FALSE
  )

  identity$season <- as.integer(identity$season)
  identity$week <- as.integer(identity$week)
  identity$game_key <- mapply(
    build_player_game_key,
    season = identity$season,
    week = identity$week,
    team = identity$team,
    opponent = identity$opponent,
    game_date = identity$game_date,
    game_id = identity$game_id,
    player_id = identity$player_id,
    USE.NAMES = FALSE
  )

  identity$position <- normalize_position(identity$position)
  valid_positions <- c("QB", "RB", "WR", "TE", "K")
  invalid_mask <- is.na(identity$position) | !identity$position %in% valid_positions

  required_mask <- is.na(identity$season) |
    is.na(identity$week) |
    identity$team == "" |
    is.na(identity$team) |
    is.na(identity$player_id) |
    identity$player_id == "" |
    invalid_mask

  if (any(required_mask)) {
    stop("Found ", sum(required_mask), " rows missing required identity columns. ",
         "Ensure nflreadr returned season/week/team/player_id/position for each player-game.")
  }

  identity$gameday <- identity$game_date
  identity <- identity[order(identity$player_id, identity$season, identity$week, identity$game_date), ]
  identity <- identity[!duplicated(identity[, c("player_id", "game_key")]), ]

  # Schedule-derived home/away context (no string parsing)
  schedule_lookup <- build_schedule_team_lookup(seasons)
  if (!is.null(schedule_lookup)) {
    identity <- merge(
      identity,
      schedule_lookup[, c("season", "week", "team", "opponent", "game_id",
                          "gameday", "home_team", "away_team", "home_away", "is_home"),
                      drop = FALSE],
      by = c("season", "week", "team", "opponent"),
      all.x = TRUE,
      suffixes = c("", "_sched")
    )
    if ("game_id_sched" %in% names(identity)) {
      identity$game_id <- ifelse(
        is.na(identity$game_id) | identity$game_id == "",
        identity$game_id_sched,
        identity$game_id
      )
    }
    if ("gameday_sched" %in% names(identity)) {
      identity$gameday <- ifelse(is.na(identity$gameday), identity$gameday_sched, identity$gameday)
      identity$game_date <- ifelse(is.na(identity$game_date), identity$gameday_sched, identity$game_date)
    }
    if ("home_away_sched" %in% names(identity)) {
      identity$home_away <- identity$home_away_sched
    }
    if ("is_home" %in% names(identity)) {
      identity$is_home <- as.integer(identity$is_home)
    }
    # Fallback: fill missing schedule context by game_id + team
    if (any(is.na(identity$is_home)) && "game_id" %in% names(identity)) {
      schedule_by_game <- unique(schedule_lookup[, c("game_id", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      identity <- merge(
        identity,
        schedule_by_game,
        by = c("game_id", "team"),
        all.x = TRUE,
        suffixes = c("", "_game")
      )
      identity$is_home <- ifelse(is.na(identity$is_home), identity$is_home_game, identity$is_home)
      identity$home_away <- ifelse(is.na(identity$home_away), identity$home_away_game, identity$home_away)
      identity$opponent <- ifelse(is.na(identity$opponent), identity$opponent_game, identity$opponent)
      identity$home_team <- ifelse(is.na(identity$home_team), identity$home_team_game, identity$home_team)
      identity$away_team <- ifelse(is.na(identity$away_team), identity$away_team_game, identity$away_team)
      identity$gameday <- ifelse(is.na(identity$gameday), identity$gameday_game, identity$gameday)
      identity$game_date <- ifelse(is.na(identity$game_date), identity$gameday_game, identity$game_date)
    }
    # Fallback: fill missing schedule context by season/week/team
    if (any(is.na(identity$is_home))) {
      schedule_by_team <- unique(schedule_lookup[, c("season", "week", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      identity <- merge(
        identity,
        schedule_by_team,
        by = c("season", "week", "team"),
        all.x = TRUE,
        suffixes = c("", "_team")
      )
      identity$is_home <- ifelse(is.na(identity$is_home), identity$is_home_team, identity$is_home)
      identity$home_away <- ifelse(is.na(identity$home_away), identity$home_away_team, identity$home_away)
      identity$opponent <- ifelse(is.na(identity$opponent), identity$opponent_team, identity$opponent)
      identity$home_team <- ifelse(is.na(identity$home_team), identity$home_team_team, identity$home_team)
      identity$away_team <- ifelse(is.na(identity$away_team), identity$away_team_team, identity$away_team)
      identity$gameday <- ifelse(is.na(identity$gameday), identity$gameday_team, identity$gameday)
      identity$game_date <- ifelse(is.na(identity$game_date), identity$gameday_team, identity$game_date)
    }
    helper_cols <- grep("(_sched)$", names(identity), value = TRUE)
    helper_cols <- c(helper_cols, grep("(_game)$", names(identity), value = TRUE))
    helper_cols <- c(helper_cols, grep("(_team)$", names(identity), value = TRUE))
    if (length(helper_cols) > 0) {
      identity <- identity[, setdiff(names(identity), helper_cols), drop = FALSE]
    }
  }

  rownames(identity) <- NULL

  if (write_cache) {
    write_parquet_cache(identity, player_week_identity_path, "player week identity")
  }

  identity
}

build_rb_weekly_stats <- function(seasons,
                                  season_type = "REG",
                                  write_cache = TRUE) {
  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  stats$position <- positions

  rb_mask <- stats$position == "RB"
  if (sum(rb_mask, na.rm = TRUE) == 0) {
    stop("No RB rows found for seasons: ", paste(seasons, collapse = ", "))
  }

  rb_stats <- stats[rb_mask, , drop = FALSE]

  # Normalize RB stat column names for downstream feature code
  rename_map <- c(
    rushing_yards   = "rush_yards",
    rushing_tds     = "rush_tds",
    receiving_yards = "rec_yards",
    receiving_tds   = "rec_tds"
  )
  for (src in names(rename_map)) {
    if (src %in% names(rb_stats)) {
      rb_stats[[rename_map[[src]]]] <- rb_stats[[src]]
    }
  }
  for (col in c("rush_yards", "rush_tds", "rec_yards", "rec_tds")) {
    if (!col %in% names(rb_stats)) {
      rb_stats[[col]] <- 0
    }
  }

  player_ids <- as.character(select_first_available(rb_stats, c("player_id", "gsis_id"), NA))
  player_names <- as.character(select_first_available(rb_stats, c("player_name", "player_display_name", "name"), NA))
  teams <- normalize_team_abbr(select_first_available(rb_stats, c("team", "team_abbr", "team_name"), NA), rb_stats$season)
  opponents <- normalize_team_abbr(select_first_available(rb_stats, c("opponent_team", "opponent", "opp_team", "defteam"), NA), rb_stats$season)
  game_dates <- safe_parse_date(select_first_available(rb_stats, c("gameday", "game_date", "date"), NA), nrow(rb_stats))
  home_aways <- normalize_home_away(select_first_available(rb_stats, c("home_away", "location", "game_location"), NA))
  game_ids <- as.character(select_first_available(rb_stats, c("game_id"), NA))
  team_ids <- as.character(select_first_available(rb_stats, c("team_id", "team_id_alt"), NA))
  season_types <- if ("season_type" %in% names(rb_stats)) {
    as.character(rb_stats$season_type)
  } else {
    rep(toupper(trimws(season_type)), nrow(rb_stats))
  }

  rushing_yards <- as.double(select_first_available(rb_stats, c("rushing_yards", "rush_yards", "rush_yards"), 0))
  rushing_tds <- as.integer(select_first_available(rb_stats, c("rushing_tds", "rush_tds"), 0))
  carries <- as.integer(select_first_available(rb_stats, c("carries", "rush_attempts"), 0))
  targets <- as.integer(select_first_available(rb_stats, c("targets"), 0))
  receptions <- as.integer(select_first_available(rb_stats, c("receptions", "rec", "rec_catches"), 0))
  receiving_yards <- as.double(select_first_available(rb_stats, c("receiving_yards", "rec_yards", "receiving_yards"), 0))
  receiving_tds <- as.integer(select_first_available(rb_stats, c("receiving_tds", "rec_tds"), 0))

  rb_dataset <- data.frame(
    player_id = player_ids,
    player_name = player_names,
    season = as.integer(rb_stats$season),
    week = as.integer(rb_stats$week),
    team = teams,
    opponent = opponents,
    home_away = home_aways,
    season_type = season_types,
    team_id = team_ids,
    game_id = game_ids,
    game_date = game_dates,
    carries = carries,
    rushing_yards = rushing_yards,
    rushing_tds = rushing_tds,
    targets = targets,
    receptions = receptions,
    receiving_yards = receiving_yards,
    receiving_tds = receiving_tds,
    position = "RB",
    stringsAsFactors = FALSE
  )

  # Schedule-derived home/away context (no string parsing)
  schedule_lookup <- build_schedule_team_lookup(seasons)
  if (!is.null(schedule_lookup)) {
    rb_dataset <- merge(
      rb_dataset,
      schedule_lookup[, c("season", "week", "team", "opponent", "game_id",
                          "gameday", "home_team", "away_team", "home_away", "is_home"),
                      drop = FALSE],
      by = c("season", "week", "team", "opponent"),
      all.x = TRUE,
      suffixes = c("", "_sched")
    )
    if ("game_id_sched" %in% names(rb_dataset)) {
      rb_dataset$game_id <- ifelse(
        is.na(rb_dataset$game_id) | rb_dataset$game_id == "",
        rb_dataset$game_id_sched,
        rb_dataset$game_id
      )
    }
    if ("gameday_sched" %in% names(rb_dataset)) {
      rb_dataset$game_date <- ifelse(is.na(rb_dataset$game_date), rb_dataset$gameday_sched, rb_dataset$game_date)
      rb_dataset$gameday <- ifelse(is.na(rb_dataset$gameday), rb_dataset$gameday_sched, rb_dataset$gameday)
    }
    if ("home_away_sched" %in% names(rb_dataset)) {
      rb_dataset$home_away <- rb_dataset$home_away_sched
    }
    rb_dataset$is_home <- if ("is_home" %in% names(rb_dataset)) as.integer(rb_dataset$is_home) else NA_integer_
    # Fallback: fill missing schedule context by game_id + team
    if (any(is.na(rb_dataset$is_home)) && "game_id" %in% names(rb_dataset)) {
      schedule_by_game <- unique(schedule_lookup[, c("game_id", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      rb_dataset <- merge(
        rb_dataset,
        schedule_by_game,
        by = c("game_id", "team"),
        all.x = TRUE,
        suffixes = c("", "_game")
      )
      rb_dataset$is_home <- ifelse(is.na(rb_dataset$is_home), rb_dataset$is_home_game, rb_dataset$is_home)
      rb_dataset$home_away <- ifelse(is.na(rb_dataset$home_away), rb_dataset$home_away_game, rb_dataset$home_away)
      rb_dataset$opponent <- ifelse(is.na(rb_dataset$opponent), rb_dataset$opponent_game, rb_dataset$opponent)
      rb_dataset$home_team <- ifelse(is.na(rb_dataset$home_team), rb_dataset$home_team_game, rb_dataset$home_team)
      rb_dataset$away_team <- ifelse(is.na(rb_dataset$away_team), rb_dataset$away_team_game, rb_dataset$away_team)
      rb_dataset$gameday <- ifelse(is.na(rb_dataset$gameday), rb_dataset$gameday_game, rb_dataset$gameday)
      rb_dataset$game_date <- ifelse(is.na(rb_dataset$game_date), rb_dataset$gameday_game, rb_dataset$game_date)
    }
    # Fallback: fill missing schedule context by season/week/team
    if (any(is.na(rb_dataset$is_home))) {
      schedule_by_team <- unique(schedule_lookup[, c("season", "week", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      rb_dataset <- merge(
        rb_dataset,
        schedule_by_team,
        by = c("season", "week", "team"),
        all.x = TRUE,
        suffixes = c("", "_team")
      )
      rb_dataset$is_home <- ifelse(is.na(rb_dataset$is_home), rb_dataset$is_home_team, rb_dataset$is_home)
      rb_dataset$home_away <- ifelse(is.na(rb_dataset$home_away), rb_dataset$home_away_team, rb_dataset$home_away)
      rb_dataset$opponent <- ifelse(is.na(rb_dataset$opponent), rb_dataset$opponent_team, rb_dataset$opponent)
      rb_dataset$home_team <- ifelse(is.na(rb_dataset$home_team), rb_dataset$home_team_team, rb_dataset$home_team)
      rb_dataset$away_team <- ifelse(is.na(rb_dataset$away_team), rb_dataset$away_team_team, rb_dataset$away_team)
      rb_dataset$gameday <- ifelse(is.na(rb_dataset$gameday), rb_dataset$gameday_team, rb_dataset$gameday)
      rb_dataset$game_date <- ifelse(is.na(rb_dataset$game_date), rb_dataset$gameday_team, rb_dataset$game_date)
    }
    helper_cols <- grep("(_sched)$", names(rb_dataset), value = TRUE)
    helper_cols <- c(helper_cols, grep("(_game)$", names(rb_dataset), value = TRUE))
    helper_cols <- c(helper_cols, grep("(_team)$", names(rb_dataset), value = TRUE))
    if (length(helper_cols) > 0) {
      rb_dataset <- rb_dataset[, setdiff(names(rb_dataset), helper_cols), drop = FALSE]
    }
  } else {
    rb_dataset$is_home <- NA_integer_
  }

  # Canonical RB stat aliases required by feature code
  rb_dataset$rush_yards <- rb_dataset$rushing_yards
  rb_dataset$rush_tds   <- rb_dataset$rushing_tds
  rb_dataset$rec_yards  <- rb_dataset$receiving_yards
  rb_dataset$rec_tds    <- rb_dataset$receiving_tds

  # Ensure no NA values in canonical columns
  rb_dataset$rush_yards[is.na(rb_dataset$rush_yards)] <- 0
  rb_dataset$rush_tds[is.na(rb_dataset$rush_tds)] <- 0L
  rb_dataset$rec_yards[is.na(rb_dataset$rec_yards)] <- 0
  rb_dataset$rec_tds[is.na(rb_dataset$rec_tds)] <- 0L

  rb_dataset$game_key <- mapply(
    build_player_game_key,
    season = rb_dataset$season,
    week = rb_dataset$week,
    team = rb_dataset$team,
    opponent = rb_dataset$opponent,
    game_date = rb_dataset$game_date,
    game_id = rb_dataset$game_id,
    player_id = rb_dataset$player_id,
    USE.NAMES = FALSE
  )

  rb_dataset$gameday <- rb_dataset$game_date
  rb_dataset <- rb_dataset[order(rb_dataset$player_id, rb_dataset$season, rb_dataset$week, rb_dataset$game_date), ]
  rb_dataset <- rb_dataset[!duplicated(rb_dataset[, c("player_id", "game_key")]), ]

  count_cols <- c("carries", "targets", "receptions", "rushing_tds", "receiving_tds")
  for (col in count_cols) {
    rb_dataset[[col]] <- ifelse(is.na(rb_dataset[[col]]), 0L, rb_dataset[[col]])
  }
  rb_dataset$rushing_yards <- ifelse(is.na(rb_dataset$rushing_yards), 0, rb_dataset$rushing_yards)
  rb_dataset$receiving_yards <- ifelse(is.na(rb_dataset$receiving_yards), 0, rb_dataset$receiving_yards)

  # Canonical opponent / defense mapping
  rb_dataset$defense_team <- rb_dataset$opponent

  # Drop rows without real opponents (bye / inactive / malformed)
  invalid <- is.na(rb_dataset$defense_team) | rb_dataset$defense_team == ""
  if (any(invalid)) {
    warning(
      sprintf(
        "Dropping %d RB weekly rows without opponent (bye/inactive)",
        sum(invalid)
      ),
      call. = FALSE
    )
    rb_dataset <- rb_dataset[!invalid, , drop = FALSE]
  }

  stopifnot(!any(is.na(rb_dataset$defense_team)))

  rownames(rb_dataset) <- NULL

  if (write_cache) {
    write_parquet_cache(rb_dataset, rb_weekly_stats_path, "RB weekly stats")
  }

  rb_dataset
}

build_wr_weekly_stats <- function(seasons,
                                  season_type = "REG",
                                  write_cache = TRUE) {
  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  stats$position <- positions

  wr_mask <- stats$position == "WR"
  if (sum(wr_mask, na.rm = TRUE) == 0) {
    stop("No WR rows found for seasons: ", paste(seasons, collapse = ", "))
  }

  wr_stats <- stats[wr_mask, , drop = FALSE]

  player_ids <- as.character(select_first_available(wr_stats, c("player_id", "gsis_id"), NA))
  player_names <- as.character(select_first_available(wr_stats, c("player_name", "player_display_name", "name"), NA))
  teams <- normalize_team_abbr(select_first_available(wr_stats, c("team", "team_abbr", "team_name"), NA), wr_stats$season)
  opponents <- normalize_team_abbr(select_first_available(wr_stats, c("opponent_team", "opponent", "opp_team", "defteam"), NA), wr_stats$season)
  game_dates <- safe_parse_date(select_first_available(wr_stats, c("gameday", "game_date", "date"), NA), nrow(wr_stats))
  home_aways <- normalize_home_away(select_first_available(wr_stats, c("home_away", "location", "game_location"), NA))
  game_ids <- as.character(select_first_available(wr_stats, c("game_id"), NA))
  team_ids <- as.character(select_first_available(wr_stats, c("team_id", "team_id_alt"), NA))
  season_types <- if ("season_type" %in% names(wr_stats)) {
    as.character(wr_stats$season_type)
  } else {
    rep(toupper(trimws(season_type)), nrow(wr_stats))
  }

  targets <- as.integer(select_first_available(wr_stats, c("targets"), 0))
  receptions <- as.integer(select_first_available(wr_stats, c("receptions", "rec", "rec_catches"), 0))
  receiving_yards <- as.double(select_first_available(wr_stats, c("receiving_yards", "rec_yards"), 0))
  receiving_tds <- as.integer(select_first_available(wr_stats, c("receiving_tds", "rec_tds"), 0))
  air_yards <- as.double(select_first_available(wr_stats, c("air_yards", "rec_air_yards", "receiving_air_yards"), NA))

  wr_dataset <- data.frame(
    player_id = player_ids,
    player_name = player_names,
    season = as.integer(wr_stats$season),
    week = as.integer(wr_stats$week),
    team = teams,
    opponent = opponents,
    home_away = home_aways,
    season_type = season_types,
    team_id = team_ids,
    game_id = game_ids,
    game_date = game_dates,
    targets = targets,
    receptions = receptions,
    receiving_yards = receiving_yards,
    receiving_tds = receiving_tds,
    air_yards = air_yards,
    position = "WR",
    stringsAsFactors = FALSE
  )

  # Schedule-derived home/away context (no string parsing)
  schedule_lookup <- build_schedule_team_lookup(seasons)
  if (!is.null(schedule_lookup)) {
    wr_dataset <- merge(
      wr_dataset,
      schedule_lookup[, c("season", "week", "team", "opponent", "game_id",
                          "gameday", "home_team", "away_team", "home_away", "is_home"),
                      drop = FALSE],
      by = c("season", "week", "team", "opponent"),
      all.x = TRUE,
      suffixes = c("", "_sched")
    )
    if ("game_id_sched" %in% names(wr_dataset)) {
      wr_dataset$game_id <- ifelse(
        is.na(wr_dataset$game_id) | wr_dataset$game_id == "",
        wr_dataset$game_id_sched,
        wr_dataset$game_id
      )
    }
    if ("gameday_sched" %in% names(wr_dataset)) {
      wr_dataset$game_date <- ifelse(is.na(wr_dataset$game_date), wr_dataset$gameday_sched, wr_dataset$game_date)
      wr_dataset$gameday <- ifelse(is.na(wr_dataset$gameday), wr_dataset$gameday_sched, wr_dataset$gameday)
    }
    if ("home_away_sched" %in% names(wr_dataset)) {
      wr_dataset$home_away <- wr_dataset$home_away_sched
    }
    wr_dataset$is_home <- if ("is_home" %in% names(wr_dataset)) as.integer(wr_dataset$is_home) else NA_integer_
    # Fallback: fill missing schedule context by game_id + team
    if (any(is.na(wr_dataset$is_home)) && "game_id" %in% names(wr_dataset)) {
      schedule_by_game <- unique(schedule_lookup[, c("game_id", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      wr_dataset <- merge(
        wr_dataset,
        schedule_by_game,
        by = c("game_id", "team"),
        all.x = TRUE,
        suffixes = c("", "_game")
      )
      wr_dataset$is_home <- ifelse(is.na(wr_dataset$is_home), wr_dataset$is_home_game, wr_dataset$is_home)
      wr_dataset$home_away <- ifelse(is.na(wr_dataset$home_away), wr_dataset$home_away_game, wr_dataset$home_away)
      wr_dataset$opponent <- ifelse(is.na(wr_dataset$opponent), wr_dataset$opponent_game, wr_dataset$opponent)
      wr_dataset$home_team <- ifelse(is.na(wr_dataset$home_team), wr_dataset$home_team_game, wr_dataset$home_team)
      wr_dataset$away_team <- ifelse(is.na(wr_dataset$away_team), wr_dataset$away_team_game, wr_dataset$away_team)
      wr_dataset$gameday <- ifelse(is.na(wr_dataset$gameday), wr_dataset$gameday_game, wr_dataset$gameday)
      wr_dataset$game_date <- ifelse(is.na(wr_dataset$game_date), wr_dataset$gameday_game, wr_dataset$game_date)
    }
    # Fallback: fill missing schedule context by season/week/team
    if (any(is.na(wr_dataset$is_home))) {
      schedule_by_team <- unique(schedule_lookup[, c("season", "week", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      wr_dataset <- merge(
        wr_dataset,
        schedule_by_team,
        by = c("season", "week", "team"),
        all.x = TRUE,
        suffixes = c("", "_team")
      )
      wr_dataset$is_home <- ifelse(is.na(wr_dataset$is_home), wr_dataset$is_home_team, wr_dataset$is_home)
      wr_dataset$home_away <- ifelse(is.na(wr_dataset$home_away), wr_dataset$home_away_team, wr_dataset$home_away)
      wr_dataset$opponent <- ifelse(is.na(wr_dataset$opponent), wr_dataset$opponent_team, wr_dataset$opponent)
      wr_dataset$home_team <- ifelse(is.na(wr_dataset$home_team), wr_dataset$home_team_team, wr_dataset$home_team)
      wr_dataset$away_team <- ifelse(is.na(wr_dataset$away_team), wr_dataset$away_team_team, wr_dataset$away_team)
      wr_dataset$gameday <- ifelse(is.na(wr_dataset$gameday), wr_dataset$gameday_team, wr_dataset$gameday)
      wr_dataset$game_date <- ifelse(is.na(wr_dataset$game_date), wr_dataset$gameday_team, wr_dataset$game_date)
    }
    helper_cols <- grep("(_sched)$", names(wr_dataset), value = TRUE)
    helper_cols <- c(helper_cols, grep("(_game)$", names(wr_dataset), value = TRUE))
    helper_cols <- c(helper_cols, grep("(_team)$", names(wr_dataset), value = TRUE))
    if (length(helper_cols) > 0) {
      wr_dataset <- wr_dataset[, setdiff(names(wr_dataset), helper_cols), drop = FALSE]
    }
  } else {
    wr_dataset$is_home <- NA_integer_
  }

  wr_dataset$game_key <- mapply(
    build_player_game_key,
    season = wr_dataset$season,
    week = wr_dataset$week,
    team = wr_dataset$team,
    opponent = wr_dataset$opponent,
    game_date = wr_dataset$game_date,
    game_id = wr_dataset$game_id,
    player_id = wr_dataset$player_id,
    USE.NAMES = FALSE
  )

  wr_dataset$gameday <- wr_dataset$game_date
  wr_dataset <- wr_dataset[order(wr_dataset$player_id, wr_dataset$season, wr_dataset$week, wr_dataset$game_date), ]
  wr_dataset <- wr_dataset[!duplicated(wr_dataset[, c("player_id", "game_key")]), ]

  count_cols <- c("targets", "receptions", "receiving_tds")
  for (col in count_cols) {
    wr_dataset[[col]] <- ifelse(is.na(wr_dataset[[col]]), 0L, wr_dataset[[col]])
  }
  wr_dataset$receiving_yards <- ifelse(is.na(wr_dataset$receiving_yards), 0, wr_dataset$receiving_yards)

  # Canonical opponent / defense mapping
  wr_dataset$defense_team <- wr_dataset$opponent

  # Drop rows without real opponents (bye / inactive / malformed)
  invalid <- is.na(wr_dataset$defense_team) | wr_dataset$defense_team == ""
  if (any(invalid)) {
    warning(
      sprintf(
        "Dropping %d WR weekly rows without opponent (bye/inactive)",
        sum(invalid)
      ),
      call. = FALSE
    )
    wr_dataset <- wr_dataset[!invalid, , drop = FALSE]
  }

  stopifnot(!any(is.na(wr_dataset$defense_team)))

  rownames(wr_dataset) <- NULL

  if (write_cache) {
    write_parquet_cache(wr_dataset, wr_weekly_stats_path, "WR weekly stats")
  }

  wr_dataset
}

build_te_weekly_stats <- function(seasons,
                                  season_type = "REG",
                                  write_cache = TRUE) {
  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  stats$position <- positions

  te_mask <- stats$position == "TE"
  if (sum(te_mask, na.rm = TRUE) == 0) {
    stop("No TE rows found for seasons: ", paste(seasons, collapse = ", "))
  }

  te_stats <- stats[te_mask, , drop = FALSE]

  player_ids <- as.character(select_first_available(te_stats, c("player_id", "gsis_id"), NA))
  player_names <- as.character(select_first_available(te_stats, c("player_name", "player_display_name", "name"), NA))
  teams <- normalize_team_abbr(select_first_available(te_stats, c("team", "team_abbr", "team_name"), NA), te_stats$season)
  opponents <- normalize_team_abbr(select_first_available(te_stats, c("opponent_team", "opponent", "opp_team", "defteam"), NA), te_stats$season)
  game_dates <- safe_parse_date(select_first_available(te_stats, c("gameday", "game_date", "date"), NA), nrow(te_stats))
  home_aways <- normalize_home_away(select_first_available(te_stats, c("home_away", "location", "game_location"), NA))
  game_ids <- as.character(select_first_available(te_stats, c("game_id"), NA))
  team_ids <- as.character(select_first_available(te_stats, c("team_id", "team_id_alt"), NA))
  season_types <- if ("season_type" %in% names(te_stats)) {
    as.character(te_stats$season_type)
  } else {
    rep(toupper(trimws(season_type)), nrow(te_stats))
  }

  targets <- as.integer(select_first_available(te_stats, c("targets"), 0))
  receptions <- as.integer(select_first_available(te_stats, c("receptions", "rec", "rec_catches"), 0))
  receiving_yards <- as.double(select_first_available(te_stats, c("receiving_yards", "rec_yards"), 0))
  receiving_tds <- as.integer(select_first_available(te_stats, c("receiving_tds", "rec_tds"), 0))
  air_yards <- as.double(select_first_available(te_stats, c("air_yards", "rec_air_yards", "receiving_air_yards"), NA))

  te_dataset <- data.frame(
    player_id = player_ids,
    player_name = player_names,
    season = as.integer(te_stats$season),
    week = as.integer(te_stats$week),
    team = teams,
    opponent = opponents,
    home_away = home_aways,
    season_type = season_types,
    team_id = team_ids,
    game_id = game_ids,
    game_date = game_dates,
    targets = targets,
    receptions = receptions,
    receiving_yards = receiving_yards,
    receiving_tds = receiving_tds,
    air_yards = air_yards,
    position = "TE",
    stringsAsFactors = FALSE
  )

  # Schedule-derived home/away context (no string parsing)
  schedule_lookup <- build_schedule_team_lookup(seasons)
  if (!is.null(schedule_lookup)) {
    te_dataset <- merge(
      te_dataset,
      schedule_lookup[, c("season", "week", "team", "opponent", "game_id",
                          "gameday", "home_team", "away_team", "home_away", "is_home"),
                      drop = FALSE],
      by = c("season", "week", "team", "opponent"),
      all.x = TRUE,
      suffixes = c("", "_sched")
    )
    if ("game_id_sched" %in% names(te_dataset)) {
      te_dataset$game_id <- ifelse(
        is.na(te_dataset$game_id) | te_dataset$game_id == "",
        te_dataset$game_id_sched,
        te_dataset$game_id
      )
    }
    if ("gameday_sched" %in% names(te_dataset)) {
      te_dataset$game_date <- ifelse(is.na(te_dataset$game_date), te_dataset$gameday_sched, te_dataset$game_date)
      te_dataset$gameday <- ifelse(is.na(te_dataset$gameday), te_dataset$gameday_sched, te_dataset$gameday)
    }
    if ("home_away_sched" %in% names(te_dataset)) {
      te_dataset$home_away <- te_dataset$home_away_sched
    }
    te_dataset$is_home <- if ("is_home" %in% names(te_dataset)) as.integer(te_dataset$is_home) else NA_integer_
    # Fallback: fill missing schedule context by game_id + team
    if (any(is.na(te_dataset$is_home)) && "game_id" %in% names(te_dataset)) {
      schedule_by_game <- unique(schedule_lookup[, c("game_id", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      te_dataset <- merge(
        te_dataset,
        schedule_by_game,
        by = c("game_id", "team"),
        all.x = TRUE,
        suffixes = c("", "_game")
      )
      te_dataset$is_home <- ifelse(is.na(te_dataset$is_home), te_dataset$is_home_game, te_dataset$is_home)
      te_dataset$home_away <- ifelse(is.na(te_dataset$home_away), te_dataset$home_away_game, te_dataset$home_away)
      te_dataset$opponent <- ifelse(is.na(te_dataset$opponent), te_dataset$opponent_game, te_dataset$opponent)
      te_dataset$home_team <- ifelse(is.na(te_dataset$home_team), te_dataset$home_team_game, te_dataset$home_team)
      te_dataset$away_team <- ifelse(is.na(te_dataset$away_team), te_dataset$away_team_game, te_dataset$away_team)
      te_dataset$gameday <- ifelse(is.na(te_dataset$gameday), te_dataset$gameday_game, te_dataset$gameday)
      te_dataset$game_date <- ifelse(is.na(te_dataset$game_date), te_dataset$gameday_game, te_dataset$game_date)
    }
    # Fallback: fill missing schedule context by season/week/team
    if (any(is.na(te_dataset$is_home))) {
      schedule_by_team <- unique(schedule_lookup[, c("season", "week", "team", "opponent",
                                                     "home_team", "away_team", "home_away",
                                                     "is_home", "gameday"), drop = FALSE])
      te_dataset <- merge(
        te_dataset,
        schedule_by_team,
        by = c("season", "week", "team"),
        all.x = TRUE,
        suffixes = c("", "_team")
      )
      te_dataset$is_home <- ifelse(is.na(te_dataset$is_home), te_dataset$is_home_team, te_dataset$is_home)
      te_dataset$home_away <- ifelse(is.na(te_dataset$home_away), te_dataset$home_away_team, te_dataset$home_away)
      te_dataset$opponent <- ifelse(is.na(te_dataset$opponent), te_dataset$opponent_team, te_dataset$opponent)
      te_dataset$home_team <- ifelse(is.na(te_dataset$home_team), te_dataset$home_team_team, te_dataset$home_team)
      te_dataset$away_team <- ifelse(is.na(te_dataset$away_team), te_dataset$away_team_team, te_dataset$away_team)
      te_dataset$gameday <- ifelse(is.na(te_dataset$gameday), te_dataset$gameday_team, te_dataset$gameday)
      te_dataset$game_date <- ifelse(is.na(te_dataset$game_date), te_dataset$gameday_team, te_dataset$game_date)
    }
    helper_cols <- grep("(_sched)$", names(te_dataset), value = TRUE)
    helper_cols <- c(helper_cols, grep("(_game)$", names(te_dataset), value = TRUE))
    helper_cols <- c(helper_cols, grep("(_team)$", names(te_dataset), value = TRUE))
    if (length(helper_cols) > 0) {
      te_dataset <- te_dataset[, setdiff(names(te_dataset), helper_cols), drop = FALSE]
    }
  } else {
    te_dataset$is_home <- NA_integer_
  }

  te_dataset$game_key <- mapply(
    build_player_game_key,
    season = te_dataset$season,
    week = te_dataset$week,
    team = te_dataset$team,
    opponent = te_dataset$opponent,
    game_date = te_dataset$game_date,
    game_id = te_dataset$game_id,
    player_id = te_dataset$player_id,
    USE.NAMES = FALSE
  )

  te_dataset$gameday <- te_dataset$game_date
  te_dataset <- te_dataset[order(te_dataset$player_id, te_dataset$season, te_dataset$week, te_dataset$game_date), ]
  te_dataset <- te_dataset[!duplicated(te_dataset[, c("player_id", "game_key")]), ]

  count_cols <- c("targets", "receptions", "receiving_tds")
  for (col in count_cols) {
    te_dataset[[col]] <- ifelse(is.na(te_dataset[[col]]), 0L, te_dataset[[col]])
  }
  te_dataset$receiving_yards <- ifelse(is.na(te_dataset$receiving_yards), 0, te_dataset$receiving_yards)

  # Canonical opponent / defense mapping
  te_dataset$defense_team <- te_dataset$opponent

  # Drop rows without real opponents (bye / inactive / malformed)
  invalid <- is.na(te_dataset$defense_team) | te_dataset$defense_team == ""
  if (any(invalid)) {
    warning(
      sprintf(
        "Dropping %d TE weekly rows without opponent (bye/inactive)",
        sum(invalid)
      ),
      call. = FALSE
    )
    te_dataset <- te_dataset[!invalid, , drop = FALSE]
  }

  stopifnot(!any(is.na(te_dataset$defense_team)))

  rownames(te_dataset) <- NULL

  if (write_cache) {
    write_parquet_cache(te_dataset, te_weekly_stats_path, "TE weekly stats")
  }

  te_dataset
}

read_player_week_identity_cache <- function() {
  read_parquet_cache(player_week_identity_path, "Player week identity cache", "scripts/refresh_weekly_cache.R")
}

read_rb_weekly_stats_cache <- function() {
  read_parquet_cache(rb_weekly_stats_path, "RB weekly stats cache", "scripts/refresh_weekly_cache.R")
}

read_rb_weekly_features_cache <- function() {
  read_parquet_cache(rb_weekly_features_path, "RB weekly features cache", "scripts/refresh_weekly_cache.R")
}

read_wr_weekly_stats_cache <- function() {
  read_parquet_cache(wr_weekly_stats_path, "WR weekly stats cache", "scripts/refresh_weekly_cache.R")
}

read_wr_weekly_features_cache <- function() {
  read_parquet_cache(wr_weekly_features_path, "WR weekly features cache", "scripts/refresh_weekly_cache.R")
}

read_te_weekly_stats_cache <- function() {
  read_parquet_cache(te_weekly_stats_path, "TE weekly stats cache", "scripts/refresh_weekly_cache.R")
}

read_te_weekly_features_cache <- function() {
  read_parquet_cache(te_weekly_features_path, "TE weekly features cache", "scripts/refresh_weekly_cache.R")
}

#' Build player directory cache from nflreadr
#'
#' Creates a canonical player directory using nflreadr::load_players().
#' This provides full names and player_id mappings for name resolution.
#' player_id is the source of truth; weekly stats may use abbreviated names.
#'
#' @param write_cache Logical, if TRUE write to parquet cache (default TRUE)
#' @return data.frame with player directory columns
build_player_directory <- function(write_cache = TRUE) {
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("Package 'nflreadr' is required. Install with install.packages('nflreadr').")
  }
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with install.packages('arrow').")
  }
  
  cat("Loading player directory from nflreadr (live if available)...\n")
  cache_path <- player_directory_path
  players <- tryCatch(
    nflreadr::load_players(),
    error = function(e) {
      message("WARNING: nflreadr::load_players() failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(players) || nrow(players) == 0) {
    message("WARNING: Live player download unavailable.")
    
    if (file.exists(cache_path)) {
      message("Falling back to cached player directory: ", cache_path)
      players <- arrow::read_parquet(cache_path)
    } else {
      stop(
        "Player directory unavailable: live download failed and no cached copy exists.",
        call. = FALSE
      )
    }
  }
  
  # Required columns: player_id, full_name, first_name, last_name
  required_cols <- c("gsis_id", "display_name", "first_name", "last_name")
  
  # Map nflreadr column names to canonical names
  # gsis_id -> player_id
  # display_name -> full_name
  if (!"gsis_id" %in% names(players)) {
    # Try alternative column names
    if ("player_id" %in% names(players)) {
      players$gsis_id <- players$player_id
    } else if ("player_gsis_id" %in% names(players)) {
      players$gsis_id <- players$player_gsis_id
    } else {
      stop("Cannot find player_id column in nflreadr::load_players() output. ",
           "Expected: gsis_id, player_id, or player_gsis_id")
    }
  }
  
  if (!"display_name" %in% names(players)) {
    if ("full_name" %in% names(players)) {
      players$display_name <- players$full_name
    } else if ("name" %in% names(players)) {
      players$display_name <- players$name
    } else {
      stop("Cannot find full_name column in nflreadr::load_players() output. ",
           "Expected: display_name, full_name, or name")
    }
  }
  
  if (!"first_name" %in% names(players)) {
    stop("Cannot find first_name column in nflreadr::load_players() output.")
  }
  
  if (!"last_name" %in% names(players)) {
    stop("Cannot find last_name column in nflreadr::load_players() output.")
  }
  
  # Build canonical player directory
  player_dir <- data.frame(
    player_id = as.character(players$gsis_id),
    full_name = as.character(players$display_name),
    first_name = as.character(players$first_name),
    last_name = as.character(players$last_name),
    stringsAsFactors = FALSE
  )

  # Optional draft metadata (leave NA when unavailable)
  draft_round <- select_first_available(players, c("draft_round"), NA)
  draft_pick_overall <- select_first_available(players, c("draft_pick_overall", "draft_pick"), NA)
  player_dir$draft_round <- as.integer(draft_round)
  player_dir$draft_pick_overall <- as.integer(draft_pick_overall)
  
  # Remove rows with missing player_id or full_name
  player_dir <- player_dir[
    !is.na(player_dir$player_id) & player_dir$player_id != "" &
    !is.na(player_dir$full_name) & player_dir$full_name != "",
    , drop = FALSE
  ]
  
  if (nrow(player_dir) == 0) {
    stop("Player directory built with zero valid rows. Check nflreadr::load_players() output.")
  }
  
  # Remove duplicates (keep first occurrence)
  player_dir <- player_dir[!duplicated(player_dir$player_id), , drop = FALSE]
  
  # Add canonical name for matching
  # Define canonicalize_name helper if not available
  canonicalize_name_local <- function(x) {
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
  
  player_dir$canonical_name <- canonicalize_name_local(player_dir$full_name)
  
  # Order by player_id for consistency
  player_dir <- player_dir[order(player_dir$player_id), ]
  rownames(player_dir) <- NULL
  
  if (write_cache) {
    dir.create(dirname(player_directory_path), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      {
        arrow::write_parquet(player_dir, player_directory_path)
        cat("  Wrote player directory cache with", nrow(player_dir), "players\n")
      },
      error = function(e) {
        message("WARNING: Failed to write player directory cache: ", e$message)
      }
    )
  }
  
  player_dir
}

read_player_directory_cache <- function() {
  read_parquet_cache(player_directory_path, "Player directory cache", "scripts/refresh_weekly_cache.R")
}

