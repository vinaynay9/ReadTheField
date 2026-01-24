## Build Weekly Player Layers
##
## Provides functions to ingest nflreadr weekly player stats and materialize
## Layer 1 (player-week identity) and Layer 2 (position-specific stats) caches.

# Internal env for caching nflreadr downloads within a session.
.weekly_player_stats_env <- new.env(parent = emptyenv())

repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
player_week_identity_path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")
rb_weekly_stats_path <- file.path(repo_root, "data", "cache", "rb_weekly_stats.parquet")
rb_weekly_features_path <- file.path(repo_root, "data", "processed", "rb_weekly_features.parquet")
wr_weekly_stats_path <- file.path(repo_root, "data", "cache", "wr_weekly_stats.parquet")
wr_weekly_features_path <- file.path(repo_root, "data", "processed", "wr_weekly_features.parquet")
te_weekly_stats_path <- file.path(repo_root, "data", "cache", "te_weekly_stats.parquet")
te_weekly_features_path <- file.path(repo_root, "data", "processed", "te_weekly_features.parquet")
qb_weekly_stats_path <- file.path(repo_root, "data", "cache", "qb_weekly_stats.parquet")
qb_weekly_features_path <- file.path(repo_root, "data", "processed", "qb_player_weekly_features.parquet")
k_weekly_stats_path <- file.path(repo_root, "data", "cache", "k_weekly_stats.parquet")
k_weekly_features_path <- file.path(repo_root, "data", "processed", "k_weekly_features.parquet")
player_directory_path <- file.path(repo_root, "data", "cache", "player_directory.parquet")

# Ensure cache helpers are available for build_game_key
if (!exists("build_game_key")) {
  stop("Cache helpers not loaded. Source R/simulation/bootstrap_simulation.R before calling build_weekly_player_layers.")
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
    stop("load_schedules not loaded. Source R/simulation/bootstrap_simulation.R before calling build_schedule_team_lookup.")
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

  snapshot_key <- paste0(
    "weekly_player_stats_",
    season_type_input,
    "_",
    paste(seasons, collapse = "_"),
    ".rds"
  )
  use_raw_cache <- isTRUE(getOption("READTHEFIELD_USE_RAW_CACHE", TRUE))
  freeze_raw <- isTRUE(getOption("READTHEFIELD_FREEZE_RAW", FALSE))
  if (exists("read_raw_snapshot") && use_raw_cache) {
    cached_raw <- read_raw_snapshot(snapshot_key)
    if (!is.null(cached_raw) && nrow(cached_raw) > 0) {
      assign(cache_key, cached_raw, envir = .weekly_player_stats_env)
      return(cached_raw)
    }
  }
  if (freeze_raw) {
    stop("Raw snapshot missing for weekly_player_stats (", snapshot_key, "). Set READTHEFIELD_FREEZE_RAW=FALSE to download.")
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
  if (exists("write_raw_snapshot")) {
    write_raw_snapshot(stats_df, snapshot_key)
  }
  if (exists("write_snapshot_info")) {
    write_snapshot_info(c(
      paste0("weekly_player_stats: ", snapshot_key),
      paste0("seasons: ", paste(seasons, collapse = ",")),
      paste0("season_type: ", season_type_input),
      paste0("rows: ", nrow(stats_df))
    ))
  }
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

get_allowed_team_abbrs <- function() {
  path <- file.path("data", "teams", "teams.csv")
  teams <- character(0)
  if (file.exists(path)) {
    team_df <- read.csv(path, stringsAsFactors = FALSE)
    if ("abbr" %in% names(team_df)) {
      teams <- toupper(as.character(team_df$abbr))
    } else if ("team" %in% names(team_df)) {
      teams <- toupper(as.character(team_df$team))
    }
  }
  legacy <- c("OAK", "SD", "STL", "LA", "WAS", "JAC")
  sort(unique(c(teams, legacy)))
}

validate_position_team_contract <- function(df,
                                            label,
                                            position_col = "position",
                                            team_col = "team") {
  if (is.null(df) || nrow(df) == 0) {
    stop(label, " is empty; cannot validate position/team contract.")
  }
  if (!position_col %in% names(df)) {
    stop(label, " missing position column: ", position_col)
  }
  if (!team_col %in% names(df)) {
    stop(label, " missing team column: ", team_col)
  }

  positions <- toupper(trimws(as.character(df[[position_col]])))
  teams <- toupper(trimws(as.character(df[[team_col]])))

  if (any(is.na(positions) | positions == "")) {
    stop(label, " contains missing positions.")
  }
  if (any(is.na(teams) | teams == "")) {
    stop(label, " contains missing teams.")
  }

  allowed_positions <- c("QB", "RB", "WR", "TE", "K")
  invalid_pos <- setdiff(unique(positions), allowed_positions)
  if (length(invalid_pos) > 0) {
    stop(label, " contains invalid positions: ", paste(invalid_pos, collapse = ", "))
  }

  allowed_teams <- get_allowed_team_abbrs()
  if (length(allowed_teams) > 0) {
    invalid_teams <- setdiff(unique(teams), allowed_teams)
    if (length(invalid_teams) > 0) {
      stop(label, " contains invalid team codes: ", paste(invalid_teams, collapse = ", "))
    }
  }
  invisible(TRUE)
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
  teams <- canonicalize_team_abbr(teams)
  opponents <- canonicalize_team_abbr(opponents)
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

build_qb_weekly_stats <- function(seasons,
                                  season_type = "REG",
                                  write_cache = TRUE) {
  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)
  
  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  stats$position <- positions
  
  qb_mask <- stats$position == "QB"
  if (sum(qb_mask, na.rm = TRUE) == 0) {
    warning("No QB rows found in weekly player stats.", call. = FALSE)
    return(data.frame())
  }
  
  qb_dataset <- stats[qb_mask, , drop = FALSE]
  qb_dataset <- qb_dataset[!is.na(qb_dataset$player_id) & !is.na(qb_dataset$season) & !is.na(qb_dataset$week), , drop = FALSE]

  qb_col_candidates <- list(
    pass_attempts = c("attempts", "pass_attempts"),
    completions = c("completions", "pass_completions"),
    pass_yards = c("passing_yards", "pass_yards"),
    pass_tds = c("passing_tds", "pass_tds"),
    interceptions = c("passing_interceptions", "interceptions"),
    sacks_taken = c("sacks_suffered", "qb_sacks", "sacks_taken"),
    rush_attempts = c("carries", "rush_attempts", "rushing_attempts"),
    rush_yards = c("rushing_yards", "rush_yards"),
    rush_tds = c("rushing_tds", "rush_tds")
  )
  qb_col_found <- vapply(qb_col_candidates, function(cands) {
    hit <- intersect(cands, names(qb_dataset))
    if (length(hit) > 0) hit[1] else NA_character_
  }, character(1))
  cat("QB weekly stats column report:\n")
  for (nm in names(qb_col_found)) {
    cat("  ", nm, ": ", if (!is.na(qb_col_found[[nm]])) qb_col_found[[nm]] else "(missing)", "\n", sep = "")
  }
  if (is.na(qb_col_found[["rush_tds"]])) {
    stop("QB weekly stats missing rushing TD column. Expected one of: ",
         paste(qb_col_candidates$rush_tds, collapse = ", "), call. = FALSE)
  }

  if (file.exists(player_week_identity_path)) {
    identity <- read_parquet_cache(player_week_identity_path, "Player week identity cache", "scripts/refresh_weekly_cache.R")
    identity_cols <- intersect(c("player_id", "season", "week", "game_id", "gameday", "opponent", "home_away", "is_home", "game_key"), names(identity))
    qb_dataset <- merge(qb_dataset, identity[, identity_cols, drop = FALSE],
                        by = c("player_id", "season", "week"), all.x = TRUE, suffixes = c("", "_ident"))
    if ("home_away_ident" %in% names(qb_dataset)) {
      qb_dataset$home_away <- ifelse(is.na(qb_dataset$home_away), qb_dataset$home_away_ident, qb_dataset$home_away)
    }
    if ("is_home" %in% names(qb_dataset)) {
      qb_dataset$is_home <- as.integer(qb_dataset$is_home)
    }
    helper_cols <- grep("(_ident)$", names(qb_dataset), value = TRUE)
    if (length(helper_cols) > 0) {
      qb_dataset <- qb_dataset[, setdiff(names(qb_dataset), helper_cols), drop = FALSE]
    }
  }
  if (!"opponent" %in% names(qb_dataset)) {
    qb_dataset$opponent <- normalize_team_abbr(select_first_available(qb_dataset, c("opponent_team", "opp_team", "defteam"), NA), qb_dataset$season)
  }

  # Schedule-derived home/away context and game_id (no string parsing).
  sched_lookup <- build_schedule_team_lookup(unique(qb_dataset$season))
  if (!is.null(sched_lookup) && nrow(sched_lookup) > 0) {
    sched_cols <- intersect(c("team", "season", "week", "opponent", "home_away", "is_home", "game_id", "gameday"), names(sched_lookup))
    qb_dataset <- merge(
      qb_dataset,
      sched_lookup[, sched_cols, drop = FALSE],
      by = c("season", "week", "team", "opponent"),
      all.x = TRUE,
      suffixes = c("", "_sched")
    )
    if ("game_id_sched" %in% names(qb_dataset)) {
      qb_dataset$game_id <- ifelse(
        is.na(qb_dataset$game_id) | qb_dataset$game_id == "",
        qb_dataset$game_id_sched,
        qb_dataset$game_id
      )
    }
    if (!"game_date" %in% names(qb_dataset) && "gameday" %in% names(qb_dataset)) {
      qb_dataset$game_date <- qb_dataset$gameday
    }
    if ("gameday_sched" %in% names(qb_dataset)) {
      if ("game_date" %in% names(qb_dataset)) {
        qb_dataset$game_date <- ifelse(is.na(qb_dataset$game_date), qb_dataset$gameday_sched, qb_dataset$game_date)
      }
      if ("gameday" %in% names(qb_dataset)) {
        qb_dataset$gameday <- ifelse(is.na(qb_dataset$gameday), qb_dataset$gameday_sched, qb_dataset$gameday)
      }
    }
    if ("home_away_sched" %in% names(qb_dataset)) {
      qb_dataset$home_away <- ifelse(is.na(qb_dataset$home_away), qb_dataset$home_away_sched, qb_dataset$home_away)
    }
    if ("is_home_sched" %in% names(qb_dataset)) {
      qb_dataset$is_home <- ifelse(is.na(qb_dataset$is_home), qb_dataset$is_home_sched, qb_dataset$is_home)
    }
    helper_cols <- grep("(_sched)$", names(qb_dataset), value = TRUE)
    if (length(helper_cols) > 0) {
      qb_dataset <- qb_dataset[, setdiff(names(qb_dataset), helper_cols), drop = FALSE]
    }
  }

  # Fallback: construct deterministic game_id when missing after schedule joins.
  if (!exists("build_game_key")) {
    stop("Cache helpers not loaded. Source R/simulation/bootstrap_simulation.R before building game keys.")
  }
  if (exists("build_game_key") && "game_id" %in% names(qb_dataset)) {
    missing_game_id <- is.na(qb_dataset$game_id) | qb_dataset$game_id == ""
    if (any(missing_game_id)) {
      qb_dataset$game_id[missing_game_id] <- mapply(
        build_game_key,
        season = qb_dataset$season[missing_game_id],
        week = qb_dataset$week[missing_game_id],
        game_date = if ("game_date" %in% names(qb_dataset)) qb_dataset$game_date[missing_game_id] else qb_dataset$gameday[missing_game_id],
        team = qb_dataset$team[missing_game_id],
        opponent = qb_dataset$opponent[missing_game_id],
        game_id = qb_dataset$game_id[missing_game_id],
        USE.NAMES = FALSE
      )
    }
  }
  qb_dataset$defense_team <- qb_dataset$opponent
  qb_dataset <- qb_dataset[!is.na(qb_dataset$opponent) & qb_dataset$opponent != "", , drop = FALSE]
  
  if (write_cache) {
    write_parquet_cache(qb_dataset, qb_weekly_stats_path, "QB weekly stats")
  }
  
  qb_dataset
}

build_k_weekly_stats <- function(seasons,
                                 season_type = "REG",
                                 write_cache = TRUE) {
  # Kicking stats are not guaranteed in default load_player_stats output.
  # Pull explicit kicking stat_type to ensure FG/PAT data exist.
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("Package 'nflreadr' is required. Install with install.packages('nflreadr').")
  }
  stats <- nflreadr::load_player_stats(
    seasons = seasons,
    summary_level = "week",
    stat_type = "kicking"
  )
  stats <- as.data.frame(stats, stringsAsFactors = FALSE)
  if ("season_type" %in% names(stats) && !is.null(season_type)) {
    stats <- stats[stats$season_type == toupper(trimws(season_type)), , drop = FALSE]
  }
  
  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  stats$position <- positions
  
  k_mask <- stats$position == "K"
  if (sum(k_mask, na.rm = TRUE) == 0) {
    warning("No K rows found in weekly player stats.", call. = FALSE)
    return(data.frame())
  }
  
  k_dataset <- stats[k_mask, , drop = FALSE]
  k_dataset <- k_dataset[!is.na(k_dataset$player_id) & !is.na(k_dataset$season) & !is.na(k_dataset$week), , drop = FALSE]

  if (file.exists(player_week_identity_path)) {
    identity <- read_parquet_cache(player_week_identity_path, "Player week identity cache", "scripts/refresh_weekly_cache.R")
    identity_cols <- intersect(c("player_id", "season", "week", "game_id", "gameday", "opponent", "home_away", "is_home", "game_key"), names(identity))
    k_dataset <- merge(k_dataset, identity[, identity_cols, drop = FALSE],
                       by = c("player_id", "season", "week"), all.x = TRUE, suffixes = c("", "_ident"))
    if ("home_away_ident" %in% names(k_dataset)) {
      k_dataset$home_away <- ifelse(is.na(k_dataset$home_away), k_dataset$home_away_ident, k_dataset$home_away)
    }
    if ("is_home" %in% names(k_dataset)) {
      k_dataset$is_home <- as.integer(k_dataset$is_home)
    }
    helper_cols <- grep("(_ident)$", names(k_dataset), value = TRUE)
    if (length(helper_cols) > 0) {
      k_dataset <- k_dataset[, setdiff(names(k_dataset), helper_cols), drop = FALSE]
    }
  }
  if (!"opponent" %in% names(k_dataset)) {
    k_dataset$opponent <- normalize_team_abbr(select_first_available(k_dataset, c("opponent_team", "opp_team", "defteam"), NA), k_dataset$season)
  }
  # Fill opponent/home-away via schedule lookup when missing (kicking stats lack player-week identity rows).
  if (any(is.na(k_dataset$opponent) | k_dataset$opponent == "")) {
    sched_lookup <- build_schedule_team_lookup(unique(k_dataset$season))
    if (!is.null(sched_lookup) && nrow(sched_lookup) > 0) {
      sched_cols <- intersect(c("team", "season", "week", "opponent", "home_away", "is_home", "game_id", "gameday"), names(sched_lookup))
      k_dataset <- merge(
        k_dataset,
        sched_lookup[, sched_cols, drop = FALSE],
        by = c("team", "season", "week"),
        all.x = TRUE,
        suffixes = c("", "_sched")
      )
      if ("opponent_sched" %in% names(k_dataset)) {
        k_dataset$opponent <- ifelse(is.na(k_dataset$opponent) | k_dataset$opponent == "", k_dataset$opponent_sched, k_dataset$opponent)
      }
      if ("home_away_sched" %in% names(k_dataset)) {
        k_dataset$home_away <- ifelse(is.na(k_dataset$home_away) | k_dataset$home_away == "", k_dataset$home_away_sched, k_dataset$home_away)
      }
      if ("is_home_sched" %in% names(k_dataset)) {
        k_dataset$is_home <- ifelse(is.na(k_dataset$is_home), k_dataset$is_home_sched, k_dataset$is_home)
      }
      if ("game_id_sched" %in% names(k_dataset)) {
        k_dataset$game_id <- ifelse(is.na(k_dataset$game_id), k_dataset$game_id_sched, k_dataset$game_id)
      }
      if ("gameday_sched" %in% names(k_dataset)) {
        k_dataset$gameday <- ifelse(is.na(k_dataset$gameday), k_dataset$gameday_sched, k_dataset$gameday)
      }
      sched_helper <- grep("_sched$", names(k_dataset), value = TRUE)
      if (length(sched_helper) > 0) {
        k_dataset <- k_dataset[, setdiff(names(k_dataset), sched_helper), drop = FALSE]
      }
    }
  }
  k_dataset$defense_team <- k_dataset$opponent
  k_dataset <- k_dataset[!is.na(k_dataset$opponent) & k_dataset$opponent != "", , drop = FALSE]
  
  if (write_cache) {
    write_parquet_cache(k_dataset, k_weekly_stats_path, "K weekly stats")
  }
  
  k_dataset
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

read_qb_weekly_stats_cache <- function() {
  read_parquet_cache(qb_weekly_stats_path, "QB weekly stats cache", "scripts/refresh_weekly_cache.R")
}

read_qb_weekly_features_cache <- function() {
  read_parquet_cache(qb_weekly_features_path, "QB weekly features cache", "scripts/refresh_weekly_cache.R")
}

read_k_weekly_stats_cache <- function() {
  read_parquet_cache(k_weekly_stats_path, "K weekly stats cache", "scripts/refresh_weekly_cache.R")
}

read_k_weekly_features_cache <- function() {
  read_parquet_cache(k_weekly_features_path, "K weekly features cache", "scripts/refresh_weekly_cache.R")
}

#' Build player directory cache from player_dim (authoritative position/team)
#'
#' Creates a canonical player directory using the most recent season row per player
#' in player_dim. player_dim is the single source of truth for position/team.
#'
#' @param write_cache Logical, if TRUE write to parquet cache (default TRUE)
#' @return data.frame with player directory columns
build_player_directory <- function(write_cache = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with install.packages('arrow').")
  }
  canonicalize_name_local <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(character(0))
    }
    x <- as.character(x)
    x <- tolower(x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    x[nchar(x) == 0] <- NA_character_
    x
  }
  build_full_name <- function(first_name, last_name) {
    first_name <- trimws(as.character(first_name))
    last_name <- trimws(as.character(last_name))
    ifelse(first_name != "" & last_name != "", paste(first_name, last_name), NA_character_)
  }

  if (!exists("read_player_week_identity_cache")) {
    stop("Player identity cache loader not available. Source R/data/build_weekly_player_layers.R.")
  }
  pwi <- read_player_week_identity_cache()
  if (is.null(pwi) || nrow(pwi) == 0) {
    stop("player_week_identity cache is required to build player_directory.")
  }

  base <- unique(pwi[, c("player_id", "position", "team"), drop = FALSE])
  base$player_id <- as.character(base$player_id)
  base$position <- normalize_position(base$position)
  base$team <- toupper(trimws(as.character(base$team)))

  snapshot_players <- NULL
  snapshot_rosters <- NULL
  snapshot_weekly <- NULL
  if (exists("read_raw_snapshot")) {
    snapshot_players <- read_raw_snapshot("players.rds")
    if (is.null(snapshot_players) || nrow(snapshot_players) == 0) snapshot_players <- NULL
    snap_dir <- get_snapshot_dir(create = FALSE)
    if (!is.null(snap_dir) && dir.exists(snap_dir)) {
      roster_files <- list.files(snap_dir, pattern = "^rosters_.*\\.rds$", full.names = TRUE)
      if (length(roster_files) > 0) {
        snapshot_rosters <- tryCatch(readRDS(roster_files[1]), error = function(e) NULL)
      }
      weekly_files <- list.files(snap_dir, pattern = "^weekly_player_stats_REG_.*\\.rds$", full.names = TRUE)
      if (length(weekly_files) > 0) {
        snapshot_weekly <- tryCatch(readRDS(weekly_files[1]), error = function(e) NULL)
      }
    }
  }

  meta <- data.frame(player_id = base$player_id, stringsAsFactors = FALSE)

  if (!is.null(snapshot_players) && nrow(snapshot_players) > 0) {
    players_df <- as.data.frame(snapshot_players, stringsAsFactors = FALSE)
    if (!("gsis_id" %in% names(players_df)) && "player_id" %in% names(players_df)) {
      players_df$gsis_id <- players_df$player_id
    }
    players_df$player_id <- as.character(select_first_available(players_df, c("player_id", "gsis_id"), NA))
    players_df$full_name <- as.character(select_first_available(players_df, c("display_name", "full_name", "name"), NA))
    players_df$first_name <- as.character(select_first_available(players_df, c("first_name"), NA))
    players_df$last_name <- as.character(select_first_available(players_df, c("last_name"), NA))
    players_df$canonical_name <- canonicalize_name_local(players_df$full_name)
    players_df$draft_round <- as.integer(select_first_available(players_df, c("draft_round"), NA))
    players_df$draft_pick_overall <- as.integer(select_first_available(players_df, c("draft_pick_overall", "draft_pick"), NA))
    players_df <- players_df[!is.na(players_df$player_id) & players_df$player_id != "", , drop = FALSE]
    players_df <- players_df[!duplicated(players_df$player_id), , drop = FALSE]
    meta <- merge(meta, players_df[, c("player_id", "full_name", "first_name", "last_name",
                                       "canonical_name", "draft_round", "draft_pick_overall"), drop = FALSE],
                  by = "player_id", all.x = TRUE)
  }

  if (!is.null(snapshot_weekly) && nrow(snapshot_weekly) > 0) {
    weekly_df <- as.data.frame(snapshot_weekly, stringsAsFactors = FALSE)
    weekly_df$player_id <- as.character(select_first_available(weekly_df, c("player_id", "gsis_id"), NA))
    weekly_df$player_name <- as.character(select_first_available(weekly_df, c("player_name", "player_display_name", "name"), NA))
    weekly_df <- weekly_df[!is.na(weekly_df$player_id) & weekly_df$player_id != "", , drop = FALSE]
    weekly_df <- weekly_df[!is.na(weekly_df$player_name) & weekly_df$player_name != "", , drop = FALSE]
    weekly_df <- weekly_df[!duplicated(weekly_df$player_id), , drop = FALSE]
    meta <- merge(meta, weekly_df[, c("player_id", "player_name"), drop = FALSE], by = "player_id", all.x = TRUE)
  }

  if (!is.null(snapshot_rosters) && nrow(snapshot_rosters) > 0) {
    roster_df <- as.data.frame(snapshot_rosters, stringsAsFactors = FALSE)
    roster_df$player_id <- as.character(select_first_available(roster_df, c("player_id", "gsis_id"), NA))
    roster_df$full_name_roster <- as.character(select_first_available(roster_df, c("full_name", "display_name", "name"), NA))
    roster_df$first_name_roster <- as.character(select_first_available(roster_df, c("first_name"), NA))
    roster_df$last_name_roster <- as.character(select_first_available(roster_df, c("last_name"), NA))
    roster_df <- roster_df[!is.na(roster_df$player_id) & roster_df$player_id != "", , drop = FALSE]
    roster_df <- roster_df[!duplicated(roster_df$player_id), , drop = FALSE]
    meta <- merge(meta, roster_df[, c("player_id", "full_name_roster", "first_name_roster", "last_name_roster"), drop = FALSE],
                  by = "player_id", all.x = TRUE)
  }

  meta$full_name <- dplyr::coalesce(
    meta$full_name,
    meta$full_name_roster
  )
  meta$first_name <- dplyr::coalesce(meta$first_name, meta$first_name_roster)
  meta$last_name <- dplyr::coalesce(meta$last_name, meta$last_name_roster)

  meta$player_name <- dplyr::coalesce(
    meta$full_name,
    meta$canonical_name,
    build_full_name(meta$first_name, meta$last_name),
    meta$player_name,
    meta$player_id
  )

  player_dir <- merge(base, meta, by = "player_id", all.x = TRUE)
  player_dir$player_name <- ifelse(
    is.na(player_dir$player_name) | player_dir$player_name == "",
    player_dir$player_id,
    player_dir$player_name
  )

  missing_player_id <- is.na(player_dir$player_id) | player_dir$player_id == ""
  if (any(missing_player_id)) {
    stop("Player directory validation failed: missing player_id values detected.")
  }

  valid_positions <- c("QB", "RB", "WR", "TE", "K")
  invalid_positions <- setdiff(unique(player_dir$position), valid_positions)
  if (length(invalid_positions) > 0) {
    stop(
      "Player directory validation failed: invalid positions found: ",
      paste(invalid_positions, collapse = ", ")
    )
  }

  team_present <- !is.na(player_dir$team) & player_dir$team != ""
  if (any(!team_present)) {
    stop("Player directory validation failed: team is missing for one or more players.")
  }

  allowed_teams <- get_allowed_team_abbrs()
  if (length(allowed_teams) > 0) {
    invalid_teams <- setdiff(unique(player_dir$team), allowed_teams)
    if (length(invalid_teams) > 0) {
      stop(
        "Player directory validation failed: invalid team codes detected: ",
        paste(invalid_teams, collapse = ", ")
      )
    }
  }

  player_dir <- player_dir[order(player_dir$player_name, player_dir$team), ]
  rownames(player_dir) <- NULL

  missing_required <- sum(is.na(player_dir$position) | player_dir$position == "" |
                            is.na(player_dir$team) | player_dir$team == "")
  if (missing_required > 0) {
    stop("Player directory validation failed: missing team/position values.")
  }
  if (requireNamespace("dplyr", quietly = TRUE)) {
    orphans <- dplyr::anti_join(
      dplyr::distinct(pwi, player_id),
      dplyr::distinct(player_dir, player_id),
      by = "player_id"
    )
    if (nrow(orphans) > 0) {
      stop("Player directory validation failed: missing player_ids from player_week_identity.")
    }
  }

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
