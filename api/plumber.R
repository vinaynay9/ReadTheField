# ReadTheField API
# Start from repo root with: Rscript api/run_api.R

setwd(normalizePath(".."))

if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Package 'plumber' is required. Install with install.packages('plumber').")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
}

source("R/simulation/simulate_player_game_v1.R")

if (!exists("read_player_directory_cache") || !exists("read_player_week_identity_cache")) {
  source("R/data/build_weekly_player_layers.R", local = TRUE)
}
if (!exists("load_schedules")) {
  source("R/data/load_schedules.R", local = TRUE)
}
if (!exists("get_available_seasons_from_cache")) {
  source("R/utils/cache_helpers.R", local = TRUE)
}

get_col <- function(df, candidates, default = NA) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  rep(default, nrow(df))
}

normalize_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\.", "", x)
  x <- gsub("\\s+", " ", x)
  x
}

resolve_player_id <- function(player_name, directory_df) {
  if (is.null(player_name) || !nzchar(trimws(player_name))) return(NULL)
  if (is.null(directory_df) || nrow(directory_df) == 0) return(NULL)

  if ("canonical_name" %in% names(directory_df)) {
    directory_df$canonical_name <- as.character(directory_df$canonical_name)
  } else if ("full_name" %in% names(directory_df)) {
    directory_df$canonical_name <- normalize_name(directory_df$full_name)
  } else if ("player_name" %in% names(directory_df)) {
    directory_df$canonical_name <- normalize_name(directory_df$player_name)
  } else {
    directory_df$canonical_name <- ""
  }

  query <- normalize_name(player_name)
  match <- directory_df[directory_df$canonical_name == query, , drop = FALSE]
  if (nrow(match) == 0) {
    return(NULL)
  }

  id_col <- if ("player_id" %in% names(match)) "player_id" else "gsis_id"
  as.character(match[[id_col]][1])
}

get_available_seasons <- function() {
  seasons <- integer(0)
  if (exists("get_available_seasons_from_cache")) {
    seasons <- get_available_seasons_from_cache()
    if (length(seasons) == 0) {
      seasons <- get_available_seasons_from_cache("schedules")
    }
  }
  if (length(seasons) == 0 && exists("read_player_week_identity_cache")) {
    identity <- read_player_week_identity_cache()
    if (!is.null(identity) && "season" %in% names(identity)) {
      seasons <- unique(as.integer(identity$season))
    }
  }
  seasons <- seasons[!is.na(seasons)]
  sort(unique(seasons))
}

get_players <- function() {
  dir <- read_player_directory_cache()
  if (is.null(dir) || nrow(dir) == 0) {
    return(data.frame())
  }
  ids <- as.character(get_col(dir, c("player_id", "gsis_id"), ""))
  names <- as.character(get_col(dir, c("full_name", "player_name", "name"), ""))
  positions <- toupper(as.character(get_col(dir, c("position"), "")))
  teams <- toupper(as.character(get_col(dir, c("team", "team_abbr"), "")))
  seasons <- as.integer(get_col(dir, c("season", "season_year"), NA))

  players <- data.frame(
    player_id = ids,
    player_name = names,
    position = positions,
    team = teams,
    season = seasons,
    stringsAsFactors = FALSE
  )
  players <- players[players$player_id != "" & players$player_name != "", , drop = FALSE]

  if ("season" %in% names(players)) {
    players <- players[order(players$player_id, -players$season), , drop = FALSE]
  }
  players <- players[!duplicated(players$player_id), , drop = FALSE]
  players <- players[order(players$player_name, players$team), , drop = FALSE]
  rownames(players) <- NULL
  players
}

get_teams <- function() {
  seasons <- get_available_seasons()
  if (length(seasons) == 0) {
    return(data.frame())
  }
  sched <- load_schedules(seasons = seasons, cache_only = TRUE)
  if (is.null(sched) || nrow(sched) == 0) {
    dir <- read_player_directory_cache()
    if (is.null(dir) || nrow(dir) == 0) return(data.frame())
    teams <- unique(toupper(as.character(get_col(dir, c("team", "team_abbr"), ""))))
    teams <- teams[teams != ""]
    return(data.frame(team = teams, name = teams, stringsAsFactors = FALSE))
  }

  home_team <- toupper(as.character(get_col(sched, c("home_team", "home_team_abbr"), "")))
  away_team <- toupper(as.character(get_col(sched, c("away_team", "away_team_abbr"), "")))
  home_name <- as.character(get_col(sched, c("home_team_name", "home_team_full"), ""))
  away_name <- as.character(get_col(sched, c("away_team_name", "away_team_full"), ""))

  teams <- data.frame(
    team = c(home_team, away_team),
    name = c(home_name, away_name),
    stringsAsFactors = FALSE
  )
  teams <- teams[teams$team != "", , drop = FALSE]
  teams$name <- ifelse(is.na(teams$name) | teams$name == "", teams$team, teams$name)
  teams <- teams[!duplicated(teams$team), , drop = FALSE]
  teams <- teams[order(teams$name), , drop = FALSE]
  rownames(teams) <- NULL
  teams
}

get_player_games <- function(player_id) {
  identity <- read_player_week_identity_cache()
  if (is.null(identity) || nrow(identity) == 0) {
    return(data.frame())
  }
  identity$player_id <- as.character(identity$player_id)
  rows <- identity[identity$player_id == as.character(player_id), , drop = FALSE]
  if (nrow(rows) == 0) {
    return(data.frame())
  }
  rows$season <- as.integer(rows$season)
  rows$week <- as.integer(rows$week)
  rows <- rows[order(-rows$season, -rows$week), , drop = FALSE]
  rows <- rows[, c("season", "week", "opponent", "team", "home_away", "game_date", "game_id"), drop = FALSE]
  rownames(rows) <- NULL
  rows
}

get_next_game <- function(player_id) {
  dir <- read_player_directory_cache()
  if (is.null(dir) || nrow(dir) == 0) return(NULL)
  dir$player_id <- as.character(get_col(dir, c("player_id", "gsis_id"), ""))
  rows <- dir[dir$player_id == as.character(player_id), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)

  team <- toupper(as.character(get_col(rows, c("team", "team_abbr"), "")))
  team <- team[team != ""][1]
  if (is.na(team) || team == "") return(NULL)

  seasons <- get_available_seasons()
  if (length(seasons) == 0) return(NULL)
  current_season <- max(seasons, na.rm = TRUE)
  sched <- load_schedules(seasons = current_season, cache_only = TRUE)
  if (is.null(sched) || nrow(sched) == 0) return(NULL)

  home_team <- toupper(as.character(get_col(sched, c("home_team", "home_team_abbr"), "")))
  away_team <- toupper(as.character(get_col(sched, c("away_team", "away_team_abbr"), "")))
  game_date <- as.Date(get_col(sched, c("gameday", "game_date"), NA))
  weeks <- as.integer(get_col(sched, c("week"), NA))

  team_mask <- home_team == team | away_team == team
  sched_team <- sched[team_mask, , drop = FALSE]
  if (nrow(sched_team) == 0) return(NULL)

  sched_team$game_date <- game_date[team_mask]
  sched_team$week <- weeks[team_mask]
  sched_team$home_team <- home_team[team_mask]
  sched_team$away_team <- away_team[team_mask]

  today <- as.Date(Sys.time())
  future <- sched_team[!is.na(sched_team$game_date) & sched_team$game_date >= today, , drop = FALSE]
  if (nrow(future) == 0) return(NULL)

  future <- future[order(future$game_date, future$week), , drop = FALSE]
  next_row <- future[1, , drop = FALSE]

  opponent <- if (next_row$home_team == team) next_row$away_team else next_row$home_team
  home_away <- if (next_row$home_team == team) "HOME" else "AWAY"

  list(
    season = as.integer(current_season),
    week = as.integer(next_row$week),
    team = team,
    opponent = opponent,
    home_away = home_away,
    game_date = as.character(next_row$game_date)
  )
}

#* Enable CORS
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 204
    return(list())
  }
  plumber::forward()
}

#* List available players
#* @get /players
players_endpoint <- function() {
  list(players = get_players())
}

#* List available teams
#* @get /teams
teams_endpoint <- function() {
  list(teams = get_teams())
}

#* List available seasons
#* @get /seasons
seasons_endpoint <- function() {
  list(seasons = get_available_seasons())
}

#* List historical games for a player
#* @get /player/<player_id>/games
player_games_endpoint <- function(player_id, res) {
  if (is.null(player_id) || !nzchar(player_id)) {
    res$status <- 400
    return(list(status = "error", message = "player_id is required"))
  }
  list(games = get_player_games(player_id))
}

#* Get next scheduled game for a player
#* @get /player/<player_id>/next_game
player_next_game_endpoint <- function(player_id, res) {
  if (is.null(player_id) || !nzchar(player_id)) {
    res$status <- 400
    return(list(status = "error", message = "player_id is required"))
  }
  next_game <- get_next_game(player_id)
  if (is.null(next_game)) {
    res$status <- 404
    return(list(status = "error", message = "No upcoming game found for this player."))
  }
  next_game
}

#* Run simulation
#* @post /simulate
simulate_endpoint <- function(req, res) {
  body <- NULL
  if (!is.null(req$postBody) && nzchar(req$postBody)) {
    body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  }
  if (is.null(body)) {
    res$status <- 400
    return(list(status = "error", message = "Request body is required."))
  }

  directory_df <- read_player_directory_cache()
  player_id <- body$player_id
  if (is.null(player_id) || !nzchar(trimws(as.character(player_id)))) {
    player_id <- resolve_player_id(body$player_name, directory_df)
  }

  simulate_player_game_v1(
    player_id = player_id,
    season = body$season,
    week = body$week,
    n_sims = if (!is.null(body$n_sims)) body$n_sims else 500,
    availability_policy = if (!is.null(body$availability_policy)) body$availability_policy else "played_only",
    seed = if (!is.null(body$seed)) body$seed else NULL,
    schema_version = if (!is.null(body$schema_version)) body$schema_version else "v1",
    mode = if (!is.null(body$mode)) body$mode else NULL,
    schedule_opponent = if (!is.null(body$schedule_opponent)) body$schedule_opponent else NULL,
    schedule_home_away = if (!is.null(body$schedule_home_away)) body$schedule_home_away else NULL
  )
}
