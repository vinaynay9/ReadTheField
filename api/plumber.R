# ReadTheField API
# Start from repo root with: Rscript api/run_api.R

repo_root <- getOption("READTHEFIELD_REPO_ROOT")
if (is.null(repo_root) || !nzchar(repo_root)) {
  stop("READTHEFIELD_REPO_ROOT not set. Start the API with Rscript api/run_api.R.")
}
repo_root <- normalizePath(repo_root, mustWork = TRUE)

source_repo <- function(path, local = FALSE) {
  source(file.path(repo_root, path), local = local)
}

if (!requireNamespace("plumber", quietly = TRUE)) {
  stop("Package 'plumber' is required. Install with install.packages('plumber').")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
}

source_repo("R/simulation/bootstrap_simulation.R")

PLAYER_DIRECTORY <- NULL
PLAYER_WEEK_IDENTITY <- NULL
TEAM_REGISTRY <- NULL

load_parquet_df <- function(label, path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read ", label, ". Install with install.packages('arrow').")
  }
  if (!file.exists(path)) {
    stop(label, " missing at ", path)
  }
  df <- arrow::read_parquet(path)
  if (is.null(df) || nrow(df) == 0) {
    stop(label, " is empty at ", path)
  }
  df <- as.data.frame(df)
  stopifnot(is.data.frame(df))
  df
}

get_player_directory <- function() {
  if (is.null(PLAYER_DIRECTORY)) {
    path <- file.path(repo_root, "data", "cache", "player_directory.parquet")
    PLAYER_DIRECTORY <<- load_parquet_df("player_directory.parquet", path)
  }
  PLAYER_DIRECTORY
}

get_player_week_identity <- function() {
  if (is.null(PLAYER_WEEK_IDENTITY)) {
    path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")
    PLAYER_WEEK_IDENTITY <<- load_parquet_df("player_week_identity.parquet", path)
  }
  PLAYER_WEEK_IDENTITY
}

get_team_registry <- function() {
  if (is.null(TEAM_REGISTRY)) {
    path <- file.path(repo_root, "data", "teams", "teams.csv")
    if (!file.exists(path)) {
      stop("teams.csv missing at ", path)
    }
    team_df <- read.csv(path, stringsAsFactors = FALSE)
    required_cols <- c("team", "slug", "name", "conference", "division", "color_primary", "color_secondary")
    missing <- setdiff(required_cols, names(team_df))
    if (length(missing) > 0) {
      stop("teams.csv missing columns: ", paste(missing, collapse = ", "))
    }
    team_df$team <- toupper(as.character(team_df$team))
    team_df$slug <- as.character(team_df$slug)
    team_df$name <- as.character(team_df$name)
    TEAM_REGISTRY <<- team_df[order(team_df$conference, team_df$division, team_df$name), , drop = FALSE]
  }
  TEAM_REGISTRY
}

api_ok <- function(data) {
  list(ok = TRUE, data = data)
}

api_error <- function(res, status, error_code, message, details = list()) {
  if (!is.null(res)) {
    res$status <- status
  }
  list(ok = FALSE, error_code = error_code, message = message, details = details)
}

# api_safe behavior contract (explicit answers for audit):
# - Can api_safe() return ok=FALSE without throwing? Yes, via api_error() on caught errors or contract violations.
# - Does api_safe() treat NULL differently under plumber? No; NULL results always map to api_error().
# - Is res ever NULL or malformed? res can be NULL for direct R calls; plumber passes a response object over HTTP.
# - Is api_ok() ever skipped? Yes, if fn() errors or returns a non-contract object; api_error() is returned instead.
# - Always returns a list with an explicit ok field (TRUE/FALSE); never returns NULL.
api_safe <- function(res, fn, context) {
  result <- tryCatch(
    fn(),
    error = function(e) {
      tb <- utils::capture.output(sys.calls())
      return(api_error(
        res,
        500L,
        "internal_error",
        paste0("API error in ", context, ": ", conditionMessage(e)),
        details = list(traceback = tb)
      ))
    }
  )
  if (is.null(result)) {
    return(api_error(
      res,
      500L,
      "null_response",
      paste0("API handler ", context, " returned NULL."),
      details = list(context = context)
    ))
  }
  if (!is.list(result) || is.null(result$ok)) {
    return(api_error(
      res,
      500L,
      "invalid_response",
      paste0("API handler ", context, " returned non-contract response."),
      details = list(context = context, class = class(result))
    ))
  }
  result
}

map_error_status <- function(error_type, error_code) {
  if (is.null(error_type)) return(500L)
  error_type <- as.character(error_type)
  error_code <- if (is.null(error_code)) "" else as.character(error_code)
  if (error_type == "invalid_input") {
    if (error_code %in% c("player_id_unknown")) return(404L)
    return(400L)
  }
  if (error_type %in% c("data_unavailable", "player_inactive")) return(422L)
  if (error_type == "internal_error") return(500L)
  500L
}

validate_api_caches <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for API cache validation.")
  }
  required <- c(
    "data/cache/player_directory.parquet",
    "data/cache/player_week_identity.parquet",
    "data/processed/player_dim.parquet",
    "data/processed/defense_weekly_features.parquet",
    "data/processed/rb_weekly_features.parquet",
    "data/processed/wr_weekly_features.parquet",
    "data/processed/te_weekly_features.parquet",
    "data/processed/qb_weekly_features.parquet",
    "data/processed/qb_player_weekly_features.parquet",
    "data/processed/k_weekly_features.parquet"
  )
  for (path in required) {
    full_path <- file.path(repo_root, path)
    if (!file.exists(full_path)) {
      stop("Required cache missing: ", path)
    }
    df <- arrow::read_parquet(full_path)
    if (is.null(df) || nrow(df) == 0) {
      stop("Required cache is empty: ", path)
    }
  }
  manifest_path <- file.path(repo_root, "data", "cache", "cache_manifest_latest.rds")
  if (!file.exists(manifest_path)) {
    stop("Cache manifest missing: data/cache/cache_manifest_latest.rds")
  }
  invisible(TRUE)
}

list_simulation_cache_paths <- function() {
  c(
    "data/processed/player_dim.parquet",
    "data/processed/defense_weekly_features.parquet",
    "data/processed/rb_weekly_features.parquet",
    "data/processed/wr_weekly_features.parquet",
    "data/processed/te_weekly_features.parquet",
    "data/processed/qb_weekly_features.parquet",
    "data/processed/qb_player_weekly_features.parquet",
    "data/processed/k_weekly_features.parquet"
  )
}

validate_simulation_caches <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for simulation cache validation.")
  }
  required <- list_simulation_cache_paths()
  missing <- character(0)
  empty <- character(0)
  for (path in required) {
    full_path <- file.path(repo_root, path)
    if (!file.exists(full_path)) {
      missing <- c(missing, path)
      next
    }
    df <- arrow::read_parquet(full_path)
    if (is.null(df) || nrow(df) == 0) {
      empty <- c(empty, path)
    }
  }
  list(missing = unique(missing), empty = unique(empty))
}

read_player_directory_cache <- function() {
  get_player_directory()
}

read_player_week_identity_cache <- function() {
  get_player_week_identity()
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
    identity <- get_player_week_identity()
    if (!is.null(identity) && "season" %in% names(identity)) {
      seasons <- unique(as.integer(identity$season))
    }
  }
  seasons <- seasons[!is.na(seasons)]
  sort(unique(seasons))
}

safe_get_seasons <- function() {
  seasons <- tryCatch(
    get_available_seasons(),
    error = function(e) NULL
  )

  if (is.null(seasons) || length(seasons) == 0) {
    identity <- tryCatch(get_player_week_identity(), error = function(e) NULL)
    if (!is.null(identity) && "season" %in% names(identity)) {
      seasons <- sort(unique(identity$season))
    }
  }

  if (is.null(seasons) || length(seasons) == 0) {
    seasons <- integer(0)
  }

  seasons
}

get_players <- function() {
  dir <- get_player_directory()
  ids <- as.character(get_col(dir, c("player_id", "gsis_id"), ""))
  names <- as.character(get_col(dir, c("player_name", "full_name", "name"), ""))
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
  players <- players[
    players$player_id != "" &
      players$player_name != "" &
      players$position != "" &
      players$team != "",
    , drop = FALSE
  ]
  # Keep all positions; eligibility is computed separately.

  if ("season" %in% names(players)) {
    players <- players[order(players$player_id, -players$season), , drop = FALSE]
  }
  players <- players[!duplicated(players$player_id), , drop = FALSE]
  players <- players[order(players$player_name, players$team), , drop = FALSE]
  rownames(players) <- NULL
  players
}

add_player_eligibility <- function(players_df) {
  if (!exists("evaluate_player_eligibility")) {
    return(players_df)
  }
  min_games_required <- if (exists("get_min_games_required")) get_min_games_required() else 4L
  min_train_rows <- if (exists("get_min_train_rows")) get_min_train_rows() else 200L

  identity <- get_player_week_identity()
  counts <- table(as.character(identity$player_id))

  feature_ids <- list()
  feature_rows <- list()
  for (pos in c("RB", "WR", "TE", "QB", "K")) {
    df <- get_feature_cache_for_position(pos)
    feature_rows[[pos]] <- if (is.null(df)) 0L else nrow(df)
    if (!is.null(df) && "player_id" %in% names(df)) {
      feature_ids[[pos]] <- unique(as.character(df$player_id))
    } else {
      feature_ids[[pos]] <- character(0)
    }
  }

  players_df$eligible <- TRUE
  players_df$eligibility_reason <- NA_character_
  for (i in seq_len(nrow(players_df))) {
    pid <- as.character(players_df$player_id[i])
    pos <- toupper(as.character(players_df$position[i]))
    games <- if (pid %in% names(counts)) as.integer(counts[[pid]]) else 0L
    if (games < min_games_required) {
      players_df$eligible[i] <- FALSE
      players_df$eligibility_reason[i] <- "insufficient_history"
      next
    }
    if (!pos %in% names(feature_ids)) {
      players_df$eligible[i] <- FALSE
      players_df$eligibility_reason[i] <- "position_unsupported"
      next
    }
    if (feature_rows[[pos]] < min_train_rows) {
      players_df$eligible[i] <- FALSE
      players_df$eligibility_reason[i] <- "insufficient_training_rows"
      next
    }
    if (!pid %in% feature_ids[[pos]]) {
      players_df$eligible[i] <- FALSE
      players_df$eligibility_reason[i] <- "feature_rows_missing"
      next
    }
  }
  players_df
}

safe_add_player_eligibility <- function(players_df) {
  players_df$eligible <- FALSE
  players_df$eligibility_reason <- "eligibility_unavailable"

  identity <- NULL
  counts <- NULL
  if (exists("get_player_week_identity")) {
    identity <- tryCatch(get_player_week_identity(), error = function(e) NULL)
  }
  if (!is.null(identity) && nrow(identity) > 0 && "player_id" %in% names(identity)) {
    counts <- table(as.character(identity$player_id))
  }
  min_games_required <- if (exists("get_min_games_required")) get_min_games_required() else 4L

  for (i in seq_len(nrow(players_df))) {
    res <- tryCatch(
      {
        pid <- as.character(players_df$player_id[i])
        pos <- toupper(as.character(players_df$position[i]))
        games <- if (!is.null(counts) && pid %in% names(counts)) as.integer(counts[[pid]]) else NA_integer_
        if (is.na(games)) {
          list(eligible = FALSE, reason = "eligibility_unavailable")
        } else if (games < min_games_required) {
          list(eligible = FALSE, reason = "insufficient_history")
        } else if (!pos %in% c("QB", "RB", "WR", "TE", "K")) {
          list(eligible = FALSE, reason = "position_unsupported")
        } else {
          list(eligible = TRUE, reason = NA_character_)
        }
      },
      error = function(e) {
        list(eligible = FALSE, reason = "eligibility_unavailable")
      }
    )

    if (is.list(res) && !is.null(res$eligible)) {
      players_df$eligible[i] <- isTRUE(res$eligible)
      players_df$eligibility_reason[i] <- if (!is.null(res$reason)) res$reason else "unknown"
    }
  }

  players_df
}

get_teams <- function() {
  teams <- get_team_registry()
  rownames(teams) <- NULL
  teams
}

get_player_games <- function(player_id) {
  identity <- get_player_week_identity()
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

load_schedule_for_season <- function(season) {
  path <- file.path(repo_root, "data", "cache", "schedules.rds")
  if (!file.exists(path)) return(data.frame())
  sched <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(sched) || nrow(sched) == 0) return(data.frame())
  sched <- as.data.frame(sched)
  if ("season" %in% names(sched)) {
    sched <- sched[as.integer(sched$season) == as.integer(season), , drop = FALSE]
  }
  sched
}

normalize_team <- function(team) {
  team <- toupper(trimws(as.character(team)))
  if (exists("canonicalize_team_abbr")) {
    team <- canonicalize_team_abbr(team)
  }
  team
}

detect_playoffs_started <- function(sched, today) {
  if (is.null(sched) || nrow(sched) == 0) return(FALSE)
  if ("game_type" %in% names(sched)) {
    game_type <- as.character(sched$game_type)
    return(any(!is.na(game_type) & game_type != "REG"))
  }
  if ("season_type" %in% names(sched)) {
    season_type <- as.character(sched$season_type)
    return(any(!is.na(season_type) & season_type != "REG"))
  }
  game_date <- as.Date(get_col(sched, c("gameday", "game_date"), NA))
  week <- as.integer(get_col(sched, c("week"), NA))
  if (all(is.na(game_date)) || all(is.na(week))) return(FALSE)
  max_date <- suppressWarnings(max(game_date, na.rm = TRUE))
  max_week <- suppressWarnings(max(week, na.rm = TRUE))
  if (is.finite(max_date) && is.finite(max_week)) {
    return(max_week >= 19 || (max_date < today && max_week >= 18))
  }
  FALSE
}

team_made_playoffs <- function(team, sched) {
  team <- normalize_team(team)
  if (!is.null(sched) && nrow(sched) > 0) {
    home_team <- normalize_team(get_col(sched, c("home_team", "home_team_abbr"), ""))
    away_team <- normalize_team(get_col(sched, c("away_team", "away_team_abbr"), ""))
    week <- suppressWarnings(as.integer(get_col(sched, c("week"), NA)))
    post_mask <- !is.na(week) & week >= 19
    team_in_post <- (home_team == team | away_team == team) & post_mask
    return(any(team_in_post))
  }
  NA
}

playoff_round_label <- function(row) {
  if (!is.null(row$game_type)) {
    gt <- toupper(as.character(row$game_type))
    if (gt %in% c("WC", "WILD", "WILD_CARD")) return("Wild Card")
    if (gt %in% c("DIV", "DIVISIONAL")) return("Divisional")
    if (gt %in% c("CONF", "CONFERENCE")) return("Conference")
    if (gt %in% c("SB", "SUPERBOWL", "SUPER_BOWL")) return("Super Bowl")
  }
  if (!is.null(row$week)) {
    wk <- suppressWarnings(as.integer(row$week))
    if (is.finite(wk)) {
      if (wk == 19) return("Wild Card")
      if (wk == 20) return("Divisional")
      if (wk == 21) return("Conference")
      if (wk >= 22) return("Super Bowl")
    }
  }
  "Playoffs"
}

get_next_game <- function(player_id) {
  dir <- get_player_directory()
  dir$player_id <- as.character(get_col(dir, c("player_id", "gsis_id"), ""))
  rows <- dir[dir$player_id == as.character(player_id), , drop = FALSE]
  if (nrow(rows) == 0) return(list(status = "player_not_found"))

  team <- normalize_team(get_col(rows, c("team", "team_abbr"), ""))
  team <- team[team != ""][1]
  if (is.na(team) || team == "") return(list(status = "team_missing"))

  seasons <- get_available_seasons()
  if (length(seasons) == 0) return(list(status = "seasons_unavailable"))
  current_season <- max(seasons, na.rm = TRUE)
  identity <- get_player_week_identity()
  identity <- identity[as.integer(identity$season) == as.integer(current_season), , drop = FALSE]
  if (nrow(identity) == 0) return(list(status = "season_unavailable"))
  if (!any(as.character(identity$player_id) == as.character(player_id))) {
    return(list(status = "not_in_current_season"))
  }

  sched <- load_schedule_for_season(current_season)
  if (is.null(sched) || nrow(sched) == 0) return(list(status = "schedule_unavailable"))

  home_team <- normalize_team(get_col(sched, c("home_team", "home_team_abbr"), ""))
  away_team <- normalize_team(get_col(sched, c("away_team", "away_team_abbr"), ""))
  game_date <- as.Date(get_col(sched, c("gameday", "game_date"), NA))
  weeks <- as.integer(get_col(sched, c("week"), NA))
  game_type <- if ("game_type" %in% names(sched)) as.character(sched$game_type) else NULL
  season_type <- if ("season_type" %in% names(sched)) as.character(sched$season_type) else NULL

  team_mask <- home_team == team | away_team == team
  sched_team <- sched[team_mask, , drop = FALSE]
  if (nrow(sched_team) == 0) return(list(status = "team_not_on_schedule"))

  sched_team$game_date <- game_date[team_mask]
  sched_team$week <- weeks[team_mask]
  sched_team$home_team <- home_team[team_mask]
  sched_team$away_team <- away_team[team_mask]
  if (!is.null(game_type)) sched_team$game_type <- game_type[team_mask]
  if (!is.null(season_type)) sched_team$season_type <- season_type[team_mask]

  today <- as.Date(Sys.time())
  playoffs_started <- detect_playoffs_started(sched, today)
  if (playoffs_started) {
    made_playoffs <- team_made_playoffs(team, sched)
    if (isTRUE(made_playoffs)) {
      post_mask <- rep(TRUE, nrow(sched_team))
      if ("game_type" %in% names(sched_team)) {
        post_mask <- !is.na(sched_team$game_type) & sched_team$game_type != "REG"
      } else if ("season_type" %in% names(sched_team)) {
        post_mask <- !is.na(sched_team$season_type) & sched_team$season_type != "REG"
      } else if ("week" %in% names(sched_team)) {
        post_mask <- !is.na(sched_team$week) & as.integer(sched_team$week) >= 19
      }
      post_games <- sched_team[post_mask, , drop = FALSE]
      future <- post_games[!is.na(post_games$game_date) & post_games$game_date >= today, , drop = FALSE]
      if (nrow(future) == 0) {
        return(list(status = "season_complete"))
      }
      future <- future[order(future$game_date, future$week), , drop = FALSE]
      next_row <- future[1, , drop = FALSE]
      opponent <- if (next_row$home_team == team) next_row$away_team else next_row$home_team
      home_away <- if (next_row$home_team == team) "HOME" else "AWAY"
      round_label <- playoff_round_label(next_row)
      return(list(
        status = "playoffs_upcoming",
        next_game = list(
          season = as.integer(current_season),
          week = as.integer(next_row$week),
          team = team,
          opponent = opponent,
          home_away = home_away,
          game_date = as.character(next_row$game_date),
          round = round_label
        )
      ))
    }
    if (identical(made_playoffs, FALSE)) {
      return(list(status = "playoffs_not_qualified"))
    }
    return(list(status = "playoffs_unknown"))
  }

  future <- sched_team[!is.na(sched_team$game_date) & sched_team$game_date >= today, , drop = FALSE]
  if (nrow(future) == 0) {
    if (any(!is.na(sched_team$game_date)) && max(sched_team$game_date, na.rm = TRUE) < today) {
      return(list(status = "season_complete"))
    }
    return(list(status = "no_future_games"))
  }

  future <- future[order(future$game_date, future$week), , drop = FALSE]
  next_row <- future[1, , drop = FALSE]

  opponent <- if (next_row$home_team == team) next_row$away_team else next_row$home_team
  home_away <- if (next_row$home_team == team) "HOME" else "AWAY"

  list(
    status = "ok",
    next_game = list(
      season = as.integer(current_season),
      week = as.integer(next_row$week),
      team = team,
      opponent = opponent,
      home_away = home_away,
      game_date = as.character(next_row$game_date)
    )
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
players_endpoint <- function(res) {
  api_safe(res, function() {
    players <- get_players()
    pd_path <- file.path(repo_root, "data", "cache", "player_directory.parquet")
    if (is.null(players) || !is.data.frame(players)) {
      message("players_endpoint: invalid players object from ", pd_path)
      return(api_error(
        res,
        500L,
        "empty_players_directory",
        "player_directory returned no rows.",
        details = list(path = pd_path, rows = 0L)
      ))
    }
    if (nrow(players) == 0) {
      message("players_endpoint: empty players from ", pd_path)
      return(api_error(
        res,
        500L,
        "empty_players_directory",
        "player_directory returned no rows.",
        details = list(path = pd_path, rows = 0L)
      ))
    }
    stopifnot(is.data.frame(players))
    stopifnot(nrow(players) > 0)
    players <- safe_add_player_eligibility(players)
    api_ok(list(players = players))
  }, "players_endpoint")
}

#* List available teams
#* @get /teams
teams_endpoint <- function(res) {
  api_safe(res, function() {
    api_ok(list(teams = get_teams()))
  }, "teams_endpoint")
}

#* List available seasons
#* @get /seasons
seasons_endpoint <- function(res) {
  api_safe(res, function() {
    seasons <- safe_get_seasons()
    api_ok(list(seasons = seasons))
  }, "seasons_endpoint")
}

#* List historical games for a player
#* @get /player/<player_id>/games
player_games_endpoint <- function(player_id, res) {
  api_safe(res, function() {
    if (is.null(player_id) || !nzchar(player_id)) {
      return(api_error(res, 400L, "player_id_missing", "player_id is required"))
    }
    dir <- get_player_directory()
    if (!any(as.character(dir$player_id) == as.character(player_id))) {
      return(api_error(res, 404L, "player_not_found", "player_id not found"))
    }
    games <- get_player_games(player_id)
    if (nrow(games) == 0) {
      return(api_error(res, 404L, "games_not_found", "No historical games found for this player."))
    }
    min_games_required <- if (exists("get_min_games_required")) get_min_games_required() else 4L
    warning <- NULL
    if (nrow(games) < min_games_required) {
      warning <- list(code = "insufficient_history", message = "Player has limited historical games.")
    }
    api_ok(list(games = games, warning = warning))
  }, "player_games_endpoint")
}

#* Get next scheduled game for a player
#* @get /player/<player_id>/next_game
player_next_game_endpoint <- function(player_id, res) {
  api_safe(res, function() {
    if (is.null(player_id) || !nzchar(player_id)) {
      return(api_error(res, 400L, "player_id_missing", "player_id is required"))
    }
    schedule_path <- file.path(repo_root, "data", "cache", "schedules.rds")
    if (!file.exists(schedule_path)) {
      return(api_error(res, 500L, "schedule_unavailable", "Schedule cache missing."))
    }
    identity_path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")
    if (!file.exists(identity_path)) {
      return(api_error(res, 500L, "player_week_identity_missing", "player_week_identity cache missing."))
    }
    next_game <- get_next_game(player_id)
    if (is.null(next_game) || is.null(next_game$status)) {
      return(api_error(res, 500L, "next_game_error", "Unable to resolve next game."))
    }
    if (next_game$status == "ok") {
      return(api_ok(list(next_game = next_game$next_game)))
    }
    if (next_game$status == "playoffs_upcoming") {
      return(api_ok(list(
        next_game = next_game$next_game,
        reason = "playoffs_upcoming",
        message = "Playoffs have started."
      )))
    }
    if (next_game$status == "playoffs_not_qualified") {
      return(api_ok(list(
        next_game = NULL,
        reason = "playoffs_no_qualify",
        message = "Playoffs have started. This player did not make the playoffs."
      )))
    }
    if (next_game$status == "playoffs_unknown") {
      return(api_ok(list(
        next_game = NULL,
        reason = "playoffs_unknown",
        message = "Playoffs have started."
      )))
    }
    if (next_game$status == "season_complete") {
      return(api_ok(list(next_game = NULL, reason = "season_complete")))
    }
    if (next_game$status == "not_in_current_season") {
      return(api_ok(list(next_game = NULL, reason = "not_in_current_season")))
    }
    if (next_game$status == "player_not_found") {
      return(api_error(res, 404L, "player_not_found", "player_id not found"))
    }
    if (next_game$status == "schedule_unavailable") {
      return(api_error(res, 500L, "schedule_unavailable", "Schedule data unavailable."))
    }
    api_error(res, 404L, "next_game_not_found", "No upcoming game found for this player.")
  }, "player_next_game_endpoint")
}

#* Run simulation
#* @post /simulate
simulate_endpoint <- function(req, res) {
  body <- NULL
  if (!is.null(req$postBody) && nzchar(req$postBody)) {
    body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  }
  if (is.null(body)) {
    return(api_error(res, 400L, "request_body_missing", "Request body is required."))
  }

  directory_df <- get_player_directory()
  player_id <- body$player_id
  if (is.null(player_id) || !nzchar(trimws(as.character(player_id)))) {
    player_id <- resolve_player_id(body$player_name, directory_df)
  }
  if (is.null(player_id) || !nzchar(trimws(as.character(player_id)))) {
    return(api_error(res, 404L, "player_not_found", "Player not found."))
  }

  api_safe(res, function() {
    if (is.null(repo_root) || !nzchar(repo_root) || !dir.exists(repo_root)) {
      return(api_error(res, 500L, "repo_root_invalid", "API repo root is invalid."))
    }
    pd_path <- file.path(repo_root, "data", "cache", "player_directory.parquet")
    pwi_path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")
    if (!file.exists(pd_path)) {
      return(api_error(res, 500L, "player_directory_missing", "player_directory cache missing."))
    }
    if (!file.exists(pwi_path)) {
      return(api_error(res, 500L, "player_week_identity_missing", "player_week_identity cache missing."))
    }
    cache_status <- validate_simulation_caches()
    if (length(cache_status$missing) > 0 || length(cache_status$empty) > 0) {
      return(api_error(
        res,
        500L,
        "simulation_cache_missing",
        "Required simulation caches are missing or empty.",
        details = list(missing = cache_status$missing, empty = cache_status$empty)
      ))
    }
    old_opts <- options(
      READTHEFIELD_FREEZE_RAW = TRUE,
      READTHEFIELD_USE_RAW_CACHE = TRUE
    )
    on.exit(options(old_opts), add = TRUE)
    result <- simulate_player_game_v1(
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
    if (is.null(result) || (is.list(result) && isFALSE(result$ok))) {
      error_type <- if (!is.null(result$error_type)) result$error_type else "internal_error"
      error_code <- if (!is.null(result$error_code)) result$error_code else "simulation_failed"
      status <- map_error_status(error_type, error_code)
      message <- if (!is.null(result$message)) result$message else "Simulation failed."
      details <- if (!is.null(result$details)) result$details else list()
      return(api_error(res, status, error_code, message, details))
    }
    if (is.null(result$summary) || nrow(result$summary) == 0 || is.null(result$metadata)) {
      return(api_error(res, 500L, "simulation_invalid", "Simulation returned incomplete results."))
    }
    result$ok <- NULL
    api_ok(result)
  }, "simulate_endpoint")
}
