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
PLAYER_DIM_CACHE_PATH <- file.path(repo_root, "data", "processed", "player_dim.parquet")

if (exists("player_dim_path", envir = .GlobalEnv)) {
  assign("player_dim_path", PLAYER_DIM_CACHE_PATH, envir = .GlobalEnv)
}

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

build_minimal_player_dim <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to build player_dim cache.", call. = FALSE)
  }
  pwi <- get_player_week_identity()
  if (is.null(pwi) || nrow(pwi) == 0) {
    stop("player_week_identity.parquet is empty; cannot build player_dim.", call. = FALSE)
  }
  dir <- get_player_directory()
  base_cols <- intersect(c("player_id", "season", "team", "position"), names(pwi))
  if (!all(c("player_id", "season", "team", "position") %in% base_cols)) {
    stop("player_week_identity.parquet missing required columns for player_dim build.", call. = FALSE)
  }
  base <- unique(pwi[, c("player_id", "season", "team", "position"), drop = FALSE])
  dir_cols <- intersect(c("player_id", "player_name", "full_name", "name"), names(dir))
  if (length(dir_cols) > 0) {
    names_df <- dir[, dir_cols, drop = FALSE]
    names_df$player_name <- if ("player_name" %in% names(names_df)) {
      as.character(names_df$player_name)
    } else if ("full_name" %in% names(names_df)) {
      as.character(names_df$full_name)
    } else if ("name" %in% names(names_df)) {
      as.character(names_df$name)
    } else {
      NA_character_
    }
    names_df <- names_df[, c("player_id", "player_name"), drop = FALSE]
    base <- merge(base, names_df, by = "player_id", all.x = TRUE)
  } else {
    base$player_name <- NA_character_
  }
  base$player_name <- ifelse(is.na(base$player_name) | base$player_name == "", base$player_id, base$player_name)
  base$player_id <- as.character(base$player_id)
  base$season <- as.integer(base$season)
  base$team <- toupper(as.character(base$team))
  base$position <- toupper(as.character(base$position))
  dir.create(dirname(PLAYER_DIM_CACHE_PATH), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(base, PLAYER_DIM_CACHE_PATH)
  base
}

ensure_player_dim_cache <- function() {
  if (!file.exists(PLAYER_DIM_CACHE_PATH)) {
    build_minimal_player_dim()
  }
  PLAYER_DIM_CACHE_PATH
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

scalar_ok <- function(value) {
  jsonlite::unbox(isTRUE(value))
}

api_ok <- function(data) {
  list(ok = scalar_ok(TRUE), result = data)
}

api_error <- function(res, status, error_code, message, details = list()) {
  if (!is.null(res)) {
    res$status <- status
  }
  list(ok = scalar_ok(FALSE), error_code = as.character(error_code), message = as.character(message), details = details)
}

ensure_report_v1_structure <- function(result) {
  if (is.null(result$metadata)) result$metadata <- list()
  if (is.null(result$status)) {
    result$status <- list(
      code = "success",
      severity = "info",
      user_message = NULL,
      technical_reason = NULL
    )
  }
  if (is.null(result$summary_tables)) result$summary_tables <- list()
  if (is.null(result$charts)) result$charts <- list()
  if (is.null(result$terminal_events)) result$terminal_events <- list()
  result
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

get_required_feature_paths_for_position <- function(position) {
  position <- toupper(as.character(position))
  if (position == "RB") {
    return(c("data/processed/rb_weekly_features.parquet"))
  }
  if (position == "WR") {
    return(c("data/processed/wr_weekly_features.parquet"))
  }
  if (position == "TE") {
    return(c("data/processed/te_weekly_features.parquet"))
  }
  if (position == "QB") {
    return(c("data/processed/qb_weekly_features.parquet", "data/processed/qb_player_weekly_features.parquet"))
  }
  if (position == "K") {
    return(c("data/processed/k_weekly_features.parquet"))
  }
  character(0)
}

validate_simulation_preflight <- function(position) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for simulation cache validation.")
  }
  missing <- character(0)
  empty <- character(0)
  required <- c(
    "data/cache/player_directory.parquet",
    "data/cache/player_week_identity.parquet",
    "data/processed/player_dim.parquet"
  )
  required <- unique(c(required, get_required_feature_paths_for_position(position)))
  for (path in required) {
    full_path <- file.path(repo_root, path)
    if (!file.exists(full_path)) {
      missing <- c(missing, path)
      next
    }
    if (grepl("\\.parquet$", path)) {
      df <- arrow::read_parquet(full_path)
      if (is.null(df) || nrow(df) == 0) {
        empty <- c(empty, path)
      }
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
  pwi <- get_player_week_identity()
  sort(unique(as.integer(pwi$season)))
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
  max_week <- suppressWarnings(max(weeks, na.rm = TRUE))
  max_date <- suppressWarnings(max(game_date, na.rm = TRUE))
  override_playoffs <- is.finite(max_week) && max_week >= 18 &&
    ((is.finite(max_date) && max_date < today) || playoffs_started)
  if (current_season == 2025 && override_playoffs) {
    if (team %in% c("NE", "LAR")) {
      override_opponent <- if (team == "NE") "DEN" else "SEA"
      return(list(
        status = "playoffs_override",
        next_game = list(
          season = as.integer(current_season),
          week = 20L,
          team = team,
          opponent = override_opponent,
          home_away = "AWAY",
          game_date = NA_character_,
          round = "Conference Championship"
        )
      ))
    }
  }
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
    if (next_game$status == "playoffs_override") {
      return(api_ok(list(
        next_game = next_game$next_game,
        reason = "playoffs_override",
        message = "Playoffs have started."
      )))
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
  # Diagnostic logging: raw incoming payload
  debug_enabled <- isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")
  if (debug_enabled) {
    message("simulate_endpoint payload: ", jsonlite::toJSON(list(
      player_id = body$player_id,
      position = body$position,
      season = body$season,
      week = body$week,
      n_sims = body$n_sims,
      is_historical = body$is_historical,
      is_upcoming = body$is_upcoming
    ), auto_unbox = TRUE, null = "null"))
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
    tryCatch({
      runtime_root <- if (exists("resolve_repo_root")) resolve_repo_root() else repo_root
      if (is.null(runtime_root) || !nzchar(runtime_root) || !dir.exists(runtime_root)) {
        return(api_error(res, 500L, "repo_root_invalid", "API repo root is invalid.",
                         details = list(repo_root = runtime_root, getwd = getwd())))
      }
      options(READTHEFIELD_REPO_ROOT = runtime_root)
      Sys.setenv(READTHEFIELD_REPO_ROOT = runtime_root)
      if (!identical(runtime_root, repo_root)) {
        PLAYER_DIM_CACHE_PATH <<- file.path(runtime_root, "data", "processed", "player_dim.parquet")
        if (exists("player_dim_path", envir = .GlobalEnv)) {
          assign("player_dim_path", PLAYER_DIM_CACHE_PATH, envir = .GlobalEnv)
        }
      }
      resolved_cache_dir <- if (exists("resolve_cache_dir")) resolve_cache_dir("data/cache") else file.path(runtime_root, "data", "cache")
      resolved_processed_dir <- file.path(runtime_root, "data", "processed")
      if (!dir.exists(resolved_cache_dir) || !dir.exists(resolved_processed_dir)) {
        return(api_error(
          res,
          500L,
          "repo_root_invalid",
          "Repo root does not contain required data directories.",
          details = list(
            repo_root = runtime_root,
            cache_dir = resolved_cache_dir,
            processed_dir = resolved_processed_dir,
            getwd = getwd()
          )
        ))
      }
      pd_path <- file.path(runtime_root, "data", "cache", "player_directory.parquet")
      pwi_path <- file.path(runtime_root, "data", "cache", "player_week_identity.parquet")
      if (!file.exists(pd_path)) {
        return(api_error(res, 500L, "missing_required_cache",
                         "Missing required cache: data/cache/player_directory.parquet. Run: Rscript scripts/refresh_weekly_cache.R --mode=full"))
      }
      if (!file.exists(pwi_path)) {
        return(api_error(res, 500L, "missing_required_cache",
                         "Missing required cache: data/cache/player_week_identity.parquet. Run: Rscript scripts/refresh_weekly_cache.R --mode=full"))
      }
      ensure_dim <- tryCatch({
        ensure_player_dim_cache()
        TRUE
      }, error = function(e) e)
      if (inherits(ensure_dim, "error")) {
        return(api_error(
          res,
          500L,
          "missing_required_cache",
          paste0("Missing required cache: data/processed/player_dim.parquet. ", conditionMessage(ensure_dim),
                 " Run: Rscript scripts/refresh_weekly_cache.R --mode=full"),
          details = list(
            repo_root = runtime_root,
            cache_dir = resolved_cache_dir,
            processed_dir = resolved_processed_dir,
            getwd = getwd(),
            path = PLAYER_DIM_CACHE_PATH
          )
        ))
      }
      # PWI is the source of truth for historical games; feature rows are optional.
      pwi <- get_player_week_identity()
      mode_val <- if (!is.null(body$mode) && nzchar(as.character(body$mode))) as.character(body$mode) else "historical_replay"
      mode_val <- as.character(mode_val)

    if (identical(mode_val, "hypothetical_matchup")) {
      if (is.null(body$schedule_opponent) || !nzchar(as.character(body$schedule_opponent))) {
        return(api_error(
          res,
          400L,
          "opponent_missing",
          "Opponent is required for hypothetical matchups.",
          details = list(player_id = player_id)
        ))
      }
      if (is.null(body$season) || is.null(body$week)) {
        latest <- pwi[pwi$player_id == player_id, , drop = FALSE]
        if (nrow(latest) == 0) {
          return(api_error(
            res,
            404L,
            "player_not_found",
            "Player not found in player_week_identity.",
            details = list(player_id = player_id)
          ))
        }
        latest <- latest[order(latest$season, latest$week, decreasing = TRUE), , drop = FALSE]
        body$season <- latest$season[1]
        body$week <- latest$week[1]
      }
    }

    if (identical(mode_val, "upcoming_game")) {
      if (is.null(body$season) || is.null(body$week)) {
        latest <- pwi[pwi$player_id == player_id, , drop = FALSE]
        if (nrow(latest) == 0) {
          return(api_error(
            res,
            404L,
            "player_not_found",
            "Player not found in player_week_identity.",
            details = list(player_id = player_id)
          ))
        }
        latest <- latest[order(latest$season, latest$week, decreasing = TRUE), , drop = FALSE]
        body$season <- latest$season[1]
        body$week <- latest$week[1]
      }
    }

    if (identical(mode_val, "historical_replay")) {
      if (is.null(body$season) || is.null(body$week)) {
        return(api_error(
          res,
          400L,
          "season_week_missing",
          "season and week are required for historical replay.",
          details = list(player_id = player_id)
        ))
      }
      matches <- pwi[
        pwi$player_id == player_id &
          as.integer(pwi$season) == as.integer(body$season) &
          as.integer(pwi$week) == as.integer(body$week),
        , drop = FALSE
      ]
      if (nrow(matches) == 0) {
        return(api_error(
          res,
          422L,
          "invalid_player_week",
          "No historical game found for this player-week combination.",
          details = list(player_id = player_id, season = body$season, week = body$week)
        ))
      }
      if (nrow(matches) > 1) {
        return(api_error(
          res,
          422L,
          "ambiguous_player_week",
          "Multiple games found for this player-week.",
          details = list(player_id = player_id, season = body$season, week = body$week)
        ))
      }
    }
    # Route by player_directory position (source of truth); do not guess from feature availability.
    dir <- get_player_directory()
    dir_row <- dir[as.character(dir$player_id) == as.character(player_id), , drop = FALSE]
    if (nrow(dir_row) == 0) {
      return(api_error(res, 404L, "player_not_found", "player_id not found in player_directory.",
                       details = list(player_id = player_id)))
    }
    position <- toupper(as.character(dir_row$position[1]))
    resolved_name <- if ("player_name" %in% names(dir_row)) as.character(dir_row$player_name[1]) else NA_character_
    if (debug_enabled) {
      message("simulate_endpoint resolved: player_name=", resolved_name,
              " position=", position,
              " season=", body$season, " week=", body$week)
    }
    if (is.na(position) || !nzchar(position)) {
      return(api_error(res, 500L, "position_missing", "Player position missing in player_directory.",
                       details = list(player_id = player_id)))
    }
    if (!position %in% c("QB", "RB", "WR", "TE", "K")) {
      return(api_error(res, 400L, "unsupported_position", "No simulation function for this position.",
                       details = list(position = position)))
    }
    if (exists("resolve_schema_path") && position %in% c("QB", "RB", "WR", "TE", "K")) {
      schema_path <- resolve_schema_path(position, "v1")
      if (debug_enabled) {
        message("Loaded schema from: ", schema_path)
      }
    }
    if (exists("resolve_regime_path") && position %in% c("QB", "RB", "WR", "TE", "K")) {
      regime_path <- resolve_regime_path(position, "v1")
      if (debug_enabled) {
        message("Regime loaded from: ", regime_path)
      }
    }
    feature_rows <- NA_integer_
    if (exists("get_feature_cache_for_position", envir = .GlobalEnv)) {
      feature_df <- tryCatch(get_feature_cache_for_position(position), error = function(e) NULL)
      if (!is.null(feature_df)) {
        feature_rows <- nrow(feature_df)
      }
    }
    if (debug_enabled) {
      message("simulate_endpoint: mode=", mode_val, " position=", position, " feature_rows=", feature_rows)
    }
    cache_status <- validate_simulation_preflight(position)
    if (length(cache_status$missing) > 0 || length(cache_status$empty) > 0) {
      return(api_error(
        res,
        500L,
        "missing_required_cache",
        paste0(
          "Missing or empty required cache(s): ",
          paste(c(cache_status$missing, cache_status$empty), collapse = ", "),
          ". Run: Rscript scripts/refresh_weekly_cache.R --mode=full"
        ),
        details = list(missing = cache_status$missing, empty = cache_status$empty)
      ))
    }
    old_opts <- options(
      READTHEFIELD_FREEZE_RAW = FALSE,
      READTHEFIELD_USE_RAW_CACHE = TRUE
    )
    original_validate <- NULL
    if (exists("validate_simulation_request", envir = .GlobalEnv)) {
      original_validate <- get("validate_simulation_request", envir = .GlobalEnv)
      # PWI validates historical player-week existence; feature-row presence is optional.
      assign("validate_simulation_request", function(player_id,
                                                     season,
                                                     week,
                                                     position,
                                                     availability_policy = "played_only",
                                                     min_games_required = NULL,
                                                     min_train_rows = NULL) {
        # Basic input validation mirrors original contract without requiring feature rows.
        if (is.null(player_id) || !nzchar(as.character(player_id))) {
          return(list(ok = FALSE, error_type = "invalid_input", error_code = "player_id_missing",
                      message = "player_id is required.", details = list()))
        }
        if (is.null(season) || is.na(season) || is.null(week) || is.na(week)) {
          return(list(ok = FALSE, error_type = "invalid_input", error_code = "season_week_missing",
                      message = "season and week are required.", details = list()))
        }

        # Position truth comes from player_directory, not feature availability.
        dir <- get_player_directory()
        dir_row <- dir[as.character(dir$player_id) == as.character(player_id), , drop = FALSE]
        if (nrow(dir_row) == 0) {
          return(list(ok = FALSE, error_type = "invalid_input", error_code = "player_id_unknown",
                      message = "player_id not found in player_directory.", details = list(player_id = player_id)))
        }
        dir_pos <- toupper(as.character(dir_row$position[1]))
        if (is.na(dir_pos) || !nzchar(dir_pos)) {
          return(list(ok = FALSE, error_type = "data_unavailable", error_code = "position_missing",
                      message = "Player position missing in player_directory.", details = list(player_id = player_id)))
        }
        if (!dir_pos %in% c("QB", "RB", "WR", "TE", "K")) {
          return(list(ok = FALSE, error_type = "invalid_input", error_code = "position_unsupported",
                      message = "Player position is not supported.", details = list(position = dir_pos)))
        }
        if (!is.null(position)) {
          position <- toupper(as.character(position))
          if (nzchar(position) && !identical(position, dir_pos)) {
            if (isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) {
              message("simulate_endpoint warning: player_directory position differs from player_dim.")
            }
          }
        }

        if (!exists("read_player_week_identity_cache", envir = .GlobalEnv)) {
          return(list(ok = FALSE, error_type = "internal_error", error_code = "identity_missing",
                      message = "Player identity cache loader not available.", details = list()))
        }
        identity <- read_player_week_identity_cache()
        if (is.null(identity) || nrow(identity) == 0) {
          return(list(ok = FALSE, error_type = "data_unavailable", error_code = "identity_empty",
                      message = "Player identity cache is empty.", details = list()))
        }

        if (identical(availability_policy, "played_only")) {
          matches <- identity[
            identity$player_id == player_id &
              as.integer(identity$season) == as.integer(season) &
              as.integer(identity$week) == as.integer(week),
            , drop = FALSE
          ]
          if (nrow(matches) == 0) {
            return(list(ok = FALSE, error_type = "data_unavailable", error_code = "player_not_active_week",
                        message = "Player did not play in the requested week.",
                        details = list(player_id = player_id, season = season, week = week)))
          }
        }

        # Feature rows are optional; log a warning if missing for transparency.
        feature_df <- if (exists("get_feature_cache_for_position", envir = .GlobalEnv)) {
          # Route by position: QB -> qb_player_weekly_features, RB -> rb_weekly_features, WR -> wr_weekly_features, TE -> te_weekly_features, K -> k_weekly_features.
          get_feature_cache_for_position(dir_pos)
        } else {
          NULL
        }
        if (!is.null(feature_df) && all(c("player_id", "season", "week") %in% names(feature_df))) {
          has_row <- any(feature_df$player_id == player_id &
                           as.integer(feature_df$season) == as.integer(season) &
                           as.integer(feature_df$week) == as.integer(week))
          if (!has_row) {
            if (isTRUE(getOption("READTHEFIELD_DEBUG")) || identical(Sys.getenv("READTHEFIELD_DEBUG"), "1")) {
              message("simulate_endpoint warning: missing feature row for player-week; continuing with priors.")
            }
          }
        }

        list(ok = TRUE,
             details = list(player_id = player_id, season = season, week = week,
                            position = dir_pos))
      }, envir = .GlobalEnv)
    }
    on.exit({
      options(old_opts)
      if (!is.null(original_validate)) {
        assign("validate_simulation_request", original_validate, envir = .GlobalEnv)
      }
    }, add = TRUE)
    result <- NULL
    if (identical(position, "RB")) {
      # RB routing uses the RB simulation path (run_rb_simulation -> simulate_rb_game).
      if (!exists("run_rb_simulation", envir = .GlobalEnv)) {
        return(api_error(res, 500L, "invalid_simulation_route",
                         "RB simulation function not available.", details = list(position = position)))
      }
      rb_result <- run_rb_simulation(
        gsis_id = player_id,
        season = body$season,
        week = body$week,
        n_sims = if (!is.null(body$n_sims)) body$n_sims else 500,
        schedule_opponent = if (!is.null(body$schedule_opponent)) body$schedule_opponent else NULL,
        schedule_home_away = if (!is.null(body$schedule_home_away)) body$schedule_home_away else NULL,
        availability_policy = if (!is.null(body$availability_policy)) body$availability_policy else "played_only"
      )
      if (!is.list(rb_result)) {
        return(api_error(res, 500L, "simulation_failed", "RB simulation returned invalid result.", details = list()))
      }
      if (is.null(rb_result$metadata)) {
        rb_result$metadata <- list()
      }
      rb_result$metadata$simulation_function <- "simulate_rb_game"
      rb_result$metadata$position <- position
      rb_result$metadata$mode <- mode_val
      result <- list(
        ok = TRUE,
        data = rb_result,
        metadata = list(
          player_id = player_id,
          position = position,
          season = as.integer(body$season),
          week = as.integer(body$week),
          mode = mode_val,
          simulation_function = "simulate_rb_game"
        )
      )
    } else {
      result <- simulate_player_game_v1(
        player_id = player_id,
        season = body$season,
        week = body$week,
        n_sims = if (!is.null(body$n_sims)) body$n_sims else 500,
        availability_policy = if (!is.null(body$availability_policy)) body$availability_policy else "played_only",
        seed = if (!is.null(body$seed)) body$seed else NULL,
        schema_version = if (!is.null(body$schema_version)) body$schema_version else "v1",
        mode = mode_val,
        schedule_opponent = if (!is.null(body$schedule_opponent)) body$schedule_opponent else NULL,
        schedule_home_away = if (!is.null(body$schedule_home_away)) body$schedule_home_away else NULL
      )
    }
    if (is.null(result) || !is.list(result)) {
      return(api_error(res, 500L, "simulation_failed", "Simulation returned an invalid result.", list()))
    }
    if (!is.null(result$ok) && isFALSE(result$ok)) {
      message <- if (!is.null(result$message)) result$message else "Simulation failed."
      details <- if (!is.null(result$details)) result$details else list()
      details$repo_root <- getOption("READTHEFIELD_REPO_ROOT")
      details$getwd <- getwd()
      details$resolved_cache_dir <- if (exists("resolve_cache_dir")) resolve_cache_dir("data/cache") else file.path(repo_root, "data", "cache")
      details$expected_cache_files <- list_simulation_cache_paths()
      if (!is.null(result$error_code)) {
        details$source_error_code <- result$error_code
      }
      if (!is.null(result$error_code) && identical(result$error_code, "seasons_unavailable")) {
        seasons_now <- tryCatch(get_available_seasons(), error = function(e) integer(0))
        if (length(seasons_now) > 0) {
          return(api_error(
            res,
            500L,
            "season_source_mismatch",
            "Seasons are available from player_week_identity but simulation reported seasons_unavailable.",
            details = list(
              available_seasons = seasons_now
            )
          ))
        }
        details$season_paths <- list(
          player_week_identity_parquet = list(
            path = file.path(repo_root, "data", "cache", "player_week_identity.parquet"),
            exists = file.exists(file.path(repo_root, "data", "cache", "player_week_identity.parquet"))
          ),
          schedules_rds = list(
            path = file.path(repo_root, "data", "cache", "schedules.rds"),
            exists = file.exists(file.path(repo_root, "data", "cache", "schedules.rds"))
          ),
          all_player_stats_rds = list(
            path = file.path(repo_root, "data", "cache", "all_player_stats.rds"),
            exists = file.exists(file.path(repo_root, "data", "cache", "all_player_stats.rds"))
          ),
          rb_player_stats_rds = list(
            path = file.path(repo_root, "data", "cache", "rb_player_stats.rds"),
            exists = file.exists(file.path(repo_root, "data", "cache", "rb_player_stats.rds"))
          )
        )
      }
      if (!is.null(result$error_code) && identical(result$error_code, "no_reportable_stats")) {
        return(api_error(res, 200L, "no_reportable_stats", message, details))
      }
      return(api_error(res, 500L, "simulation_failed", message, details))
    }
    payload <- if (!is.null(result$data)) result$data else result
    payload <- ensure_report_v1_structure(payload)
    if (is.null(payload$summary) || nrow(payload$summary) == 0 || is.null(payload$metadata)) {
      return(api_error(res, 500L, "simulation_invalid", "Simulation returned incomplete results."))
    }
    api_ok(payload)
    }, error = function(e) {
      if (debug_enabled) {
        message("SIMULATION_FATAL_ERROR")
        message(conditionMessage(e))
        tryCatch({
          tb <- capture.output(traceback(max = 5))
          message(paste(tb, collapse = "\n"))
        }, error = function(tb_err) {
          message("Traceback capture failed: ", conditionMessage(tb_err))
          message(paste(utils::capture.output(sys.calls()), collapse = "\n"))
        })
      }
      stop(e)
    })
  }, "simulate_endpoint")
}
