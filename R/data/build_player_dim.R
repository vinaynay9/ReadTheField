 # Build Player Dimension Table
 #
 # Creates a season-level player dimension cache for UI and search.
# One row per (player_id, season) for offensive skill positions + kickers.
 #
 # Dependencies:
 #   - nflreadr
 #   - arrow
 #
 # Usage:
 #   player_dim <- build_player_dim(seasons = 2021:2024, write_cache = TRUE)
 
player_dim_path <- file.path("data", "processed", "player_dim.parquet")

# Schema utilities
canonicalize_names <- function(df) {
  names(df) <- tolower(names(df))
  names(df) <- gsub("[^a-z0-9_]+", "_", names(df))
  df
}

rename_aliases <- function(df) {
  alias_map <- c(
    "gsisid" = "gsis_id",
    "gsis_player_id" = "gsis_id",
    "player_gsis_id" = "gsis_id",
    "fullname" = "full_name",
    "player_name" = "full_name",
    "name" = "full_name",
    "firstname" = "first_name",
    "lastname" = "last_name",
    "lname" = "last_name",
    "pos" = "position",
    "team_abbr" = "team",
    "recent_team" = "team",
    "current_team" = "team",
    "headshot" = "headshot_url",
    "headshoturi" = "headshot_url",
    "headshot_url_small" = "headshot_url"
  )
  hits <- intersect(names(alias_map), names(df))
  if (length(hits) > 0) {
    names(df)[match(hits, names(df))] <- unname(alias_map[hits])
  }
  df
}

coalesce_cols <- function(df, out, candidates) {
  candidates <- intersect(candidates, names(df))
  if (length(candidates) == 0) {
    df[[out]] <- NA
    return(df)
  }
  
  # Extract column vectors explicitly
  values <- lapply(candidates, function(col) df[[col]])
  
  # Coalesce manually across vectors
  df[[out]] <- Reduce(
    function(x, y) dplyr::coalesce(x, y),
    values
  )
  df
}

assert_required_cols <- function(df, required, context = "data frame") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s missing required columns: %s\nAvailable columns: %s",
        context,
        paste(missing, collapse = ", "),
        paste(names(df), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

 #' Build player dimension table
 #'
 #' @param seasons Integer vector of seasons to include
 #' @param write_cache Logical, if TRUE write to parquet cache
#' @return data.frame with one row per (player_id, season)
 build_player_dim <- function(seasons, write_cache = TRUE) {
   
   if (missing(seasons) || length(seasons) == 0) {
     stop("seasons is required to build player_dim")
   }
   
   seasons <- as.integer(seasons)
   seasons <- seasons[!is.na(seasons)]
   if (length(seasons) == 0) {
     stop("seasons must contain valid integers")
   }
   
   if (!requireNamespace("nflreadr", quietly = TRUE)) {
     stop("Package 'nflreadr' is required to build player_dim.")
   }
   
   if (!requireNamespace("arrow", quietly = TRUE)) {
     stop("Package 'arrow' is required to build player_dim.")
   }
   
   players <- NULL
   use_raw_cache <- isTRUE(getOption("READTHEFIELD_USE_RAW_CACHE", TRUE))
   freeze_raw <- isTRUE(getOption("READTHEFIELD_FREEZE_RAW", FALSE))
   if (exists("read_raw_snapshot") && use_raw_cache) {
     players <- read_raw_snapshot("players.rds")
   }
   if (is.null(players) || nrow(players) == 0) {
     if (freeze_raw) {
       stop("Raw snapshot missing for players.rds. Set READTHEFIELD_FREEZE_RAW=FALSE to download.")
     }
     players <- nflreadr::load_players()
     if (exists("write_raw_snapshot")) {
       write_raw_snapshot(players, "players.rds")
     }
     if (exists("write_snapshot_info")) {
       write_snapshot_info(c(
         "players: players.rds",
         paste0("players_rows: ", if (is.null(players)) 0 else nrow(players))
       ))
     }
   }
   if (is.null(players) || nrow(players) == 0) {
     if (file.exists(player_dim_path)) {
       warning("nflreadr::load_players() returned empty result. Using cached player_dim.", call. = FALSE)
       return(arrow::read_parquet(player_dim_path))
     }
     stop("nflreadr::load_players() returned empty result. Cannot build player_dim.")
   }
   
   rosters <- NULL
   roster_key <- paste0("rosters_", paste(seasons, collapse = "_"), ".rds")
   if (exists("read_raw_snapshot") && use_raw_cache) {
     rosters <- read_raw_snapshot(roster_key)
   }
   if (is.null(rosters) || nrow(rosters) == 0) {
     if (freeze_raw) {
       stop("Raw snapshot missing for ", roster_key, ". Set READTHEFIELD_FREEZE_RAW=FALSE to download.")
     }
     rosters <- nflreadr::load_rosters(seasons = seasons)
     if (exists("write_raw_snapshot")) {
       write_raw_snapshot(rosters, roster_key)
     }
     if (exists("write_snapshot_info")) {
       write_snapshot_info(c(
         paste0("rosters: ", roster_key),
         paste0("roster_seasons: ", paste(seasons, collapse = ",")),
         paste0("roster_rows: ", if (is.null(rosters)) 0 else nrow(rosters))
       ))
     }
   }
   if (is.null(rosters) || nrow(rosters) == 0) {
     if (file.exists(player_dim_path)) {
       warning("nflreadr::load_rosters() returned empty result. Using cached player_dim.", call. = FALSE)
       return(arrow::read_parquet(player_dim_path))
     }
     stop("nflreadr::load_rosters() returned empty result. Cannot build player_dim.")
   }
   
   # Normalize player identifiers
   if (!"gsis_id" %in% names(players)) {
     if ("player_id" %in% names(players)) {
       players$gsis_id <- players$player_id
     } else if ("player_gsis_id" %in% names(players)) {
       players$gsis_id <- players$player_gsis_id
     } else {
       stop("Cannot find gsis_id in load_players() output.")
     }
   }
   
   if (!"gsis_id" %in% names(rosters)) {
     if ("player_id" %in% names(rosters)) {
       rosters$gsis_id <- rosters$player_id
     } else if ("player_gsis_id" %in% names(rosters)) {
       rosters$gsis_id <- rosters$player_gsis_id
     } else {
       stop("Cannot find gsis_id in load_rosters() output.")
     }
   }
   
   # Normalize name fields
   if (!"display_name" %in% names(players)) {
     if ("full_name" %in% names(players)) {
       players$display_name <- players$full_name
     } else if ("name" %in% names(players)) {
       players$display_name <- players$name
     } else {
       stop("Cannot find display_name in load_players() output.")
     }
   }
   
   if (!"first_name" %in% names(players) || !"last_name" %in% names(players)) {
     stop("load_players() output must include first_name and last_name.")
   }
   
   # Normalize headshot URL
   headshot_col <- NULL
   for (col in c("headshot_url", "headshot", "headshot_url_nfl")) {
     if (col %in% names(players)) {
       headshot_col <- col
       break
     }
   }
   if (is.null(headshot_col)) {
     stop("No headshot URL column found in load_players() output.")
   }
   
   # Normalize roster fields
   if (!"season" %in% names(rosters)) {
     stop("load_rosters() output must include season.")
   }
   if (!"team" %in% names(rosters)) {
     stop("load_rosters() output must include team.")
   }
   if (!"position" %in% names(rosters)) {
     stop("load_rosters() output must include position.")
   }
   
   rosters$season <- as.integer(rosters$season)
   rosters$team <- toupper(trimws(as.character(rosters$team)))
   rosters$position <- toupper(trimws(as.character(rosters$position)))
   
   allowed_positions <- c("QB", "RB", "WR", "TE", "K")
   rosters <- rosters[rosters$position %in% allowed_positions, , drop = FALSE]
   
   if (nrow(rosters) == 0) {
     stop("Roster filter produced zero rows for allowed positions.")
   }
   
   # Build season-level roster dim (one row per gsis_id, season)
   roster_dim <- aggregate(
     list(team = rosters$team, position = rosters$position),
     by = list(gsis_id = as.character(rosters$gsis_id),
               season = rosters$season),
     FUN = function(x) {
       x <- x[!is.na(x)]
       if (length(x) == 0) return(NA_character_)
       tail(x, 1)
     }
   )
   
   # Join player directory fields
  player_dir <- data.frame(
    gsis_id = as.character(players$gsis_id),
    full_name = as.character(players$display_name),
    first_name = as.character(players$first_name),
    last_name = as.character(players$last_name),
    display_name = as.character(players$display_name),
    headshot_url = as.character(players[[headshot_col]]),
    height = if ("height" %in% names(players)) as.numeric(players$height) else NA_real_,
    weight = if ("weight" %in% names(players)) as.numeric(players$weight) else NA_real_,
    age = if ("age" %in% names(players)) as.numeric(players$age) else NA_real_,
    draft_round = if ("draft_round" %in% names(players)) as.integer(players$draft_round) else NA_integer_,
    draft_pick_overall = if ("draft_pick_overall" %in% names(players)) as.integer(players$draft_pick_overall)
    else if ("draft_pick" %in% names(players)) as.integer(players$draft_pick) else NA_integer_,
    stringsAsFactors = FALSE
  )
   
   player_dim <- merge(roster_dim, player_dir, by = "gsis_id", all.x = TRUE)
   
  # Active flag (presence on roster for season)
  player_dim$active <- TRUE
  
  # Schema normalization and validation
  required_cols <- c("player_id", "season", "team", "position")
  optional_cols <- c(
    "gsis_id",
    "full_name",
    "first_name",
    "last_name",
    "display_name",
    "headshot_url",
    "has_headshot",
    "height",
    "weight",
    "age",
    "active",
    "draft_round",
    "draft_pick_overall"
  )
  
  player_dim <- player_dim |>
    canonicalize_names() |>
    rename_aliases()
  
  player_dim <- coalesce_cols(player_dim, "gsis_id", c("gsis_id"))
  player_dim <- coalesce_cols(player_dim, "player_id", c("player_id", "gsis_id"))
  player_dim <- coalesce_cols(player_dim, "season", c("season", "year"))
  player_dim <- coalesce_cols(player_dim, "team", c("team", "team_abbr", "recent_team", "current_team"))
  player_dim <- coalesce_cols(player_dim, "position", c("position", "pos"))

  identity_path <- file.path("data", "cache", "player_week_identity.parquet")
  if (file.exists(identity_path)) {
    identity <- tryCatch(arrow::read_parquet(identity_path), error = function(e) NULL)
    if (!is.null(identity) && nrow(identity) > 0) {
      identity$player_id <- as.character(identity$player_id)
      identity$season <- as.integer(identity$season)
      identity$week <- as.integer(identity$week)
      identity$position <- toupper(trimws(as.character(identity$position)))
      identity$team <- toupper(trimws(as.character(identity$team)))
      identity <- identity[
        !is.na(identity$player_id) & identity$player_id != "" &
          !is.na(identity$season) & !is.na(identity$week) &
          !is.na(identity$team) & identity$team != "" &
          !is.na(identity$position) & identity$position != "",
        , drop = FALSE
      ]
      if (nrow(identity) > 0) {
        identity <- identity[order(identity$player_id, identity$season, identity$week, identity$game_date), ]
        identity$key <- paste(identity$player_id, identity$season)
        latest_identity <- identity[!duplicated(identity$key, fromLast = TRUE), , drop = FALSE]
        latest_identity <- latest_identity[, c("player_id", "season", "team", "position"), drop = FALSE]
        player_dim <- merge(
          player_dim,
          latest_identity,
          by.x = c("player_id", "season"),
          by.y = c("player_id", "season"),
          all.x = TRUE,
          suffixes = c("", "_identity")
        )
        player_dim$team <- ifelse(
          !is.na(player_dim$team_identity) & player_dim$team_identity != "",
          player_dim$team_identity,
          player_dim$team
        )
        player_dim$position <- ifelse(
          !is.na(player_dim$position_identity) & player_dim$position_identity != "",
          player_dim$position_identity,
          player_dim$position
        )
        player_dim$team_identity <- NULL
        player_dim$position_identity <- NULL
      }
    }
  }
  
  if (!("first_name" %in% names(player_dim))) player_dim$first_name <- NA_character_
  if (!("last_name" %in% names(player_dim))) player_dim$last_name <- NA_character_
  if (!("full_name" %in% names(player_dim))) player_dim$full_name <- NA_character_
  
  player_dim$full_name <- dplyr::coalesce(
    player_dim$full_name,
    dplyr::if_else(
      !is.na(player_dim$first_name) & !is.na(player_dim$last_name),
      paste(player_dim$first_name, player_dim$last_name),
      NA_character_
    )
  )
  
  if (!("headshot_url" %in% names(player_dim))) {
    player_dim$headshot_url <- NA_character_
  }
  player_dim$has_headshot <- !is.na(player_dim$headshot_url)
  
  if (!("active" %in% names(player_dim))) {
    player_dim$active <- TRUE
  }
  
  assert_required_cols(player_dim, required_cols, context = "player_dim build output")

  player_dim$position <- toupper(trimws(as.character(player_dim$position)))
  player_dim$team <- canonicalize_team_abbr(player_dim$team)

  allowed_positions <- c("QB", "RB", "WR", "TE", "K")
  invalid_pos <- setdiff(unique(player_dim$position), allowed_positions)
  if (length(invalid_pos) > 0) {
    stop("player_dim contains invalid positions: ", paste(invalid_pos, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(player_dim$position) | player_dim$position == "")) {
    stop("player_dim contains missing positions.", call. = FALSE)
  }

  team_path <- file.path("data", "teams", "teams.csv")
  allowed_teams <- character(0)
  if (file.exists(team_path)) {
    team_df <- read.csv(team_path, stringsAsFactors = FALSE)
    if ("abbr" %in% names(team_df)) {
      allowed_teams <- toupper(as.character(team_df$abbr))
    } else if ("team" %in% names(team_df)) {
      allowed_teams <- toupper(as.character(team_df$team))
    }
  }
  allowed_teams <- sort(unique(allowed_teams))
  if (length(allowed_teams) > 0) {
    invalid_teams <- setdiff(unique(player_dim$team), allowed_teams)
    if (length(invalid_teams) > 0) {
      stop("player_dim contains invalid team codes: ", paste(invalid_teams, collapse = ", "), call. = FALSE)
    }
  }
  if (any(is.na(player_dim$team) | player_dim$team == "")) {
    stop("player_dim contains missing team values.", call. = FALSE)
  }
  
  player_dim <- dplyr::as_tibble(player_dim) |>
    dplyr::select(dplyr::any_of(c(required_cols, optional_cols)))
  
  # Validation: one row per (player_id, season)
  dupes <- player_dim |>
    dplyr::count(player_id, season) |>
    dplyr::filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(
      paste(
        "Duplicate (player_id, season) rows in player_dim.",
        paste(capture.output(print(dupes)), collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  # Headshot availability (presentation only)
  missing_headshot <- player_dim |>
    dplyr::filter(active, is.na(headshot_url))
  if (nrow(missing_headshot) > 0) {
    warning(
      sprintf(
        "Active players missing headshot_url: %s",
        paste(missing_headshot$player_id, collapse = ", ")
      ),
      call. = FALSE
    )
  }
   
   if (write_cache) {
     dir.create(dirname(player_dim_path), recursive = TRUE, showWarnings = FALSE)
     arrow::write_parquet(player_dim, player_dim_path)
   }
   
   player_dim
 }
 
 read_player_dim_cache <- function() {
   if (!requireNamespace("arrow", quietly = TRUE)) {
     stop("Package 'arrow' is required to read player_dim cache.")
   }
   if (!file.exists(player_dim_path)) {
     stop("player_dim cache not found at ", player_dim_path, 
          ". Run scripts/refresh_weekly_cache.R to generate it.")
   }
   arrow::read_parquet(player_dim_path)
 }
