# Simulation request validation (preflight guardrails)
# Ensures eligibility, feature availability, and training data sufficiency.

get_min_games_required <- function() {
  val <- getOption("READTHEFIELD_MIN_GAMES_REQUIRED", NA_integer_)
  if (is.null(val) || is.na(val) || val < 1) {
    val <- as.integer(Sys.getenv("READTHEFIELD_MIN_GAMES_REQUIRED", "4"))
  }
  as.integer(val)
}

get_min_train_rows <- function() {
  val <- getOption("READTHEFIELD_MIN_TRAIN_ROWS", NA_integer_)
  if (is.null(val) || is.na(val) || val < 1) {
    val <- as.integer(Sys.getenv("READTHEFIELD_MIN_TRAIN_ROWS", "200"))
  }
  as.integer(val)
}

get_feature_cache_for_position <- function(position) {
  position <- toupper(as.character(position))
  if (position == "RB" && exists("read_rb_weekly_features_cache")) {
    return(read_rb_weekly_features_cache())
  }
  if (position == "WR" && exists("read_wr_weekly_features_cache")) {
    return(read_wr_weekly_features_cache())
  }
  if (position == "TE" && exists("read_te_weekly_features_cache")) {
    return(read_te_weekly_features_cache())
  }
  if (position == "QB" && exists("read_qb_weekly_features_cache")) {
    return(read_qb_weekly_features_cache())
  }
  if (position == "K" && exists("read_k_weekly_features_cache")) {
    return(read_k_weekly_features_cache())
  }
  NULL
}

get_player_game_counts <- function(identity_df) {
  if (is.null(identity_df) || nrow(identity_df) == 0) {
    return(integer(0))
  }
  identity_df$player_id <- as.character(identity_df$player_id)
  table(identity_df$player_id)
}

has_feature_row <- function(feature_df, player_id, season, week) {
  if (is.null(feature_df) || nrow(feature_df) == 0) return(FALSE)
  if (!all(c("player_id", "season", "week") %in% names(feature_df))) return(FALSE)
  player_id <- as.character(player_id)
  season <- as.integer(season)
  week <- as.integer(week)
  any(feature_df$player_id == player_id &
        as.integer(feature_df$season) == season &
        as.integer(feature_df$week) == week)
}

validate_simulation_request <- function(player_id,
                                        season,
                                        week,
                                        position,
                                        availability_policy = "played_only",
                                        min_games_required = NULL,
                                        min_train_rows = NULL) {
  min_games_required <- if (is.null(min_games_required)) get_min_games_required() else as.integer(min_games_required)
  min_train_rows <- if (is.null(min_train_rows)) get_min_train_rows() else as.integer(min_train_rows)

  if (is.null(player_id) || !nzchar(as.character(player_id))) {
    return(list(ok = FALSE, error_type = "invalid_input", error_code = "player_id_missing",
                message = "player_id is required.", details = list()))
  }
  if (is.null(season) || is.na(season) || is.null(week) || is.na(week)) {
    return(list(ok = FALSE, error_type = "invalid_input", error_code = "season_week_missing",
                message = "season and week are required.", details = list()))
  }

  if (!exists("read_player_dim_cache")) {
    return(list(ok = FALSE, error_type = "internal_error", error_code = "player_dim_missing",
                message = "Player dimension cache loader not available.", details = list()))
  }
  player_dim <- read_player_dim_cache()
  if (is.null(player_dim) || nrow(player_dim) == 0) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "player_dim_empty",
                message = "Player dimension cache is empty.", details = list()))
  }

  player_id <- as.character(player_id)
  season <- as.integer(season)
  week <- as.integer(week)

  dim_row <- player_dim[player_dim$player_id == player_id & player_dim$season == season, , drop = FALSE]
  if (nrow(dim_row) == 0) {
    any_row <- player_dim[player_dim$player_id == player_id, , drop = FALSE]
    if (nrow(any_row) == 0) {
      return(list(ok = FALSE, error_type = "invalid_input", error_code = "player_id_unknown",
                  message = "player_id not found in cache.", details = list(player_id = player_id)))
    }
  }

  position <- toupper(as.character(position))
  if (is.na(position) || !nzchar(position)) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "position_missing",
                message = "Player position missing in cache.", details = list(player_id = player_id)))
  }

  if (!exists("read_player_week_identity_cache")) {
    return(list(ok = FALSE, error_type = "internal_error", error_code = "identity_missing",
                message = "Player identity cache loader not available.", details = list()))
  }

  identity <- read_player_week_identity_cache()
  if (is.null(identity) || nrow(identity) == 0) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "identity_empty",
                message = "Player identity cache is empty.", details = list()))
  }

  counts <- get_player_game_counts(identity)
  player_games <- if (player_id %in% names(counts)) as.integer(counts[[player_id]]) else 0L
  if (player_games < min_games_required) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "insufficient_history",
                message = "Insufficient historical games for player.",
                details = list(player_id = player_id, games = player_games, min_games_required = min_games_required)))
  }

  if (identical(availability_policy, "played_only")) {
    played_rows <- identity[
      identity$player_id == player_id &
        as.integer(identity$season) == season &
        as.integer(identity$week) == week,
      , drop = FALSE
    ]
    if (nrow(played_rows) == 0) {
      return(list(ok = FALSE, error_type = "data_unavailable", error_code = "player_not_active_week",
                  message = "Player did not play in the requested week.",
                  details = list(player_id = player_id, season = season, week = week)))
    }
  }

  feature_df <- get_feature_cache_for_position(position)
  if (is.null(feature_df) || nrow(feature_df) == 0) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "features_empty",
                message = "Feature cache is empty for position.", details = list(position = position)))
  }

  if (nrow(feature_df) < min_train_rows) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "insufficient_training_rows",
                message = "Insufficient training rows for modeling.",
                details = list(position = position, rows = nrow(feature_df), min_train_rows = min_train_rows)))
  }

  if (!has_feature_row(feature_df, player_id, season, week)) {
    return(list(ok = FALSE, error_type = "data_unavailable", error_code = "feature_row_missing",
                message = "No feature row available for requested player-week.",
                details = list(player_id = player_id, season = season, week = week, position = position)))
  }

  list(ok = TRUE,
       details = list(player_id = player_id, season = season, week = week,
                      position = position, games = player_games))
}

evaluate_player_eligibility <- function(player_id, position) {
  min_games_required <- get_min_games_required()
  min_train_rows <- get_min_train_rows()

  if (!exists("read_player_week_identity_cache")) {
    return(list(eligible = FALSE, reason = "identity_cache_missing"))
  }
  identity <- read_player_week_identity_cache()
  if (is.null(identity) || nrow(identity) == 0) {
    return(list(eligible = FALSE, reason = "identity_cache_empty"))
  }

  counts <- get_player_game_counts(identity)
  games <- if (player_id %in% names(counts)) as.integer(counts[[player_id]]) else 0L
  if (games < min_games_required) {
    return(list(eligible = FALSE, reason = "insufficient_history", games = games, min_games_required = min_games_required))
  }

  feature_df <- get_feature_cache_for_position(position)
  if (is.null(feature_df) || nrow(feature_df) == 0) {
    return(list(eligible = FALSE, reason = "features_empty"))
  }
  if (nrow(feature_df) < min_train_rows) {
    return(list(eligible = FALSE, reason = "insufficient_training_rows", rows = nrow(feature_df), min_train_rows = min_train_rows))
  }

  if (!"player_id" %in% names(feature_df)) {
    return(list(eligible = FALSE, reason = "feature_schema_missing_player_id"))
  }
  if (!any(feature_df$player_id == player_id)) {
    return(list(eligible = FALSE, reason = "feature_rows_missing"))
  }

  list(eligible = TRUE, reason = NA_character_)
}
