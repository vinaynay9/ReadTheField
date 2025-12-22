suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
})

# Assemble WR Training Data
#
# Builds WR weekly feature matrix and target columns for modeling.

assemble_wr_weekly_features <- function(wr_weekly_stats) {
  if (is.null(wr_weekly_stats) || nrow(wr_weekly_stats) == 0) {
    stop("wr_weekly_stats must be provided with at least one row")
  }

  if (!exists("get_passing_defense_roll1_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/passing_defense_features.R")
    }
  }

  required_cols <- c(
    "player_id", "player_name", "team", "opponent", "season", "week",
    "game_key", "game_date", "targets", "receptions",
    "receiving_yards", "receiving_tds", "air_yards", "home_away"
  )
  missing <- setdiff(required_cols, names(wr_weekly_stats))
  if (length(missing) > 0) {
    stop("Missing required columns in wr_weekly_stats: ", paste(missing, collapse = ", "))
  }

  stats <- wr_weekly_stats
  stats$season <- as.integer(stats$season)
  stats$week <- as.integer(stats$week)
  stats$game_date <- as.Date(stats$game_date)
  stats$gameday <- stats$game_date
  stats$home_away <- toupper(trimws(as.character(stats$home_away)))
  if ("is_home" %in% names(stats)) {
    stats$is_home <- as.integer(stats$is_home)
  } else {
    stats$is_home <- ifelse(
      stats$home_away == "HOME", 1L,
      ifelse(stats$home_away == "AWAY", 0L, NA_integer_)
    )
  }
  stats <- stats[order(stats$player_id, stats$season, stats$week, stats$game_date), ]

  if (!exists("build_wr_features")) {
    if (file.exists("R/positions/WR/build_wr_features.R")) {
      source("R/positions/WR/build_wr_features.R", local = TRUE)
    } else {
      stop("R/positions/WR/build_wr_features.R is required to build rolling features")
    }
  }

  features <- build_wr_features(stats)

  # Rookie detection based on first recorded NFL season (no imputation)
  first_season <- stats %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(first_season = min(season, na.rm = TRUE), .groups = "drop")
  features <- features %>%
    dplyr::left_join(first_season, by = "player_id") %>%
    dplyr::mutate(is_rookie = !is.na(first_season) & season == first_season)
  features$is_rookie <- as.logical(features$is_rookie)

  # Join defensive features (opponent context)
  defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  if (!file.exists(defense_weekly_features_path)) {
    stop("Missing defensive weekly features file: ", defense_weekly_features_path)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read defensive features.")
  }

  def_features <- arrow::read_parquet(defense_weekly_features_path)
  if (nrow(def_features) == 0) {
    stop("Defensive weekly features are empty. Cannot proceed with WR features.")
  }

  required_def_cols <- get_passing_defense_roll1_features()
  join_keys <- c("season", "week", "defense_team")
  missing_def_cols <- setdiff(c(join_keys, required_def_cols), names(def_features))
  if (length(missing_def_cols) > 0) {
    stop("Defensive weekly features missing required columns: ",
         paste(missing_def_cols, collapse = ", "))
  }

  def_cols <- c(
    join_keys,
    required_def_cols,
    get_passing_defense_roll3_features(),
    get_passing_defense_roll5_features(),
    "defense_data_available",
    "rolling_window_complete"
  )
  def_cols <- intersect(def_cols, names(def_features))

  features <- merge(
    features,
    def_features[, def_cols, drop = FALSE],
    by.x = c("season", "week", "opponent"),
    by.y = c("season", "week", "defense_team"),
    all.x = TRUE
  )

  # Join team offense context (lagged roll1)
  team_offense_context_path <- file.path("data", "processed", "team_offense_context.parquet")
  if (!file.exists(team_offense_context_path)) {
    stop("Missing team offense context file: ", team_offense_context_path)
  }
  team_offense_context <- arrow::read_parquet(team_offense_context_path)
  if (nrow(team_offense_context) == 0) {
    stop("Team offense context is empty. Cannot proceed with WR features.")
  }
  toc_cols <- c(
    "team", "season", "week",
    "team_qb_pass_attempts_roll1", "team_qb_pass_yards_roll1", "team_qb_pass_tds_roll1",
    "team_wr_targets_total_roll1", "team_wr_air_yards_roll1",
    "team_wr_target_share_top1_roll1", "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1", "team_rb_targets_total_roll1"
  )
  toc_cols <- intersect(toc_cols, names(team_offense_context))
  features <- features %>%
    left_join(team_offense_context[, toc_cols, drop = FALSE],
              by = c("team", "season", "week"))

  # Join prior-season cumulative stats (season - 1 aggregates)
  prior_season_stats_path <- file.path("data", "processed", "prior_season_player_stats.parquet")
  if (!file.exists(prior_season_stats_path)) {
    stop("Missing prior-season player stats file: ", prior_season_stats_path)
  }
  prior_season_stats <- arrow::read_parquet(prior_season_stats_path)
  prior_cols <- c(
    "player_id", "season",
    "prev_season_targets_total", "prev_season_receptions_total",
    "prev_season_rec_yards_total", "prev_season_rec_tds_total",
    "prev_season_games_played"
  )
  prior_cols <- intersect(prior_cols, names(prior_season_stats))
  if (length(prior_cols) == 0) {
    stop("Prior-season stats file missing expected columns. Cannot join prior-season aggregates.")
  }
  prior_season_stats <- prior_season_stats[, prior_cols, drop = FALSE]
  features <- features %>%
    left_join(prior_season_stats, by = c("player_id", "season"))

  # Player static attributes (season-level)
  if (exists("read_player_dim_cache")) {
    player_dim <- read_player_dim_cache()
    dim_cols <- c("gsis_id", "season", "height", "weight", "age")
    dim_cols <- intersect(dim_cols, names(player_dim))
    if (length(dim_cols) > 0) {
      player_dim <- player_dim[, dim_cols, drop = FALSE]
      if ("gsis_id" %in% names(player_dim) && !"player_id" %in% names(player_dim)) {
        player_dim$player_id <- player_dim$gsis_id
      }
      features <- features %>%
        left_join(player_dim, by = c("player_id", "season"))
    }
  }

  # Optional draft metadata (non-imputing; NA when unavailable)
  draft_path <- file.path("data", "external", "player_metadata.parquet")
  draft_meta <- NULL
  if (file.exists(draft_path) && file.info(draft_path)$size > 0) {
    draft_meta <- tryCatch(arrow::read_parquet(draft_path), error = function(e) NULL)
  }
  if (is.null(draft_meta) || nrow(draft_meta) == 0) {
    if (exists("read_player_directory_cache")) {
      draft_meta <- tryCatch(read_player_directory_cache(), error = function(e) NULL)
    }
  }
  if (!is.null(draft_meta) && nrow(draft_meta) > 0) {
    draft_cols <- intersect(c("player_id", "gsis_id", "draft_round", "draft_pick_overall"), names(draft_meta))
    if ("gsis_id" %in% draft_cols && !"player_id" %in% draft_cols) {
      draft_meta$player_id <- draft_meta$gsis_id
      draft_cols <- unique(c("player_id", "draft_round", "draft_pick_overall"))
    }
    draft_meta <- draft_meta[, draft_cols, drop = FALSE]
    draft_meta$draft_round <- as.integer(draft_meta$draft_round)
    draft_meta$draft_pick_overall <- as.integer(draft_meta$draft_pick_overall)
    draft_meta <- draft_meta[!duplicated(draft_meta$player_id), ]
    features <- features %>%
      dplyr::select(-dplyr::any_of(c("draft_round", "draft_pick_overall"))) %>%
      left_join(draft_meta, by = "player_id")
  }
  if (!"draft_round" %in% names(features)) {
    features$draft_round <- NA_integer_
  }
  if (!"draft_pick_overall" %in% names(features)) {
    features$draft_pick_overall <- NA_integer_
  }

  # Rookie gating: prior-season stats intentionally excluded
  if ("is_rookie" %in% names(features)) {
    prior_cols <- grep("^prev_season_", names(features), value = TRUE)
    if (length(prior_cols) > 0) {
      features[features$is_rookie, prior_cols] <- NA
    }
  }

  # Add WR v1 regime column based on week
  if (!is.numeric(features$week)) {
    features$week <- as.integer(features$week)
  }
  if (file.exists("R/positions/WR/wr_regime_v1.R")) {
    source("R/positions/WR/wr_regime_v1.R", local = TRUE)
    features$wr_regime <- determine_wr_regime(features$week)
  } else {
    features$wr_regime <- ifelse(features$week <= 3, "early",
                                 ifelse(features$week <= 5, "mid",
                                        ifelse(features$week <= 7, "late", "standard")))
  }
  features$wr_regime <- as.character(features$wr_regime)

  # Targets (post-game, for training)
  features$target_targets <- features$targets
  features$target_receptions <- features$receptions
  features$target_rec_yards <- features$receiving_yards
  features$target_rec_tds <- features$receiving_tds

  final_cols <- c(
    "game_id", "game_key", "player_id", "season", "week", "gameday",
    "player_name", "team", "position", "opponent", "home_away", "is_home",
    "height", "weight", "age",
    "prev_season_targets_total", "prev_season_receptions_total",
    "prev_season_rec_yards_total", "prev_season_rec_tds_total",
    "prev_season_games_played",
    "team_qb_pass_attempts_roll1", "team_qb_pass_yards_roll1", "team_qb_pass_tds_roll1",
    "team_wr_targets_total_roll1", "team_wr_air_yards_roll1",
    "team_wr_target_share_top1_roll1", "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1", "team_rb_targets_total_roll1",
    "targets_roll1", "targets_roll3", "targets_roll5",
    "receptions_roll1", "receptions_roll3", "receptions_roll5",
    "rec_yards_roll1", "rec_yards_roll3", "rec_yards_roll5",
    "air_yards_roll1", "air_yards_roll3", "air_yards_roll5",
    "target_share_roll1", "air_yards_share_roll1",
    get_passing_defense_all_features(),
    "defense_data_available", "rolling_window_complete",
    "is_rookie", "draft_round", "draft_pick_overall",
    "target_targets", "target_receptions", "target_rec_yards", "target_rec_tds",
    "wr_regime"
  )

  available_cols <- intersect(final_cols, names(features))
  features <- features[, available_cols, drop = FALSE]

  if (!"wr_regime" %in% names(features)) {
    stop("wr_regime column missing after training data assembly.")
  }

  features <- features[order(features$season, features$week, features$gameday, features$player_id), ]
  rownames(features) <- NULL

  if (file.exists("R/positions/WR/wr_schema_v1.R")) {
    source("R/positions/WR/wr_schema_v1.R", local = TRUE)
    validate_wr_v1_target_schema(features, strict = TRUE)
  }

  if (nrow(features) == 0) {
    stop("CRITICAL: Final WR training dataset has 0 rows.")
  }

  return(features)
}

empty_wr_training_df <- function() {
  data.frame(
    game_id = character(0),
    player_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character(0)),
    player_name = character(0),
    team = character(0),
    position = character(0),
    opponent = character(0),
    home_away = character(0),
    is_home = integer(0),
    height = double(0),
    weight = double(0),
    age = double(0),
    prev_season_targets_total = double(0),
    prev_season_receptions_total = double(0),
    prev_season_rec_yards_total = double(0),
    prev_season_rec_tds_total = double(0),
    prev_season_games_played = double(0),
    team_qb_pass_attempts_roll1 = double(0),
    team_qb_pass_yards_roll1 = double(0),
    team_qb_pass_tds_roll1 = double(0),
    team_wr_targets_total_roll1 = double(0),
    team_wr_air_yards_roll1 = double(0),
    team_wr_target_share_top1_roll1 = double(0),
    team_wr_target_share_top2_roll1 = double(0),
    team_te_targets_total_roll1 = double(0),
    team_rb_targets_total_roll1 = double(0),
    targets_roll1 = double(0),
    targets_roll3 = double(0),
    targets_roll5 = double(0),
    receptions_roll1 = double(0),
    receptions_roll3 = double(0),
    receptions_roll5 = double(0),
    rec_yards_roll1 = double(0),
    rec_yards_roll3 = double(0),
    rec_yards_roll5 = double(0),
    air_yards_roll1 = double(0),
    air_yards_roll3 = double(0),
    air_yards_roll5 = double(0),
    target_share_roll1 = double(0),
    air_yards_share_roll1 = double(0),
    opp_pass_yards_allowed_roll1 = double(0),
    opp_yards_per_pass_allowed_roll1 = double(0),
    opp_points_allowed_roll1 = double(0),
    opp_sacks_roll1 = double(0),
    opp_tfl_roll1 = double(0),
    opp_interceptions_roll1 = double(0),
    opp_passes_defended_roll1 = double(0),
    def_interceptions_roll3 = double(0),
    def_passes_defended_roll3 = double(0),
    opp_pass_yards_allowed_roll5 = double(0),
    opp_yards_per_pass_allowed_roll5 = double(0),
    opp_points_allowed_roll5 = double(0),
    opp_sacks_roll5 = double(0),
    opp_tfl_roll5 = double(0),
    def_interceptions_roll5 = double(0),
    def_passes_defended_roll5 = double(0),
    defense_data_available = logical(0),
    rolling_window_complete = logical(0),
    is_rookie = logical(0),
    draft_round = integer(0),
    draft_pick_overall = integer(0),
    target_targets = integer(0),
    target_receptions = integer(0),
    target_rec_yards = double(0),
    target_rec_tds = integer(0),
    stringsAsFactors = FALSE
  )
}
