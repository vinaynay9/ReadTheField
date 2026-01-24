suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
})

# Assemble QB Training Data
#
# Builds QB weekly feature matrix and target columns for modeling.

assemble_qb_weekly_features <- function(qb_weekly_stats) {
  if (is.null(qb_weekly_stats) || nrow(qb_weekly_stats) == 0) {
    stop("qb_weekly_stats must be provided with at least one row")
  }

  if (!exists("build_qb_player_features")) {
    if (file.exists("R/positions/QB/build_qb_features.R")) {
      source("R/positions/QB/build_qb_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/QB/build_qb_features.R")
    }
  }
  if (!exists("get_passing_defense_roll1_features")) {
    if (file.exists("R/positions/passing_defense_features.R")) {
      source("R/positions/passing_defense_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/passing_defense_features.R")
    }
  }

  features <- build_qb_player_features(qb_weekly_stats)

  # Rookie detection based on first recorded NFL season (no imputation)
  first_season <- features %>%
    group_by(player_id) %>%
    summarise(first_season = min(season, na.rm = TRUE), .groups = "drop")
  features <- features %>%
    left_join(first_season, by = "player_id") %>%
    mutate(is_rookie = !is.na(first_season) & season == first_season)
  features$is_rookie <- as.logical(features$is_rookie)

  # Prior-season QB stats (season - 1 totals)
  qb_stats <- qb_weekly_stats
  pick_col <- function(df, candidates) {
    for (candidate in candidates) {
      if (candidate %in% names(df)) {
        return(df[[candidate]])
      }
    }
    rep(NA, nrow(df))
  }
  qb_stats$pass_attempts <- suppressWarnings(as.numeric(pick_col(qb_stats, c("attempts", "pass_attempts"))))
  qb_stats$pass_yards <- suppressWarnings(as.numeric(pick_col(qb_stats, c("passing_yards", "pass_yards"))))
  qb_stats$pass_tds <- suppressWarnings(as.numeric(pick_col(qb_stats, c("passing_tds", "pass_tds"))))
  qb_stats$interceptions_thrown <- suppressWarnings(as.numeric(pick_col(qb_stats, c("passing_interceptions", "interceptions"))))
  qb_stats$sacks_taken <- suppressWarnings(as.numeric(pick_col(qb_stats, c("sacks_suffered", "qb_sacks", "sacks_taken"))))
  qb_stats$qb_rush_attempts <- suppressWarnings(as.numeric(pick_col(qb_stats, c("carries", "rush_attempts", "rushing_attempts"))))
  qb_stats$qb_rush_yards <- suppressWarnings(as.numeric(pick_col(qb_stats, c("rushing_yards", "rush_yards"))))
  qb_stats$qb_rush_tds <- suppressWarnings(as.numeric(pick_col(qb_stats, c("rushing_tds", "rush_tds"))))

  prev_season_stats <- qb_stats %>%
    group_by(player_id, season) %>%
    summarise(
      prev_season_pass_attempts_total = sum(pass_attempts, na.rm = TRUE),
      prev_season_pass_yards_total = sum(pass_yards, na.rm = TRUE),
      prev_season_pass_tds_total = sum(pass_tds, na.rm = TRUE),
      prev_season_interceptions_thrown_total = sum(interceptions_thrown, na.rm = TRUE),
      prev_season_sacks_taken_total = sum(sacks_taken, na.rm = TRUE),
      prev_season_rush_attempts_total = sum(qb_rush_attempts, na.rm = TRUE),
      prev_season_rush_yards_total = sum(qb_rush_yards, na.rm = TRUE),
      prev_season_rush_tds_total = sum(qb_rush_tds, na.rm = TRUE),
      prev_season_games_played = sum(!is.na(week)),
      .groups = "drop"
    ) %>%
    mutate(season = season + 1L)

  features <- features %>%
    left_join(prev_season_stats, by = c("player_id", "season"))

  # Join player static attributes
  repo_root <- if (exists("resolve_repo_root")) resolve_repo_root() else "."
  dim_path <- file.path(repo_root, "data", "processed", "player_dim.parquet")
  if (file.exists(dim_path)) {
    player_dim <- arrow::read_parquet(dim_path)
    dim_cols <- intersect(c("gsis_id", "season", "height", "weight", "age"), names(player_dim))
    dim_join <- player_dim[, dim_cols, drop = FALSE]
    names(dim_join)[names(dim_join) == "gsis_id"] <- "player_id"
    dim_join <- dim_join[!duplicated(dim_join[, c("player_id", "season")]), ]
    features <- features %>%
      left_join(dim_join, by = c("player_id", "season"))
  } else {
    features$height <- NA_real_
    features$weight <- NA_real_
    features$age <- NA_real_
  }

  # Join defensive features (opponent context)
  defense_weekly_features_path <- file.path(repo_root, "data", "processed", "defense_weekly_features.parquet")
  if (!file.exists(defense_weekly_features_path)) {
    stop("Missing defensive weekly features file: ", defense_weekly_features_path)
  }
  def_features <- arrow::read_parquet(defense_weekly_features_path)
  def_cols <- c("season", "week", "defense_team",
                get_passing_defense_roll1_features(),
                get_passing_defense_roll3_features(),
                get_passing_defense_roll5_features())
  def_cols <- unique(def_cols)
  features <- merge(
    features,
    def_features[, def_cols, drop = FALSE],
    by.x = c("season", "week", "opponent"),
    by.y = c("season", "week", "defense_team"),
    all.x = TRUE
  )

  # Team offensive context (lag-1, prior week)
  team_offense_context_path <- file.path(repo_root, "data", "processed", "team_offense_context.parquet")
  if (!file.exists(team_offense_context_path)) {
    stop("Missing team offense context file: ", team_offense_context_path)
  }
  team_offense_context <- arrow::read_parquet(team_offense_context_path)
  toc_cols <- c(
    "team", "season", "week",
    "team_wr_target_share_top1_roll1",
    "team_wr_target_share_top2_roll1",
    "team_te_targets_total_roll1",
    "team_rb_targets_total_roll1"
  )
  toc_cols <- intersect(toc_cols, names(team_offense_context))
  features <- features %>%
    left_join(team_offense_context[, toc_cols, drop = FALSE],
              by = c("team", "season", "week"))

  # Interaction features (QB): explicit combinations of QB usage and team/opponent context.
  add_interaction <- function(df, col_a, col_b, out_col) {
    if (col_a %in% names(df) && col_b %in% names(df)) {
      df[[out_col]] <- suppressWarnings(as.numeric(df[[col_a]])) * suppressWarnings(as.numeric(df[[col_b]]))
    } else {
      df[[out_col]] <- NA_real_
      missing <- setdiff(c(col_a, col_b), names(df))
      message("Interaction ", out_col, " missing base columns: ", paste(missing, collapse = ", "))
    }
    df
  }
  features <- add_interaction(features, "target_qb_rush_attempts_roll5", "team_points_roll5",
                              "target_qb_rush_attempts_roll5_x_team_points_roll5")
  features <- add_interaction(features, "target_qb_rush_attempts_roll5", "def_points_defense_allowed_roll5",
                              "target_qb_rush_attempts_roll5_x_def_points_defense_allowed_roll5")
  features <- add_interaction(features, "target_qb_rush_td_rate_roll5", "team_redzone_td_rate",
                              "target_qb_rush_td_rate_roll5_x_team_redzone_td_rate")
  message("Added QB interaction features: target_qb_rush_attempts_roll5_x_team_points_roll5, ",
          "target_qb_rush_attempts_roll5_x_def_points_defense_allowed_roll5, ",
          "target_qb_rush_td_rate_roll5_x_team_redzone_td_rate")

  # Optional draft metadata (non-imputing; NA when unavailable)
  draft_path <- file.path(repo_root, "data", "external", "player_metadata.parquet")
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
    draft_meta <- draft_meta[, c("player_id", "draft_round", "draft_pick_overall")]
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

  # QB regime label
  features <- features %>%
    mutate(
      qb_regime = case_when(
        week <= 3 ~ "early",
        week <= 5 ~ "mid",
        week <= 7 ~ "late",
        TRUE ~ "standard"
      )
    )

  # Targets (post-game, for training)
  features$target_pass_attempts_qb <- features$target_pass_attempts_qb
  features$target_completions_qb <- features$target_completions_qb
  features$target_pass_yards_qb <- features$target_pass_yards_qb
  features$target_pass_tds_qb <- features$target_pass_tds_qb
  features$target_interceptions_qb_thrown <- features$target_interceptions_qb_thrown
  features$target_sacks_qb_taken <- features$target_sacks_qb_taken
  features$target_qb_rush_attempts <- features$target_qb_rush_attempts
  features$target_qb_rush_yards <- features$target_qb_rush_yards
  features$target_qb_rush_tds <- features$target_qb_rush_tds

  # Drop malformed rows
  bad_rows <- is.na(features$player_id) | is.na(features$season) | is.na(features$week)
  if (any(bad_rows)) {
    features <- features[!bad_rows, , drop = FALSE]
  }

  schema_path <- if (exists("resolve_schema_path")) {
    resolve_schema_path("QB", "v1")
  } else {
    file.path(getOption("READTHEFIELD_REPO_ROOT", "."), "R", "positions", "QB", "qb_schema_v1.R")
  }
  if (file.exists(schema_path)) {
    source(schema_path, local = TRUE)
    validate_qb_v1_target_schema(features, strict = TRUE)
  } else {
    stop("Missing QB schema at ", schema_path)
  }

  features
}

assemble_qb_training_data <- function(seasons) {
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified")
    return(empty_qb_training_df())
  }

  if (!exists("read_qb_weekly_stats_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R")
    }
  }
  qb_stats <- read_qb_weekly_stats_cache()
  if (nrow(qb_stats) == 0) {
    stop("QB weekly stats cache is empty.")
  }

  qb_data <- assemble_qb_weekly_features(qb_stats)
  qb_data <- qb_data[qb_data$season %in% seasons, , drop = FALSE]

  if (nrow(qb_data) == 0) {
    stop("CRITICAL: Final QB training dataset has 0 rows.")
  }

  if (!"qb_regime" %in% names(qb_data)) {
    stop("qb_regime column missing after training data assembly.")
  }

  qb_data
}

empty_qb_training_df <- function() {
  data.frame(
    game_id = character(0),
    game_key = character(0),
    player_id = character(0),
    season = integer(0),
    week = integer(0),
    gameday = as.Date(character()),
    player_name = character(0),
    team = character(0),
    position = character(0),
    opponent = character(0),
    home_away = character(0),
    is_home = integer(0),
    height = double(0),
    weight = double(0),
    age = double(0),
    is_rookie = logical(0),
    draft_round = integer(0),
    draft_pick_overall = integer(0),
    prev_season_pass_attempts_total = double(0),
    prev_season_pass_yards_total = double(0),
    prev_season_pass_tds_total = double(0),
    prev_season_interceptions_thrown_total = double(0),
    prev_season_sacks_taken_total = double(0),
    prev_season_rush_attempts_total = double(0),
    prev_season_rush_yards_total = double(0),
    prev_season_rush_tds_total = double(0),
    prev_season_games_played = double(0),
    team_wr_target_share_top1_roll1 = double(0),
    team_wr_target_share_top2_roll1 = double(0),
    team_te_targets_total_roll1 = double(0),
    team_rb_targets_total_roll1 = double(0),
    target_pass_attempts_qb_roll1 = double(0),
    target_pass_attempts_qb_roll3 = double(0),
    target_pass_attempts_qb_roll5 = double(0),
    target_completions_qb_roll1 = double(0),
    target_completions_qb_roll3 = double(0),
    target_completions_qb_roll5 = double(0),
    target_completion_pct_qb_roll1 = double(0),
    target_completion_pct_qb_roll3 = double(0),
    target_completion_pct_qb_roll5 = double(0),
    target_pass_yards_qb_roll1 = double(0),
    target_pass_yards_qb_roll3 = double(0),
    target_pass_yards_qb_roll5 = double(0),
    target_pass_tds_qb_roll1 = double(0),
    target_pass_tds_qb_roll3 = double(0),
    target_pass_tds_qb_roll5 = double(0),
    target_interceptions_qb_thrown_roll1 = double(0),
    target_interceptions_qb_thrown_roll3 = double(0),
    target_interceptions_qb_thrown_roll5 = double(0),
    target_sacks_qb_taken_roll1 = double(0),
    target_sacks_qb_taken_roll3 = double(0),
    target_sacks_qb_taken_roll5 = double(0),
    target_air_yards_qb_roll1 = double(0),
    target_air_yards_qb_roll3 = double(0),
    target_air_yards_qb_roll5 = double(0),
    target_qb_rush_attempts_roll1 = double(0),
    target_qb_rush_attempts_roll3 = double(0),
    target_qb_rush_attempts_roll5 = double(0),
    target_qb_rush_yards_roll1 = double(0),
    target_qb_rush_yards_roll3 = double(0),
    target_qb_rush_yards_roll5 = double(0),
    target_qb_rush_tds_roll1 = double(0),
    target_qb_rush_tds_roll3 = double(0),
    target_qb_rush_tds_roll5 = double(0),
    target_qb_rush_td_rate_roll3 = double(0),
    target_qb_rush_td_rate_roll5 = double(0),
    target_pass_attempts_qb = double(0),
    target_completions_qb = double(0),
    target_pass_yards_qb = double(0),
    target_pass_tds_qb = double(0),
    target_interceptions_qb_thrown = double(0),
    target_sacks_qb_taken = double(0),
    target_qb_rush_attempts = double(0),
    target_qb_rush_yards = double(0),
    target_qb_rush_tds = double(0),
    target_qb_rush_attempts_roll5_x_team_points_roll5 = double(0),
    target_qb_rush_attempts_roll5_x_def_points_defense_allowed_roll5 = double(0),
    target_qb_rush_td_rate_roll5_x_team_redzone_td_rate = double(0),
    qb_regime = character(0)
  )
}
