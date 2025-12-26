suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
})

# Assemble K Training Data
#
# Builds K weekly feature matrix and target columns for modeling.

assemble_k_weekly_features <- function(k_weekly_stats) {
  if (is.null(k_weekly_stats) || nrow(k_weekly_stats) == 0) {
    stop("k_weekly_stats must be provided with at least one row")
  }

  if (!exists("build_k_features")) {
    if (file.exists("R/positions/K/build_k_features.R")) {
      source("R/positions/K/build_k_features.R", local = TRUE)
    } else {
      stop("Missing R/positions/K/build_k_features.R")
    }
  }

  features <- build_k_features(k_weekly_stats)

  # Rookie detection based on first recorded NFL season (no imputation)
  first_season <- features %>%
    group_by(player_id) %>%
    summarise(first_season = min(season, na.rm = TRUE), .groups = "drop")
  features <- features %>%
    left_join(first_season, by = "player_id") %>%
    mutate(is_rookie = !is.na(first_season) & season == first_season)
  features$is_rookie <- as.logical(features$is_rookie)

  # Join player static attributes
  dim_path <- file.path("data", "processed", "player_dim.parquet")
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

  # Join defensive context (sacks/points allowed)
  defense_weekly_features_path <- file.path("data", "processed", "defense_weekly_features.parquet")
  if (!file.exists(defense_weekly_features_path)) {
    stop("Missing defensive weekly features file: ", defense_weekly_features_path)
  }
  def_features <- arrow::read_parquet(defense_weekly_features_path)
  def_cols <- c(
    "season", "week", "defense_team",
    "def_sacks_defense_forced_roll1", "def_sacks_defense_forced_roll3", "def_sacks_defense_forced_roll5",
    "def_points_defense_allowed_roll1", "def_points_defense_allowed_roll3", "def_points_defense_allowed_roll5"
  )
  def_cols <- intersect(def_cols, names(def_features))
  features <- merge(
    features,
    def_features[, def_cols, drop = FALSE],
    by.x = c("season", "week", "opponent"),
    by.y = c("season", "week", "defense_team"),
    all.x = TRUE
  )

  # Optional draft metadata (non-imputing; NA when unavailable)
  draft_path <- file.path("data", "external", "player_metadata.parquet")
  if (file.exists(draft_path)) {
    draft_meta <- arrow::read_parquet(draft_path)
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

  # K regime label
  features <- features %>%
    mutate(
      k_regime = case_when(
        week <= 3 ~ "early",
        week <= 5 ~ "mid",
        week <= 7 ~ "late",
        TRUE ~ "standard"
      )
    )

  # Targets
  features$target_fg_made_k <- features$target_fg_made_k
  features$target_pat_made_k <- features$target_pat_made_k

  bad_rows <- is.na(features$player_id) | is.na(features$season) | is.na(features$week)
  if (any(bad_rows)) {
    features <- features[!bad_rows, , drop = FALSE]
  }

  if (file.exists("R/positions/K/k_schema_v1.R")) {
    source("R/positions/K/k_schema_v1.R", local = TRUE)
    validate_k_v1_target_schema(features, strict = TRUE)
  }

  features
}

assemble_k_training_data <- function(seasons) {
  if (missing(seasons) || length(seasons) == 0) {
    warning("No seasons specified")
    return(empty_k_training_df())
  }

  if (!exists("read_k_weekly_stats_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R")
    }
  }
  k_stats <- read_k_weekly_stats_cache()
  if (nrow(k_stats) == 0) {
    stop("K weekly stats cache is empty.")
  }

  k_data <- assemble_k_weekly_features(k_stats)
  k_data <- k_data[k_data$season %in% seasons, , drop = FALSE]

  if (nrow(k_data) == 0) {
    stop("CRITICAL: Final K training dataset has 0 rows.")
  }

  if (!"k_regime" %in% names(k_data)) {
    stop("k_regime column missing after training data assembly.")
  }

  k_data
}

empty_k_training_df <- function() {
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
    target_fg_attempts_k_roll1 = double(0),
    target_fg_attempts_k_roll3 = double(0),
    target_fg_attempts_k_roll5 = double(0),
    target_fg_made_k_roll1 = double(0),
    target_fg_made_k_roll3 = double(0),
    target_fg_made_k_roll5 = double(0),
    target_fg_pct_k_roll1 = double(0),
    target_fg_pct_k_roll3 = double(0),
    target_fg_pct_k_roll5 = double(0),
    target_pat_attempts_k_roll1 = double(0),
    target_pat_attempts_k_roll3 = double(0),
    target_pat_attempts_k_roll5 = double(0),
    target_pat_made_k_roll1 = double(0),
    target_pat_made_k_roll3 = double(0),
    target_pat_made_k_roll5 = double(0),
    target_pat_pct_k_roll1 = double(0),
    target_pat_pct_k_roll3 = double(0),
    target_pat_pct_k_roll5 = double(0),
    target_fg_long_k_roll1 = double(0),
    target_fg_long_k_roll3 = double(0),
    target_fg_long_k_roll5 = double(0),
    def_sacks_defense_forced_roll1 = double(0),
    def_sacks_defense_forced_roll3 = double(0),
    def_sacks_defense_forced_roll5 = double(0),
    def_points_defense_allowed_roll1 = double(0),
    def_points_defense_allowed_roll3 = double(0),
    def_points_defense_allowed_roll5 = double(0),
    target_fg_attempts_k = double(0),
    target_fg_made_k = double(0),
    target_fg_pct_k = double(0),
    target_pat_attempts_k = double(0),
    target_pat_made_k = double(0),
    target_pat_pct_k = double(0),
    target_fg_long_k = double(0),
    k_regime = character(0)
  )
}
