# Build RB Training Data
#
# Constructs training-ready RB dataset from cached features.
# Enforces minimum 3-game history rule and separates features from outcomes.
#
# Dependencies:
#   - R/data/build_weekly_player_layers.R (for read_rb_weekly_features_cache)
#
# Usage:
#   training_data <- build_rb_training_data(seasons_train = 2021:2024)

#' Build RB training data from cached features
#'
#' Reads rb_weekly_features.parquet and constructs training-ready dataset
#' with explicit feature matrix X and outcome variables Y.
#' Enforces minimum 3-game history rule (drops ineligible rows).
#'
#' @param seasons_train Integer vector of seasons to include in training data
#' @param seasons_eval Optional integer vector of seasons to exclude from training (for evaluation)
#' @return List with:
#'   - data: data.frame with all columns (features + targets)
#'   - X: data.frame with feature columns only
#'   - Y: data.frame with outcome columns only
#'   - metadata: list with n_rows, n_eligible, n_dropped, seasons_used
build_rb_training_data <- function(seasons_train, seasons_eval = NULL) {
  
  if (missing(seasons_train) || is.null(seasons_train) || length(seasons_train) == 0) {
    stop("seasons_train is required")
  }
  
  seasons_train <- sort(unique(as.integer(seasons_train)))
  seasons_train <- seasons_train[!is.na(seasons_train)]
  
  if (length(seasons_train) == 0) {
    stop("No valid seasons provided in seasons_train")
  }
  
  if (!exists("read_rb_weekly_features_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("R/data/build_weekly_player_layers.R is required")
    }
  }
  
  # Load cached features
  features_all <- read_rb_weekly_features_cache()
  
  if (nrow(features_all) == 0) {
    stop("RB weekly features cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }
  
  # Filter to training seasons
  features <- features_all[features_all$season %in% seasons_train, , drop = FALSE]
  
  if (nrow(features) == 0) {
    stop("No features found for training seasons: ", paste(seasons_train, collapse = ", "))
  }
  
  # Exclude evaluation seasons if provided
  if (!is.null(seasons_eval) && length(seasons_eval) > 0) {
    seasons_eval <- sort(unique(as.integer(seasons_eval)))
    features <- features[!features$season %in% seasons_eval, , drop = FALSE]
  }
  
  n_before_eligibility <- nrow(features)
  
  # Enforce minimum 3-game history rule
  # A row is eligible if at least one rolling feature (3-game window) is non-NA
  # This ensures the player has at least 3 prior games
  rolling_cols_3 <- grep("_roll3$", names(features), value = TRUE)
  
  if (length(rolling_cols_3) == 0) {
    stop("No 3-game rolling features found. Cannot enforce minimum history rule.")
  }
  
  # Check if any 3-game rolling feature is non-NA
  has_history <- rep(FALSE, nrow(features))
  for (col in rolling_cols_3) {
    has_history <- has_history | !is.na(features[[col]])
  }
  
  features_eligible <- features[has_history, , drop = FALSE]
  n_dropped <- n_before_eligibility - nrow(features_eligible)
  
  if (nrow(features_eligible) == 0) {
    stop("No eligible rows after enforcing minimum 3-game history rule. ",
         "All players must have at least 3 prior games.")
  }
  
  # Separate features (X) and outcomes (Y)
  # Feature columns: all rolling features + metadata (excluding targets)
  feature_cols <- c(
    # Identity/metadata
    "player_id", "player_name", "season", "week", "team", "opponent",
    "game_key", "game_date", "gameday", "home_away", "is_home",
    # Rolling features
    grep("_roll[0-9]+$", names(features_eligible), value = TRUE)
  )
  feature_cols <- intersect(feature_cols, names(features_eligible))
  
  # Outcome columns: RB v1 contract outcomes
  # Required: target_carries, target_rushing_yards, target_receiving_yards, target_receptions, target_total_touchdowns
  required_outcomes <- c("target_carries", "target_rush_yards", "target_rush_tds", 
                         "target_receptions", "target_rec_yards", "target_rec_tds")
  
  missing_outcomes <- setdiff(required_outcomes, names(features_eligible))
  if (length(missing_outcomes) > 0) {
    stop("Missing required outcome columns: ", paste(missing_outcomes, collapse = ", "),
         ". Cannot construct RB v1 training data.")
  }
  
  # Compute target_total_touchdowns from split TDs
  features_eligible$target_total_touchdowns <- 
    features_eligible$target_rush_tds + features_eligible$target_rec_tds
  
  # Map nflreadr column names to v1 contract names
  # Note: features may have target_rush_yards or target_rushing_yards
  if ("target_rush_yards" %in% names(features_eligible) && !"target_rushing_yards" %in% names(features_eligible)) {
    features_eligible$target_rushing_yards <- features_eligible$target_rush_yards
  }
  if ("target_rec_yards" %in% names(features_eligible) && !"target_receiving_yards" %in% names(features_eligible)) {
    features_eligible$target_receiving_yards <- features_eligible$target_rec_yards
  }
  
  # RB v1 outcome columns (explicitly defined)
  outcome_cols <- c("target_carries", "target_rushing_yards", "target_receiving_yards", 
                    "target_receptions", "target_total_touchdowns")
  
  # Verify all outcome columns exist
  missing_v1_outcomes <- setdiff(outcome_cols, names(features_eligible))
  if (length(missing_v1_outcomes) > 0) {
    stop("Missing RB v1 outcome columns: ", paste(missing_v1_outcomes, collapse = ", "),
         ". Cannot construct training data.")
  }
  
  X <- features_eligible[, feature_cols, drop = FALSE]
  Y <- features_eligible[, outcome_cols, drop = FALSE]
  
  # Metadata
  metadata <- list(
    n_rows_total = n_before_eligibility,
    n_rows_eligible = nrow(features_eligible),
    n_rows_dropped = n_dropped,
    seasons_used = sort(unique(features_eligible$season)),
    feature_cols = feature_cols,
    outcome_cols = outcome_cols,
    min_history_games = 3
  )
  
  list(
    data = features_eligible,
    X = X,
    Y = Y,
    metadata = metadata
  )
}

