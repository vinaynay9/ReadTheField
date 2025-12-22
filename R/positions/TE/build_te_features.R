# Build TE Features
#
# Computes rolling features for TE player-game data.
# All features use strictly lagged windows (no current-game leakage).

suppressPackageStartupMessages({
  library(dplyr)
})

#' Build rolling features for TE data (season-bounded)
#'
#' Rolling windows are strictly lagged: the current game is never included.
#' Week 1 of any season will have NA for all rolling features.
#'
#' @param te_data data.frame with columns:
#'   - player_id, season, week, team
#'   - targets, receptions, receiving_yards, air_yards
#' @return data.frame with original columns plus rolling features
build_te_features <- function(te_data) {
  if (is.null(te_data) || nrow(te_data) == 0) {
    warning("Empty te_data provided, returning empty result")
    return(empty_te_features_df())
  }

  required_cols <- c("player_id", "season", "week", "team",
                     "targets", "receptions", "receiving_yards", "air_yards")
  missing_cols <- setdiff(required_cols, names(te_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if (!"rec_yards" %in% names(te_data)) {
    te_data$rec_yards <- te_data$receiving_yards
  }

  te_data <- te_data[order(te_data$player_id, te_data$season, te_data$week), ]

  # Team totals for share features (no leakage; share is lagged later)
  team_totals <- te_data %>%
    group_by(team, season, week) %>%
    summarise(
      team_targets = sum(targets, na.rm = TRUE),
      team_air_yards = if (all(is.na(air_yards))) NA_real_ else sum(air_yards, na.rm = TRUE),
      .groups = "drop"
    )
  te_data <- te_data %>%
    left_join(team_totals, by = c("team", "season", "week"))

  te_data$target_share <- ifelse(
    !is.na(te_data$team_targets) & te_data$team_targets > 0,
    te_data$targets / te_data$team_targets,
    NA_real_
  )
  te_data$air_yards_share <- ifelse(
    !is.na(te_data$team_air_yards) & te_data$team_air_yards > 0,
    te_data$air_yards / te_data$team_air_yards,
    NA_real_
  )

  n <- nrow(te_data)
  te_data$targets_roll1 <- NA_real_
  te_data$targets_roll3 <- NA_real_
  te_data$targets_roll5 <- NA_real_
  te_data$receptions_roll1 <- NA_real_
  te_data$receptions_roll3 <- NA_real_
  te_data$receptions_roll5 <- NA_real_
  te_data$rec_yards_roll1 <- NA_real_
  te_data$rec_yards_roll3 <- NA_real_
  te_data$rec_yards_roll5 <- NA_real_
  te_data$air_yards_roll1 <- NA_real_
  te_data$air_yards_roll3 <- NA_real_
  te_data$air_yards_roll5 <- NA_real_
  te_data$target_share_roll1 <- NA_real_
  te_data$air_yards_share_roll1 <- NA_real_

  player_seasons <- unique(te_data[, c("player_id", "season"), drop = FALSE])
  player_seasons <- player_seasons[order(player_seasons$player_id, player_seasons$season), ]

  for (i in seq_len(nrow(player_seasons))) {
    pid <- player_seasons$player_id[i]
    season_val <- player_seasons$season[i]

    idx <- which(te_data$player_id == pid & te_data$season == season_val)
    if (length(idx) < 2) next

    weeks_in_season <- te_data$week[idx]
    if (any(diff(weeks_in_season) <= 0)) {
      stop("Weeks not strictly increasing for player ", pid, " season ", season_val)
    }

    targets <- te_data$targets[idx]
    receptions <- te_data$receptions[idx]
    rec_yards <- te_data$rec_yards[idx]
    air_yards <- te_data$air_yards[idx]
    target_share <- te_data$target_share[idx]
    air_yards_share <- te_data$air_yards_share[idx]

    te_data$targets_roll1[idx] <- c(NA_real_, head(targets, -1))
    te_data$targets_roll3[idx] <- lagged_roll_mean(targets, window = 3)
    te_data$targets_roll5[idx] <- lagged_roll_mean(targets, window = 5)

    te_data$receptions_roll1[idx] <- c(NA_real_, head(receptions, -1))
    te_data$receptions_roll3[idx] <- lagged_roll_mean(receptions, window = 3)
    te_data$receptions_roll5[idx] <- lagged_roll_mean(receptions, window = 5)

    te_data$rec_yards_roll1[idx] <- c(NA_real_, head(rec_yards, -1))
    te_data$rec_yards_roll3[idx] <- lagged_roll_mean(rec_yards, window = 3)
    te_data$rec_yards_roll5[idx] <- lagged_roll_mean(rec_yards, window = 5)

    te_data$air_yards_roll1[idx] <- c(NA_real_, head(air_yards, -1))
    te_data$air_yards_roll3[idx] <- lagged_roll_mean(air_yards, window = 3)
    te_data$air_yards_roll5[idx] <- lagged_roll_mean(air_yards, window = 5)

    te_data$target_share_roll1[idx] <- c(NA_real_, head(target_share, -1))
    te_data$air_yards_share_roll1[idx] <- c(NA_real_, head(air_yards_share, -1))
  }

  # Enforce Week 1 roll1 features are NA (season-boundary safety)
  te_data <- te_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_roll1"),
        ~ dplyr::if_else(week == 1, NA_real_, .)
      )
    )

  # Drop temporary share inputs
  te_data$team_targets <- NULL
  te_data$team_air_yards <- NULL
  te_data$target_share <- NULL
  te_data$air_yards_share <- NULL

  rownames(te_data) <- NULL
  return(te_data)
}

#' Create empty TE features data.frame with correct schema
#'
#' @return data.frame with zero rows and correct feature columns
empty_te_features_df <- function() {
  data.frame(
    player_id = character(0),
    player_name = character(0),
    game_id = character(0),
    season = integer(0),
    week = integer(0),
    team = character(0),
    position = character(0),
    targets = integer(0),
    receptions = integer(0),
    receiving_yards = double(0),
    air_yards = double(0),
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
    stringsAsFactors = FALSE
  )
}
