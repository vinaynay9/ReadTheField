# Build Prior-Season Player Stats (lagged season aggregates)
#
# Computes previous-season usage and efficiency signals for each player,
# shifted to the subsequent season for leakage-safe joins. Rookies will have
# NA values. No imputation is performed.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Build prior-season player aggregates
#'
#' @param seasons Integer vector of seasons to include (current-season values; previous seasons inferred)
#' @param season_type Season type filter (default "REG")
#' @param write_cache Logical, write parquet to data/processed/prior_season_player_stats.parquet
#' @return data.frame with one row per (player_id, season) containing previous-season aggregates
build_prior_season_player_stats <- function(seasons,
                                            season_type = "REG",
                                            write_cache = TRUE) {
  if (missing(seasons) || length(seasons) == 0) {
    stop("seasons vector is required to build prior-season player stats")
  }

  if (!exists("load_weekly_player_stats_from_nflreadr") ||
      !exists("select_first_available")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R for loading weekly player stats")
    }
  }

  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)
  if (is.null(stats) || nrow(stats) == 0) {
    stop("No weekly player stats available for seasons: ", paste(seasons, collapse = ", "))
  }

  player_ids <- as.character(select_first_available(stats, c("player_id", "gsis_id"), NA))
  seasons_vec <- as.integer(stats$season)
  weeks <- as.integer(stats$week)
  carries <- as.numeric(select_first_available(stats, c("carries", "rush_attempts", "rushing_attempts"), 0))
  targets <- as.numeric(select_first_available(stats, c("targets"), 0))
  rush_yards <- as.numeric(select_first_available(stats, c("rushing_yards", "rush_yards"), 0))
  rec_yards <- as.numeric(select_first_available(stats, c("receiving_yards", "rec_yards"), 0))

  df <- data.frame(
    player_id = player_ids,
    season = seasons_vec,
    week = weeks,
    carries = carries,
    targets = targets,
    rush_yards = rush_yards,
    rec_yards = rec_yards,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(player_id), player_id != "", !is.na(season), !is.na(week))

  df <- df %>%
    group_by(player_id, season) %>%
    summarise(
      prev_season_carries_total = sum(carries, na.rm = TRUE),
      prev_season_targets_total = sum(targets, na.rm = TRUE),
      prev_season_rush_yards_total = sum(rush_yards, na.rm = TRUE),
      prev_season_rec_yards_total = sum(rec_yards, na.rm = TRUE),
      prev_season_games_played = dplyr::n_distinct(week),
      .groups = "drop"
    ) %>%
    mutate(season = season + 1L)

  if (nrow(df) == 0) {
    warning("Prior-season stats computed zero rows (likely rookie-only seasons). Returning empty frame.")
  }

  if (write_cache) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to write prior-season stats. Install with install.packages('arrow').")
    }
    output_path <- file.path("data", "processed", "prior_season_player_stats.parquet")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(df, output_path)
  }

  df
}
