#!/usr/bin/env Rscript

# Debug QB feature row for a single player-week.
# Prints the exact row used for simulation and recomputes strict roll windows
# from qb_weekly_stats for verification.

suppressPackageStartupMessages({
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
arg_map <- list()
for (arg in args) {
  if (grepl("^--", arg)) {
    parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- parts[1]
    val <- if (length(parts) > 1) parts[2] else ""
    arg_map[[key]] <- val
  }
}

player_name <- if (!is.null(arg_map$player)) arg_map$player else NA_character_
player_id <- if (!is.null(arg_map$player_id)) arg_map$player_id else NA_character_
season <- if (!is.null(arg_map$season)) as.integer(arg_map$season) else NA_integer_
week <- if (!is.null(arg_map$week)) as.integer(arg_map$week) else NA_integer_

if (is.na(season) || is.na(week)) {
  stop("Provide --season and --week (e.g., --season=2024 --week=9)")
}

if (file.exists("R/simulation/bootstrap_simulation.R")) {
  source("R/simulation/bootstrap_simulation.R", local = TRUE)
} else {
  stop("Missing R/simulation/bootstrap_simulation.R")
}

if (!exists("read_qb_weekly_stats_cache") || !exists("read_qb_weekly_features_cache")) {
  stop("QB cache readers not loaded.")
}

qb_stats <- read_qb_weekly_stats_cache()
qb_features <- read_qb_weekly_features_cache()

resolve_player_id <- function(name) {
  if (!file.exists("data/cache/player_directory.parquet")) {
    stop("Missing player_directory.parquet for name resolution.")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' required for player directory.")
  }
  dir <- arrow::read_parquet("data/cache/player_directory.parquet")
  cand <- dir %>% filter(tolower(full_name) == tolower(name))
  if (nrow(cand) == 0) {
    stop("No QB match found for name: ", name)
  }
  if (nrow(cand) > 1) {
    stop("Ambiguous QB name. Provide --player_id.")
  }
  as.character(cand$player_id[1])
}

if (is.na(player_id) || player_id == "") {
  if (is.na(player_name) || player_name == "") {
    stop("Provide either --player or --player_id.")
  }
  player_id <- resolve_player_id(player_name)
}

player_id <- as.character(player_id)

feature_row <- qb_features %>%
  filter(player_id == !!player_id, season == !!season, week == !!week)

if (nrow(feature_row) == 0) {
  stop("No QB feature row found for player_id=", player_id,
       " season=", season, " week=", week)
}

feature_row <- feature_row[1, , drop = FALSE]

cat("\n=== QB Feature Row (simulation input) ===\n")
cols_to_print <- c(
  "player_id", "player_name", "season", "week",
  "team", "opponent", "game_id", "gameday",
  "target_sacks_qb_taken_roll1", "target_sacks_qb_taken_roll3", "target_sacks_qb_taken_roll5",
  "target_qb_rush_attempts_roll1", "target_qb_rush_attempts_roll3", "target_qb_rush_attempts_roll5",
  "target_qb_rush_yards_roll1", "target_qb_rush_yards_roll3", "target_qb_rush_yards_roll5",
  "target_qb_rush_tds_roll1", "target_qb_rush_tds_roll3", "target_qb_rush_tds_roll5"
)
cols_to_print <- cols_to_print[cols_to_print %in% names(feature_row)]
print(feature_row[, cols_to_print, drop = FALSE])

cat("\n=== Recomputed prior-game rolls (strict, within season) ===\n")
qb_stats_player <- qb_stats %>%
  filter(player_id == !!player_id, season == !!season) %>%
  arrange(week)

if (nrow(qb_stats_player) == 0) {
  stop("No QB stats rows found for player_id=", player_id, " season=", season)
}

pick_col <- function(df, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) {
      return(df[[candidate]])
    }
  }
  rep(NA_real_, nrow(df))
}

qb_stats_player <- qb_stats_player %>%
  mutate(
    sacks_taken = as.numeric(pick_col(., c("sacks_suffered", "sacks_taken", "passing_sacks"))),
    rush_attempts = as.numeric(pick_col(., c("carries", "rush_attempts", "rushing_attempts"))),
    rush_yards = as.numeric(pick_col(., c("rushing_yards", "rush_yards"))),
    rush_tds = as.numeric(pick_col(., c("rushing_tds", "rush_tds")))
  )

target_idx <- which(qb_stats_player$week == week)
if (length(target_idx) == 0) {
  stop("No QB stats row for target week in qb_weekly_stats.")
}
target_idx <- target_idx[1]

prior_games <- qb_stats_player[seq_len(target_idx - 1), ]
prior_games_tail <- tail(prior_games, 5)

print(prior_games_tail[, c("game_id", "week", "gameday", "sacks_taken", "rush_attempts", "rush_yards", "rush_tds")])

roll_mean <- function(x, n) {
  if (length(x) < n || any(is.na(tail(x, n)))) return(NA_real_)
  mean(tail(x, n))
}

recomputed <- data.frame(
  roll1_sacks = if (nrow(prior_games) >= 1) tail(prior_games$sacks_taken, 1) else NA_real_,
  roll3_sacks = roll_mean(prior_games$sacks_taken, 3),
  roll5_sacks = roll_mean(prior_games$sacks_taken, 5),
  roll1_rush_attempts = if (nrow(prior_games) >= 1) tail(prior_games$rush_attempts, 1) else NA_real_,
  roll3_rush_attempts = roll_mean(prior_games$rush_attempts, 3),
  roll5_rush_attempts = roll_mean(prior_games$rush_attempts, 5),
  roll1_rush_yards = if (nrow(prior_games) >= 1) tail(prior_games$rush_yards, 1) else NA_real_,
  roll3_rush_yards = roll_mean(prior_games$rush_yards, 3),
  roll5_rush_yards = roll_mean(prior_games$rush_yards, 5),
  roll1_rush_tds = if (nrow(prior_games) >= 1) tail(prior_games$rush_tds, 1) else NA_real_,
  roll3_rush_tds = roll_mean(prior_games$rush_tds, 3),
  roll5_rush_tds = roll_mean(prior_games$rush_tds, 5)
)
print(recomputed)
