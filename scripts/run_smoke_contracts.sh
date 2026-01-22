#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1"; exit 1; }

READTHEFIELD_REPO_ROOT="${ROOT_DIR}" Rscript - <<'RS' >/dev/null && pass "cache schema"
options(READTHEFIELD_REPO_ROOT = Sys.getenv("READTHEFIELD_REPO_ROOT"))
suppressMessages({library(arrow); library(jsonlite)})
required <- c("data/cache/player_directory.parquet",
             "data/cache/player_week_identity.parquet",
             "data/processed/player_dim.parquet",
             "data/processed/defense_weekly_features.parquet",
             "data/processed/rb_weekly_features.parquet",
             "data/processed/wr_weekly_features.parquet",
             "data/processed/te_weekly_features.parquet",
             "data/processed/qb_weekly_features.parquet",
             "data/processed/qb_player_weekly_features.parquet",
             "data/processed/k_weekly_features.parquet")
missing <- required[!file.exists(required)]
if (length(missing) > 0) stop("Missing caches: ", paste(missing, collapse = ", "))
pd <- read_parquet("data/cache/player_directory.parquet")
required_cols <- c("player_id", "player_name", "position", "team")
if (!all(required_cols %in% names(pd))) stop("player_directory missing columns")
if (any(is.na(pd$team) | pd$team == "")) stop("player_directory missing team")
if (any(is.na(pd$position) | pd$position == "")) stop("player_directory missing position")
cat("CACHE_OK\n")
RS

READTHEFIELD_REPO_ROOT="${ROOT_DIR}" Rscript - <<'RS' >/dev/null && pass "api contracts"
options(READTHEFIELD_REPO_ROOT = Sys.getenv("READTHEFIELD_REPO_ROOT"))
source("api/plumber.R")
resp <- players_endpoint()
if (!isTRUE(resp$ok)) stop("players endpoint not ok")
if (is.null(resp$data$players) || nrow(resp$data$players) == 0) stop("players empty")
team_resp <- teams_endpoint()
if (!isTRUE(team_resp$ok)) stop("teams endpoint not ok")
if (nrow(team_resp$data$teams) != 32) stop("teams length != 32")
players <- resp$data$players
players <- players[players$eligible == TRUE, , drop = FALSE]
if (nrow(players) == 0) stop("no eligible players")
pid <- players$player_id[1]
res <- list()
games <- player_games_endpoint(pid, res)
if (!isTRUE(games$ok)) stop("player games not ok")
season <- games$data$games$season[1]
week <- games$data$games$week[1]
req <- list(postBody = jsonlite::toJSON(list(player_id = pid, season = season, week = week, n_sims = 200, availability_policy = "played_only", schema_version = "v1", mode = "historical_replay"), auto_unbox = TRUE))
sim <- simulate_endpoint(req, res)
if (!isTRUE(sim$ok)) stop("simulate not ok")
if (is.null(sim$data$summary) || nrow(sim$data$summary) == 0) stop("simulate summary empty")
cat("API_OK\n")
RS

pass "simulate success"
