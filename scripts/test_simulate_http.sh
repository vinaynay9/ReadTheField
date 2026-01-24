#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export READTHEFIELD_REPO_ROOT="${READTHEFIELD_REPO_ROOT:-$ROOT_DIR}"

read -r PID SEASON WEEK <<<"$(Rscript -e 'options(READTHEFIELD_REPO_ROOT=Sys.getenv("READTHEFIELD_REPO_ROOT"));
  suppressMessages({library(arrow); library(dplyr)});
  pwi <- read_parquet(file.path(Sys.getenv("READTHEFIELD_REPO_ROOT"), "data/cache/player_week_identity.parquet"));
  wr <- read_parquet(file.path(Sys.getenv("READTHEFIELD_REPO_ROOT"), "data/processed/wr_weekly_features.parquet"));
  joined <- inner_join(pwi, wr, by = c("player_id","season","week"));
  pos_col <- if ("position.x" %in% names(joined)) "position.x" else "position";
  joined <- joined[joined[[pos_col]] == "WR", , drop = FALSE];
  joined <- joined[order(joined$season, joined$week, decreasing = TRUE), , drop = FALSE];
  if (nrow(joined) == 0) { stop("No WR rows found for test.") }
  cat(joined$player_id[1], joined$season[1], joined$week[1])')"

echo "Selected WR player-week: $PID $SEASON $WEEK"

payload=$(cat <<EOF
{"player_id":"$PID","season":$SEASON,"week":$WEEK,"mode":"historical_replay","availability_policy":"played_only","schema_version":"v1","n_sims":500,"seed":4242}
EOF
)

resp=$(curl -s -X POST http://localhost:8000/simulate -H "Content-Type: application/json" -d "$payload")
echo "$resp" | jq '.ok, (.data.summary|length)'
ok=$(echo "$resp" | jq -r '.ok')
len=$(echo "$resp" | jq -r '.data.summary | length')
if [[ "$ok" != "true" || "$len" -le 0 ]]; then
  echo "Simulation failed"
  exit 1
fi
echo "Simulation OK"
