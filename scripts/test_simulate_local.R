options(READTHEFIELD_REPO_ROOT = Sys.getenv("READTHEFIELD_REPO_ROOT", "."))
source(file.path(getOption("READTHEFIELD_REPO_ROOT"), "api", "plumber.R"))

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

pwi <- read_parquet(file.path(getOption("READTHEFIELD_REPO_ROOT"), "data", "cache", "player_week_identity.parquet"))
wr <- read_parquet(file.path(getOption("READTHEFIELD_REPO_ROOT"), "data", "processed", "wr_weekly_features.parquet"))

joined <- inner_join(pwi, wr, by = c("player_id", "season", "week"))
pos_col <- if ("position.x" %in% names(joined)) "position.x" else "position"
joined <- joined[joined[[pos_col]] == "WR", , drop = FALSE]
joined <- joined[order(joined$season, joined$week, decreasing = TRUE), , drop = FALSE]
if (nrow(joined) == 0) {
  stop("No WR rows found for local simulate test.")
}

pid <- joined$player_id[1]
season <- joined$season[1]
week <- joined$week[1]

payload <- list(
  player_id = pid,
  season = season,
  week = week,
  mode = "historical_replay",
  availability_policy = "played_only",
  schema_version = "v1",
  n_sims = 500,
  seed = 4242
)

req <- list(postBody = jsonlite::toJSON(payload, auto_unbox = TRUE))
resp <- simulate_endpoint(req, list())

if (is.null(resp$ok) || isFALSE(resp$ok)) {
  stop("simulate_endpoint returned ok=FALSE")
}
if (is.null(resp$data$summary) || nrow(resp$data$summary) == 0) {
  stop("simulate_endpoint returned empty summary")
}

cat("Local simulate OK for", pid, season, week, "\n")
