options(READTHEFIELD_REPO_ROOT = Sys.getenv("READTHEFIELD_REPO_ROOT"))

if (file.exists("R/simulation/simulation_mode_policy.R")) {
  source("R/simulation/simulation_mode_policy.R", local = TRUE)
} else {
  stop("Missing R/simulation/simulation_mode_policy.R")
}

schedules <- data.frame(
  season = rep(2025L, 18),
  week = 1:18,
  home_team = rep("AAA", 18),
  away_team = rep("BBB", 18),
  stringsAsFactors = FALSE
)

fallback <- resolve_postseason_fallback(
  mode = "upcoming_game",
  target_season = 2025L,
  target_week = 20L,
  schedules = schedules
)

if (!isTRUE(fallback$apply)) {
  stop("Expected postseason fallback to apply for week 20.")
}
if (!identical(as.integer(fallback$effective_season), 2025L)) {
  stop("Expected effective_season to remain 2025.")
}
if (!identical(as.integer(fallback$effective_week), 18L)) {
  stop("Expected effective_week to be 18.")
}

cat("Postseason fallback OK\n")
