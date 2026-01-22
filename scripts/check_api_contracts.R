#!/usr/bin/env Rscript

repo_root <- Sys.getenv("READTHEFIELD_REPO_ROOT")
if (!nzchar(repo_root)) {
  repo_root <- getwd()
}
options(READTHEFIELD_REPO_ROOT = repo_root)

pass <- TRUE
report <- function(label, ok, details = "") {
  status <- if (ok) "PASS" else "FAIL"
  cat(status, "-", label)
  if (nzchar(details)) cat(":", details)
  cat("\n")
  if (!ok) pass <<- FALSE
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  report("arrow available", FALSE, "package missing")
  quit(status = 1)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  report("dplyr available", FALSE, "package missing")
  quit(status = 1)
}

library(arrow)
library(dplyr)

pd_path <- file.path(repo_root, "data", "cache", "player_directory.parquet")
pwi_path <- file.path(repo_root, "data", "cache", "player_week_identity.parquet")

if (!file.exists(pd_path)) {
  report("player_directory present", FALSE, "missing")
  quit(status = 1)
}
if (!file.exists(pwi_path)) {
  report("player_week_identity present", FALSE, "missing")
  quit(status = 1)
}

pd <- read_parquet(pd_path)
pwi <- read_parquet(pwi_path)

pd <- as.data.frame(pd)
pwi <- as.data.frame(pwi)

pct_name_is_id <- mean(pd$player_name == pd$player_id, na.rm = TRUE)
report("player_name not id-like", pct_name_is_id <= 0.10, paste0("pct_name_is_id=", round(pct_name_is_id, 4)))

orphans <- nrow(anti_join(distinct(pwi, player_id), distinct(pd, player_id), by = "player_id"))
report("player_directory covers all PWI ids", orphans == 0, paste0("orphans=", orphans))

if (!requireNamespace("plumber", quietly = TRUE)) {
  report("plumber available", FALSE, "package missing")
  quit(status = 1)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  report("jsonlite available", FALSE, "package missing")
  quit(status = 1)
}

source(file.path(repo_root, "api", "plumber.R"))

resp <- tryCatch(players_endpoint(), error = function(e) e)
if (inherits(resp, "error")) {
  report("/players handler", FALSE, conditionMessage(resp))
} else {
  ok_flag <- is.list(resp) && isTRUE(resp$ok)
  players <- NULL
  if (ok_flag && is.list(resp$data) && !is.null(resp$data$players)) {
    players <- resp$data$players
  }
  players_len <- 0
  if (is.data.frame(players)) {
    players_len <- nrow(players)
  } else if (is.list(players)) {
    players_len <- length(players)
  }
  report("/players contract ok", ok_flag, "ok flag")
  report("/players non-empty", players_len > 0, paste0("players_len=", players_len))
}

quit(status = if (pass) 0 else 1)
