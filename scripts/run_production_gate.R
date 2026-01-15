# Production Gate Orchestration Script
#
# Runs cache refresh, smoke tests, and representative simulations in a
# fail-fast, deterministic sequence.

if (file.exists("README.md") && file.exists("R") && file.exists("scripts")) {
  # Already in project root
} else {
  script_path <- commandArgs(trailingOnly = FALSE)
  if (length(script_path) > 0) {
    script_file <- sub("--file=", "", script_path[grep("--file=", script_path)])
    if (length(script_file) > 0 && file.exists(script_file)) {
      project_root <- dirname(dirname(normalizePath(script_file)))
      setwd(project_root)
    }
  }
}

options(warn = 1)

fail_fast <- function(msg) {
  cat("ERROR:", msg, "\n")
  quit(status = 1)
}

if (file.exists("R/simulation/warning_policy_v1.R")) {
  source("R/simulation/warning_policy_v1.R", local = TRUE)
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  seed <- 4242L
  n_sims <- 400L
  for (arg in args) {
    if (grepl("^--seed=", arg)) {
      seed <- as.integer(sub("^--seed=", "", arg))
    } else if (grepl("^--n_sims=", arg)) {
      n_sims <- as.integer(sub("^--n_sims=", "", arg))
    }
  }
  if (is.na(seed)) {
    fail_fast("Invalid --seed value.")
  }
  if (is.na(n_sims) || n_sims <= 0) {
    fail_fast("Invalid --n_sims value.")
  }
  list(seed = seed, n_sims = n_sims)
}

extract_warnings_from_output <- function(lines) {
  warnings <- character(0)
  capture_next <- FALSE
  for (line in lines) {
    if (capture_next) {
      warnings <- c(warnings, line)
      capture_next <- FALSE
      next
    }
    if (grepl("^Warning message:", line)) {
      capture_next <- TRUE
      next
    }
    if (grepl("^Warning in", line) || grepl("^Warning:", line)) {
      warnings <- c(warnings, line)
      next
    }
    if (grepl("glm\\.fit:", line)) {
      warnings <- c(warnings, line)
    }
  }
  warnings <- trimws(warnings)
  warnings[nzchar(warnings)]
}

handle_warning_lines <- function(lines, context) {
  if (!exists("is_allowed_warning_v1")) {
    return(invisible(NULL))
  }
  warnings <- extract_warnings_from_output(lines)
  if (length(warnings) == 0) return(invisible(NULL))
  for (msg in warnings) {
    if (is_allowed_warning_v1(msg)) {
      cat("Allowed warning [", context, "]: ", msg, "\n", sep = "")
    } else {
      fail_fast(paste0("Unexpected warning in ", context, ": ", msg))
    }
  }
}

run_rscript <- function(script_path, args = character(0)) {
  cat("Running:", script_path, "\n")
  output <- suppressWarnings(system2("Rscript", c(script_path, args), stdout = TRUE, stderr = TRUE))
  handle_warning_lines(output, script_path)
  if (length(output) > 0) {
    cat(paste(output, collapse = "\n"), "\n")
  }
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    fail_fast(paste0("Command failed: Rscript ", script_path))
  }
}

get_latest_season <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_integer_)
  seasons <- df$season
  seasons <- seasons[!is.na(seasons)]
  if (length(seasons) == 0) return(NA_integer_)
  max(as.integer(seasons))
}

pick_representative_row <- function(df, position, seed) {
  if (is.null(df) || nrow(df) == 0) {
    fail_fast(paste0(position, " weekly stats cache is empty."))
  }
  df$season <- as.integer(df$season)
  df$week <- as.integer(df$week)
  latest_season <- get_latest_season(df)
  if (is.na(latest_season)) {
    fail_fast(paste0("No valid seasons found for ", position, " stats cache."))
  }
  candidates <- df[
    df$season == latest_season &
      !is.na(df$player_id) &
      !is.na(df$season) &
      !is.na(df$week),
    , drop = FALSE
  ]
  if (nrow(candidates) == 0) {
    fail_fast(paste0("No candidate rows found for ", position, " in season ", latest_season, "."))
  }
  set.seed(seed)
  pick <- candidates[sample.int(nrow(candidates), size = 1), , drop = FALSE]
  pick
}

args <- parse_args()
cat("=== Production Gate ===\n")
cat("Seed:", args$seed, "\n")
cat("Representative sims:", args$n_sims, "draws\n\n")

cat("Step 1: Refresh weekly caches\n")
run_rscript("scripts/refresh_weekly_cache.R")
cat("  OK\n\n")

cat("Step 2: Smoke tests\n")
smoke_tests <- c(
  "scripts/smoke_test_rb_simulation.R",
  "scripts/smoke_test_wr_simulation.R",
  "scripts/smoke_test_te_simulation.R",
  "scripts/smoke_test_qb_simulation.R",
  "scripts/smoke_test_k_simulation.R"
)
for (test_script in smoke_tests) {
  run_rscript(test_script)
  cat("  OK:", test_script, "\n")
}
cat("\n")

cat("Step 3: Representative simulation (sanity check)\n")
tryCatch({
  source("R/simulation/bootstrap_simulation.R")
}, error = function(e) {
  fail_fast(paste0("Failed to load bootstrap: ", conditionMessage(e)))
})
if (!exists("simulate_player_game_v1")) {
  fail_fast("simulate_player_game_v1 not loaded. Check bootstrap_simulation.R.")
}

stats_df <- read_rb_weekly_stats_cache()
sim_seed <- args$seed + 10L
row <- pick_representative_row(stats_df, "RB", sim_seed)
player_id <- as.character(row$player_id[1])
season <- as.integer(row$season[1])
week <- as.integer(row$week[1])
cat("  Position: RB",
    "| seed:", sim_seed,
    "| player_id:", player_id,
    "| season:", season,
    "| week:", week, "\n")
result <- if (exists("handle_warnings_v1")) {
  handle_warnings_v1(
    simulate_player_game_v1(
      player_id = player_id,
      season = season,
      week = week,
      n_sims = args$n_sims,
      availability_policy = "played_only",
      seed = sim_seed
    ),
    context = "representative_RB"
  )
} else {
  simulate_player_game_v1(
    player_id = player_id,
    season = season,
    week = week,
    n_sims = args$n_sims,
    availability_policy = "played_only",
    seed = sim_seed
  )
}
if (is.null(result) || is.null(result$draws) || nrow(result$draws) != args$n_sims) {
  fail_fast("Representative simulation failed for position RB.")
}

cat("\nProduction gate completed successfully.\n")
quit(status = 0)
