# Run RB Simulation CLI - Generalized
# TODO: Script name remains RB for legacy reasons; entrypoint is position-aware.
#
# Usage (Rscript):
#   Rscript scripts/run_rb_simulation_cli.R --player="Christian McCaffrey" --season=2024 --week=8 --n_sims=5000
#
# This script:
#   - Resolves player name using cached player directory
#   - Runs position-aware simulation using gsis_id only
#   - Prints player summary + technical diagnostics
#   - Writes output to player_simulation_output.txt

# Set working directory to project root
if (file.exists("README.md") && file.exists("R") && file.exists("scripts")) {
  cat("Working directory:", getwd(), "\n")
  cat("Project root verified.\n\n")
} else {
  project_root <- NULL
  
  tryCatch({
    script_path <- commandArgs(trailingOnly = FALSE)
    if (length(script_path) > 0) {
      script_file <- sub("--file=", "", script_path[grep("--file=", script_path)])
      if (length(script_file) > 0 && file.exists(script_file)) {
        project_root <- dirname(dirname(normalizePath(script_file)))
      }
    }
  }, silent = TRUE)
  
  if (is.null(project_root)) {
    if (file.exists("../README.md") && file.exists("../R") && file.exists("../scripts")) {
      project_root <- normalizePath("..")
    }
  }
  
  if (is.null(project_root)) {
    common_paths <- c(
      "C:/Users/vinay/ReadTheField",
      normalizePath("C:/Users/vinay/ReadTheField", mustWork = FALSE)
    )
    for (path in common_paths) {
      if (file.exists(path) &&
          file.exists(file.path(path, "README.md")) &&
          file.exists(file.path(path, "R")) &&
          file.exists(file.path(path, "scripts"))) {
        project_root <- path
        break
      }
    }
  }
  
  if (is.null(project_root) || !file.exists(project_root)) {
    cat("ERROR: Cannot automatically detect project root.\n")
    cat("Current working directory:", getwd(), "\n")
    cat("\nPlease run one of the following:\n")
    cat("  1. setwd('C:/Users/vinay/ReadTheField')\n")
    cat("  2. Or navigate to the project folder in RStudio\n")
    cat("  3. Or run: setwd(dirname(rstudioapi::getActiveDocumentContext()$path))\n")
    cat("\nThen run: source('scripts/run_rb_simulation_cli.R')\n")
    stop("Project root not found. Please set working directory first.")
  }
  
  setwd(project_root)
  cat("Working directory set to:", getwd(), "\n")
  cat("Project root verified.\n\n")
}
options(warn = 1)

main <- function() {
  cat("Loading simulation bootstrap...\n")
  
  tryCatch({
    source("R/simulation/bootstrap_simulation.R")
    cat("  - bootstrap_simulation.R loaded\n")
  }, error = function(e) {
    stop("Failed to load simulation bootstrap: ", conditionMessage(e))
  })
  
  tryCatch({
    source("R/simulation/print_player_simulation.R")
    cat("  - print_player_simulation.R loaded\n")
  }, error = function(e) {
    stop("Failed to load print_player_simulation.R: ", conditionMessage(e))
  })
  
  tryCatch({
    source("R/simulation/print_rb_simulation.R")
    cat("  - print_rb_simulation.R loaded\n")
  }, error = function(e) {
    stop("Failed to load print_rb_simulation.R: ", conditionMessage(e))
  })
  
  tryCatch({
    source("R/simulation/write_rb_simulation.R")
    cat("  - write_rb_simulation.R loaded\n")
  }, error = function(e) {
    stop("Failed to load write_rb_simulation.R: ", conditionMessage(e))
  })
  
  cat("All functions loaded successfully.\n\n")
  
  # ========================================================================
  # ARGUMENT PARSING
  # ========================================================================
  target_player_query <- NA_character_
  target_season <- NA_integer_
  target_week <- NA_integer_
  n_sims <- 5000L
  availability_policy <- "played_only"
  counterfactual_mode <- FALSE
  counterfactual_team <- NA_character_
  
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    for (arg in args) {
      if (grepl("^--player=", arg)) {
        target_player_query <- sub("^--player=", "", arg)
      } else if (grepl("^--season=", arg)) {
        target_season <- as.integer(sub("^--season=", "", arg))
      } else if (grepl("^--week=", arg)) {
        target_week <- as.integer(sub("^--week=", "", arg))
      } else if (grepl("^--n_sims=", arg)) {
        n_sims <- as.integer(sub("^--n_sims=", "", arg))
      } else if (grepl("^--availability_policy=", arg)) {
        availability_policy <- sub("^--availability_policy=", "", arg)
      } else if (grepl("^--counterfactual_mode=", arg)) {
        counterfactual_mode <- tolower(sub("^--counterfactual_mode=", "", arg)) %in% c("true", "1", "yes")
      } else if (grepl("^--counterfactual_team=", arg)) {
        counterfactual_team <- toupper(sub("^--counterfactual_team=", "", arg))
      }
    }
  }
  
  if (is.na(target_player_query) || !nzchar(trimws(target_player_query))) {
    stop("Missing required --player argument (player name).")
  }
  if (is.na(target_season)) {
    stop("Missing required --season argument.")
  }
  if (is.na(target_week)) {
    stop("Missing required --week argument.")
  }
  if (is.na(n_sims) || n_sims <= 0) {
    stop("Invalid --n_sims value. Must be a positive integer.")
  }
  availability_policy <- validate_availability_policy(availability_policy)
  if (isTRUE(counterfactual_mode) && (is.na(counterfactual_team) || !nzchar(counterfactual_team))) {
    stop("--counterfactual_team is required when --counterfactual_mode=TRUE.")
  }
  
  cat("========================================\n")
  cat("Running search for:", target_player_query, "| season", target_season, "week", target_week, "| sims", n_sims, "\n")
  cat("Availability policy:", availability_policy, "\n")
  if (is_counterfactual_policy(availability_policy)) {
    cat("NOTE: Counterfactual simulation enabled.\n")
  }
  if (isTRUE(counterfactual_mode)) {
    cat("Counterfactual roster team:", counterfactual_team, "\n")
  }
  cat("========================================\n\n")
  
  # ========================================================================
  # Player search (cache-based)
  # ========================================================================
  if (!exists("read_player_directory_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R for player directory access.")
    }
  }
  
  player_dir <- read_player_directory_cache()
  if (nrow(player_dir) == 0) {
    stop("Player directory cache is empty. Run scripts/refresh_weekly_cache.R first.")
  }
  
  canonicalize_name <- function(x) {
    x <- as.character(x)
    x <- tolower(x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    x[nchar(x) == 0] <- NA_character_
    x
  }
  
  query_canon <- canonicalize_name(target_player_query)
  if (is.na(query_canon)) {
    stop("Player query is empty after normalization.")
  }
  
  player_dir$canonical_name <- if ("canonical_name" %in% names(player_dir)) {
    as.character(player_dir$canonical_name)
  } else {
    canonicalize_name(player_dir$full_name)
  }
  
  exact_matches <- player_dir[player_dir$canonical_name == query_canon, , drop = FALSE]
  if (nrow(exact_matches) == 0) {
    partial_matches <- player_dir[grepl(query_canon, player_dir$canonical_name, fixed = TRUE), , drop = FALSE]
  } else {
    partial_matches <- exact_matches
  }
  
  if (nrow(partial_matches) == 0) {
    stop("No matching players found for query '", target_player_query, "'.")
  }
  
  if (nrow(partial_matches) > 1) {
    preview <- partial_matches[seq_len(min(10, nrow(partial_matches))), , drop = FALSE]
    candidates <- paste0(preview$full_name, " (", preview$player_id, ")")
    stop(
      "Ambiguous player name '", target_player_query, "'. Matches found: ",
      paste(candidates, collapse = "; "),
      if (nrow(partial_matches) > 10) paste0(" ... and ", nrow(partial_matches) - 10, " more.") else ""
    )
  }
  
  chosen <- partial_matches[1, ]
  chosen_gsis_id <- chosen$player_id
  chosen_name <- chosen$full_name
  
  player_team <- NA_character_
  player_position <- "RB"
  if (exists("read_player_dim_cache")) {
    player_dim <- read_player_dim_cache()
    dim_row <- player_dim[player_dim$gsis_id == chosen_gsis_id & player_dim$season == target_season, , drop = FALSE]
    if (nrow(dim_row) > 0) {
      player_team <- dim_row$team[1]
      player_position <- dim_row$position[1]
    }
    if (is.na(player_team) && nrow(player_dim) > 0) {
      last_dim <- player_dim[player_dim$gsis_id == chosen_gsis_id, , drop = FALSE]
      if (nrow(last_dim) > 0) {
        last_dim <- last_dim[order(last_dim$season, decreasing = TRUE), , drop = FALSE]
        player_team <- last_dim$team[1]
        player_position <- last_dim$position[1]
      }
    }
  }
  
  if (is.na(player_team)) {
    cat("Selected: ", chosen_name, " (", player_position, ")\n", sep = "")
  } else {
    cat("Selected: ", chosen_name, " (", player_position, " ", player_team, ")\n", sep = "")
  }

  # ========================================================================
  # Schedule resolution (game_id/game_date/home/away/opponent)
  # ========================================================================
  if (!exists("load_schedules")) {
    if (file.exists("R/data/load_schedules.R")) {
      source("R/data/load_schedules.R", local = TRUE)
    } else {
      stop("Missing R/data/load_schedules.R for schedule resolution.")
    }
  }
  schedule_team <- if (isTRUE(counterfactual_mode)) counterfactual_team else player_team
  if (is.na(schedule_team) || !nzchar(schedule_team)) {
    stop("Unable to resolve team for schedule lookup. Check player_dim or pass --counterfactual_team.")
  }
  schedules <- load_schedules(seasons = target_season, cache_only = TRUE)
  if (nrow(schedules) == 0) {
    stop("Schedule cache is empty for season ", target_season, ".")
  }
  sched_match <- schedules[
    schedules$season == target_season &
      schedules$week == target_week &
      (schedules$home_team == schedule_team | schedules$away_team == schedule_team),
    , drop = FALSE
  ]
  if (nrow(sched_match) == 0) {
    stop("No schedule entry found for team ", schedule_team, " season ", target_season, " week ", target_week, ".")
  }
  if (nrow(sched_match) > 1) {
    stop("Multiple schedule entries found for team ", schedule_team, " season ", target_season, " week ", target_week, ".")
  }
  sched_row <- sched_match[1, ]
  schedule_game_id <- as.character(sched_row$game_id)
  schedule_game_date <- sched_row$gameday
  schedule_home_away <- if (sched_row$home_team == schedule_team) "HOME" else "AWAY"
  schedule_opponent <- if (sched_row$home_team == schedule_team) sched_row$away_team else sched_row$home_team
  if (is.na(schedule_game_id) || !nzchar(schedule_game_id)) {
    stop("Schedule resolution returned missing game_id for team ", schedule_team, ".")
  }
  
  # ========================================================================
  # LAYER 1: Pure Computation
  # ========================================================================
  result <- simulate_player_game(
    gsis_id = chosen_gsis_id,
    season = target_season,
    week = target_week,
    n_sims = n_sims,
    availability_policy = availability_policy,
    counterfactual_mode = counterfactual_mode,
    counterfactual_team = counterfactual_team,
    schedule_game_id = schedule_game_id,
    schedule_game_date = schedule_game_date,
    schedule_home_away = schedule_home_away,
    schedule_opponent = schedule_opponent
  )

  if (!is.null(result$status) && identical(result$status, "error")) {
    cat("ERROR: Simulation failed\n")
    cat("  Type:", result$error_type, "\n")
    cat("  Player:", if (!is.null(result$player_name)) result$player_name else "(unknown)", "\n")
    cat("  Season/Week:", result$season, "Week", result$week, "\n")
    cat("  Reason:", result$reason, "\n")
    if (isTRUE(result$counterfactual_allowed) && !isTRUE(counterfactual_mode)) {
      cat("\nCounterfactual option available:\n")
      cat("  Re-run with --counterfactual_mode=TRUE --counterfactual_team=<TEAM>\n")
      cat("  This simulates the player as healthy on the selected roster.\n")
    }
    stop("Simulation failed; see details above.")
  }
  
  # ========================================================================
  # LAYER 2: Presentation
  # ========================================================================
  print_player_simulation(result)
  print_rb_simulation(result)
  
  # ========================================================================
  # LAYER 3: Persistence
  # ========================================================================
  write_rb_simulation(result, "player_simulation_output.txt", overwrite = TRUE)
  cat("Output saved to: player_simulation_output.txt\n")
}

exit_code <- 0L
tryCatch(
  {
    main()
    exit_code <- 0L
  },
  error = function(e) {
    message("ERROR: ", conditionMessage(e))
    exit_code <<- 1L
  }
)
quit(status = exit_code)
