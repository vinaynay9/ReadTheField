# Run RB Simulation - Generic Player Game Simulation
#
# This script runs a date-verified and opponent-verified simulation for
# any RB player's game, with full input validation.
# 
# Configuration: Set target_player_name, target_date, target_away_team, target_home_team
# Example: Bijan Robinson, 2025-12-11, ATL @ TB
#
# Architecture:
#   Layer 1: run_rb_simulation() - pure computation (no printing, no files)
#   Layer 2: print_rb_simulation() - presentation (console output)
#   Layer 3: write_rb_simulation() - persistence (file writing)

# Set working directory to project root
# This script tries multiple methods to find the project root automatically

# Method 1: Check if we're already in project root
if (file.exists("README.md") && file.exists("R") && file.exists("scripts")) {
  # Already in project root - do nothing
  cat("Working directory:", getwd(), "\n")
  cat("Project root verified.\n\n")
} else {
  # Method 2: Try to detect from script location (when run via Rscript)
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
  
  # Method 3: Check parent directory
  if (is.null(project_root)) {
    if (file.exists("../README.md") && file.exists("../R") && file.exists("../scripts")) {
      project_root <- normalizePath("..")
    }
  }
  
  # Method 4: Try common project root path
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
  
  # If still not found, prompt user
  if (is.null(project_root) || !file.exists(project_root)) {
    cat("ERROR: Cannot automatically detect project root.\n")
    cat("Current working directory:", getwd(), "\n")
    cat("\nPlease run one of the following:\n")
    cat("  1. setwd('C:/Users/vinay/ReadTheField')\n")
    cat("  2. Or navigate to the project folder in RStudio\n")
    cat("  3. Or run: setwd(dirname(rstudioapi::getActiveDocumentContext()$path))\n")
    cat("\nThen run: source('scripts/run_bijan_simulation.R')\n")
    stop("Project root not found. Please set working directory first.")
  }
  
  setwd(project_root)
  cat("Working directory set to:", getwd(), "\n")
  cat("Project root verified.\n\n")
}

# Source required functions with error handling
cat("Loading required functions...\n")
tryCatch({
  source("R/data/load_schedules.R")
  cat("  - load_schedules.R loaded\n")
}, error = function(e) {
  stop("Failed to load load_schedules.R: ", conditionMessage(e))
})

tryCatch({
  source("R/data/load_player_stats.R")
  cat("  - load_player_stats.R loaded\n")
}, error = function(e) {
  stop("Failed to load load_player_stats.R: ", conditionMessage(e))
})

tryCatch({
  source("R/features/build_rb_features.R")
  cat("  - build_rb_features.R loaded\n")
}, error = function(e) {
  stop("Failed to load build_rb_features.R: ", conditionMessage(e))
})

tryCatch({
  source("R/utils/rolling_helpers.R")
  cat("  - rolling_helpers.R loaded\n")
}, error = function(e) {
  stop("Failed to load rolling_helpers.R: ", conditionMessage(e))
})

tryCatch({
  source("R/assemble/assemble_rb_training_data.R")
  cat("  - assemble_rb_training_data.R loaded\n")
}, error = function(e) {
  stop("Failed to load assemble_rb_training_data.R: ", conditionMessage(e))
})

tryCatch({
  source("R/models/fit_rb_models.R")
  cat("  - fit_rb_models.R loaded\n")
}, error = function(e) {
  stop("Failed to load fit_rb_models.R: ", conditionMessage(e))
})

tryCatch({
  source("R/simulation/simulate_rb_game.R")
  cat("  - simulate_rb_game.R loaded\n")
}, error = function(e) {
  stop("Failed to load simulate_rb_game.R: ", conditionMessage(e))
})

tryCatch({
  source("R/utils/ppr_scoring.R")
  cat("  - ppr_scoring.R loaded\n")
}, error = function(e) {
  stop("Failed to load ppr_scoring.R: ", conditionMessage(e))
})

# Source the new three-layer architecture functions
tryCatch({
  source("R/simulation/run_rb_simulation.R")
  cat("  - run_rb_simulation.R loaded\n")
}, error = function(e) {
  stop("Failed to load run_rb_simulation.R: ", conditionMessage(e))
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

# ============================================================================
# CONFIGURATION: Set target player and game
# ============================================================================
# These can be changed to simulate any player/game
target_player_name_patterns <- c("B.Robinson", "Bijan Robinson")  # Try these name patterns
target_date <- as.Date("2025-12-11")
target_away_team <- "ATL"
target_home_team <- "TB"

# Extract season and week from date (will be validated by run_rb_simulation)
# For now, we'll pass the date and let run_rb_simulation find the game
# But we need to determine which team is home/away
# Based on the configuration: ATL @ TB means ATL is away, TB is home

# ============================================================================
# LAYER 1: Pure Computation
# ============================================================================
# Run simulation exactly ONCE - no printing, no file writing

result <- run_rb_simulation(
  player_name = target_player_name_patterns,
  team = target_away_team,  # Player's team
  opponent = target_home_team,  # Opponent team
  season = 2025,  # Will be validated against game_date
  week = 15,  # Will be validated against game_date
  n_sims = 5000,
  game_date = target_date
)

# ============================================================================
# LAYER 2: Presentation
# ============================================================================
# Print results to console

print_rb_simulation(result)

# ============================================================================
# LAYER 3: Persistence
# ============================================================================
# Write results to file (overwrites if exists)

write_rb_simulation(result, "rb_simulation_output.txt", overwrite = TRUE)

cat("Output saved to: rb_simulation_output.txt\n")
