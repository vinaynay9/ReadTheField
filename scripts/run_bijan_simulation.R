# Run Player Game Simulation - Fully Automated
#
# This script runs a fully automated simulation for any player's game.
# Simply specify player name and game date - everything else is auto-detected!
# 
# Configuration: Set target_player_name and target_date
# Example: Bijan Robinson, 2025-12-11
#
# The system will automatically:
#   - Detect player position (RB/WR/TE/QB/K)
#   - Detect player's team
#   - Detect opponent team
#   - Detect season and week
#   - Route to appropriate position-specific simulation
#
# Architecture:
#   Layer 1: simulate_player_game() - fully automated (auto-detects everything)
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
cat("Loading simulation bootstrap...\n")

# Source the bootstrap (loads all simulation dependencies)
tryCatch({
  source("R/simulation/bootstrap_simulation.R")
  cat("  - bootstrap_simulation.R loaded\n")
}, error = function(e) {
  stop("Failed to load simulation bootstrap: ", conditionMessage(e))
})

# Source the presentation and persistence functions
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

# ============================================================================
# CONFIGURATION: Set target player and game
# ============================================================================
# Simply specify player name and game date - everything else is auto-detected!
target_player_name <- "Bijan Robinson"
target_season <- 2024
target_week <- 8

# ============================================================================
# LAYER 1: Pure Computation - Fully Automated
# ============================================================================
# Run simulation - auto-detects position, team, opponent, season, week

result <- simulate_player_game(
  player_name = target_player_name,
  season = target_season,
  week = target_week,
  n_sims = 5000
)

# ============================================================================
# LAYER 2: Presentation
# ============================================================================
# Print player-specific results (clean stats-focused output)

print_player_simulation(result)

# Print technical diagnostics (process details)
print_rb_simulation(result)

# ============================================================================
# LAYER 3: Persistence
# ============================================================================
# Write results to file (overwrites if exists)

write_rb_simulation(result, "rb_simulation_output.txt", overwrite = TRUE)

cat("Output saved to: rb_simulation_output.txt\n")


