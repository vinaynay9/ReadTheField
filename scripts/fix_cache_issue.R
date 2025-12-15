# Fix Cache Issue - Run this in RStudio
# This script will diagnose and fix the cache problem

cat("=== NFLverse Cache Fix Script ===\n\n")

# Step 1: Check/Install nflreadr
cat("Step 1: Checking nflreadr...\n")
if (!requireNamespace("nflreadr", quietly = TRUE)) {
  cat("  nflreadr not installed. Installing now...\n")
  install.packages("nflreadr", repos = "https://cloud.r-project.org")
  library(nflreadr)
  cat("  ✓ nflreadr installed successfully\n\n")
} else {
  library(nflreadr)
  cat("  ✓ nflreadr is already installed (version:", as.character(packageVersion("nflreadr")), ")\n\n")
}

# Step 2: Test download with current year (2024)
cat("Step 2: Testing download with 2024 season...\n")
tryCatch({
  test_data <- nflreadr::load_schedules(seasons = 2024)
  cat("  ✓ Successfully downloaded", nrow(test_data), "games for 2024\n\n")
}, error = function(e) {
  cat("  ✗ Download failed:", e$message, "\n")
  cat("  This might be a network/GitHub issue.\n\n")
  stop("Cannot proceed without download capability")
})

# Step 3: Load cache helpers
cat("Step 3: Loading cache helpers...\n")
if (file.exists("R/utils/cache_helpers.R")) {
  source("R/utils/cache_helpers.R")
  cat("  ✓ Cache helpers loaded\n\n")
} else {
  stop("Cannot find R/utils/cache_helpers.R")
}

# Step 4: Ensure cache directory exists
cat("Step 4: Ensuring cache directory exists...\n")
if (!dir.exists("data/cache")) {
  dir.create("data/cache", recursive = TRUE)
  cat("  ✓ Created data/cache directory\n\n")
} else {
  cat("  ✓ Cache directory exists\n\n")
}

# Step 5: Load and cache schedules (2024-2025)
cat("Step 5: Caching schedules...\n")
if (file.exists("R/data/load_schedules.R")) {
  source("R/data/load_schedules.R")
  
  # Cache both 2024 and 2025 together (more efficient)
  cat("  Caching 2024-2025 seasons...\n")
  tryCatch({
    schedules_all <- load_schedules(
      seasons = c(2024, 2025),
      use_cache = FALSE,
      write_cache = TRUE,
      retries = 5  # More retries for reliability
    )
    cat("  ✓ Cached", nrow(schedules_all), "total games\n")
    if (2024 %in% schedules_all$season) {
      cat("    - 2024:", sum(schedules_all$season == 2024), "games\n")
    }
    if (2025 %in% schedules_all$season) {
      cat("    - 2025:", sum(schedules_all$season == 2025), "games\n")
    }
    cat("\n")
  }, error = function(e) {
    cat("  ✗ Failed to cache schedules:", e$message, "\n")
    cat("  Trying individual seasons...\n")
    
    # Try 2024 separately
    tryCatch({
      schedules_2024 <- load_schedules(
        seasons = 2024,
        use_cache = FALSE,
        write_cache = TRUE,
        retries = 5
      )
      cat("  ✓ Cached", nrow(schedules_2024), "games for 2024\n")
    }, error = function(e2) {
      cat("  ✗ 2024 failed:", e2$message, "\n")
    })
    
    # Try 2025 separately
    tryCatch({
      schedules_2025 <- load_schedules(
        seasons = 2025,
        use_cache = FALSE,
        write_cache = TRUE,
        retries = 5
      )
      cat("  ✓ Cached", nrow(schedules_2025), "games for 2025\n\n")
    }, error = function(e2) {
      cat("  ✗ 2025 failed:", e2$message, "\n")
      cat("  This might be a network issue. Check your internet connection.\n\n")
    })
  })
  
} else {
  stop("Cannot find R/data/load_schedules.R")
}

# Step 6: Cache player stats
cat("Step 6: Caching player stats...\n")
if (file.exists("R/data/load_player_stats.R")) {
  source("R/data/load_player_stats.R")
  
  cat("  Caching RB stats for 2024-2025...\n")
  tryCatch({
    rb_stats <- load_rb_stats(
      seasons = c(2024, 2025),
      use_cache = FALSE,
      write_cache = TRUE,
      retries = 5
    )
    cat("  ✓ Cached", nrow(rb_stats), "RB player-game records\n")
    if (2024 %in% rb_stats$season) {
      cat("    - 2024:", sum(rb_stats$season == 2024), "records\n")
    }
    if (2025 %in% rb_stats$season) {
      cat("    - 2025:", sum(rb_stats$season == 2025), "records\n")
    }
  }, error = function(e) {
    cat("  ⚠ RB stats cache failed:", e$message, "\n")
    cat("  Trying 2024 only...\n")
    tryCatch({
      rb_stats_2024 <- load_rb_stats(
        seasons = 2024,
        use_cache = FALSE,
        write_cache = TRUE,
        retries = 5
      )
      cat("  ✓ Cached", nrow(rb_stats_2024), "RB records for 2024\n")
    }, error = function(e2) {
      cat("  ✗ 2024 RB stats also failed:", e2$message, "\n")
    })
  })
  
  # Try all player stats
  if (file.exists("R/data/load_all_player_stats.R")) {
    source("R/data/load_all_player_stats.R")
    cat("  Caching all player stats for 2024-2025...\n")
    tryCatch({
      all_stats <- load_all_player_stats(
        seasons = c(2024, 2025),
        use_cache = FALSE,
        write_cache = TRUE,
        retries = 5
      )
      cat("  ✓ Cached", nrow(all_stats), "player-game records\n")
      if (2024 %in% all_stats$season) {
        cat("    - 2024:", sum(all_stats$season == 2024), "records\n")
      }
      if (2025 %in% all_stats$season) {
        cat("    - 2025:", sum(all_stats$season == 2025), "records\n")
      }
      cat("\n")
    }, error = function(e) {
      cat("  ⚠ All player stats cache failed:", e$message, "\n")
      cat("  Trying 2024 only...\n")
      tryCatch({
        all_stats_2024 <- load_all_player_stats(
          seasons = 2024,
          use_cache = FALSE,
          write_cache = TRUE,
          retries = 5
        )
        cat("  ✓ Cached", nrow(all_stats_2024), "player records for 2024\n\n")
      }, error = function(e2) {
        cat("  ✗ 2024 player stats also failed:", e2$message, "\n\n")
      })
    })
  }
} else {
  cat("  ⚠ Skipping player stats cache (file not found)\n\n")
}

cat("=== Cache Fix Complete ===\n")
cat("You can now run scripts/run_bijan_simulation.R\n")
cat("The simulation is configured for 2025-12-11 (Bijan Robinson vs TB)\n")

