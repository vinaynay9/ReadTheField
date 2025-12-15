# Test nflreadr schedules loading
# Run this to diagnose schedule download issues

cat("=== Testing nflreadr::load_schedules ===\n\n")

library(nflreadr)

# Test 1: Load all schedules
cat("Test 1: Loading all available schedules...\n")
tryCatch({
  all_schedules <- nflreadr::load_schedules()
  cat("  Success! Loaded", nrow(all_schedules), "total games\n")
  if (nrow(all_schedules) > 0) {
    available_seasons <- sort(unique(all_schedules$season))
    cat("  Available seasons:", paste(available_seasons, collapse = ", "), "\n")
    cat("  2024 games:", sum(all_schedules$season == 2024), "\n")
    cat("  2025 games:", sum(all_schedules$season == 2025), "\n\n")
  }
}, error = function(e) {
  cat("  ERROR:", e$message, "\n\n")
})

# Test 2: Load specific season 2024
cat("Test 2: Loading 2024 season specifically...\n")
tryCatch({
  sched_2024 <- nflreadr::load_schedules(seasons = 2024)
  cat("  Success! Loaded", nrow(sched_2024), "games for 2024\n\n")
}, error = function(e) {
  cat("  ERROR:", e$message, "\n\n")
})

# Test 3: Load specific season 2025
cat("Test 3: Loading 2025 season specifically...\n")
tryCatch({
  sched_2025 <- nflreadr::load_schedules(seasons = 2025)
  cat("  Success! Loaded", nrow(sched_2025), "games for 2025\n\n")
}, error = function(e) {
  cat("  ERROR:", e$message, "\n\n")
})

# Test 4: Load both 2024 and 2025
cat("Test 4: Loading 2024 and 2025 together...\n")
tryCatch({
  sched_both <- nflreadr::load_schedules(seasons = c(2024, 2025))
  cat("  Success! Loaded", nrow(sched_both), "games total\n")
  if (nrow(sched_both) > 0) {
    cat("  2024:", sum(sched_both$season == 2024), "games\n")
    cat("  2025:", sum(sched_both$season == 2025), "games\n\n")
  }
}, error = function(e) {
  cat("  ERROR:", e$message, "\n\n")
})

cat("=== Test Complete ===\n")
cat("If all tests show 0 rows, there may be a nflreadr version issue.\n")
cat("Try: update.packages('nflreadr')\n")

