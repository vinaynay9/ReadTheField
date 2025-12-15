# Refresh NFLverse Cache
#
# One-time script to populate local cache of nflverse data.
# Run this when online to create local cache files for offline use.
#
# Usage:
#   Rscript scripts/refresh_nflverse_cache.R
#
# Or from R console:
#   source("scripts/refresh_nflverse_cache.R")

# Set working directory to project root
# Assumes script is run from project root or scripts/ directory
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Load required functions
if (!exists("load_schedules")) {
  if (file.exists("R/data/load_schedules.R")) {
    source("R/data/load_schedules.R")
  } else {
    stop("Cannot find R/data/load_schedules.R")
  }
}

if (!exists("load_rb_stats")) {
  if (file.exists("R/data/load_player_stats.R")) {
    source("R/data/load_player_stats.R")
  } else {
    stop("Cannot find R/data/load_player_stats.R")
  }
}

if (!exists("load_all_player_stats")) {
  if (file.exists("R/data/load_all_player_stats.R")) {
    source("R/data/load_all_player_stats.R")
  } else {
    stop("Cannot find R/data/load_all_player_stats.R")
  }
}

# Load cache helpers
if (!exists("save_cache")) {
  if (file.exists("R/utils/cache_helpers.R")) {
    source("R/utils/cache_helpers.R")
  } else {
    stop("Cannot find R/utils/cache_helpers.R")
  }
}

# Determine season range (1999 to current year)
current_year <- as.integer(format(Sys.Date(), "%Y"))
seasons_to_cache <- 1999:current_year

cat("Refreshing NFLverse cache...\n")
cat("Season range:", min(seasons_to_cache), "to", max(seasons_to_cache), "\n\n")

# Cache schedules
cat("Caching schedules...\n")
tryCatch({
  schedules <- load_schedules(
    seasons = seasons_to_cache,
    use_cache = FALSE,
    write_cache = TRUE,
    retries = 3
  )
  
  if (nrow(schedules) == 0) {
    stop("No schedule data loaded")
  }
  
  cat("  Cached", nrow(schedules), "games\n")
}, error = function(e) {
  stop("Failed to cache schedules: ", e$message, 
       "\n\nIf you're offline or GitHub is unavailable, this script cannot run.",
       "\nPlease run this script when online to create the cache.")
})

# Cache RB stats
cat("Caching RB stats...\n")
tryCatch({
  rb_stats <- load_rb_stats(
    seasons = seasons_to_cache,
    use_cache = FALSE,
    write_cache = TRUE,
    retries = 3
  )
  
  if (nrow(rb_stats) == 0) {
    warning("No RB stats loaded, but continuing...")
  } else {
    cat("  Cached", nrow(rb_stats), "RB player-game records\n")
  }
}, error = function(e) {
  warning("Failed to cache RB stats: ", e$message, 
          "\nSchedules cached successfully, but RB stats cache failed.")
})

# Cache all player stats (for position detection)
cat("Caching all player stats (for position detection)...\n")
tryCatch({
  all_stats <- load_all_player_stats(
    seasons = seasons_to_cache,
    use_cache = FALSE,
    write_cache = TRUE,
    retries = 3
  )
  
  if (nrow(all_stats) == 0) {
    warning("No player stats loaded, but continuing...")
  } else {
    cat("  Cached", nrow(all_stats), "player-game records\n")
  }
}, error = function(e) {
  warning("Failed to cache all player stats: ", e$message, 
          "\nOther caches successful, but all player stats cache failed.")
})

cat("\nDone. Cached files saved to data/cache/\n")

