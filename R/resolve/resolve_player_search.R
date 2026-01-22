# Resolve Player Search
#
# Returns candidate players for UI search by substring match.
# Uses cached player dimension table (data/processed/player_dim.parquet).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

#' Resolve player search results for UI
#'
#' @param query Character search string (e.g., "robinson")
#' @param season Integer season to filter
#' @param positions Character vector of positions to include
#' @return tibble with player_id, full_name, position, team, headshot_url
resolve_player_search <- function(query,
                                  season,
                                  team = NULL,
                                  positions = c("QB", "RB", "WR", "TE", "K")) {
  if (missing(query) || is.null(query) || !nzchar(trimws(query))) {
    stop("query is required for player search")
  }
  if (missing(season) || is.null(season) || is.na(season)) {
    stop("season is required for player search")
  }
  season <- as.integer(season)
  if (is.na(season)) {
    stop("season must be a valid integer")
  }
  
  if (!exists("read_player_dim_cache")) {
    if (file.exists("R/data/build_player_dim.R")) {
      source("R/data/build_player_dim.R", local = TRUE)
    } else {
      stop("Missing R/data/build_player_dim.R. Cannot load player_dim cache.")
    }
  }
  
  player_dim <- read_player_dim_cache()
  if (nrow(player_dim) == 0) {
    stop("player_dim cache is empty. Run scripts/refresh_weekly_cache.R to populate it.")
  }
  
  query_clean <- tolower(trimws(query))
  positions <- toupper(trimws(positions))
  team <- if (!is.null(team)) toupper(trimws(team)) else NULL
  
  matches <- player_dim %>%
    mutate(
      full_name_lower = tolower(full_name),
      last_name_lower = tolower(last_name)
    ) %>%
    filter(
      season == !!season,
      active == TRUE,
      position %in% positions,
      grepl(query_clean, full_name_lower, fixed = TRUE) |
        grepl(query_clean, last_name_lower, fixed = TRUE)
    )
  
  if (!is.null(team)) {
    matches <- matches %>% filter(team == !!team)
  }
  
  matches <- matches %>%
    select(player_id, full_name, position, team, headshot_url) %>%
    arrange(full_name, team)
  
  as_tibble(matches)
}
