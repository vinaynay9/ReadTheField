# Build Future K Feature Row
#
# Constructs a synthetic feature row for future games using the last completed
# historical game for that player. Uses only historical data and updates
# opponent/home-away from schedules.

build_future_k_feature_row <- function(player_id,
                                       season,
                                       week,
                                       team,
                                       opponent,
                                       home_away,
                                       game_date = NULL) {
  if (missing(player_id) || is.null(player_id) || length(player_id) == 0) {
    stop("player_id is required for future feature row construction")
  }
  player_id <- as.character(player_id)
  season <- as.integer(season)
  week <- as.integer(week)
  team <- as.character(team)
  opponent <- as.character(opponent)
  home_away <- toupper(trimws(as.character(home_away)))
  if (!home_away %in% c("HOME", "AWAY")) {
    stop("home_away must be 'HOME' or 'AWAY', got: ", home_away)
  }

  if (!exists("read_k_weekly_features_cache")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R")
    }
  }
  k_features <- read_k_weekly_features_cache()
  if (nrow(k_features) == 0) {
    stop("K weekly features cache is empty. Cannot build future row.")
  }

  history <- k_features[
    k_features$player_id == player_id &
      k_features$season == season &
      k_features$week < week,
    , drop = FALSE
  ]
  if (nrow(history) == 0) {
    stop("No prior K history for player ", player_id, " in season ", season,
         " before week ", week, ". Cannot build future row.")
  }
  history <- history[order(history$week, history$gameday, decreasing = TRUE), , drop = FALSE]
  last_row <- history[1, , drop = FALSE]

  last_row$team <- team
  last_row$opponent <- opponent
  last_row$home_away <- home_away
  last_row$is_home <- ifelse(home_away == "HOME", 1L, 0L)
  last_row$week <- week
  last_row$season <- season
  last_row$gameday <- as.Date(NA)
  last_row$game_date <- as.Date(NA)

  last_row
}
