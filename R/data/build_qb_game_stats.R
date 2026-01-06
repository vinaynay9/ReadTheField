# Build QB Game Stats
#
# Aggregates QB offensive stats to team-game level. This is used to build
# leakage-safe rolling QB context features for WR/TE pipelines.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Build QB team-game stats
#'
#' Aggregates QB passing stats to one row per (team, season, week).
#' Uses opponent-facing QB stats (offensive output) to characterize team QB strength.
#'
#' @param seasons Integer vector of seasons
#' @param season_type Season type (default "REG")
#' @return data.frame with QB team-game stats
build_qb_game_stats <- function(seasons, season_type = "REG") {
  if (missing(seasons) || length(seasons) == 0) {
    stop("seasons vector is required to build QB game stats")
  }

  if (!exists("load_weekly_player_stats_from_nflreadr") ||
      !exists("select_first_available") ||
      !exists("normalize_position")) {
    if (file.exists("R/data/build_weekly_player_layers.R")) {
      source("R/data/build_weekly_player_layers.R", local = TRUE)
    } else {
      stop("Missing R/data/build_weekly_player_layers.R for loading weekly player stats")
    }
  }

  stats <- load_weekly_player_stats_from_nflreadr(seasons, season_type)
  if (is.null(stats) || nrow(stats) == 0) {
    stop("No weekly player stats available for seasons: ", paste(seasons, collapse = ", "))
  }

  positions <- normalize_position(select_first_available(stats, c("position", "position_group"), NA))
  teams <- toupper(as.character(select_first_available(stats, c("team", "team_abbr", "recent_team", "team_name"), NA)))
  seasons_vec <- as.integer(stats$season)
  weeks <- as.integer(stats$week)

  # nflreadr weekly player stats (load_player_stats) commonly expose:
  # attempts/completions/passing_yards/passing_interceptions, with schema drift
  # across seasons. We use select_first_available to stay compatible.
  pass_attempts <- as.numeric(select_first_available(stats, c("attempts", "pass_attempts", "passing_attempts"), NA))
  pass_completions <- as.numeric(select_first_available(stats, c("completions", "pass_completions", "passing_completions"), NA))
  pass_yards <- as.numeric(select_first_available(stats, c("passing_yards", "pass_yards", "net_passing_yards"), NA))
  # Prefer explicit passing interceptions fields from nflreadr/player stats.
  # This represents QB-thrown INTs (offense-caused), not defense-caught totals.
  interceptions <- as.numeric(select_first_available(
    stats,
    c("passing_interceptions", "interceptions_thrown", "pass_interceptions", "interceptions"),
    NA
  ))
  # Sacks taken by the QB are exposed in nflreadr weekly player stats as
  # passing_sacks / sacks_taken (varies by season). This is QB-caused pressure.
  qb_sacks_taken <- as.numeric(select_first_available(
    stats,
    # nflreadr load_player_stats exposes sacks_suffered for QB sacks taken.
    c("sacks_suffered", "sacks_taken", "passing_sacks", "qb_sacked", "sacked", "sacks"),
    NA
  ))
  qb_rush_attempts <- as.numeric(select_first_available(
    stats,
    c("carries", "rush_attempts", "rushing_attempts"),
    NA
  ))
  qb_rush_yards <- as.numeric(select_first_available(
    stats,
    c("rushing_yards", "rush_yards"),
    NA
  ))
  qb_rush_tds <- as.numeric(select_first_available(
    stats,
    c("rushing_tds", "rush_tds"),
    NA
  ))

  qb_df <- data.frame(
    team = teams,
    season = seasons_vec,
    week = weeks,
    position = positions,
    pass_attempts = pass_attempts,
    pass_completions = pass_completions,
    pass_yards = pass_yards,
    interceptions_thrown = interceptions,
    sacks_taken = qb_sacks_taken,
    rush_attempts = qb_rush_attempts,
    rush_yards = qb_rush_yards,
    rush_tds = qb_rush_tds,
    stringsAsFactors = FALSE
  ) %>%
    filter(position == "QB", !is.na(team), team != "", !is.na(season), !is.na(week))

  if (nrow(qb_df) == 0) {
    stop("No QB rows found in weekly player stats. Cannot build QB game stats.")
  }

  # Multi-QB games: use the QB with the most pass attempts (starter proxy).
  # This yields one row per (team, season, week) without mixing multiple QBs.
  qb_primary <- qb_df %>%
    group_by(team, season, week) %>%
    arrange(
      desc(ifelse(is.na(pass_attempts), -Inf, pass_attempts)),
      desc(ifelse(is.na(pass_completions), -Inf, pass_completions)),
      desc(ifelse(is.na(pass_yards), -Inf, pass_yards)),
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    ungroup()

  qb_agg <- qb_primary %>%
    transmute(
      team,
      season,
      week,
      qb_pass_attempts = pass_attempts,
      qb_pass_completions = pass_completions,
      qb_pass_yards = pass_yards,
      qb_interceptions_thrown = interceptions_thrown,
      qb_sacks_taken = sacks_taken,
      qb_rush_attempts = rush_attempts,
      qb_rush_yards = rush_yards,
      qb_rush_tds = rush_tds
    ) %>%
    mutate(
      qb_completion_pct = ifelse(!is.na(qb_pass_attempts) & qb_pass_attempts > 0,
                                 qb_pass_completions / qb_pass_attempts, NA_real_),
      qb_yards_per_attempt = ifelse(!is.na(qb_pass_attempts) & qb_pass_attempts > 0,
                                    qb_pass_yards / qb_pass_attempts, NA_real_)
    )

  dupes <- duplicated(qb_agg[, c("team", "season", "week")])
  if (any(dupes)) {
    stop("Duplicate rows detected in QB game stats for (team, season, week)")
  }

  qb_agg
}
